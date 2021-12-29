;;; turbo-log.el --- The simple package for fast log selected region                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/turbo-log
;; Package-Requires: ((emacs "24.4") (tree-sitter "0.16.1"))
;; Version: 2.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality for fast line/region logging with additional meta information
;; like line number, buffer name, and some info from syntax table.
;; Out of the box it works with golang, python, js and typescript languages.
;; This package needs the https://emacs-tree-sitter.github.io/tree-sitter-mode/


;;; Code:

;;;; Variables
(require 'subr-x)
(require 'simple)
(require 'tree-sitter)

(defcustom turbo-log-msg-format-template "\"TCL: %s\""
  "Template for formatting entire log message."
  :group 'turbo-log
  :type 'string)

(defcustom turbo-log-payload-format-template "%s: "
  "Template for formatting payload info (current selected region or clipboard)."
  :group 'turbo-log
  :type 'string)

(defcustom turbo-log-line-number-format-template "[line %s]"
  "Template for formatting line number.
Line number will not be shown when value is nil."
  :group 'turbo-log
  :type 'string)

(defcustom turbo-log-buffer-name-format-template "[%s]"
  "Template for formatting buffer name.
Will not be visible when its nil."
  :group 'turbo-log
  :type 'string)

(defconst turbo-log--default-ecmascript-config
  '(:loggers ("console.log(%s)" "console.debug(%s)" "console.warn(%s)")
    :msg-format-template "'TCL: %s'")
  "Common configurations for ecmascript`s based modes.")

(defcustom turbo-log-loggers
  `((typescript-mode ,turbo-log--default-ecmascript-config)
    (js-mode ,turbo-log--default-ecmascript-config)
    (js2-mode ,turbo-log--default-ecmascript-config)
    (typescript-tsx-mode ,turbo-log--default-ecmascript-config)
    (rjsx-mode ,turbo-log--default-ecmascript-config)
    (ng2-ts-mode ,turbo-log--default-ecmascript-config)
    (rjsx-mode ,turbo-log--default-ecmascript-config)
    (web-mode ,turbo-log--default-ecmascript-config)
    (vue-mode ,turbo-log--default-ecmascript-config)
    (rust-mode (:loggers ("println!(%s);")))
    (rustic-mode (:loggers ("println!(%s);" "{}")))
    (python-mode (:loggers ("print(%s)") :comment-string "#"))
    (emacs-lisp-mode (:loggers (("(message %s)" " %s")) :comment-string ";;"))
    (go-mode (:loggers ("fmt.Println(%s)"
                        ("fmt.Printf(%s)" " %v")))))
  "Mode/config pairs."
  :group 'turbo-log
  :type '(alist (symbol (plist :key-type symbol :value-type (alist :value-type (group string))))))

(defcustom turbo-log-allow-insert-without-tree-sitter-p nil
  "Allow insert logger when tree-sitter is disabled.
In such case log line will be inserted next line."
  :group 'turbo-log
  :type 'boolean)


(defconst turbo-log--top-level-structures
  '(function class program statement_block return_statement if_statement class_declaration
             source_file block array variable_declarator function_declaration assignment
             function_definition short_var_declaration def arrow_function method_definition expression_statement)
  "Top level structure for detecting paste place.")

(defconst turbo-log--nodes-for-end-position-inserting '(array variable_declarator assignment short_var_declaration expression_statement)
  "Node types for inserting logger after end position.")

(defconst turbo-log--nodes-allowed-insert-next-line '(statement_block block arrow_function)
  "Node types that allow to insert logger next line.")

(defconst turbo-log--nodes-allowed-insert-previous-line '(return_statement if_statement)
  "Node types that allow to insert logger next line.")

(defconst turbo-log--nodes-ignored '(class_declaration)
  "Nodes that not allowed to be inserted as logger.")

(defconst turbo-log--comment-string "//"
  "Common string for find comments.")

(defun turbo-log--symbol-value-or-nil (symbol)
  "Return value by SYMBOL if exist, if not - return nil."
  (if (boundp symbol)
      (symbol-value symbol)
    nil))

(defmacro turbo-log--get-logger-config (logger-config key)
  "Magic macros for extract config from LOGGER-CONFIG plist by KEY.
When value is nil config will be taken from global scope.
I know that it's a magic. And huge evil. But i like it."
  `(or (plist-get ,logger-config (symbol-value (intern (concatenate 'string ":" (symbol-name ',key)))))
       (turbo-log--symbol-value-or-nil (intern (concatenate 'string "turbo-log-" (symbol-name ',key))))
       (turbo-log--symbol-value-or-nil (intern (concatenate 'string "turbo-log--" (symbol-name ',key))))))

(defun turbo-log--find-top-level-node (node)
  "Find top level structure for current NODE.
Could return function class program statement_block or nil."
  (let ((current-node node)
        (current-type (tsc-node-type node)))
    (while (not (member current-type turbo-log--top-level-structures))

      (setq current-node (tsc-get-parent current-node))
      (message "%s" (tsc-node-type current-node))
      (when current-node
        (setq current-type (tsc-node-type current-node))))
    `(,current-node ,current-type)))

(defun turbo-log--node-next-line-insert-p (node-type)
  "Return t when its possible to insert logger next line by NODE-TYPE."
  (member node-type turbo-log--nodes-allowed-insert-next-line))

(defun turbo-log--node-previous-line-insert-p (node-type)
  "Return t when its possible to insert logger next line by NODE-TYPE."
  (member node-type turbo-log--nodes-allowed-insert-previous-line))

(defun turbo-log--node-ignored-p (node-type)
  "Check by NODE-TYPE, is it possible to use current node as log position."
  (member node-type turbo-log--nodes-ignored))

(defun turbo-log--goto-line (line-number)
  "Move cursor to provided LINE-NUMBER."
  (forward-line (- line-number (line-number-at-pos))))

;; TODO: replace to build enitre log content
(defun turbo-log--format-meta-info (line-number)
  "Format meta information by provided config.
Insert LINE-NUMBER and buffer name."

  (let ((line-number (if turbo-log-line-number-format-template (format turbo-log-line-number-format-template line-number)))
        (buffer-name (if turbo-log-buffer-name-format-template (format turbo-log-buffer-name-format-template (buffer-name)))))
    (concat line-number buffer-name)))

(defun turbo-log--get-line-text (line-number)
  "Get text from LINE-NUMBER under point."
  (turbo-log--goto-line line-number)
  (thing-at-point 'line))

(defun turbo-log--find-next-block-statement (parent-node)
  "Find next block statement from current line by PARENT-NODE."
  (message (concat "\n" (make-string 120 ?')))
  (let* ((cursor (tsc-make-cursor parent-node))
         (current-node parent-node)
         (current-type (tsc-node-type current-node))
         (cursor-res t))

    (while (or (not (member current-type '(block program statement_block source_file :))) (not cursor-res))

      (setq cursor-res (cond
                        ((tsc-goto-next-sibling cursor) t)
                        ((tsc-goto-first-child cursor) t)
                        ((progn (tsc-goto-parent cursor)
                                (tsc-goto-next-sibling cursor)) t)
                        (t nil)))

      (setq current-node (tsc-current-node cursor))

      (message "[%s] | %s: %s" current-type current-node (tsc-node-end-position current-node))
      (when current-node
        (setq current-type (tsc-node-type current-node))))

    (goto-char (tsc-node-start-position current-node))
    (+ (line-number-at-pos) 1)))

(defun turbo-log--node-end-position-insert-p (node-type parent-node)
  "Return t when NODE-TYPE and PARENT-NODE need to be inserted at end of node."
  (when (member node-type turbo-log--nodes-for-end-position-inserting)
    (goto-char (tsc-node-end-position parent-node))
    (+ (line-number-at-pos) 1)))

(defun turbo-log--get-insert-line-number-by-node-type (node-type parent-node)
  "Return line number for insertion by current NODE-TYPE and PARENT-NODE."
  (cond ((turbo-log--node-end-position-insert-p node-type parent-node))
        ((turbo-log--node-next-line-insert-p node-type) (+ (line-number-at-pos) 1))
        ((turbo-log--node-previous-line-insert-p node-type) (line-number-at-pos))
        ((turbo-log--node-ignored-p node-type) nil)
        (t (turbo-log--find-next-block-statement parent-node))))

;; TODO: check its need?
(defun turbo-log--insert-with-indent (line-number texts)
  "Insert every messages from TEXTS alists.
Every text will be put at new line relative LINE-NUMBER"
  (turbo-log--goto-line line-number)
  ;; (end-of-line)
  ;; (newline-and-indent)
  (let ((start-selection (point)))
    (dolist (text texts)
      (when text
        (insert text)
        (setq line-number (+ line-number 1))))
    (indent-region start-selection (point))))

(defun turbo-log--insert-logger-by-mode (logger-config
                                         insert-line-number
                                         log-message
                                         &optional
                                         force-select-first-logger-p
                                         include-close-bracket-p)
  "Insert LOG-MESSAGE by LOGGER-CONFIG into INSERT-LINE-NUMBER.
When FORCE-SELECT-FIRST-LOGGER-P is true first logger will be selected.
Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?
when INCLUDE-CLOSE-BRACKET-P is t \n} will be inserted after log message"

  (save-excursion
    (let* ((loggers (plist-get logger-config :loggers))
           (logger (if (or force-select-first-logger-p (eq (length loggers) 1))
                       (or (car-safe (car loggers)) (car loggers))
                     (completing-read "Choose logger: " loggers)))
           (variable-format-template (or (nth 1 (assoc logger loggers)) ""))
           ;; (qwe (progn
           ;;        (message "Selected logger: %s | %s, %s > logger-config: %s" (type-of logger) logger consistent-logger-config-p logger-config)))
           (msg-template (turbo-log--get-logger-config logger-config msg-format-template))
           (payload-format-template (turbo-log--get-logger-config logger-config payload-format-template))
           (meta-info (turbo-log--format-meta-info insert-line-number))
           (log-info-text (format msg-template (concat meta-info
                                                       (format payload-format-template log-message)
                                                       variable-format-template)))
           (log-msg (format logger (concat log-info-text ", " log-message)))
           (close-bracket (if include-close-bracket-p "\n}" ""))
           (log-msgs `(,log-msg ,close-bracket))
           (post-insert-hooks (plist-get logger-config :post-insert-hooks)))

      ;; (message "logger-typs: %s\n logger: %s\n loggers-type:%s\n loggers: %s\n"
      ;;          (type-of logger)
      ;;          logger
      ;;          (type-of loggers)
      ;;          loggers)

      (turbo-log--goto-line insert-line-number)
      (insert "\n")
      (turbo-log--insert-with-indent insert-line-number log-msgs)

      (message "LOGGERS META: %s" logger-config)
      (message "%s : === %s" log-msgs include-close-bracket-p)
      ;; TODO: separated func for hook call
      (dolist (hook post-insert-hooks)
        (when (functionp hook)
          (funcall hook))))))

(defun turbo-log--get-log-text (&optional past-from-clipboard-p)
  "Return text that should be inserted inside logger.
When PAST-FROM-CLIPBOARD-P provided it will be inserted from clipboard."
  (string-trim (if past-from-clipboard-p
                   (current-kill 0 t)
                 (buffer-substring (region-beginning) (region-end)))))

(defun turbo-log--get-real-point ()
  "Get real point.
Cause evil change `point' function value by 1
inside `region-p'"   (if (and (bound-and-true-p evil-mode) (region-active-p))
                         (- (point) 1)
                       (point)))

(defun turbo-log--line-with-empty-body-p (line-number)
  "Return t when LINE-NUMBER line is function with empty body."
  (save-excursion
    (string-match "[^\']+{[[:blank:]]*}[$ ]*" (turbo-log--get-line-text line-number))))

(defun turbo-log--find-insert-line-number ()
  "Find insert position."
  (save-excursion
    (let* ((node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))
           (parent-node-type-pair (turbo-log--find-top-level-node node))
           (parent-node (car parent-node-type-pair))
           (parent-node-type (car (cdr parent-node-type-pair)))
           (insert-line-number (turbo-log--get-insert-line-number-by-node-type parent-node-type parent-node)))

      (message (make-string 80 ?-))
      (message "Result: %s" parent-node-type)
      (message "Insert pos: %s" insert-line-number)
      (message (make-string 80 ?|))
      insert-line-number)))

(defun turbo-log--remove-closed-bracket (line-number)
  "Remove } from end of line at LINE-NUMBER position."
  (save-excursion
    (turbo-log--goto-line (- line-number 1))
    (beginning-of-line)
    (search-forward-regexp "}[[:blank:]]*")
    (replace-match "")))

(defun turbo-log--normilize-regexp (regexp)
  "Screen REGEXP string."
  (dolist (p '(("/" "\\/")
               ("[" "\\\[")
               ("]" "\\\]")
               ("\"" "\\\"")
               ("." "\.")))
    (setq regexp (string-replace (car p) (cadr p) regexp)))
  regexp)

(defun turbo-log--build-log-regexp (comment-type)
  "Build regexp for finding loggers made by turbo-log.
COMMENT-TYPE - type of comment, could be `commented' `uncommented' and `both'"
  (let* ((logger-meta (car (cdr (assoc major-mode turbo-log-loggers))))
         (loggers (mapcar
                   (lambda (v) (if (consp v) (car v) v))
                   (plist-get logger-meta :loggers)))
         (comment-string (turbo-log--get-logger-config logger-meta comment-string))
         (safety-comment-string (string-replace "/" "\\/" comment-string))
         (msg-template (turbo-log--normilize-regexp (turbo-log--get-logger-config logger-meta msg-format-template)))
         (line-number-formatter (turbo-log--get-logger-config logger-meta line-number-format-template))
         (line-number-info (and line-number-formatter (format (turbo-log--normilize-regexp line-number-formatter) "[0-9]+")))
         (log-message (format msg-template (concat line-number-info "[^'\"]+")))

         (log-prefix (cond ((eq comment-type 'uncommented) "^[[:blank:]]+")
                           ((eq comment-type 'commented) (concat "^[[:blank:]]+" safety-comment-string "[[:blank:]]+"))
                           ((eq comment-type 'both) (concat "^[[:blank:]]+" (format "\\(%s\\)?" safety-comment-string) "[[:blank:]]*"))))
         (regexps '()))

    (dolist (l loggers)
      (push (concat log-prefix (format (turbo-log--normilize-regexp l) (concat log-message "[^\'\(\)]+")) ";?") regexps))

    (unless logger-meta (message "Sorry, turbo-log is not available for %s." major-mode))
    (string-join regexps "\\|")))

(defun turbo-log--handle-comments (comment-type func)
  "Apply operation for found line with logs.
COMMENT-TYPE - type of comment, could be `commented' `uncommented' and `both'
FUNC - function that will accept start and end point of found log line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((start-pos))
      (while (setq start-pos (re-search-forward (turbo-log--build-log-regexp comment-type) nil t))
        (funcall func (match-beginning 0) start-pos)
        (forward-line)))))

;;;###autoload
(defun turbo-log-print (&optional insert-immediately-p past-from-clipboard-p)
  "Log selected region for current major mode.
Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?
INSERT-IMMEDIATELY-P - should insert first available logger?"
  (interactive)
  ;; TODO: debug only
  ;; (save-window-excursion
  ;;   (switch-to-buffer "*Messages*")
  ;;   (erase-buffer))

  (if (or (bound-and-true-p tree-sitter-mode) turbo-log-allow-insert-without-tree-sitter-p)
      (let* ((logger-config (car (cdr (assoc major-mode turbo-log-loggers))))
             (insert-line-number (cond (past-from-clipboard-p (line-number-at-pos))
                                       ((not (bound-and-true-p tree-sitter-mode)) (+ (line-number-at-pos) 1))
                                       (t (turbo-log--find-insert-line-number))))
             (previous-line-empty-body-p (and insert-line-number (turbo-log--line-with-empty-body-p (- insert-line-number 1)) (not past-from-clipboard-p)))
             (log-message (turbo-log--get-log-text past-from-clipboard-p)))

        (unless logger-config
          (message "Turbo-log: No configuration provided for %s mode" major-mode))

        (when (and insert-line-number logger-config)
          (when previous-line-empty-body-p (turbo-log--remove-closed-bracket insert-line-number))
          (turbo-log--insert-logger-by-mode logger-config insert-line-number log-message insert-immediately-p previous-line-empty-body-p)))
    (message "For turbo-log package you need to enable tree-sitter-mode.")))

;;;###autoload
(defun turbo-log-print-immediately ()
  "Log selected region for current major mode without ask a logger from list.
Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?"
  (interactive)
  (turbo-log-print t nil))

;;;###autoload
(defun turbo-log-paste-as-logger ()
  "Past text from clipboard as logged text."
  (interactive)
  (end-of-line)
  (forward-line)
  (turbo-log-print nil t)
  (end-of-line))

;;;###autoload
(defun turbo-log-paste-as-logger-immediately ()
  "Past text from clipboard as logged text immediately."
  (interactive)
  (end-of-line)
  (forward-line)
  (turbo-log-print t t)
  (end-of-line))

;;;###autoload
(defun turbo-log-comment-all-logs ()
  "Hide all log messages made by turbo-log."
  (interactive)
  (turbo-log--handle-comments 'uncommented 'comment-or-uncomment-region))

;;;###autoload
(defun turbo-log-uncomment-all-logs ()
  "Hide all log messages made by turbo-log."
  (interactive)
  (turbo-log--handle-comments 'commented 'comment-or-uncomment-region))

;;;###autoload
(defun turbo-log-delete-all-logs ()
  "Hide all log messages made by turbo-log."
  (interactive)
  (turbo-log--handle-comments 'both (lambda (start-point end-point)
                                      (delete-region start-point (+ end-point 1)))))
(provide 'turbo-log)
;;; turbo-log.el ends here
