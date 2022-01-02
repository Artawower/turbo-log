;;; turbo-log.el --- The simple package for fast log selected region                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/turbo-log
;; Package-Requires: ((emacs "25.1") (seq "2.21") (string-inflection "1.0.16"))
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
(require 'seq)
(require 'tree-sitter)
(require 'evil)
(require 'tramp)
(require 'string-inflection)

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

(defcustom turbo-log-buffer-name-format-template "[%s] "
  "Template for formatting buffer name.
Will not be visible when its nil."
  :group 'turbo-log
  :type 'string)

(defcustom turbo-log-argument-divider ","
  "Divider for list of arguments."
  :group 'turbo-log
  :type 'string)

(defconst turbo-log--default-ecmascript-config
  '(:loggers ("console.log(%s)" "console.debug(%s)" "console.warn(%s)")
    :jump-list ((class_declaration (method_definition "constructor")))
    :msg-format-template "'TCL: %s'"
    :message-node-types (identifier member_expression))
  "Common configurations for ecmascript`s based modes.")

(defconst turbo-log--message-node-types nil
  "List of nodes that will be printed from current line.
When not provided entire region will be printed.")

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
    (emacs-lisp-mode (:loggers (("(message %s)" " %s")) :comment-string ";;" :argument-divider ""))
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

(defconst turbo-log--block-statements '(block program statement_block source_file :)
  "Nodes that indicate about new block statement.")

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
  `(or (when ,logger-config (plist-get ,logger-config (symbol-value (intern (seq-concatenate 'string ":" (symbol-name ',key))))))
       (turbo-log--symbol-value-or-nil (intern (seq-concatenate 'string "turbo-log-" (symbol-name ',key))))
       (turbo-log--symbol-value-or-nil (intern (seq-concatenate 'string "turbo-log--" (symbol-name ',key))))))

(defun turbo-log--find-top-level-node (node &optional logger-config)
  "Find top level structure for current NODE.
Could return function class program statement_block or nil.
When LOGGER-CONFIG provided top-level structure will be used from plist."

  (let ((current-node node)
        (current-type (tsc-node-type node))
        (top-level-structure (turbo-log--get-logger-config logger-config top-level-structures)))
    (while (not (member current-type top-level-structure))
      (setq current-node (tsc-get-parent current-node))
      (when current-node
        (setq current-type (tsc-node-type current-node))))
    `(,current-node ,current-type)))

(defun turbo-log--node-next-line-insert-p (node-type logger-config)
  "Return t when its possible to insert logger next line by NODE-TYPE.
LOGGER-CONFIG - configuration of current logger."
  (member node-type (turbo-log--get-logger-config logger-config nodes-allowed-insert-next-line)))

(defun turbo-log--node-previous-line-insert-p (node-type logger-config)
  "Return t when its possible to insert logger next line by NODE-TYPE.
LOGGER-CONFIG - configuration of current logger."
  (member node-type (turbo-log--get-logger-config logger-config nodes-allowed-insert-previous-line)))

(defun turbo-log--node-ignored-p (node-type logger-config)
  "Check by NODE-TYPE, is it possible to use current node as log position.
LOGGER-CONFIG - configuration of current logger."
  (member node-type (turbo-log--get-logger-config logger-config nodes-ignored)))

(defun turbo-log--goto-line (line-number)
  "Move cursor to provided LINE-NUMBER."
  (forward-line (- line-number (line-number-at-pos))))

(defun turbo-log--format-meta-info (line-number)
  "Format meta information by provided config.
Insert LINE-NUMBER and buffer name."

  (let* ((line-number (if turbo-log-line-number-format-template (format turbo-log-line-number-format-template line-number)))
         (real-buffer-name (if (file-remote-p default-directory)
                               (tramp-file-name-localname (tramp-dissect-file-name (buffer-name)))
                             (buffer-name)))
         (buffer-name (if turbo-log-buffer-name-format-template (format turbo-log-buffer-name-format-template real-buffer-name))))
    (concat line-number buffer-name)))

(defun turbo-log--get-line-text (line-number)
  "Get text from LINE-NUMBER under point."
  (turbo-log--goto-line line-number)
  (thing-at-point 'line))

(defun turbo-log--find-next-block-statement (parent-node &optional logger-config)
  "Find next block statement from current line by PARENT-NODE.
LOGGER-CONFIG - configuration of current logger."

  (let* ((cursor (tsc-make-cursor parent-node))
         (current-node parent-node)
         (current-type (tsc-node-type current-node))
         (cursor-res t)
         (block-statement-nodes (turbo-log--get-logger-config logger-config block-statements)))

    (while (or (not (member current-type block-statement-nodes)) (not cursor-res))
      (setq cursor-res (cond
                        ((tsc-goto-next-sibling cursor) t)
                        ((tsc-goto-first-child cursor) t)
                        ((progn (tsc-goto-parent cursor)
                                (tsc-goto-next-sibling cursor)) t)
                        (t nil)))

      (setq current-node (tsc-current-node cursor))

      (when current-node
        (setq current-type (tsc-node-type current-node))))

    (goto-char (tsc-node-start-position current-node))
    (+ (line-number-at-pos) 1)))

(defun turbo-log--node-end-position-insert-p (node-type parent-node &optional logger-config)
  "Return t when NODE-TYPE and PARENT-NODE need to be inserted at end of node.
LOGGER-CONFIG - configuration of current logger."
  (when (member node-type (turbo-log--get-logger-config logger-config nodes-for-end-position-inserting))
    (goto-char (tsc-node-end-position parent-node))
    (+ (line-number-at-pos) 1)))

(defun turbo-log--find-parent-jump-line-number (parent-node &optional logger-config)
  "Find insert position.
Called when jump line position for PARENT-NODE provided inside LOGGER-CONFIG."
  (save-excursion
    (let* ((logger-jump-list (turbo-log--get-logger-config logger-config jump-list))
           (current-node parent-node)
           (jump-literal-name (when logger-jump-list (assoc (tsc-node-type parent-node) logger-jump-list)))
           (jump-literal-name (when jump-literal-name (car (cdr jump-literal-name))))
           (search-literal (when jump-literal-name (car jump-literal-name)))
           ;; Add search name
           ;; (search-name (when logger-jump-list (car (cdr-safe jump-literal-name))))
           (cursor (tsc-make-cursor parent-node))
           insert-position (cursor-res t))


      (goto-char (point-min)) ;; NOTE: cause constructor and other literals could be above relative to current line

      (while (and (not insert-position) cursor-res logger-jump-list)
        (setq cursor-res (cond
                          ((tsc-goto-next-sibling cursor) t)
                          ((tsc-goto-first-child cursor) t)
                          ((progn (tsc-goto-parent cursor)
                                  (tsc-goto-next-sibling cursor)) t)
                          (t nil)))
        (setq current-node (tsc-current-node cursor))
        (when (eq (tsc-node-type current-node) (tsc-node-type parent-node))
          (setq cursor-res nil))

        (when (eq (tsc-node-type current-node) search-literal)
          (goto-char (tsc-node-start-position current-node))
          (setq insert-position (+ (line-number-at-pos) 1))))
      insert-position)))

(defun turbo-log--get-insert-line-number-by-node-type (node-type parent-node &optional logger-config)
  "Return line number for insertion by current NODE-TYPE and PARENT-NODE.
LOGGER-CONFIG - configuration of current logger."

  (let* ((jump-line-number (turbo-log--find-parent-jump-line-number parent-node logger-config)))
    (cond (jump-line-number jump-line-number)
          ((turbo-log--node-end-position-insert-p node-type parent-node logger-config))
          ((turbo-log--node-next-line-insert-p node-type logger-config) (+ (line-number-at-pos) 1))
          ((turbo-log--node-previous-line-insert-p node-type logger-config) (line-number-at-pos))
          ((turbo-log--node-ignored-p node-type logger-config) nil)
          (t (turbo-log--find-next-block-statement parent-node)))))

(defun turbo-log--insert-with-indent (line-number texts)
  "Insert every messages from TEXTS alists.
Every text will be put at new line relative LINE-NUMBER"
  (turbo-log--goto-line line-number)
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
           (msg-template (turbo-log--get-logger-config logger-config msg-format-template))
           (payload-format-template (turbo-log--get-logger-config logger-config payload-format-template))
           (meta-info (turbo-log--format-meta-info insert-line-number))
           (log-info-text (format msg-template (concat meta-info
                                                       (format payload-format-template log-message)
                                                       variable-format-template)))
           (argument-divider (turbo-log--get-logger-config logger-config argument-divider))
           (log-msg (format logger (concat log-info-text argument-divider " " log-message)))
           (close-bracket (if include-close-bracket-p "\n}" ""))
           (log-msgs `(,log-msg ,close-bracket))
           (post-insert-hooks (turbo-log--get-logger-config logger-config post-insert-hooks)))

      (turbo-log--goto-line insert-line-number)
      (insert "\n")
      (turbo-log--insert-with-indent insert-line-number log-msgs)

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

(defun turbo-log--find-insert-line-number (logger-config)
  "Find insert position by LOGGER-CONFIG."
  (save-excursion
    (let* ((node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))
           (parent-node-type-pair (turbo-log--find-top-level-node node logger-config))
           (parent-node (car parent-node-type-pair))
           (parent-node-type (car (cdr parent-node-type-pair)))
           (insert-line-number (turbo-log--get-insert-line-number-by-node-type parent-node-type parent-node logger-config)))
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

(defun turbo-log--extract-message-node-types (message-node-types divider)
  "Extract every nodes from current line that contained by MESSAGE-NODE-TYPES.
Result will be a string, divdded by DIVIDER."

  (save-excursion
    (beginning-of-line-text)
    (let* ((max-line-point (point-at-eol))
           (current-node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))
           (cursor-res t)
           (identifiers '()))

      (while cursor-res
        ;; FIXME: navigation by ts-tree doesn't work properly
        (setq cursor-res (re-search-forward "[[:blank:]\n]+" nil t))
        (setq current-node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))
        (message "node type: %s\nvar: %s" (tsc-node-type current-node) (buffer-substring (tsc-node-start-position current-node) (tsc-node-end-position current-node)))

        (when current-node
          (when (> (tsc-node-start-position current-node) max-line-point)
            (setq cursor-res nil))

          (when (and (member (tsc-node-type current-node) message-node-types) (tsc-node-named-p current-node))
            (push (buffer-substring (tsc-node-start-position current-node) (tsc-node-end-position current-node)) identifiers))))

      (when identifiers
        (string-join (delete-dups identifiers) divider)))))

;;;###autoload
(defun turbo-log-print (&optional insert-immediately-p past-from-clipboard-p)
  "Log selected region for current major mode.
Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?
INSERT-IMMEDIATELY-P - should insert first available logger?"
  (interactive)

  (if (or (bound-and-true-p tree-sitter-mode) turbo-log-allow-insert-without-tree-sitter-p)
      (let* ((logger-config (car (cdr (assoc major-mode turbo-log-loggers))))
             (divider (turbo-log--get-logger-config logger-config argument-divider))
             (message-node-types (turbo-log--get-logger-config logger-config message-node-types))
             (log-message (turbo-log--get-log-text past-from-clipboard-p))
             (extracted-log-message (when (and message-node-types (not (region-active-p)))
                                      (turbo-log--extract-message-node-types message-node-types divider)))
             (log-message (or extracted-log-message log-message))
             (insert-line-number (cond (past-from-clipboard-p (line-number-at-pos))
                                       ((not (bound-and-true-p tree-sitter-mode)) (+ (line-number-at-pos) 1))
                                       (t (turbo-log--find-insert-line-number logger-config))))
             (previous-line-empty-body-p (and insert-line-number (turbo-log--line-with-empty-body-p (- insert-line-number 1)) (not past-from-clipboard-p))))


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
