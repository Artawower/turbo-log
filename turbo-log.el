
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

;;; Code:

;;;; Variables
(require 'subr-x)
(require 'simple)
(require 'tree-sitter)

(defcustom turbo-log-prefix "TCL: "
  "Prefix string for every log messages."
  :group 'turbo-log
  :type 'string)

(setq turbo-log--default-ecmascript-config
  '(:loggers ("console.log(%s)" "console.debug(%s)" "console.warn(%s)")
    :post-insert-hooks (prettier-prettify lsp)))

(setq turbo-log-loggers
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
    (python-mode (:loggers ("print(%s)")))
    (go-mode (:loggers ("fmt.Println(%s)"
                        ("fmt.Printf(%s)" "%v"))))))
  ;; "Mode/config pairs."
  ;; :group 'turbo-log
  ;; :type 'string)

(defcustom turbo-log-include-buffer-name t
  "Include current buffer name to log message."
  :group 'turbo-log
  :type 'boolean)


;;;; Common functions
;; ;;;###autoload
;; (defun turbo-log-print (&optional past-from-clipboard-p)
;;   "Log selected region for current major mode.
;; Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?"
;;   (interactive)

;;   )

;; ;;;###autoload
;; (defun turbo-log-print-immediately (&optional past-from-clipboard-p)
;;   "Log selected region for current major mode without ask a logger from list.
;; Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?"
;;   (interactive)
;;   (save-excursion
;;     (let* ((logger-list (turbo-log--choose-mode))
;;            (logger (cdr logger-list)))
;;       (if logger
;;           (turbo-log--handle-logger logger nil past-from-clipboard-p)))))

;; ;;;###autoload
;; (defun turbo-log-comment-all-logs ()
;;   "Hide all log messages made by turbo-log."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward-regexp (turbo-log--build-log-regexp 'uncommented) nil t)
;;       (turbo-log--handle-log-line 'turbo-log--toggle-current-line-comment)
;;       (search-forward ")" nil t)
;;       (forward-line))))

;; ;;;###autoload
;; (defun turbo-log-uncomment-all-logs ()
;;   "Show all comments made by turbo-log."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward-regexp (turbo-log--build-log-regexp 'commented) nil t)
;;       (turbo-log--handle-log-line 'turbo-log--toggle-current-line-comment)
;;       (search-forward ")" nil t)
;;       (forward-line))))

;; ;;;###autoload
;; (defun turbo-log-delete-all-logs ()
;;   "Delete all turbo-log loggers."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward-regexp (turbo-log--build-log-regexp 'both) nil t)
;;       (turbo-log--handle-log-line 'kill-whole-line))))

;; ;;;###autoload
;; (defun turbo-log-paste-as-logger ()
;;   "Past text from clipboard as logged text."
;;   (interactive)
;;   (end-of-line)
;;   (turbo-log-print t)
;;   (forward-line)
;;   (end-of-line))

;; ;;;###autoload
;; (defun turbo-log-paste-as-logger-immediately ()
;;   "Past text from clipboard as logged text immediately."
;;   (interactive)
;;   (end-of-line)
;;   (turbo-log-print-immediately t)
;;   (forward-line)
;;   (end-of-line))

(setq turbo-log--top-level-structures
      '(function class program statement_block return_statement if_statement class_declaration
                 source_file block array expression_list variable_declarator function_declaration assignment
                 function_definition short_var_declaration def arrow_function method_definition))
;; "Top level structure for detecting paste place.")

(setq turbo-log--nodes-for-end-position-inserting '(array expression_list variable_declarator assignment short_var_declaration))
;; "Node types for inserting logger after end position.")

(setq turbo-log--nodes-allowed-insert-next-line '(statement_block block arrow_function))
;; "Node types that allow to insert logger next line.")

(defconst turbo-log--nodes-allowed-insert-previous-line '(return_statement if_statement)
  "Node types that allow to insert logger next line.")

(defconst turbo-log--nodes-ignored '(class_declaration)
  "Nodes that not allowed to be inserted as logger.")

(defun turbo-log--find-top-level-node (node)
  "Find top level structure for current NODE. Could return
function class program statement_block or nil."
  (let ((cursor (tsc-make-cursor node))
        (current-node node)
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

(defun turbo-log--format-meta-info (line-number)
  "Format meta information by provided config.
Insert LINE-NUMBER and buffer name."

  (let ((line-number (concat "[line " (format "%s" line-number) "]")))
    (if turbo-log--include-buffer-name
        (concat line-number "[" (buffer-name) "] " turbo-log--prefix " ")
      (concat line-number " " turbo-log--prefix " "))))

(defun turbo-log--find-next-block-statement (parent-node)
  "Find next block statement from current line by PARENT-NODE."
  (message (concat "\n" (make-string 120 ?')))
  (let ((cursor (tsc-make-cursor parent-node))
        (current-node parent-node)
        (current-type (tsc-node-type node))
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

    (message (concat (make-string 120 ?') "\n"))
    ;; TODO: remote it
    (message "FOUND FINISH SMB: %s | POS: [%s - %s]" (tsc-node-type current-node) (tsc-node-start-position current-node) (tsc-node-end-position current-node))
    ;; (message "INSERTED LINE NUMBER: %s" (- (line-number-at-pos) 1))
    (goto-char (tsc-node-start-position current-node))
    (+ (line-number-at-pos) 1)))

(defun turbo-log--node-end-position-insert-p (node-type parent-node)
  "Return t when node type is variable declaration."
  (when (member node-type turbo-log--nodes-for-end-position-inserting)
    (goto-char (tsc-node-end-position parent-node))
    (+ (line-number-at-pos) 1)))

(defun turbo-log--get-insert-position-by-node-type (node-type parent-node)
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

(defun turbo-log--insert-logger-by-mode (insert-position log-message &optional force-select-first-logger-p)
  "Insert LOG-MESSAGE by mode into INSERT-POSITION.
When FORCE-SELECT-FIRST-LOGGER-P is true first logger will be selected.
Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?"

  (save-excursion
    (let* ((logger-meta (car (cdr (assoc major-mode turbo-log-loggers))))
           (loggers (plist-get logger-meta :loggers))
           (logger-config (if force-select-first-logger-p
                              (car loggers)
                            (completing-read "Choose logger: " loggers)))
           (consistent-logger-config-p (consp logger-config))
           (logger (if consistent-logger-config-p
                       (car logger-config)
                     logger-config))
           (format-string (if consistent-logger-config-p
                              (car (cdr logger-config))
                            ""))
           (max-line-length (plist-get logger-meta :max-length))
           ;; (multiline-p (and max-line-length (> (length log-message) max-line-length)))
           ;; (line-break (if multiline-p "\n" ""))
           (meta-info (turbo-log--format-meta-info insert-position))
           (content-between-brackets (concat "\"" meta-info
                                             log-message ": \", "))

           (turbo-log-message (format logger (concat content-between-brackets log-message)))
           (turbo-log-messages `(,turbo-log-message))
           (post-insert-hooks (plist-get logger-meta :post-insert-hooks)))
           ;; (turbo-log-messages
           ;;  `(,(concat logger "(" line-break)
           ;;    ,(concat "\"" content-between-brackets line-break)
           ;;    ;; TODO: normalized code here
           ;;    ,(concat log-message line-break)
           ;;    ,(concat ")" ))))

      (message "Selected logger: %s | %s" logger-meta logger)
      (turbo-log--goto-line insert-position)
      (insert "\n")
      (turbo-log--insert-with-indent insert-position turbo-log-messages)

      ;; TODO: separated func for hook call
      (dolist (hook post-insert-hooks)
        (when (functionp hook)
          (funcall hook)))
      )))


(defun turbo-log--get-log-text (&optional past-from-clipboard-p)
  "Return text that should be inserted inside logger.
When PAST-FROM-CLIPBOARD-P provided it will be inserted from clipboard."
  (string-trim (if past-from-clipboard-p
                   (current-kill 0 t)
                 (buffer-substring (region-beginning) (region-end)))))

(defun turbo-log--get-real-point ()
  "Get real point. Cause evil change `point' function value by 1
inside `region-p'"
  (if (and (bound-and-true-p evil-mode) (region-active-p))
      (- (point) 1)
    (point)))

(defun turbo-log--find-insert-position ()
  "Find insert position."
  (when (bound-and-true-p tree-sitter-mode)
    (save-excursion
      (let* ((node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))
             (parent-node-type-pair (turbo-log--find-top-level-node node))
             (parent-node (car parent-node-type-pair))
             (parent-node-type (car (cdr parent-node-type-pair)))
             (insert-position (turbo-log--get-insert-position-by-node-type parent-node-type parent-node)))



        (message (make-string 80 ?-))
        (message "Result: %s" parent-node-type)
        (message "Insert pos: %s" insert-position)
        (message (make-string 80 ?|))
        insert-position))))

;; https://emacs-tree-sitter.github.io/tree-sitter-mode/
;;;###autoload
(defun turbo-log-print (&optional past-from-clipboard-p)
  "Log selected region for current major mode.
Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?"
  (interactive)
  ;; TODO: debug only
  (save-window-excursion
    (switch-to-buffer "*Messages*")
    (erase-buffer))

  (let ((insert-position (turbo-log--find-insert-position))
        (log-message (turbo-log--get-log-text)))
    (when insert-position
      (turbo-log--insert-logger-by-mode insert-position log-message)))

  (message "Start detecting nodes")
  )

(provide 'turbo-log)
;;; turbo-log.el ends here
