;;; turbo-log.el --- The simple package for fast log selected region                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/turbo-log
;; Package-Requires: ((emacs "25.1") (tree-sitter "0.16.1")  (s "1.12.0"))
;; Version: 2.1.0

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
(require 's)

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
    :jump-list ((class_declaration
                 (:current-node-types (property_identifier)
                  :destination-node-names ("constructor")
                  :parent-node-types (public_field_definition))))
    :msg-format-template "'TCL: %s'"
    :identifier-formatter-rules ((property_identifier
                                      (:formatter "this.%s"
                                       :parent-node-types (public_field_definition))))
    :identifier-node-types (identifier member_expression property_identifier))
  "Common configurations for ecmascript`s based modes.")

(defconst turbo-log--identifier-node-types nil
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
    (csharp-mode (:loggers ("Console.WriteLine(%s);")))
    (javap-mode (:loggers ("System.out.println(%s);")))
    (java-mode (:loggers ("System.out.println(%s);")))
    (ruby-mode (:loggers ("p %s;" "puts %s;") :comment-string "#" :argument-divider ","))
    (lua-mode (:loggers ("print(%s)")))
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
             function_definition short_var_declaration def arrow_function method_definition
             expression_statement return if method_parameters)
  "Top level structure for detecting paste place.")

(defconst turbo-log--nodes-for-end-position-inserting '(array variable_declarator assignment short_var_declaration expression_statement method_parameters)
  "Node types for inserting logger after end position.")

(defconst turbo-log--nodes-allowed-insert-next-line '(statement_block block arrow_function)
  "Node types that allow to insert logger next line.")

(defconst turbo-log--nodes-allowed-insert-previous-line '(return_statement if_statement return if)
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

(defmacro turbo-log-configure (&rest configs)
  "Macro for configure turbo-log package.
CONFIGS - plist of configurations

:modes - modes that should apply configurations
:strategy - strategy of configurations, could be `merge' and `replace'
`merge' is default value.

:loggers - alist of available loggers for current mode
:jump-list - list of teleport positions from one tree-sitter node to another
where car is current node type and cdr is a plist of neighbor nodes
   :current-node-types - current node types of teleport
   :destination-node-name - node name where logger should be inserted
   :parent-node-types - list of parent node types
   jump will happen only after full mathing of this fields
:msg-format-template - format of entire string message placed before literals
:identifier-formatter-rules - alist of formatters where car is node type and
cdr is plist of:
   :formatter - string template
   :parent-node-types - alist of tree-sitter nodes that have to be matched
:identifier-node-types - node types of variables/expressions that will be
used as output
literal of logger
:line-number-format-template - formatter for line number
:buffer-name-format-template - formatter for current buffer name
:payload-format-template - formatter for payload
:argument-divider - formatter for argument divider
:block-statements - tree-sitter node types which indicate about block statement
:top-level-structures - top level structure that indicated about stopping of
parsing syntax tree
:nodes-for-end-position-inserting - tree-sitter node types that indicate current
log
has to be inserted after end position of detected exps
:nodes-allowed-insert-next-line - tree-sitter nodes that allow to insert logger
next line
:nodes-allowed-insert-previous-line - tree-sitter list of nodes that indicate
logger has to be inserted previous line
:nodes-ignored - list of nodes which prevent auto logger inserting
:comment-string - string for commenting current mode"

  (let* ((strategy (or (plist-get configs :strategy) 'replace))
         (excluded-keys '(:modes :strategy))
         (modes (plist-get configs :modes))
         current-config)

    (dolist (k excluded-keys)
      (setq configs (map-delete configs k)))

    (dolist (mode modes)
      (unless (assoc mode turbo-log-loggers)
        (push `(,mode nil) turbo-log-loggers))

      (setq current-config (car (cdr-safe (assoc mode turbo-log-loggers))))

      (if (eq strategy 'replace)
          (setq current-config configs)

        (cl-loop for (k v) on configs by 'cddr do
                 (if current-config
                     (plist-put current-config k v)
                   (setq current-config `(,k ,v)))))

      (if (assq mode turbo-log-loggers)
          (setcdr (assq mode turbo-log-loggers)
                  `(,current-config))
        `(push '(,mode '(,current-config)) ,turbo-log-loggers)))))

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
         (buffer-name (if turbo-log-buffer-name-format-template (format turbo-log-buffer-name-format-template (buffer-name)))))
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

(defun turbo-log--goto-next-word ()
  "Navigate to next word."
  (re-search-forward "[[:blank:]\n\(]+" nil t))

(defun turbo-log--get-node-name (node)
  "Return name of current treesitter NODE if its a named node."
  (when (tsc-node-named-p node)
    (buffer-substring (tsc-node-start-position node) (tsc-node-end-position node))))

(defun turbo-log--find-parent-jump-line-number (node parent-node &optional logger-config)
  "Find insert position.
Called when jump line position for PARENT-NODE provided inside LOGGER-CONFIG.
NODE - current node under cursor,"
  ;; (message "TCL: [line 302][turbo-log.el] parent-node:  %s" (tsc-node-type parent-node))
  (save-excursion
    (let* ((logger-jump-list (turbo-log--get-logger-config logger-config jump-list))
           (current-node parent-node)
           (jump-literal-config (when logger-jump-list (assoc (tsc-node-type parent-node) logger-jump-list)))
           (jump-literal-config (when jump-literal-config (car (cdr jump-literal-config))))

           (matched-literals (when jump-literal-config (plist-get jump-literal-config :current-node-types)))
           (destination-names (when jump-literal-config (plist-get jump-literal-config :destination-node-names)))
           (parent-node-types (when jump-literal-config (plist-get jump-literal-config :parent-node-types)))
           insert-position
           (parent-matched (and parent-node-types
                                (not (member (tsc-node-type (tsc-get-parent node)) parent-node-types))))
           (cursor-res (turbo-log--goto-next-word)))

      (goto-char (point-min))

      ;; (message "matched-literals: %s\nparent node: %s\ndestination names: %s\nparent-node-types: %s\nparent of parent node type %s\n"
      ;;          matched-literals
      ;;          (tsc-node-type parent-node)
      ;;          destination-names
      ;;          parent-node-types
      ;;          (tsc-node-type (tsc-get-parent node)))

      (while (and (not insert-position) cursor-res logger-jump-list (not parent-matched))
        (setq current-node (tree-sitter-node-at-pos nil (point)))

        (when (and (member (tsc-node-type current-node) matched-literals)
                   (member (turbo-log--get-node-name current-node) destination-names))
          (goto-char (tsc-node-start-position current-node))
          (setq insert-position (+ (line-number-at-pos) 1)))

        (setq cursor-res (turbo-log--goto-next-word)))
      insert-position)))

(defun turbo-log--get-insert-line-number-by-node-types (node node-type parent-node &optional logger-config)
  "Return line number for insertion.
By current NODE, panrent NODE-TYPE and PARENT-NODE.
LOGGER-CONFIG - configuration of current logger."

  (let* ((jump-line-number (turbo-log--find-parent-jump-line-number node parent-node logger-config)))
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
Cause evil change `point' function value by 1 inside `region-p'"
  (if (and (bound-and-true-p evil-mode) (region-active-p))
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
           ;; TODO: move parent node type to internal function
           (insert-line-number (turbo-log--get-insert-line-number-by-node-types node parent-node-type parent-node logger-config)))
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
    (setq regexp (s-replace (car p) (cadr p) regexp)))
  regexp)

(defun turbo-log--build-log-regexp (comment-type)
  "Build regexp for finding loggers made by turbo-log.
COMMENT-TYPE - type of comment, could be `commented' `uncommented' and `both'"
  (let* ((logger-meta (car (cdr (assoc major-mode turbo-log-loggers))))
         (loggers (mapcar
                   (lambda (v) (if (consp v) (car v) v))
                   (plist-get logger-meta :loggers)))
         (comment-string (turbo-log--get-logger-config logger-meta comment-string))
         (safety-comment-string (s-replace "/" "\\/" comment-string))
         (msg-template (turbo-log--normilize-regexp (turbo-log--get-logger-config logger-meta msg-format-template)))
         (line-number-formatter (turbo-log--get-logger-config logger-meta line-number-format-template))
         (line-number-info (and line-number-formatter (format (turbo-log--normilize-regexp line-number-formatter) "[0-9]+")))
         (log-message (format msg-template (concat line-number-info "[^'\"]+")))

         (log-prefix (cond ((eq comment-type 'uncommented) "^[[:blank:]]+")
                           ((eq comment-type 'commented) (concat "^[[:blank:]]+" safety-comment-string "[[:blank:]]+"))
                           ((eq comment-type 'both) (concat "^[[:blank:]]+" (format "\\(%s\\)?" safety-comment-string) "[[:blank:]]*"))))
         (regexps '()))

    (dolist (l loggers)
      (push (concat log-prefix (format (turbo-log--normilize-regexp l) (concat log-message "[^\'\(\);]+")) ";?") regexps))

    (unless logger-meta (message "Sorry, turbo-log is not available for %s." major-mode))
    (string-join regexps "\\\|")))

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

(defun turbo-log--extract-identifier-node-types (identifier-node-types divider)
  "Extract every nodes from current line that contained by IDENTIFIER-NODE-TYPES.
Result will be a string, divdded by DIVIDER."

  (save-excursion
    (beginning-of-line-text)
    (let* ((max-line-point (point-at-eol))
           (current-node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))
           (cursor-res t)
           (identifiers '()))

      (while cursor-res
        ;; FIXME: navigation by ts-tree doesn't work properly
        (setq cursor-res (turbo-log--goto-next-word))
        (setq current-node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))

        (when current-node
          (when (> (tsc-node-start-position current-node) max-line-point)
            (setq cursor-res nil))

          (when (and (member (tsc-node-type current-node) identifier-node-types)
                     (tsc-node-named-p current-node)
                     (<= (tsc-node-start-position current-node) max-line-point))
            (push (buffer-substring (tsc-node-start-position current-node) (tsc-node-end-position current-node)) identifiers))))

      (when identifiers
        (string-join (delete-dups identifiers) divider)))))

(defun turbo-log--try-normalize-identifiers (identifier-formatter-rules log-message)
  "Format current LOG-MESSAGE by IDENTIFIER-FORMATTER-RULES.
IDENTIFIER-FORMATTER-RULES - is plist with next fields:
:formatter - string of formatter template
:parent-node-types - list of tree-sitter nodes "

  (let* ((current-node (tree-sitter-node-at-pos nil (turbo-log--get-real-point)))
         (node-type (tsc-node-type current-node))
         (parent-node-type (tsc-node-type (tsc-get-parent current-node)))
         (formatter-configs (car (cdr-safe (assoc node-type identifier-formatter-rules))))
         (formatter (plist-get formatter-configs :formatter))
         (parent-node-types (plist-get formatter-configs :parent-node-types)))

    (if (and formatter (or (not parent-node-types) (member parent-node-type parent-node-types)))
        (format formatter log-message)
      log-message)))

;;;###autoload
(defun turbo-log-print (&optional insert-immediately-p past-from-clipboard-p)
  "Log selected region for current major mode.
Optional argument PAST-FROM-CLIPBOARD-P does text inserted from clipboard?
INSERT-IMMEDIATELY-P - should insert first available logger?"
  (interactive)

  (if (or (bound-and-true-p tree-sitter-mode) turbo-log-allow-insert-without-tree-sitter-p)
      (let* ((logger-config (car (cdr (assoc major-mode turbo-log-loggers))))
             (divider (turbo-log--get-logger-config logger-config argument-divider))
             (identifier-node-types (turbo-log--get-logger-config logger-config identifier-node-types))
             (log-message (turbo-log--get-log-text past-from-clipboard-p))
             (extracted-log-message (when (and identifier-node-types (not (region-active-p)))
                                      (turbo-log--extract-identifier-node-types identifier-node-types divider)))
             (log-message (if (region-active-p)
                              log-message
                            extracted-log-message))
             (identifier-formatter-rules (turbo-log--get-logger-config logger-config identifier-formatter-rules))
             (log-message (if (and identifier-formatter-rules log-message)
                              (turbo-log--try-normalize-identifiers identifier-formatter-rules log-message)
                            log-message))
             (insert-line-number (cond (past-from-clipboard-p (line-number-at-pos))
                                       ((not (bound-and-true-p tree-sitter-mode)) (+ (line-number-at-pos) 1))
                                       (t (turbo-log--find-insert-line-number logger-config))))
             (previous-line-empty-body-p (and insert-line-number (turbo-log--line-with-empty-body-p (- insert-line-number 1)) (not past-from-clipboard-p))))

        (unless logger-config
          (message "Turbo-log: No configuration provided for %s mode" major-mode))

        (when (and insert-line-number logger-config log-message)
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
