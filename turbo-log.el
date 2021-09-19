;;; turbo-log.el --- The simple package for fast log selected region                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/turbo-log
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.8.0

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
;; It works with golang, python, js and typescript languages.

;;; Code:

;;;; Variables
(require 'subr-x)

(defvar turbo-log--prefix "TCL: "
  "Prefix string for every log messages.")


(defvar turbo-log--python-loggers '("print")
  "List of python loggers.")
(defvar turbo-log--ecmascript-loggers '("console.log")
  "List of ecmascript loggers.")
(defvar turbo-log--golang-loggers '("fmt.Println")
  "List of golang loggers.")
(defvar turbo-log--include-buffer-name t
  "Include current buffer name to log message.")
(defvar turbo-log--open-brackets '(?{ ?\( ?\[)
  "List of opened brackets.")
(defvar turbo-log--close-brackets '(?} ?\) ?\])
  "List of opened brackets.")
(defvar turbo-log--ecmascript-configs '(:include-semicolon t)
  "Additional configuration for ecmascript.
:include-semicolon - should semicolon to be added after new log message inserted")

;;;; Common functions
(defun turbo-log--calculate-space-count (text)
  "Get space count at start of provided TEXT."
  (let* ((original-text-length (length text))
         (no-start-space-text (string-trim-left text))
         (no-start-space-text-length (length no-start-space-text)))
    (- original-text-length no-start-space-text-length)))


(defun turbo-log--get-line-text (line-number)
  "Get text from LINE-NUMBER under point."
  (turbo-log--goto-line line-number)
  (thing-at-point 'line))

(defun turbo-log--goto-line (line-number)
  "Non interactive implementation of 'turbo-log--goto-line' by provided LINE-NUMBER."
  (forward-line (- line-number (line-number-at-pos))))

(defun turbo-log--return-line-p (text)
  "Check is TEXT container return keyword."
  (string-match "^[[:blank:]]*\\(return\\)[[:blank:]]+" text))

(defun turbo-log--has-text-linebreak (text)
  "Check is TEXT container \n."
  (string-match "\n" text))

(defun turbo-log--replace-string (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun turbo-log--remove-semicolon-at-end (code)
  "Remove semicolon from provided CODE block."
  (let* ((code-len (length code))
         (last-char (substring code (- code-len 1) code-len)))

    (if (string= ";" last-char)
        (substring code 0 (- code-len 1))
      code)))

(defun turbo-log--format-meta-info (line-number)
  "Format meta information by provided config.
Insert LINE-NUMBER and buffer name."

  (let ((line-number (concat "[line " (format "%s" line-number) "]")))
    (if turbo-log--include-buffer-name
        (concat line-number "[" (buffer-name) "] " turbo-log--prefix " ")
      (concat line-number " " turbo-log--prefix " "))))

(defun turbo-log--choose-logger (loggers &optional multiple-loggers-p)
  "Choose logger if multiple LOGGERS were provided.
When MULTIPLE-LOGGERS-P is nil will choose first logger from list."
  (if (and (length> loggers 1) multiple-loggers-p)
      (completing-read "Choose logger: " loggers)
    (nth 0 loggers)))

;;;; Ecmascript
(defun turbo-log--ecmascript-normilize-code (code)
  "Normalize CODE block for correct console.log func."
  (let* ((code (replace-regexp-in-string "[[:blank:]]*=[[:blank:]]*.+" "" code))
         (code (replace-regexp-in-string "\\(const\\|let\\|public\\|protected\\|private\\|var\\)[[:blank:]]+" "" code))
         ;; Remove type for typescript
         (code (replace-regexp-in-string "\\:[[:blank:]].+" "" code)))
    (turbo-log--remove-semicolon-at-end code)))

(defun turbo-log--ecmascript-find-insert-pos (current-line-number text)
  "Calculate insert position by CURRENT-LINE-NUMBER and TEXT from previous line."
  (cond ((turbo-log--return-line-p text) (- current-line-number 1))
        ((string-match "{\\|;$" text) current-line-number)
        ((string-match "\\[$" text) (progn
                                      (search-forward-regexp "\\];?$" nil t)
                                      (+ (line-number-at-pos) 1)))
        (t (let* ((current-char (char-after))
                  (before-char (char-before))
                  (all-brackets (append turbo-log--open-brackets turbo-log--close-brackets))
                  (first-back-bracket nil)
                  (brackets '())
                  (brackets (save-excursion
                              (while (and (not first-back-bracket) (not (bobp)))
                                (setq before-char (char-before))
                                (backward-char)
                                (cond ((and (length= brackets 0) (eq before-char ?{)) (setq first-back-bracket before-char))
                                      ((member before-char turbo-log--close-brackets) (setq brackets (append brackets (list current-char))))
                                      ((member before-char turbo-log--open-brackets) (setq brackets (butlast brackets)))))))
                  (brackets (if (eq first-back-bracket ?{) '(?{) '()))
                  (found nil))

             (while (and (not (eobp)) (and (not (member current-char '(?\; ?{))) (length= brackets 0)) (not found))
               (forward-char)
               (setq current-char (char-after))
               (cond ((and (eq current-char ?}) (length= brackets 0))
                      (setq found t))
                     ((and (member current-char turbo-log--close-brackets) (length> brackets 0))
                      (setq brackets (butlast brackets)))
                     ((member current-char turbo-log--open-brackets)
                      (setq brackets (append brackets (list current-char))))))
             (if found current-line-number (+ (line-number-at-pos) 1))))))

(defun turbo-log--get-selected-text ()
  "Return selected text."
  (string-trim (buffer-substring (region-beginning) (region-end))))

(defun turbo-log--get-current-line-number ()
  "Return current line number after select.  Depend on full line selected or region."
  (if (bolp)
      (line-number-at-pos)
    (+ (line-number-at-pos) 1)))


(defun turbo-log--ecmascript-print (current-line-number formatted-selected-text prev-line-text multiple-logger-p)
  "Console log for ecmascript, js/ts modes.

CURRENT-LINE-NUMBER - line number under cursor
FORMATTED-SELECTED-TEXT - formatted text without space at start position
PREV-LINE-TEXT - text from previous line
MULTIPLE-LOGGER-P - should guess list of available loggers?"

  (let* ((insert-line-number (turbo-log--ecmascript-find-insert-pos current-line-number prev-line-text))
         (insert-line-space-count (turbo-log--calculate-space-count (turbo-log--get-line-text insert-line-number)))
         (additional-spaces (make-string insert-line-space-count ? ))
         (meta-info (turbo-log--format-meta-info current-line-number))
         (normalized-code (turbo-log--ecmascript-normilize-code formatted-selected-text))
         (turbo-log--message
          (concat
           additional-spaces
           (turbo-log--choose-logger turbo-log--ecmascript-loggers multiple-logger-p)
           "('"
           meta-info
           formatted-selected-text ": ', "
           normalized-code ")"
           (if (plist-get turbo-log--ecmascript-configs :include-semicolon) ";")
           "\n")))



    (turbo-log--goto-line insert-line-number)
    (insert turbo-log--message)))

(defun turbo-log--python-find-insert-pos (current-line-number text)
  "Find insert position for python mode from CURRENT-LINE-NUMBER TEXT."
  (if (turbo-log--return-line-p text)
      (- current-line-number 1)
    current-line-number))

(defun turbo-log--extract-python-args (code)
  "Func for extract arguments from CODE."
  (let* ((code (replace-regexp-in-string "def[[:blank:]].+(" "" code))
         (code (replace-regexp-in-string ").+" "" code))
         (code (replace-regexp-in-string "[[:blank:]]?[:=]\\{1\\}\\([^,]+\\)" "" code)))
    code))

(defun turbo-log--python-normalize-code (code)
  "Normalize python CODE for correct logging."
  (let* ((code (if (string-match "def[[:blank:]]" code) (turbo-log--extract-python-args code)
                 (replace-regexp-in-string "[[:blank:]]*=[[:blank:]]*.+" "" code))))
    code))

;;;; Python logger
(defun turbo-log--python-print (current-line-number formatted-selected-text prev-line-text multiple-logger-p)
  "Printing for python mode.

CURRENT-LINE-NUMBER - line number under cursor
FORMATTED-SELECTED-TEXT - formatted text without space at start position
PREV-LINE-TEXT - text from previous line
MULTIPLE-LOGGER-P - should guess list of available loggers?"

  (let* ((insert-line-number (turbo-log--python-find-insert-pos current-line-number prev-line-text))
         (insert-line-space-count (turbo-log--calculate-space-count (turbo-log--get-line-text (- insert-line-number 1))))
         (additional-spaces (make-string insert-line-space-count ? ))
         (meta-info (turbo-log--format-meta-info current-line-number))
         (normalized-code (turbo-log--python-normalize-code formatted-selected-text))
         (turbo-log--message
          (concat
           additional-spaces
           (turbo-log--choose-logger turbo-log--python-loggers multiple-logger-p)
           "('"
           meta-info
           formatted-selected-text ": ', "
           normalized-code ")\n")))



    (turbo-log--goto-line insert-line-number)
    (insert turbo-log--message)))

;;;; Golang logger
(defun turbo-log--golang-find-insert-pos (current-line-number text)
  "Find insert position for python mode from CURRENT-LINE-NUMBER and provided TEXT."
  (if (turbo-log--return-line-p text)
      (- current-line-number 1)
    current-line-number))

(defun turbo-log--golang-print (current-line-number formatted-selected-text prev-line-text multiple-logger-p)
  "Print message for golang mode.

CURRENT-LINE-NUMBER - line number under cursor
FORMATTED-SELECTED-TEXT - formatted text without space at start position
PREV-LINE-TEXT - text from previous line
MULTIPLE-LOGGER-P - should guess list of available loggers?"

  (let* ((insert-line-number (turbo-log--golang-find-insert-pos current-line-number prev-line-text))
         (insert-line-space-count (turbo-log--calculate-space-count (turbo-log--get-line-text insert-line-number)))
         (additional-spaces (make-string (+ insert-line-space-count 1) ? ))
         (meta-info (turbo-log--format-meta-info current-line-number))
         (normalized-code formatted-selected-text)
         (turbo-log--message
          (concat
           additional-spaces
           (turbo-log--choose-logger turbo-log--golang-loggers multiple-logger-p)
           "(\""
           meta-info
           formatted-selected-text ": \", "
           normalized-code ")\n")))


    (turbo-log--goto-line insert-line-number)
    (insert turbo-log--message)))

(defvar turbo-log--modes '((typescript-mode . turbo-log--ecmascript-print)
                           (js-mode . turbo-log--ecmascript-print)
                           (ng2-ts-mode . turbo-log--ecmascript-print)
                           (web-mode . turbo-log--ecmascript-print)
                           (vue-mode . turbo-log--ecmascript-print)
                           (python-mode . turbo-log--python-print)
                           (go-mode . turbo-log--golang-print)))

(defun turbo-log--choose-mode ()
  "Chose logger by current major mode."
  (let* ((logger (assoc major-mode turbo-log--modes)))
    (if logger
        logger
      (progn (message "Logger for mode %s is not found" major-mode)
             nil))))

(defun turbo-log--handle-logger (logger-func &optional multiple-logger-p)
  "Common entrypoint for all loggers by provided LOGGER-FUNC.
MULTIPLE-LOGGER-P allow to choose logger from list interactively."
  (let* ((current-line-number (turbo-log--get-current-line-number))
         (raw-selected-text (turbo-log--get-selected-text))
         (formatted-selected-text (string-trim raw-selected-text))
         (prev-line-text (turbo-log--get-line-text (- current-line-number 1))))
    (funcall logger-func
             current-line-number
             formatted-selected-text
             prev-line-text
             multiple-logger-p)))


(defun turbo-log--join-loggers-for-regexp (loggers)
  "Build regexp by provided list of LOGGERS."
  (string-join loggers "\\|"))

(defun turbo-log--get-logger-regexps ()
  "Get loggers on regexp format."
  (concat "\\("
          (turbo-log--replace-string "." "\\." (turbo-log--join-loggers-for-regexp turbo-log--golang-loggers))
          "\\|"
          (turbo-log--replace-string "." "\\." (turbo-log--join-loggers-for-regexp turbo-log--ecmascript-loggers))
          "\\|"
          (turbo-log--join-loggers-for-regexp turbo-log--python-loggers)
          "\\)"))

(defun turbo-log--build-log-regexp (log-type)
  "Build regexp for finding loggers made by turbo-log.
LOG-TYPE can be 'commented 'uncommented 'both."

  ;; NOTE:
  ;; "^[[:blank:]]*\\(fmt\\.Println\\|console\\.log\\|print\\)([\n[:blank:]'\"]*\\[line [0-9]+\\]"
  (let* ((commented-regexp (format "^[[:blank:]]*%s[[:blank:]]*" (turbo-log--get-comment-regexp)))
         (uncommented-regexp "^[[:blank:]]*")
         (regexp (cond ((eq log-type 'commented) commented-regexp)
                       ((eq log-type 'uncommented) uncommented-regexp)
                       (t (format "\\(%s\\|%s\\)" commented-regexp uncommented-regexp))))
         (regexp (concat
                  regexp
                  (turbo-log--get-logger-regexps)
                  "([\n[:blank:]'\\\"."
                  (if (eq log-type 'commented) "\\/" "")
                  "]*"))
         (regexp (concat regexp "\\[line [0-9]+\\]")))
    regexp))

(defun turbo-log--get-comment-string ()
  "Get commet characters by current 'major-mode'."
  (if (eq major-mode 'python-mode) "# " "// "))

(defun turbo-log--get-comment-regexp ()
  "Get regexp for findings comment in current mode."
  (if (eq major-mode 'python-mode) "#" "\\/\\/"))

(defun turbo-log--comment-current-line ()
  "Comment current line."
  (beginning-of-line-text)
  (insert (turbo-log--get-comment-string)))

(defun turbo-log--uncomment-current-line ()
  "Uncomment current line."
  (end-of-line)
  (search-backward (turbo-log--get-comment-string))
  (replace-match ""))


(defun turbo-log--handle-log-line (handle-line)
  "Call HANDLE-LINE func for each comment line."
  (let* ((cycle-limitter 0)
         (start-line (progn (search-backward-regexp (turbo-log--get-logger-regexps) nil t)
                            (line-number-at-pos)))
         (end-line (progn (search-forward-regexp ");?$" nil t)
                          (line-number-at-pos))))
    (while (not (eq cycle-limitter (+ (- end-line start-line) 1)))
      (setq cycle-limitter (+ cycle-limitter 1))
      (funcall handle-line)
      (forward-line -1))))

;;;###autoload
(defun turbo-log-print ()
  "Log selected region for current major mode."
  (interactive)
  (let* ((logger-list (turbo-log--choose-mode))
         (logger (cdr logger-list)))
    (if logger
        (turbo-log--handle-logger logger t))))

;;;###autoload
(defun turbo-log-print-immediately ()
  "Log selected region for current major mode without ask a logger from list."
  (interactive)
  (let* ((logger-list (turbo-log--choose-mode))
         (logger (cdr logger-list)))
    (if logger
        (turbo-log--handle-logger logger))))

;;;###autoload
(defun turbo-log-comment-all-logs ()
  "Hide all log messages made by turbo-log."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (turbo-log--build-log-regexp 'uncommented) nil t)
      (turbo-log--handle-log-line 'turbo-log--comment-current-line)
      (search-forward ")" nil t)
      (forward-line))))



;;;###autoload
(defun turbo-log-uncomment-all-logs ()
  "Show all comments made by turbo-log."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (turbo-log--build-log-regexp 'commented) nil t)
      (turbo-log--handle-log-line 'turbo-log--uncomment-current-line)
      (search-forward ")" nil t)
      (forward-line))))

;;;###autoload
(defun turbo-log-delete-all-logs ()
  "Delete all turbo-log loggers."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (turbo-log--build-log-regexp 'both) nil t)
      (turbo-log--handle-log-line 'kill-whole-line))))

(provide 'turbo-log)
;;; turbo-log.el ends here
