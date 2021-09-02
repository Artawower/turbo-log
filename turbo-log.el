;;; turbo-log.el --- The simple package for fast log selected region                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/turbo-log
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.3.0

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

;;; Code:

(defvar turbo-log--prefix "TCL: "
  "Prefix string for every log messages.")


(defvar turbo-log--python-logger "print"
  "Function for log in python.")
(defvar turbo-log--ecmascript-logger "console.log"
  "Function for log in ecmascript.")
(defvar turbo-log--golang-logger "fmt.Println"
  "Function for log in golang.")
(defvar turbo-log--include-buffer-name t
  "Include current buffer name to log message.")


(defun turbo-log--calculate-space-count (text)
  "Get space count at start of provided TEXT."
  (let* ((original-text-length (length text))
         (no-start-space-text (string-trim-left text))
         (no-start-space-text-length (length no-start-space-text)))
    (- original-text-length no-start-space-text-length)))


(defun turbo-log--get-line-text (line-number)
  "Get text from LINE-NUMBER under point."
  (goto-line line-number)
  (thing-at-point 'line))

(defun turbo-log--is-return-line (text)
  "Check is TEXT container return keyword."
  (string-match "^[[:blank:]]*\\(return\\)[[:blank:]]+" text))

(defun turbo-log--has-text-linebreak (text)
  "Check is TEXT container \n."
  (string-match "\n" text))


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
        (concat line-number "[" (buffer-name) "] " turbo-console--prefix " ")
      (concat line-number " " turbo-console--prefix " "))))

;; Ecmascript
(defun turbo-log--ecmascript-normilize-code (code)
  "Normalize CODE block for correct console.log func."
  (let* ((code (replace-regexp-in-string "[[:blank:]]*=[[:blank:]]*.+" "" code))
         (code (replace-regexp-in-string "\\(const\\|let\\|public\\|protected\\|private\\|var\\)[[:blank:]]+" "" code))
         ;; Remove type for typescript
         (code (replace-regexp-in-string "\\:[[:blank:]].+" "" code)))
    (turbo-log--remove-semicolon-at-end code)))

;; TODO: adapt for ecmascript
(defun turbo-log--ecmascript-find-insert-pos (current-line-number text)
  "Calculate insert position by CURRENT-LINE-NUMBER and TEXT from previous line."
  (if (turbo-log--is-return-line text)
      (- current-line-number 1)
    current-line-number))

(defun turbo-log--get-selected-text ()
  "Return selected text."
  (string-trim (buffer-substring (region-beginning) (region-end))))

(defun turbo-log--get-current-line-number ()
  "Return current line number after select.  Depend on full line selected or region."
  (if (bolp)
      (line-number-at-pos)
    (+ (line-number-at-pos) 1)))


(defun turbo-log--ecmascript-print (current-line-number formatted-selected-text prev-line-text)
  "Console log for ecmascript, js/ts modes.

CURRENT-LINE-NUMBER - line number under cursor
FORMATTED-SELECTED-TEXT - formatted text without space at start position
PREV-LINE-TEXT - text from previous line"

  (let* ((insert-line-number (turbo-log--ecmascript-find-insert-pos current-line-number prev-line-text))
         (insert-line-space-count (turbo-log--calculate-space-count (turbo-log--get-line-text insert-line-number)))
         (additional-spaces (make-string insert-line-space-count ? ))
         (meta-info (turbo-log--format-meta-info current-line-number))
         (normalized-code (turbo-log--ecmascript-normilize-code formatted-selected-text))
         (turbo-log--message
          (concat
           additional-spaces
           turbo-log--ecmascript-logger
           "('"
           meta-info
           formatted-selected-text ": ', "
           normalized-code ")\n")))



    (goto-line insert-line-number)
    (insert turbo-log--message)))

;; TODO: adapt for python
(defun turbo-log--python-find-insert-pos (current-line-number text)
  "Find insert position for python mode from CURRENT-LINE-NUMBER TEXT."
  (if (turbo-log--is-return-line text)
      (- current-line-number 1)
    current-line-number))

(defun turbo-log--extract-python-args (code)
  "Func for extract arguments from CODE."
  (let* ((code (replace-regexp-in-string "def[[:blank:]].+(" "" code))
         (code (replace-regexp-in-string ").+" "" code))
         (code (replace-regexp-in-string "[[:blank:]]?[:=]\\{1\\}\\([^,]+\\)" "" code)))
    code))

(defun turbo-log--python-normalize-code (code)
  "Normalize python CODE for correct printing."
  (let* ((code (if (string-match "def[[:blank:]]" code) (turbo-log--extract-python-args code)
                 ;; NOTE: if not a function
                 (replace-regexp-in-string "[[:blank:]]*=[[:blank:]]*.+" "" code))))
    code))

;; Python logger
;; TODO: remove prev-line-text, it will be useless after correct position detection.
(defun turbo-log--python-print (current-line-number formatted-selected-text prev-line-text)
  "Printing for python mode.

CURRENT-LINE-NUMBER - line number under cursor
FORMATTED-SELECTED-TEXT - formatted text without space at start position
PREV-LINE-TEXT - text from previous line"

  (let* ((insert-line-number (turbo-log--python-find-insert-pos current-line-number prev-line-text))
         ;; TODO: add forward/backward search for first symbol for correct tab indenting. Check direction of searching
         ;; by special keywoard if/for/while/def (fo forward) return for backward
         (insert-line-space-count (turbo-log--calculate-space-count (turbo-log--get-line-text (- insert-line-number 1))))
         (additional-spaces (make-string insert-line-space-count ? ))
         (meta-info (turbo-log--format-meta-info current-line-number))
         (normalized-code (turbo-log--python-normalize-code formatted-selected-text))
         (turbo-log--message
          (concat
           additional-spaces
           turbo-log--python-logger
           "('"
           meta-info
           formatted-selected-text ": ', "
           normalized-code ")\n")))



    (goto-line insert-line-number)
    (insert turbo-log--message)))

;; Golang logger
;; TODO: adapt for golang
(defun turbo-log--golang-find-insert-pos (current-line-number text)
  "Find insert position for python mode from CURRENT-LINE-NUMBER and provided TEXT."
  (if (turbo-log--is-return-line text)
      (- current-line-number 1)
    current-line-number))

(defun turbo-log--golang-print (current-line-number formatted-selected-text prev-line-text)
  "Print message for golang mode.

CURRENT-LINE-NUMBER - line number under cursor
FORMATTED-SELECTED-TEXT - formatted text without space at start position
PREV-LINE-TEXT - text from previous line"

  (let* ((insert-line-number (turbo-log--golang-find-insert-pos current-line-number prev-line-text))
         ;; TODO: add forward/backward search for first symbol for correct tab indenting. Check direction of searching
         ;; by special keywoard if/for/while/def (fo forward) return for backward
         (insert-line-space-count (turbo-log--calculate-space-count (turbo-log--get-line-text insert-line-number)))
         (additional-spaces (make-string (+ insert-line-space-count 1) ? ))
         (meta-info (turbo-log--format-meta-info current-line-number))
         (normalized-code formatted-selected-text)
         (turbo-log--message
          (concat
           additional-spaces
           turbo-log--golang-logger
           "(\""
           meta-info
           formatted-selected-text ": \", "
           normalized-code ")\n")))


    (goto-line insert-line-number)
    (insert turbo-log--message)))

(defvar turbo-log--modes '((typescript-mode . turbo-log--ecmascript-print)
                           (js-mode . turbo-log--ecmascript-print)
                           (ng2-ts-mode . turbo-log--ecmascript-print)
                           (web-mode . turbo-log--ecmascript-print)
                           (vue-mode . turbo-log--ecmascript-print)
                           (python-mode . turbo-log--python-print)
                           (go-mode . turbo-log--golang-print)))

(defun turbo-log--chose-mode ()
  "Chose logger by current major mode."
  (let* ((logger (assoc major-mode turbo-log--modes)))
    (if (eq logger nil)
        (funcall (lambda () (message "Logger for mode %s is not found" major-mode)
                   logger))
      logger)))

(defun turbo-log--handle-logger (logger-func)
  "Common entrypoint for all loggers by provieded LOGGER-FUNC."
  (let* ((current-line-number (turbo-log--get-current-line-number))
         (raw-selected-text (turbo-log--get-selected-text))
         (formatted-selected-text (string-trim raw-selected-text))
         (prev-line-text (turbo-log--get-line-text (- current-line-number 1))))
    (funcall logger-func
             current-line-number
             formatted-selected-text
             prev-line-text)))

;;;###autoload
(defun turbo-log-print ()
  "Log selected region for current major mode."
  (interactive)
  (let* ((logger-list (turbo-log--chose-mode))
         (logger (cdr logger-list)))
    (if logger
        (turbo-log--handle-logger logger))))

(provide 'turbo-log)
;;; turbo-log.el ends here
