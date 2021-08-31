;;; turbo-log.el --- a simple package for fast log selected region                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; Keywords: emacs, logger, print, console.log, fmt.Println
;; Version: 0.0.1

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

;; This package used for fast line/region logging with additional meta information

;;; Code:

;; code goes here

(defvar turbo-console--prefix)
(defvar turbo-console--modes)

(setq turbo-console--prefix "TCL: ")

(defun turbo-console--calculate-space-count (text)
  "Get space count at start of provided TEXT."
  (let* ((original-text-length (length text))
         (no-start-space-text (string-trim-left text))
         (no-start-space-text-length (length no-start-space-text)))
    (- original-text-length no-start-space-text-length)
    ))


(defun turbo-console--get-line-text (line-number)
  "Get text from LINE-NUMBER under point."
  (goto-line line-number)
  (thing-at-point 'line))

(defun turbo-console--ecmascript-is-return (text)
  "Check is TEXT container return keyword."
  (string-match "^[[:blank:]]*\\(return\\)[[:blank:]]+" text))

(defun turbo-console--has-text-linebreak (text)
  "Check is TEXT container \n."
  (string-match "\n" text))


(defun turbo-console--remove-semicolon-at-end (code)
  "Remove semicolon from provided CODE block."
  (let* ((code-len (length code))
         (last-char (substring code (- code-len 1) code-len)))

    (if (string= ";" last-char)
        (substring code 0 (- code-len 1))
      code)
    )
  )

(defun turbo-console--ecmascript-normilize-code (code)
  "Normalize CODE block for correct console.log func."
  (let* ((code (replace-regexp-in-string "[[:blank:]]*=[[:blank:]]*.+" "" code))
         (code (replace-regexp-in-string "\\(const\\|let\\|public\\|protected\\|private\\|var\\)[[:blank:]]+" "" code))
         ;; Remove type for typescript
         (code (replace-regexp-in-string "\\:[[:blank:]].+" "" code)))
    (turbo-console--remove-semicolon-at-end code)
    ))

(defun turbo-console--ecmascript-find-insert-pos (current-line-number text)
  "Calculate insert position by CURRENT-LINE-NUMBER and TEXT from previous line."
  (message "Return found: %s" (turbo-console--ecmascript-is-return text))
  (if (turbo-console--ecmascript-is-return text)
      (- current-line-number 1)
    current-line-number
    )
  )

(defun turbo-console--get-selected-text ()
  "Return selected text."
  (string-trim (buffer-substring (region-beginning) (region-end))))

(defun turbo-console--get-current-line-number ()
  "Return current line number after select.  Depend on full line selected or region."
  (if (bolp)
      (line-number-at-pos)
    (+ (line-number-at-pos) 1)
    ))


(defun turbo-console--ecmascript-print ()
  "Console log for ecmascript, js/ts modes."
  (let* ((current-line-number (turbo-console--get-current-line-number))
         (raw-selected-text (turbo-console--get-selected-text))
         (formatted-selected-text (string-trim raw-selected-text))
         (prev-line-text (turbo-console--get-line-text (- current-line-number 1)))
         (insert-line-number (turbo-console--ecmascript-find-insert-pos current-line-number prev-line-text))
         (insert-line-space-count (turbo-console--calculate-space-count (turbo-console--get-line-text insert-line-number)))
         (additional-spaces (make-string insert-line-space-count ? ))
         (line-number-text (concat "[line " (format "%s" insert-line-number) "] "))
         (normalized-code (turbo-console--ecmascript-normilize-code formatted-selected-text))
         (turbo-console--message
          (concat
           additional-spaces
           "console.log('"
           line-number-text
           turbo-console--prefix formatted-selected-text ": ', "
           normalized-code ")\n"))
         )



    (goto-line insert-line-number)
    (insert turbo-console--message)
    ))


(setq turbo-console--modes '((typescript-mode . turbo-console--ecmascript-print)
                             (js-mode . turbo-console--ecmascript-print)
                             (ng2-ts-mode . turbo-console--ecmascript-print)
                             (org-mode . turbo-console--ecmascript-print)
                             (python-mode . turbo-console--python-print)))

(defun turbo-console--chose-mode ()
  "Chose logger by current major mode."
  (let* ((logger (assoc major-mode turbo-console--modes)))
    (if (eq logger nil)
        (funcall (lambda () (message "Logger for mode %s is not found" major-mode)
                   logger))
      logger)
    ))

(defun handle-logger (logger-func)
  "Common entrypoint for all loggers by provieded LOGGER-FUNC."
  (let* ((current-line-number (turbo-console--get-current-line-number))
         (raw-selected-text (turbo-console--get-selected-text))
         (formatted-selected-text (string-trim raw-selected-text))
         (prev-line-text (turbo-console--get-line-text (- current-line-number 1)))
         )
    (funcall logger-func current-line-number)
    )
  )

;;;###autoload
(defun turbo-console-print ()
  "Log selected region for current major mode."
  (interactive)
  (let* ((logger-list (turbo-console--chose-mode))
         (logger (cdr logger-list)))
    (if logger
        (funcall logger)
      )
    ))

(define-key global-map (kbd "C-s-l") 'turbo-console-print)

;;; _
(provide 'turbo-log)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; turbo-log.el ends here
