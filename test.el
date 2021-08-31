;;; test.el --- Testing for turbo log  -*- lexical-binding:t -*-

(require 'turbo-log)

(ert-deftest test-ecmascript-is-return ()
  "Tests return statement."
  (should (equal 0 (turbo-console--ecmascript-is-return "  return {
     name: 'name'
   }")))
  (should (equal nil (turbo-console--ecmascript-is-return "public isFuncReturnTrue(true) {")))
  (should (equal nil (turbo-console--ecmascript-is-return "returnValueFromSomeFucn('value') {")))
  )


(ert-deftest test-ecmascript-code-normalize ()
  (should (equal "this.myAwesomeVariable" (turbo-console--ecmascript-normilize-code "this.myAwesomeVariable = { name: 'Hello', world: true }")))
  (should (equal "myVar" (turbo-console--ecmascript-normilize-code "const myVar = 'Hello world'")))
  (should (equal "var" (turbo-console--ecmascript-normilize-code "const var = 'Hello world'")))
  (should (equal "hello" (turbo-console--ecmascript-normilize-code "public hello: string = 12;")))
  (should (equal "somePrivateVariable" (turbo-console--ecmascript-normilize-code "private somePrivateVariable: Subject<void> = new Subject<void>()")))
  (should (equal "protectedVar" (turbo-console--ecmascript-normilize-code "protected protectedVar: Subject<void>;")))
  (should (equal "literal" (turbo-console--ecmascript-normilize-code "var literal: string = 'qweqwe' // Some comment for future generation")))
  (should (equal "let5" (turbo-console--ecmascript-normilize-code "let let5: string // Some comment for future generation")))
  (should (equal "let6" (turbo-console--ecmascript-normilize-code "let let6: string; // Some comment for future generation"))))

(ert-deftest test-space-count-at-start-of-line ()
  "Test space count at start of line."
  (should (equal 0 (turbo-console--calculate-space-count "This is text without any space")))
  (should (equal 2 (turbo-console--calculate-space-count "  This is text with 2 spaces")))
  (should (equal 3 (turbo-console--calculate-space-count "   ")))
  (should (equal 0 (turbo-console--calculate-space-count "")))
  (should (equal 1 (turbo-console--calculate-space-count "\t")))
  (should (equal 4 (turbo-console--calculate-space-count "\t\t \t")))
  (should (equal 2 (turbo-console--calculate-space-count "\n\n")))
  )
(ert-deftest test-semicolon-removed-from-end ()
  (should (equal "const test = 12" (turbo-console--remove-semicolon-at-end "const test = 12;")))
  )

(defun test-select-current-line ()
  "Help func for select current line."
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil)
  )

(defun test-inside-mock-buffer (test-func line-number)
  "Function for navigation to LINE-NUMBER and testing TEST-FUNC in new buffer."
  (switch-to-buffer-other-window "*buffer-for-test*")
  (setq test-lines '("This is first line"
                     "second line"
                     "3 line"
                     "return someVar;"
                     "next line"
                     "and another"))

  (mapcar '(lambda (v) (insert v)
             (end-of-line)
             (newline-and-indent)
             ) test-lines)
  (goto-line line-number)
  (test-select-current-line)
  (funcall test-func)
  (erase-buffer)
  )

(ert-deftest test-correct-line-selection ()
  "Test if correct line selected for test inserting."
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-console--get-selected-text) "3 line"))) 3)
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-console--get-selected-text) "This is first line"))) 1)
  )

(ert-deftest test-ecmascript-correct-insert-position-selected ()
  "Test position for text-insertion."
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-console--ecmascript-find-insert-pos 3 "3 line") 3))) 3)
  ;; If return keyword should return previous line
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-console--ecmascript-find-insert-pos 4 "return someVar;") 3))) 4)
  )

;;; test.el ends here
