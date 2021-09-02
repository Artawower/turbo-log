;;; test.el --- Testing for turbo log  -*- lexical-binding:t -*-

(require 'turbo-log)

;;; Code:

(ert-deftest test-ecmascript-is-return ()
  "Tests return statement."
  (should (equal 0 (turbo-log--is-return-line "  return {
     name: 'name'
   }")))
  (should (equal nil (turbo-log--is-return-line "public isFuncReturnTrue(true) {")))
  (should (equal nil (turbo-log--is-return-line "returnValueFromSomeFucn('value') {"))))


(ert-deftest test-ecmascript-code-normalize ()
  (should (equal "this.myAwesomeVariable" (turbo-log--ecmascript-normilize-code "this.myAwesomeVariable = { name: 'Hello', world: true }")))
  (should (equal "myVar" (turbo-log--ecmascript-normilize-code "const myVar = 'Hello world'")))
  (should (equal "var" (turbo-log--ecmascript-normilize-code "const var = 'Hello world'")))
  (should (equal "hello" (turbo-log--ecmascript-normilize-code "public hello: string = 12;")))
  (should (equal "somePrivateVariable" (turbo-log--ecmascript-normilize-code "private somePrivateVariable: Subject<void> = new Subject<void>()")))
  (should (equal "protectedVar" (turbo-log--ecmascript-normilize-code "protected protectedVar: Subject<void>;")))
  (should (equal "literal" (turbo-log--ecmascript-normilize-code "var literal: string = 'qweqwe' // Some comment for future generation")))
  (should (equal "let5" (turbo-log--ecmascript-normilize-code "let let5: string // Some comment for future generation")))
  (should (equal "let6" (turbo-log--ecmascript-normilize-code "let let6: string; // Some comment for future generation"))))

(ert-deftest test-space-count-at-start-of-line ()
  "Test space count at start of line."
  (should (equal 0 (turbo-log--calculate-space-count "This is text without any space")))
  (should (equal 2 (turbo-log--calculate-space-count "  This is text with 2 spaces")))
  (should (equal 3 (turbo-log--calculate-space-count "   ")))
  (should (equal 0 (turbo-log--calculate-space-count "")))
  (should (equal 1 (turbo-log--calculate-space-count "\t")))
  (should (equal 4 (turbo-log--calculate-space-count "\t\t \t")))
  (should (equal 2 (turbo-log--calculate-space-count "\n\n"))))

(ert-deftest test-semicolon-removed-from-end ()
  (should (equal "const test = 12" (turbo-log--remove-semicolon-at-end "const test = 12;"))))

(defun test-select-current-line ()
  "Help func for select current line."
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))

;; TODO: add optional code block like real sample.
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
  (erase-buffer))

(ert-deftest test-correct-line-selection ()
  "Test if correct line selected for test inserting."
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-log--get-selected-text) "3 line"))) 3)
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-log--get-selected-text) "This is first line"))) 1))

(ert-deftest test-ecmascript-correct-insert-position-selected ()
  "Test position for text-insertion."
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-log--ecmascript-find-insert-pos 3 "3 line") 3))) 3)
  ;; If return keyword should return previous line
  (test-inside-mock-buffer (lambda ()
                             (should (equal (turbo-log--ecmascript-find-insert-pos 4 "return someVar;") 3))) 4))

(ert-deftest test-python-correct-code-normalize ()
  "Test python code normalization."
  (should (equal "my_variable" (turbo-log--python-normalize-code "my_variable = 123;")))
  (should (equal "spec, smile, foo" (turbo-log--python-normalize-code "def test_func(spec: str, smile: int, foo: int) -> str:")))
  (should (equal "spec, smile, foo" (turbo-log--python-normalize-code "def test_func(spec: str, smile: int, foo: [int]) -> str:")))
  (should (equal "spec, smile, foo" (turbo-log--python-normalize-code "def test_func(spec, smile: int, foo: [int]) -> str:")))
  (should (equal "spec, smile, foo" (turbo-log--python-normalize-code "def test_func(spec = 1, smile: int, foo: [int]) -> str:"))))
;;; test.el ends here

