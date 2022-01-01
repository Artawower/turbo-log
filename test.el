;;; test.el --- Testing for turbo log  -*- lexical-binding:t -*-

(require 'turbo-log)

;;; Code:

(defmacro test-inside-mock-buffer (&rest body)
  "Function for navigation to LINE-NUMBER and testing TEST-FUNC in new buffer."
  `(save-window-excursion
     (switch-to-buffer-other-window "*buffer-for-test*")
     (insert "class TestClass {
  private t: any;
  private b: string;
  private myAnotherVariable: any;

  constructor(t: any, b: string, myAnotherVariable: number) {
    console.log(\"TCL: [line 7][test.ts]t: \", t);
    this.t = t;
    this.b = b;
    this.myAnotherVariable = myAnotherVariable;
    console.log(
      \"This message will not be comment, cause it made by programmers.\"
    );
  }
}

function myFuncWithEmptyBody(qwwe) {}")
     (goto-char (point-min))
     ,@body
     (erase-buffer)
     (kill-buffer)))


(ert-deftest test-return-value-by-symbol ()
  (setq turbo-log-my-test-variable "Hello!")
  (should (equal nil (turbo-log--symbol-value-or-nil 'turbo-log-non-existing-variable)))
  (should (equal "Hello!" (turbo-log--symbol-value-or-nil 'turbo-log-my-test-variable))))

(ert-deftest test-turbo-log-get-logger-config ()
  (setq turbo-log-test-logger-config
        '(:loggers ("console.log(%s)" "console.debug(%s)" "console.warn(%s)")
          :msg-format-template "'TCL: %s'"
          :buffer-name-format-template "(%s)"))

  (setq turbo-log-payload-format-template "%s - ")

  (should (equal "%s - " (turbo-log--get-logger-config turbo-log-test-logger-config payload-format-template)))
  (should (equal "(%s)" (turbo-log--get-logger-config turbo-log-test-logger-config buffer-name-format-template)))
  (should (equal nil (turbo-log--get-logger-config turbo-log-test-logger-config non-exited-variable)))
  (should (equal "[%s] " (turbo-log--get-logger-config nil buffer-name-format-template))))


(ert-deftest test-turbo-log--find-top-level-node ())

(ert-deftest test-turbo-log--goto-line ()
  (test-inside-mock-buffer
   (turbo-log--goto-line 5)
   (should (equal 5 (line-number-at-pos))))

  (test-inside-mock-buffer
   (turbo-log--goto-line 1)
   (should (equal 1 (line-number-at-pos))))

  (test-inside-mock-buffer
   (turbo-log--goto-line 0)
   (should (equal 1 (line-number-at-pos)))))

(ert-deftest test-turbo-log-format-meta-info ()
  (test-inside-mock-buffer
   (should (equal "[line 1][*buffer-for-test*] " (turbo-log--format-meta-info 1))))

  (test-inside-mock-buffer
   (setq-local turbo-log-line-number-format-template nil)
   (should (equal "[*buffer-for-test*] " (turbo-log--format-meta-info 1))))

  (test-inside-mock-buffer
   (setq-local turbo-log-buffer-name-format-template nil)
   (should (equal "[line 5]" (turbo-log--format-meta-info 5)))))

(ert-deftest test-turbo-log-get-line-text ()
  (test-inside-mock-buffer
   (should (equal "class TestClass {\n" (turbo-log--get-line-text 1)))
   (should (equal "  private myAnotherVariable: any;\n" (turbo-log--get-line-text 4)))))


(ert-deftest test-turbo-log-get-line-text ()
  (test-inside-mock-buffer
   ;; NOTE: this t points from typescript example
   (set-mark 208)
   (goto-char 214)
   (should (equal "this.t" (turbo-log--get-log-text))))

  (test-inside-mock-buffer
   (kill-new "Hello world")
   (should (equal "Hello world" (turbo-log--get-log-text t)))))


(ert-deftest test-turbo-log-line-with-empty-body-p ()
  (test-inside-mock-buffer
   (should (not (equal nil (turbo-log--line-with-empty-body-p 17)))))

  (test-inside-mock-buffer
   (should (equal nil (turbo-log--line-with-empty-body-p 1)))))

;;; test.el ends here
