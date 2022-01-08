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

  (setq-local turbo-log-payload-format-template "%s - ")

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

(defmacro test-inside-typescript-mode-buffer (code &rest body)
  "Function for navigation to LINE-NUMBER and testing TEST-FUNC in new buffer."
  `(save-window-excursion
     (switch-to-buffer-other-window "*buffer-for-test*")
     (typescript-mode)
     (tree-sitter-mode)
     (transient-mark-mode)
     (setq-default typescript-indent-level 2)
     (insert ,code)
     ,@body
     (erase-buffer)
     (kill-buffer)))

(ert-deftest test-turbo-log-print-immediately ()
  (test-inside-typescript-mode-buffer
    "class TestClass {
  public myMethod(arg: number): number {
    return arg * 2;
  }
}"
    ;; NOTE: point for arg variable
   (set-mark 37)
   (goto-char 40)
   (turbo-log-print-immediately)
   (should (equal (buffer-substring (point-min) (point-max)) "class TestClass {
  public myMethod(arg: number): number {
    console.log('TCL: [line 3][*buffer-for-test*] arg: ', arg)
    return arg * 2;
  }
}"))))

(ert-deftest test-turbo-log-print-immediately-with-blank-body ()
  (test-inside-typescript-mode-buffer
    "function testFunc(arg0: string) {}"
    ;; NOTE: point for arg0 variable
   (set-mark 19)
   (goto-char 23)
   (turbo-log-print-immediately)
   (should (equal (buffer-substring (point-min) (point-max)) "function testFunc(arg0: string) {
  console.log('TCL: [line 2][*buffer-for-test*] arg0: ', arg0)
}"))))

(ert-deftest test-turbo-log-print-long-line-variable ()
  (test-inside-typescript-mode-buffer
    "function test() {
  const foo = 1;
  const bar = 2;
  const b = [1, 2, 3,
             10, 12, 22,
             33, 44, 15];

  const a = 4;
  if ((a = 1)) {}
}"
   ;; NOTE: point for b variable
   (set-mark 61)
   (goto-char 62)
   (turbo-log-print-immediately)
   (should (equal (buffer-substring (point-min) (point-max))
                  "function test() {
  const foo = 1;
  const bar = 2;
  const b = [1, 2, 3,
             10, 12, 22,
             33, 44, 15];
  console.log('TCL: [line 7][*buffer-for-test*] b: ', b)

  const a = 4;
  if ((a = 1)) {}
}"))))

(ert-deftest test-turbo-log-extract-correct-identifier ()
  (test-inside-typescript-mode-buffer
   "function test(): string {
  const hello = \"Hello\";
  return hello;
}"
  (set-mark 29)
  (deactivate-mark)
  (goto-char 29)
  (turbo-log-print-immediately)
  (should (equal (buffer-substring (point-min) (point-max)) "function test(): string {
  const hello = \"Hello\";
  console.log('TCL: [line 3][*buffer-for-test*] hello: ', hello)
  return hello;
}"))))


(ert-deftest test-turbo-log-log-insert-above-return ()
  (test-inside-typescript-mode-buffer
   "function test(hello: number): string {
  return hello;
}"
  ;; NOTE: hello indentifiert from return statement
  (set-mark 49)
  (goto-char 54)
  (turbo-log-print-immediately)
  (should (equal (buffer-substring (point-min) (point-max)) "function test(hello: number): string {
  console.log('TCL: [line 2][*buffer-for-test*] hello: ', hello)
  return hello;
}"))))
;;; test.el ends here
