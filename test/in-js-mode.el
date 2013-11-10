(require 'test-helper)

(defun insert-newline-once-in-js-mode ()
  (require 'js)
  (js-mode)
  (smart-newline))

(defun insert-newline-twice-in-js-mode ()
  (require 'js)
  (js-mode)
  (smart-newline)
  (smart-newline))

(defun insert-newline-triple-in-js-mode ()
  (require 'js)
  (js-mode)
  (smart-newline)
  (smart-newline)
  (smart-newline))

(ert-deftest test-js-mode-01 ()
  "insert newline at end of line in js-mode."
  (cursor-test/equal
   :actual (cursor-test/setup
            :init "
var foo = function () {
    bar(function (){
        xxxx;
    });
|};
"
            :exercise 'insert-newline-twice-in-js-mode)
   :expect (cursor-test/setup
            :init "
var foo = function () {
    bar(function (){
        xxxx;
    });

    |
};
")))
