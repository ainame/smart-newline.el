;;; cursor-test.el --- testing library for cursor position in emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2013 by Satsohi Namai

;; Author: ainame
;; URL: https://github.com/ainame/cursor-test.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24"))

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
;;;
;;; see: https://github.com/ainame/cursor-test.el
;;;

;;; Code:

(require 'cl)
(require 'ert)

(defvar cursor-test/cursor-char "|"
  "The representation of cursor in `cursor-test` library.")

(defun cursor-test/setup-cursor (cursor-char)
  (goto-char (point-max))
  (re-search-backward cursor-char nil t)
  (delete-char (length cursor-char)))

(defun cursor-test/setup-test-buffer (func)
  "call func at test-buffer and return test-buffer"
  (let ((test-buffer (generate-new-buffer "*cursor-test-test*"))
        (curr-buf (current-buffer)))
    (switch-to-buffer test-buffer)
    (funcall func)
    (switch-to-buffer curr-buf)
    test-buffer))

(defun cursor-test/pretty-format-cursor (buf point)
  (with-current-buffer buf
    (goto-char point)
    (insert cursor-test/cursor-char)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun* cursor-test/setup (&key init exercise)
  "`cursor-test/setup` works for creating argument value of `cursor-test/equal`.
INIT is the initialize value of buffer which is the string. In INIT string,
you can declare cursor position by `|` which defined at `cursor-test/cursor-char`."
  (cursor-test/setup-test-buffer
   (lambda ()
     (insert init)
     (cursor-test/setup-cursor cursor-test/cursor-char)
     (if exercise (funcall exercise)))))

(defun* cursor-test/compare-buffer (&key compare description expect actual)
  (let* ((expect-point (with-current-buffer expect (point)))
         (actual-point (with-current-buffer actual (point)))
         (result (if (eq compare 'equal)
                     (= expect-point actual-point)
                   (not (= expect-point actual-point)))))
    (if result
        t
      (progn
        (message (if description
                     (format "Fail test: %s" description)
                   "Fail test"))
        (message (format "[buffer]\n expect: \"%s\"\n actual: \"%s\""
                         (cursor-test/pretty-format-cursor expect expect-point)
                         (cursor-test/pretty-format-cursor actual actual-point)))
        (message (format "[point] expect: %d, actual: %d" expect-point actual-point))
        nil))))

(defun* cursor-test/equal (&key description expect actual)
  "`cursor-test/equal` is the assert for equal of cursor position between two buffers.
EXPECT and ACTUAL are buffer in emacs that contain the infomation of cursor position."
  (should
   (cursor-test/compare-buffer :compare 'equal :description description :expect expect :actual actual)))

(defun* cursor-test/not-equal (&key description expect actual)
  (should-not
   (cursor-test/compare-buffer :compare 'not-equal :description description :expect expect :actual actual)))


(defun* cursor-test/equal* (&key description init exercise expect)
  "`cursor-test/equal*` is the shorthand version of `cursor-test/equal`.
This function's arguments contain their's one."
  (cursor-test/equal
   :description description
   :expect (cursor-test/setup :init expect)
   :actual (cursor-test/setup :init init :exercise exercise)))

(defun* cursor-test/not-equal* (&key description init exercise expect)
  (cursor-test/not-equal
   :description description
   :expect (cursor-test/setup :init expect)
   :actual (cursor-test/setup :init init :exercise exercise)))


(provide 'cursor-test)
;;; cursor-test.el ends here
