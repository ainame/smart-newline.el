;;; smart-newline.el --- Provide smart newline(C-m) which includes open-line(C-o) and newline-and-inden(C-j).

;; Copyright (C) 2013 Satsohi Namai

;; Author: Satoshi Namai
;; Version: 0.1

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

;; Provide smart newline(C-m) which includes open-line(C-o) and newline-and-inden(C-j)
;;
;;

;;; Code:
(defvar smart-newline/open-line-count 0)
(make-variable-buffer-local 'smart-newline/open-line-count)

(defvar smart-newline/key-code-of-return 13)

(defun smart-newline/increment-open-line ()
  (setq smart-newline/open-line-count (+ smart-newline/open-line-count 1)))

(defun smart-newline/clear-open-line-count ()
  (setq smart-newline/open-line-count 0))

(defun smart-newline/post-command-hook ()
  (if (not (eq last-command-event smart-newline/key-code-of-return))
      (smart-newline/clear-open-line-count)
    nil))

(defun smart-newline/newline-and-indent ()
  (newline)
  (indent-according-to-mode))

(defun smart-newline/open-line-between ()
  (open-line 1)
  (indent-according-to-mode)
  (save-excursion
    (forward-line)
    (indent-according-to-mode)
    (previous-line)))

;;;###autoload
(defun smart-newline ()
  "newline-or-openline is a new command for merging C-m and C-o"
  (interactive)
  (let ((string-exists-before-cursor (string-match "[^\\\s\\\n\\\t]" (buffer-substring (point-at-bol) (point))))
        (string-exists-after-cursor (string-match "[^\\\s\\\n\\\t]" (buffer-substring (point) (point-at-eol)))))
    (cond ((or (and (= smart-newline/open-line-count 0)
                    (eolp))
               (and (>= smart-newline/open-line-count 2)
                    (or (not string-exists-after-cursor)
                        (and string-exists-before-cursor string-exists-after-cursor)))
               (and (= smart-newline/open-line-count 0)
                    (and string-exists-before-cursor string-exists-after-cursor)))
           (progn
             (smart-newline/newline-and-indent)
             (smart-newline/clear-open-line-count)))
          (t (progn
               (smart-newline/open-line-between)
               (smart-newline/increment-open-line))))))

(add-hook 'post-command-hook 'smart-newline/post-command-hook)

(provide 'smart-newline)

;;; smart-newline.el ends here
