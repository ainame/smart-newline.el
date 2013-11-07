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
(defvar smart-newline/key-code-of-return 13)
(defvar smart-newline/regexp-visible-chars "[^\\\s\\\n\\\t]")

(defun smart-newline/exist-string-before-cursor-p ()
  (string-match smart-newline/regexp-visible-chars (buffer-substring (point-at-bol) (point))))

(defun smart-newline/exist-string-after-cursor-p ()
  (string-match smart-newline/regexp-visible-chars (buffer-substring (+ (point) 1) (point-at-eol))))

(defun smart-newline/exist-string-on-line-p ()
  (string-match smart-newline/regexp-visible-chars
                (buffer-substring (point-at-bol) (point-at-eol))))

(defun smart-newline/exist-string-forward-line-p (num)
  (save-excursion
    (forward-line num)
    (smart-newline/exist-string-on-line-p)))

(defun smart-newline/search-exists-string-line-distance (direction limit)
  (smart-newline/search-exists-string-line-distance-count direction limit 0))

(defun smart-newline/search-exists-string-line-distance-count (direction limit distance)
  (let ((delta (cond ((> direction 0) 1) ((< direction 0) -1))))
    ;; for debug
    ;; (princ (format "dire: %s limit: %s, distance: %s, -p: %s\n"
    ;;                direction limit distance (smart-newline/exist-string-forward-line-p distance)))
    (cond ((or (<= limit 0) (smart-newline/exist-string-forward-line-p (* delta distance))) distance)
          (t (smart-newline/search-exists-string-line-distance-count direction (- limit 1) (+ distance 1))))))

(defun smart-newline/exist-string-previous-line-of-cursor-p ()
  (smart-newline/exist-string-forward-line-p -1))

(defun smart-newline/exist-string-next-line-of-cursor-p ()
  (smart-newline/exist-string-forward-line-p 1))

(defun smart-newline/exist-cursor-on-blank-line-which-be-sandwithed-p ()
  (and (not (smart-newline/exist-string-on-line-p))
       (smart-newline/exist-string-previous-line-of-cursor-p)
       (smart-newline/exist-string-next-line-of-cursor-p)))

(defun smart-newline/newline-and-indent ()
  (reindent-then-newline-and-indent))
(defun smart-newline/open-line-between ()
  (indent-according-to-mode)
  (open-line 1)
  (indent-according-to-mode)
  (save-excursion
    (forward-line)
    (indent-according-to-mode)
    (forward-line -1)))

;;;###autoload
(defun smart-newline ()
  "smart-newline is a newline command which designed for programmer."
  (interactive)
  (let ((exist-string-before-cursor      (smart-newline/exist-string-before-cursor-p))
        (exist-string-after-cursor       (smart-newline/exist-string-after-cursor-p))
        (distance-of-not-empty-line-above (smart-newline/search-exists-string-line-distance -1 3))
        (distance-of-not-empty-line-below (smart-newline/search-exists-string-line-distance 1 3)))
    (cond ((/= distance-of-not-empty-line-above distance-of-not-empty-line-below)
           (cond ((> distance-of-not-empty-line-above distance-of-not-empty-line-below)
                  (smart-newline/open-line-between))
                 (t
                  (smart-newline/newline-and-indent))))
          ((or (and (not exist-string-before-cursor) exist-string-after-cursor)
               (smart-newline/exist-cursor-on-blank-line-which-be-sandwithed-p))
           (smart-newline/open-line-between))
          ((or (eolp)
               (not exist-string-after-cursor)
               (and exist-string-before-cursor exist-string-after-cursor))
           (smart-newline/newline-and-indent))
          (t
           (smart-newline/newline-and-indent)))))

(defvar smart-newline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'smart-newline)))

;;;###autoload
(define-minor-mode smart-newline-mode
  "smart-newline-mode is a minor-mode for using smart-newline command by default key-map."
  :lighter " SN" :keymap 'smart-newline-mode-map)

(provide 'smart-newline)

;;; smart-newline.el ends here
