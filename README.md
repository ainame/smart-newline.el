smart-newline.el
================

Provide smart newline(C-m) which includes open-line(C-o) and newline-and-inden(C-j).

### INSTALL

write in your ~/.emacs.d
```emacs
(setq el-get-sources '(
        (:name smart-newline
               :type github
               :website "https://github.com/ainame/smart-newline.el"
               :pkgname "ainame/smart-newline.el")
        )
)
```

after `M-x el-get-list-packages`

### SETTING

write in your ~/.emacs.d

```emacs
(define-key global-map (kbd "C-m") 'smart-newline)
```
