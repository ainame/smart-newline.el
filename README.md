smart-newline.el
================

The `smart-newline.el` provide a stress-less newline command for programmer.
This usage is very simple. Because, you have to only press `RET` key.

!!! WARNING !!!
This is prototype version. I will change specification without notice.
And, this README's description have not implemented yet.
Please wait little.

## INSTALL

write in your ~/.emacs.d
```lisp
(setq el-get-sources '(
        (:name smart-newline
               :type github
               :website "https://github.com/ainame/smart-newline.el"
               :pkgname "ainame/smart-newline.el")
        )
)
```

after `M-x el-get-list-packages`

## CONFIGURATION

write in your ~/.emacs.d

* by minor-mode
`smart-newline` command bind to `RET` and `C-m`.

```lisp
(add-hook 'ruby-mode-hook ;; or any major-mode-hooks
  (lambda ()
    (smart-newline-mode 1)))
```

* by only using any key-bind

```lisp
(define-key global-map (kbd "C-j") 'smart-newline)
```

## BEHAVIORS

List up smart-newline behaviors.

In this section, I use following words.

* newline   - Emacs's default command at `RET` or `C-m`
* open-line - Emacs's default command at `C-o`
* indent    - `indent-according-to-mode` command and almost Emacs's default command at `TAB` or `C-i`

And in these sample figures, `|`  representation editor's cursor.

### pattern 1

```ruby
def foo|  RET  def foo
end        ->    |
               end
```

`newline and indent` when the cursor exists at end of line.

* smart-newline: `RET`
* default Emacs: `C-j` / `RET` -> `TAB`

### pattern 2

```ruby
 def foo   RET  def foo
|end        ->    |
                end
```

`open-line and indent` at begininng of line which has chars.

* smart-newline: `RET`
* default Emacs: `C-o` -> `TAB`

### pattern 3

```ruby
object = |VeryVeryVeryLongSomeClassName.create_object

 | RET
 V

object =
  |VeryVeryVeryLongSomeClassName.create_object
```

`newline and indent` like `C-j`

* smart-newline: `RET`
* default Emacs: `C-j` / `RET` -> `TAB`

### pattern 4

```ruby
def foo         def foo          def foo
end       RET   end       RET    end
|         --->  |         --->
def bar                          |
end             def bar
                end              def bar
                                 end
```

You can insert balanced blank line around start point by only `RET`.
At the end, you will write new a method in smooth.

* smart-newline: `RET` -> `RET`
* default Emacs: `RET` -> `RET` -> `C-p` / `RET` -> `C-o`

### pattern 5

```ruby
def foo         def foo
end             end
          RET
|         --->  |
def bar
end             def bar
                end
```

* smart-newline: `RET`
* default Emacs: `C-o`

### pattern 6

```ruby
def foo         def foo
end             end
|         RET
          --->  |
def bar
end             def bar
                end
```

* smart-newline: `RET`
* default Emacs: `C-o`

### pattern 7

```ruby
 def foo         def foo        def foo        def foo
 end       RET   end      RET   end      RET   end
|def bar   --->  |        --->  |        --->
 end             def bar                       |
                 end            def bar
                                end            def bar
                                               end
```

* smart-newline: `RET` -> `RET` -> `RET`
* default Emacs: `C-o` -> `C-o` -> `RET`

### pattern 8

```ruby
def foo         def foo        def foo        def foo
end|      RET   end      RET   end      RET   end
def bar   --->  |        --->           --->
end             def bar        |              |
                end            def bar
                               end            def bar
                                              end
```

* smart-newline: `RET` -> `RET` -> `RET`
* default Emacs: `RET` -> `RET` -> `C-o`
