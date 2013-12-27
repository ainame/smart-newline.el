(require 'test-helper)

(defun cperl-mode-setting ()
  (defalias 'perl-mode 'cperl-mode)
  (require 'cperl-mode)
  (cperl-mode)
  (setq cperl-close-paren-offset -4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-indent-level 4)
  (setq indent-tabs-mode nil)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-tab-always-indent t)
  (setq cperl-auto-newline nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-electric-parens nil)
  (setq cperl-electric-lbrace-space nil))

(defun insert-newline-once-and-down-cursor-in-cperl-mode ()
  (cperl-mode-setting)
  (smart-newline)
  (next-line))


(ert-deftest test-experimental-01 ()
  "remove empty previouse line in cperl-mode."
  (cursor-test/equal
   :type 'point
   :actual (cursor-test/setup
            :init "
sub foo {
    my $self = shift;
    |
    return $self->foo;
}
"
            :exercise 'insert-newline-once-and-down-cursor-in-cperl-mode)
   :expect (cursor-test/setup
            :init "
sub foo {
    my $self = shift;
    
|
    return $self->foo;
}
")))
