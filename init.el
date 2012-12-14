(load "~/.emacs.d/modules/encoding.el")                    ; encoding
(load "~/.emacs.d/modules/key.el")                         ; key
(load "~/.emacs.d/modules/misc.el")                        ; misc
(load "~/.emacs.d/modules/font.el")                        ; font
(when (and (>= emacs-major-version 23) (window-system))    ; frame
  (load "~/.emacs.d/modules/window.el"))
(load "~/.emacs.d/modules/theme.el")                       ; color theme
(load "~/.emacs.d/modules/objc.el")                        ; objective-c
(load "~/.emacs.d/modules/java.el")                        ; java
(load "~/.emacs.d/modules/js.el")                          ; javascript
(load "~/.emacs.d/modules/python.el")                      ; python
