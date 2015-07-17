(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/color-theme/")

(if window-system
    (load-theme 'atom-one-dark t)
  (load-theme 'atom-dark t))
