(add-to-list 'load-path "~/.emacs.d/packages/js3/")

(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
