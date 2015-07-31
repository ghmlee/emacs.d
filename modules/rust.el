;; rust
(add-to-list 'load-path "~/.emacs.d/packages/rust")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; racer
(setq racer-rust-src-path "~/.emacs.d/src/rustc-1.1.0/src")
(if (eq system-type 'darwin)
    (setq racer-cmd "~/.emacs.d/bin/racer/darwin"))
(add-to-list 'load-path "~/.emacs.d/packages/racer")
(eval-after-load "rust-mode" '(require 'racer))

(add-hook 'rust-mode-hook
	  '(lambda ()
	     (racer-activate)
	     (local-set-key (kbd "M-.") #'racer-find-definition)
	     (local-set-key (kbd "TAB") #'racer-complete-or-indent)))
