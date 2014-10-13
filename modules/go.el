(add-to-list 'load-path "~/.emacs.d/packages/go/")
(require 'go-mode)
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c i") 'go-goto-imports)))
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd \"M-.\") 'godef-jump)))

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-default)
	    (setq tab-width 2)
	    (setq standard-indent 2)
	    (setq indent-tabs-mode nil)))