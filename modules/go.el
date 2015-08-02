;; go?1;2c
(add-to-list 'load-path "~/.emacs.d/packages/go/")
(require 'go-mode-autoloads)
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c i") 'go-goto-imports)))
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd \"M-.\") 'godef-jump)))
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-default)
	    (setq tab-width 4)
	    (setq standard-indent 4)
	    (setq indent-tabs-mode nil)))

;; gocode
;(add-to-list 'load-path "~/.emacs.d/elpa/company-0.8.12")
;(add-to-list 'load-path "~/.emacs.d/packages/gocode")

;(require 'company)                                   ; load company mode
;(require 'company-go)

;(add-hook 'go-mode-hook (lambda ()
;			  (set (make-local-variable 'company-backends) '(company-go))
;			  (company-mode)))
;(setq company-tooltip-limit 20)                      ; bigger popup window
;(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
;(setq company-echo-delay 0)                          ; remove annoying blinking
;(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
