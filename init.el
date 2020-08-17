;;; init.el --- emacs configuration
;;; Commentary:
;;; Emacs
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; color theme
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))
(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/atom-one-dark-theme/")
(load-theme 'atom-one-dark t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; encoding
(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)

(setq load-prefer-newer t)
(setq default-korean-keyboard "2")

(require 'ucs-normalize)
(set-file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; misc
(add-to-list 'load-path "~/.emacs.d/packages/exec-path-from-shell/")
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
 (exec-path-from-shell-initialize))
;(setq path "/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/local/go/bin")

(require 'ls-lisp)
(setq ls-lisp-dirs-first t) ;; sort directory first
(setq ls-lisp-use-insert-directory-program nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq gdb-many-windows t)
(setq select-enable-clipboard t) ; Share the clipboard with x-window application
(setq make-backup-files nil)	   ; Do not make backup files
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)
(setq debug-on-error t)
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

(icomplete-mode)
(which-function-mode)

(setq which-func-unknown "?")
(set-face-attribute 'which-func nil :box '(:color "#181A1F") :bold t :foreground "#528BFF")

(put 'set-goal-column 'disabled nil)

(column-number-mode t)			; show column number
(auto-compression-mode t)
(display-time)				; Display time
(tooltip-mode t)			; Use tooltip
(when (functionp 'tool-bar-mode)        ; Don't use toolbar
  (tool-bar-mode -1))
(menu-bar-mode -1)			; Don't use menubar
(if window-system
    (scroll-bar-mode -1))               ; Don't use scrollbar
(setq inhibit-startup-message t)	; Inhibit startup message
(show-paren-mode t)                     ; Show parenthesis match
(transient-mark-mode t)			; Highlight region
;(setq vc-handled-backends nil)         ; diable builtin vc
(save-place-mode 1)                     ; remember cursor position, for emacs 25.1 or later

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; font
(set-frame-font "menlo 11" nil t)

(setq mac-allow-anti-aliasing t)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)
(global-font-lock-mode t)		; Syntax highlight

(put 'narrow-to-region 'disabled nil)

;; key
(when (and (eq system-type 'darwin) window-system)
  (setq ns-use-native-fullscreen nil)
  (setq mac-command-key-is-meta 1)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-M-z") 'toggle-frame-maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))) ; set default frame to maximized

;(global-set-key "\C-x\C-m" 'execute-extended-command)
;(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\r" 'newline-and-indent) ; auto indentation
(global-set-key "\C-c\C-s" 'ispell-word)

(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-x r") 'replace-string)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-t") nil)

;(global-set-key [C-S-iso-lefttab] 'previous-buffer)
(global-set-key [C-tab] 'next-buffer)
(global-set-key (kbd "C-c [") 'previous-buffer)
(global-set-key (kbd "C-c ]") 'next-buffer)
;(global-set-key (kbd "C-c n") 'new-buffer)
;(global-set-key (kbd "C-x k") 'kill-buffer) ; default key binding

;(global-set-key (kbd "M-]") 'next-multiframe-window)
;(global-set-key (kbd "M-[") 'previous-multiframe-window)

;(global-set-key (kbd "M-s") 'shell-command)
;(global-set-key (kbd "M-c") 'compile)
;(global-set-key (kbd "M-a") 'run-ansi-term)
(global-set-key (kbd "C-c C-t") 'multi-term)
;(global-set-key (kbd "<M-SPC>") 'toggle-input-method)

;(defun run-ansi-term ()
;  (interactive)
;  (if (equal "*ansi-term*" (buffer-name))
;      (call-interactively 'rename-buffer)
;    (if (get-buffer "*ansi-term*")
;	(switch-to-buffer "*ansi-term*")
;      (ansi-term "/bin/bash"))))

;(defun new-buffer ()
;  "Create a new frame with a new empty buffer."
;  (interactive)
;  (let ((buffer (generate-new-buffer "*new buffer*")))
;    (set-buffer-major-mode buffer)
;    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;; code
(setq-default truncate-lines t)
(setq-default line-spacing 2)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; shell
(setq sh-basic-offset 2)

;; objc
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "@\\<interface\\>"
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))

(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))

(defadvice ff-get-file-name (around ff-get-file-name-framework
				    (search-dirs
				     fname-stub
				     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
	      (header (match-string 2 fname-stub))
	      (fname-stub (concat framework ".framework/Headers/" header)))
	 ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
			      "/System/Library/Frameworks" "/Library/Frameworks"))

(add-hook 'objc-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

;; rust
(add-to-list 'load-path "~/.emacs.d/packages/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook (lambda ()
			    (setq rust-indent-offset 2)))

;; swift
(add-to-list 'load-path "~/.emacs.d/packages/swift-mode/")
(require 'swift-mode)
(add-hook 'swift-mode-hook (lambda ()
			     (setq swift-mode:basic-offset 2)))

;; go
(add-to-list 'load-path "~/.emacs.d/packages/go-mode/")
(setenv "GOPATH" (concat (getenv "HOME") "/.go"))
(require 'go-mode)
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode 1)
	    (setq tab-width 2)))

;; java
(add-hook 'java-mode-hook
	  (lambda ()
	    "Treat Java 1.5 @-style annotations as comments."
	    (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
	    (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

;; python
(add-hook 'python-mode-hook (lambda ()
			      (setq python-indent 2)))

;; ruby
(add-hook 'ruby-mode-hook (lambda ()
			    (setq ruby-indent-level 2)))
(add-to-list 'auto-mode-alist '("\\Fastfile\\'" . ruby-mode))

;; dockerfile
(add-to-list 'load-path "~/.emacs.d/packages/s/")
(add-to-list 'load-path "~/.emacs.d/packages/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; groovy
(add-to-list 'load-path "~/.emacs.d/packages/dash/")
(add-to-list 'load-path "~/.emacs.d/packages/groovy-mode/")
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))
(add-hook 'groovy-mode-hook (lambda ()
			      (setq groovy-indent-offset 2)))

;; nvm
;(require 'nvm)
;(defun do-nvm-use (version)
;  (interactive "sVersion: ")
;  (nvm-use version)
  ;; exec-path-from-shell made a new login shell at startup and imported values,
  ;; including PATH to exec-path. But nvm-use does setenv "PATH". So we need to
  ;; update exec-path to the current PATH in the Emacs process.
;  (exec-path-from-shell-copy-env "PATH"))

;; javascript
(add-to-list 'load-path "~/.emacs.d/packages/js3-mode/")
(require 'js3-mode)
(setq js-indent-level 2)

;; react
(add-to-list 'load-path "~/.emacs.d/packages/js2-mode/")
(add-to-list 'load-path "~/.emacs.d/packages/rjsx-mode/")
(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

;; prettier-js
(add-to-list 'load-path "~/.emacs.d/packages/prettier-js/")
(defun use-prettier-if-in-node-modules ()
  "Use prettier-js-mode if prettier is found in this file's
project's node_modules. Use the prettier binary from this
project."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
			(expand-file-name "node_modules/.bin/prettier"
                                          root))))
    (when (and prettier (file-executable-p prettier))
      (setq prettier-js-command prettier)
      (prettier-js-mode))))

(when (require 'prettier-js nil t)
  (make-variable-buffer-local 'prettier-js-command)
  (add-hook 'rjsx-mode-hook #'use-prettier-if-in-node-modules))

;; css
(setq css-indent-offset 2)

;; html
(add-to-list 'load-path "~/.emacs.d/packages/multi-web-mode/")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; markdown
(add-to-list 'load-path "~/.emacs.d/packages/markdown-mode/")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; yaml
(add-to-list 'load-path "~/.emacs.d/packages/yaml-mode/")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; org
(require 'org)
;(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; default
;(define-key global-map "\C-cl" 'org-store-link)
;(define-key global-map "\C-ca" 'org-agenda)
;(global-set-key (kbd "C-c C-t") 'org-todo) ; default
;(global-set-key (kbd "C-c C-s") 'org-schedule) ; default
;(global-set-key (kbd "C-c C-d") 'org-deadline) ; default
;(global-set-key (kbd "C-c C-l") 'org-insert-link) ; default
;(global-set-key (kbd "C-c C-c") 'org-ctrl-c-ctrl-c) ; default
(setq org-log-done t)
(setq org-todo-keywords '((sequence "BACKLOG" "READY" "PROGRESS" "|" "DONE")))
(set-face-attribute 'org-level-2 nil :height 1.0 :foreground "#C678DD" :background nil)
(set-face-attribute 'org-level-3 nil :height 1.0 :foreground "#D19A66" :background nil)
(set-face-attribute 'org-level-4 nil :height 1.0 :foreground "#E5C07B" :background nil)
(set-face-attribute 'org-meta-line nil :height 1.0 :foreground "#5C6370" :background nil)
(set-face-attribute 'org-document-title nil :height 1.0 :foreground "#5C6370" :background nil)
(set-face-attribute 'org-document-info nil :height 1.0 :foreground "#5C6370" :background nil)
(set-face-attribute 'org-document-info-keyword nil :height 1.0 :foreground "#5C6370" :background nil)
(set-face-attribute 'org-date nil :height 1.0 :foreground "#828997" :background nil)
(set-face-attribute 'org-special-keyword nil :height 1.0 :foreground "#E06C75" :background nil)
(set-face-attribute 'org-checkbox nil :height 1.0 :foreground "#98C379" :background nil)
(set-face-attribute 'org-link nil :height 1.0 :foreground "#5C6370" :background nil)
(set-face-attribute 'org-agenda-date nil :height 1.0 :foreground "#61AFEF" :background nil)
(set-face-attribute 'org-agenda-done nil :height 1.0 :foreground "#98C379" :background nil)
;(set-face-attribute 'org-footnote nil :height 1.0 :foreground "#E5C07B" :background nil)
;(set-face-attribute 'org-sexp-date nil :height 1.0 :foreground "#E5C07B" :background nil)

;; (defvar atom-one-dark-colors-alist
;;   (let* ((256color  (eq (display-color-cells (selected-frame)) 256))
;;          (colors `(("atom-one-dark-accent"   . "#528BFF")
;;                    ("atom-one-dark-fg"       . (if ,256color "color-248" "#ABB2BF"))
;;                    ("atom-one-dark-bg"       . (if ,256color "color-235" "#282C34"))
;;                    ("atom-one-dark-bg-1"     . (if ,256color "color-234" "#121417"))
;;                    ("atom-one-dark-bg-hl"    . (if ,256color "color-236" "#2C323C"))
;;                    ("atom-one-dark-gutter"   . (if ,256color "color-239" "#4B5363"))
;;                    ("atom-one-dark-mono-1"   . (if ,256color "color-248" "#ABB2BF"))
;;                    ("atom-one-dark-mono-2"   . (if ,256color "color-244" "#828997"))
;;                    ("atom-one-dark-mono-3"   . (if ,256color "color-240" "#5C6370"))
;;                    ("atom-one-dark-cyan"     . "#56B6C2")
;;                    ("atom-one-dark-blue"     . "#61AFEF")
;;                    ("atom-one-dark-purple"   . "#C678DD")
;;                    ("atom-one-dark-green"    . "#98C379")
;;                    ("atom-one-dark-red-1"    . "#E06C75")
;;                    ("atom-one-dark-red-2"    . "#BE5046")
;;                    ("atom-one-dark-orange-1" . "#D19A66")
;;                    ("atom-one-dark-orange-2" . "#E5C07B")
;;                    ("atom-one-dark-gray"     . (if ,256color "color-237" "#3E4451"))
;;                    ("atom-one-dark-silver"   . (if ,256color "color-247" "#9DA5B4"))
;;                    ("atom-one-dark-black"    . (if ,256color "color-233" "#21252B"))
;;                    ("atom-one-dark-border"   . (if ,256color "color-232" "#181A1F")))))

(setq org-todo-keyword-faces
      '(("BACKLOG" . (:foreground "#E5C07B" :weight bold))
	("READY" . (:foreground "#C678DD" :weight bold))
        ("PROGRESS" . (:foreground "#E06C75" :weight bold))
	("DONE" . (:foreground "#98C379" :weight bold))))

;; org-bullets
(add-to-list 'load-path "~/.emacs.d/packages/org-bullets/")
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; magit
(add-to-list 'load-path "~/.emacs.d/packages/async/")
(add-to-list 'load-path "~/.emacs.d/packages/transient/lisp/")
(add-to-list 'load-path "~/.emacs.d/packages/with-editor/")
(add-to-list 'load-path "~/.emacs.d/packages/magit/lisp/")
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; projectile
(add-to-list 'load-path "~/.emacs.d/packages/projectile/")
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c C-g") 'projectile-grep)
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;(global-set-key (kbd "C-c p K") 'projectile-remove-known-project)
(setq projectile-globally-ignored-directories
      (append '(".DS_Store" ".git" ".svn" "out" "repl" "target" "dist" "lib" "node_modules" "libs" "deploy" ".emacs.d" ".cask" "vendor")
              projectile-globally-ignored-directories))
(setq projectile-globally-ignored-files
      (append '(".#*" ".DS_Store" "*.tar.gz" "*.tgz" "*.zip" "*.png" "*.jpg" "*.gif")
              projectile-globally-ignored-files))
 (setq grep-find-ignored-directories (append '("dist" "deploy" "node_modules" "vendor") grep-find-ignored-directories))

;; eyebrowse
(add-to-list 'load-path "~/.emacs.d/packages/eyebrowse/")
(require 'eyebrowse)
(eyebrowse-mode t)
(global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
(global-set-key (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
(global-set-key (kbd "M-[") 'eyebrowse-prev-window-config)
(global-set-key (kbd "M-]") 'eyebrowse-next-window-config)
(global-set-key (kbd "M-q") 'eyebrowse-close-window-config)
;(set-face-attribute 'eyebrowse-mode-line-active nil :underline nil :bold t :foreground "#c98459")

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 10)

;; ivy, swiper, counsel
(add-to-list 'load-path "~/.emacs.d/packages/swiper/")
(add-to-list 'load-path "~/.emacs.d/packages/counsel-projectile/")
(require 'ivy)
(require 'swiper)
(require 'counsel-projectile)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-s") 'swiper)
;(global-set-key (kbd "C-c C-r") 'ivy-resume)
;(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;(global-set-key (kbd "<f1> l") 'counsel-find-library)
;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;(global-set-key (kbd "C-c g") 'counsel-git)
;(global-set-key (kbd "C-c j") 'counsel-git-grep)
;(global-set-key (kbd "C-c k") 'counsel-ag)
;(global-set-key (kbd "C-x l") 'counsel-locate)
;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(counsel-projectile-mode 1)

;; iedit
(add-to-list 'load-path "~/.emacs.d/packages/iedit/")
(require 'iedit)
;(global-set-key (kbd "C-;") 'iedit-mode) ; default key binding
(global-set-key (kbd "C-c r") 'iedit-mode)

;; wgrep
(add-to-list 'load-path "~/.emacs.d/packages/wgrep/")
(require 'wgrep)
;(global-set-key (kbd "C-c C-p") 'wgrep-change-to-wgrep-mode) ; default key binding

;; ibuffer-vc
(add-to-list 'load-path "~/.emacs.d/packages/ibuffer-vc/")
(require 'ibuffer-vc)
(add-hook 'ibuffer-hook (lambda ()
			  (ibuffer-vc-set-filter-groups-by-vc-root)
			  (ibuffer-do-sort-by-recency)))

;; multi-term
(add-to-list 'load-path "~/.emacs.d/packages/multi-term/")
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(setq term-bind-key-alist
      '(("C-c C-c" . term-interrupt-subjob)            ; default
        ("C-c C-e" . term-send-esc)                    ; default
        ("C-c C-j" . term-line-mode)
        ("C-c C-k" . term-char-mode)
        ("C-a"     . term-bol)
        ("C-b"     . term-send-left)
        ("C-f"     . term-send-right)
        ;("C-p"     . previous-line)                   ; default
        ("C-p"     . term-send-up)                     ; default
        ;("C-n"     . next-line)                       ; default
        ("C-n"     . term-send-down)                   ; default
        ("C-s"     . isearch-forward)                  ; default
        ("C-r"     . isearch-backward)                 ; default
        ("C-m"     . term-send-return)                 ; default
        ("C-y"     . term-paste)                       ; default
        ("M-f"     . term-send-forward-word)           ; default
        ("M-b"     . term-send-backward-word)          ; default
        ("M-o"     . term-send-backspace)              ; default
        ("M-p"     . term-send-up)                     ; default
        ("M-n"     . term-send-down)                   ; default
        ;; ("M-M"     . term-send-forward-kill-word)   ; default
        ("M-d"     . term-send-forward-kill-word)
        ;; ("M-N"     . term-send-backward-kill-word)  ; default
        ("M-DEL"   . term-send-backward-kill-word)
        ("M-r"     . term-send-reverse-search-history) ; default
        ("M-,"     . term-send-raw)                    ; default
        ("M-."     . comint-dynamic-complete)))

(defun terminal()
  (interactive)
  (command-execute 'multi-term))

;; company
(add-to-list 'load-path "~/.emacs.d/packages/company/")
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; flycheck
(add-to-list 'load-path "~/.emacs.d/packages/flycheck/")
(require 'flycheck)
(global-flycheck-mode)

;; which-key
(add-to-list 'load-path "~/.emacs.d/packages/which-key/")
(require 'which-key)
(which-key-mode)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; minions
(add-to-list 'load-path "~/.emacs.d/packages/minions/")
(require 'minions)
(minions-mode)
