;;============================== misc
(setq path "/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin")
(setenv "PATH" path)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq gdb-many-windows t)
(setq x-select-enable-clipboard t) ; Share the clipboard with x-window application
(setq make-backup-files nil)	   ; Do not make backup files
;;(setq frame-title-format (concat "%b - Waiting for you @ " system-name)) ; Set buffer-name in title
(setq make-backup-files nil)	   ; Do not make backup files
;(setq frame-title-format '("Emacs"))
;(set-frame-width (selected-frame) 80)
;(set-frame-height(selected-frame) 25)
;(set-frame-position (selected-frame) 150 250)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)

(iswitchb-mode)
(icomplete-mode)
(which-function-mode)

(put 'set-goal-column 'disabled nil)

;;(partial-completion-mode t)
(column-number-mode t)			; show column number
(auto-compression-mode t)
(display-time)				; Display time
;(tooltip-mode t)			; Use tooltip
;(tool-bar-mode -1)			; Don't use toolbar
(menu-bar-mode -1)			; Don't use menubar
;(scroll-bar-mode -1)			; Don't use scrollbar
(setq inhibit-startup-message t)	; Inhibit startup message
(show-paren-mode t)                     ; Show parenthesis match
(transient-mark-mode t)			; Highlight region

(when (>= emacs-major-version 23)
  (tooltip-mode t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; font lock
(setq font-lock-maximum-decoration t
      font-lock-maximum-size nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)
(global-font-lock-mode t)		; Syntax highlight

(put 'narrow-to-region 'disabled nil)
(setq debug-on-error nil)
