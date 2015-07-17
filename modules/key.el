;;============================== global set key

;; Fullscreen
(when (and (eq system-type 'darwin) window-system)
  (setq mac-command-key-is-meta 1)
  (setq mac-command-modifier 'meta)
;  (global-set-key (kbd "M-RET") 'ns-toggle-fullscreen))
  (global-set-key (kbd "C-M-z") 'toggle-frame-maximized)
  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen))

;; Ctrl key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\r" 'newline-and-indent) ; auto indentation
(global-set-key "\C-c\C-s" 'ispell-word)
(global-set-key "\C-c\C-r" 'shell-command-curr-buffer)

(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;(global-set-key "\C-z" 'run-ansi-term)
;(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;(global-set-key "\C-ch" 'hs-hide-all)
;;(global-set-key "\C-ca" 'hs-show-all)
;;(global-set-key "\C-cb" 'hs-hide-block)
;;(global-set-key "\C-cs" 'hs-show-block)

;; Meta key
(global-set-key (kbd "M-.") 'scroll-up-n)
(global-set-key (kbd "M-,") 'scroll-down-n)

(global-set-key [C-S-iso-lefttab] 'previous-buffer)
(global-set-key [C-tab] 'next-buffer)

(global-set-key (kbd "M-]") 'next-multiframe-window)
(global-set-key (kbd "M-[") 'previous-multiframe-window)

;; function keys
(global-set-key [f6] 'shell-command)
;;(global-set-key [f6] 'compile)
;;(global-unset-key [f7])

;; ansi-term
(defun run-ansi-term ()
  (interactive)
  (if (equal "*ansi-term*" (buffer-name))
      (call-interactively 'rename-buffer)
      (if (get-buffer "*ansi-term*")
   (switch-to-buffer "*ansi-term*")
   (ansi-term "/bin/bash"))))
