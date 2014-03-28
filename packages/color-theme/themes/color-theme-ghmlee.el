(require 'color-theme)

(defun color-theme-ghmlee ()
  (interactive)
  (color-theme-install
   '(color-theme-ghmlee
     ((foreground-color . "#abaeb3")
      (background-color . "#000000")
      (background-mode . dark)
      (cursor-color . "#606060")
      (mouse-color . "#606060"))

     ;;; Standard font lock faces
     (default ((t (:foreground "#abaeb3"))))
     (font-lock-comment-face ((t (:foreground "#496b83"))))
     (font-lock-comment-delimiter-face ((t (:foreground "#666688"))))
     (font-lock-doc-face ((t (:foreground "#77507b"))))
     (font-lock-doc-string-face ((t (:foreground "#496b83"))))
     (font-lock-string-face ((t (:foreground "#699abc"))))
     (font-lock-keyword-face ((t (:foreground "#3ca380"))))
     (font-lock-builtin-face ((t (:foreground "#6767ae"))))
     (font-lock-function-name-face ((t (:foreground "#3ca380"))))
     (font-lock-variable-name-face ((t (:foreground "#547B96"))))
     (font-lock-preprocessor-face ((t (:foreground "#888a85"))))
     (font-lock-constant-face ((t (:foreground "#3f5c70"))))
     (font-lock-type-face ((t (:foreground "#484879"))))
     (font-lock-warning-face ((t (:foreground "#cc0000"))))

     ;; Search
     (isearch ((t (:foreground "#ffffff" :background "#bb3311"))))
     (isearch-lazy-highlight-face ((t (:foreground "#ffffff" :background "#bb3311"))))

     ;; Emacs Interface
     (fringe ((t (:background "#000000"))))
     (border ((t (:background "#000000"))))
     (mode-line ((t (:background "#1f1f1f" :foreground "#eeeeec"))))
     (mode-line-buffer-id ((t (:background "#1f1f1f" :foreground "#eeeeec"))))
     (mode-line-inactive ((t (:background "#1f1f1f" :foreground "#888a85"))))
     (minibuffer-prompt ((t (:foreground "#9489C4"))))
     (region ((t (:background "#2e3436"))))
     
     ;; Parenthesis matching
     (show-paren-match-face ((t (:foreground "#0000ff" :background "#ffffff"))))
     (show-paren-mismatch-face ((t (:foreground "#0000ff" :background "#ffffff"))))

     ;; Line highlighting
     (highlight ((t (:background "#1f1f1f" :foreground nil))))
     (highlight-current-line-face ((t (:background "#1f1f1f" :foreground nil))))

     ;; Calendar
     (holiday-face ((t (:foreground "#cc0000"))))

     ;; Info
     (info-xref ((t (:foreground "#729fcf"))))
     (info-xref-visited ((t (:foreground "#ad7fa8"))))

     ;;; AUCTeX
     (font-latex-sectioning-5-face ((t (:foreground "#c4a000"))))
     (font-latex-bold-face ((t (:foreground "#4e9a06"))))
     (font-latex-italic-face ((t (:foreground "#4e9a06"))))
     (font-latex-math-face ((t (:foreground "#855c1b"))))
     (font-latex-string-face ((t (:foreground "#77507b"))))
     (font-latex-warning-face ((t (:foreground "#cc0000"))))
     (font-latex-slide-title-face ((t (:foreground "#c4a000"))))

     ;;; post-mode
     (post-emoticon-face ((t (:background "#edd400" :foreground "#000000"))))
     (post-header-value-face ((t (:foreground "#4e9a06"))))
     (post-header-keyword-face ((t (:foreground "#4e9a06"))))
     (post-signature-text-face ((t (:foreground "#cc0000"))))
     (post-quoted-text-face ((t (:foreground "#855c1b"))))
     (post-double-quoted-text-face ((t (:foreground "#77507b"))))
     (post-multiply-quoted-text-face ((t (:foreground "#61635e"))))
     (post-email-address-text-face ((t (:foreground "#729fcf"))))
     (post-url-face ((t (:foreground "#729fcf"))))
     )))

(provide 'color-theme-ghmlee)