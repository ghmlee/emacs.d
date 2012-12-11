;; font config
(set-default-font "menlo 11")
(when (>= emacs-major-version 23)
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                    '("Apple SD Gothic Neo" . "iso10646-1")))
