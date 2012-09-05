;;; color-theme-wombat.el
;;

(defun color-theme-wombat ()
  "Subdued color theme for Emacs by Jason Blevins.
Based on the Tango color palette."
  (interactive)
  (color-theme-install 
   '(color-theme-wombat
     ((background-color . "#242424")
      (foreground-color . "#f6f3e8")
      (cursor-color . "#656565"))

     (fringe ((t (:background "#262626"))))

     (compilation-error			((t (:foreground "#ff4545"))))
     (font-lock-comment-face		((t (:foreground "#99968b"))))
     (font-lock-doc-face		((t (:foreground "#99968b"))))
     (font-lock-constant-face		((t (:foreground "#e5786d"))))
     (font-lock-string-face		((t (:foreground "#95e454"))))
     (font-lock-variable-name-face	((t (:foreground "#cae682"))))
     (font-lock-function-name-face	((t (:foreground "#cae682"))))
     (font-lock-type-face		((t (:foreground "#ffffff"))))
     (font-lock-builtin-face		((t (:foreground "#cad6f2"))))
     (font-lock-keyword-face		((t (:foreground "#8ac6f2"))))
     (font-lock-preprocessor-face	((t (:foreground "#e5786d"))))
     (font-lock-negation-char-face	((t (:foreground "#e7f6da"))))

     (link				((t (:bold t :underline t :foreground "#8ac6f2"))))
     (show-paren-match			((t (:bold t :foreground "#f6f3e8" :background "light slate blue"))))
     (region				((t (:background "#333333"))))
     (hl-line				((t (:background "#4B3E30"))))
     (magit-diff-add			((t (:foreground "#77ee77"))))
     (magit-item-highlight		((t (:background "gray25"))))

     (isearch				((t (:foreground "black" :background "#55e454"))))
     (minibuffer-prompt			((t (:foreground "#8ac6f2"))))
     (ido-subdir			((t (:foreground "#dd9900"))))
     (ido-only-match			((t (:foreground "#95e454"))))
     (lazy-highlight			((t (:foreground "black" :background "#6a6a6a"))))
     ;; Modeline colors
     (mode-line			        ((t (:foreground "gray" :background "#171717"))))

     (erlang-compile-server-error-line   ((t (:background "#662525"))))
     (erlang-compile-server-warning-line ((t (:background "#7b4a00"))))
     (erlang-compile-server-eunit-line 	 ((t (:background "#662525"))))

     )))

(provide 'color-theme-wombat)

