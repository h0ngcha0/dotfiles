;; Add ~/.emacs.d to load-path
(add-to-list 'load-path "~/.emacs.d")

;===============================================================================
; Misc settings
;===============================================================================

;; Start emacs server
(load "server")
(unless (server-running-p) (server-start))

;; Turn of menubar, toolbar and scrollbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(toggle-scroll-bar -1)

;; Use y/n instead of yes/no for questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Columns are nice
(column-number-mode 1)

;; Mouse wheel
(mouse-wheel-mode 1)

;; Turn on global font lock mode
(global-font-lock-mode 1)

;; Turn on hilighting of brackets
(show-paren-mode t)

;; Set my email address.
(setq user-mail-address "hakan.nilsson@klarna.com")

;; Set the shell emacs uses.
(setq explicit-shell-file-name "/bin/bash")

;; Dont show startup screen
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(jira-url "https://mal.hq.kred/jira/rpc/xmlrpc")
 '(safe-local-variable-values (quote ((allout-layout . t) (erlang-indent-level . 4) (erlang-indent-level . 2)))))

;; Tetris scores files is fuck
(setq tetris-score-file
  "~/.emacs.d/tetris-scores")

;; Enable X clipboard
(setq x-select-enable-clipboard t)

;; Undo/Redo for window management (undo = C-c left, redo = C-c right)
(winner-mode 1)

(setq compilation-scroll-output 'first-error)
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
	 (bury-buffer "*compilation*")
	 (winner-undo)
	 (message "Build successful."))
	(t
	 (message "Compilation exited abnormally: %s" string))))

;===============================================================================
; Load custom modes
;===============================================================================
;; Bash completion
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)

;; Ack
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-samee "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; XML RPC

(require 'xml-rpc)

;; Autopair

(require 'autopair)
(autopair-global-mode)

;; Jira

(require 'jira)

;; Wikimedia

(require 'mediawiki)

(add-to-list 'auto-mode-alist
'("\\.wiki\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist
'("\\cred.policy\\'" . mediawiki-mode))

;; Enable ido mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ido-mode 1)
;; Always open files in read only mode
;;(global-set-key "\C-x\C-f" 'ido-find-file-read-only)
(defalias 'ido-find-file 'ido-find-file-read-only)


;; Nyan mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/nyan-mode/nyan-mode.el")
(nyan-mode 1)
(nyan-start-animation)

;; Color theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)
    (load-file "~/.emacs.d/color-themes/color-theme-wombat.el")
(color-theme-wombat)
;;(color-theme-charcoal-black)p
(setq color-theme-is-global t)

;; Erlang mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons
		 "/home/hakan.nilsson/git/otp_r14b03/install/lib/erlang/lib/tools-2.6.6.4/emacs"
		 load-path))
(setq erlang-root-dir
      "/home/hakan.nilsson/git/otp_r14b03/install/lib/erlang")
(setq exec-path (cons
		 "/home/hakan.nilsson/git/otp_r14b03/install/lib/erlang/lib/tools-2.6.6.4/bin"
		 exec-path))


(require 'erlang-start)
;; apply erlang mode to .erl, .hrl and .yaws
(autoload 'erlang-mode "erlang.el" "" t)
(add-to-list 'auto-mode-alist '("\\.[eh]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.erl.in$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.eterm$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))

;; Distel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/home/hakan.nilsson/git/distel/elisp/")
(require 'distel)
(distel-setup)
(setq erlookup-roots '("/home/hakan.nilsson/development/git/lib" "/home/hakan.nilsson/git/otp_r14b03/install/lib"))

;(setq erl-nodename-cache 'kred\@pansarhaj)
;(setq erlang-compile-server-compile-if-ok nil)

;; Auto-complete
;(defun ac-distel-setup ()
;  (setq ac-sources '(ac-source-distel)))

;(add-hook 'erlang-shell-mode-hook 'ac-distel-setup)

;(defun distel-fred-node (&optional arg)
;  "Switch distel to use FRED node"
;  (interactive "p")
;  (setq derl-cookie "fred")
;  (setq erl-nodename-cache 'fred\@pansarhaj))

;(defun distel-kred-node (&optional arg)
;  "Switch distel to use KRED node"
;  (interactive "p")
;  (setq derl-cookie nil)
;  (setq erl-nodename-cache 'kred\@pansarhaj))

;(require 'erlang-compile-server)
;(erlang-compile-server-setup)
;(setq erlang-compile-server-check-on-save t)
;(setq erlang-compile-server-enable-eunit nil)

;; EDTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/edts/")
(require 'edts-start)

(setq edts-projects
      '(( ;; FRED
	 (name       . "fred")
	 (root       . "~/development/fred")
	 (node-sname . "fred"))
	( ;; KRED
	 (name       . "kred")
	 (root       . "~/development/kred")
	 (node-sname . "kred"))
	( ;; EDTS
	 (name       . "edts-dev")
	 (root       . "~/.emacs.d/edts/")
	 (node-sname . "edts-dev"))
	))

;; Wrangler - Erlang refactoring tool ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-to-list 'load-path "/usr/local/share/wrangler/elisp")
;(require 'wrangler)

;; Smex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Python Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;(add-to-list 'load-path "~/.emacs.d/plugins/eproject")
;(load-file "~/.emacs.d/hakan-eproject.el")

;; Flymake erlang mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'flymake)
;(defun flymake-erlang-init ()
;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;		     'flymake-create-temp-inplace))
;	 (local-file (file-relative-name temp-file
;		(file-name-directory buffer-file-name))))
;    (list "~/.emacs.d/eflymake.erl" (list local-file))))
;
;(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))


;===============================================================================
; Global key bindings
;===============================================================================
(fset 'nice-list-element
   [?\C-e ?\M-b ?\M-f ?\C-k ?\C-a ?\C-n ?\C-d tab backspace backspace ?, ? ])
(global-set-key [f8] 'nice-list-element)


(global-set-key "\C-c\C-f" 'find-file)

;; Compile
;;(global-set-key "\C-cc" 'compile)
 ; TODO: Add cool compile + reload modules :D

;; Revert buffer
(global-set-key [f5] 'revert-buffer)

;; Goto line
(global-set-key "\C-l" 'goto-line)
(global-set-key "\M-g" 'goto-line)

;; Scrolling keybindings
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

(global-set-key "\M-v" 'scroll-one-line-up)
(global-set-key "\C-v"  'scroll-one-line-down)
(global-set-key [S-down] 'scroll-one-line-up)
(global-set-key [S-up]  'scroll-one-line-down)

;; This lets you do M-zf to find a (grep)regexp in all kred-sources.
;; Then you can use next error to find the match in the code.
;; E.g. ^C-zf -> Find: OCRGIRO_BNO_START ^C-.
(defun kfind-at (path word)
  (grep-find
   (concat "find " path
	   (concat " -name '.svn' -prune -o -name '*~' -prune -o -name '*html' -prune -o -type f -print0 | xargs -0 -e grep -n -e " word))))

(defun kfind (word)
  (interactive "MFind: ")
  (kfind-at
   (concat
    (car (split-string (buffer-file-name) "lib"))
    "lib/")
   word))

(defun kred-compile (&optional arg)
  "Compile and reload modules"
  (interactive "p")
  (compile "make -j12" nil)
  (shell-command "kl")
)


(global-set-key "\C-cf" 'kfind)
(global-set-key "\C-cc" 'kred-compile)
(global-set-key "\C-cz" 'erl-who-calls)
(global-set-key "\C-cd" 'erl-fdoc-describe)
(global-set-key "\C-cm" 'erl-find-module)
;; flymake
(global-set-key "\C-ce" 'flymake-display-err-menu-for-current-line)
(global-set-key "\C-cn" 'flymake-goto-next-error)
(global-set-key "\C-cp" 'flymake-goto-prev-error)

(global-set-key "\C-ca" 'align-regexp)
(global-set-key "\C-cw" 'delete-trailing-whitespace)

(global-set-key "\C-cg" 'magit-status)

(global-set-key "\C-cu" 'winner-undo)
(global-set-key "\C-cr" 'winner-redo)

;; key binding for auto complete
(global-set-key (kbd "C-'") 'hippie-expand)
;; auto-complete settings
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-list
                                         try-expand-line))

;; arrange for effective window-switching
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)
(global-set-key [M-left]  'windmove-left)
(global-set-key [M-right] 'windmove-right)

(global-set-key "\C-h" 'delete-backward-char)

;===============================================================================
; Hooks
;===============================================================================

;;; These are my customizations for C mode
(defun my-c-mode-hook ()
  ;; Use k&r style formatting
  (c-set-style "stroustrup")
  ;; Use 4 as the amount for indentation
  (setq c-basic-offset 4)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)

;;; These are my customizations for Erlang mode
(defun my-erlang-mode-hook ()

;  (setq compilation-read-command nil)
;  (setq-default compile-command "make -j12")

  ;; Set default encodings
  (set-terminal-coding-system 'iso-8859-1)
  (set-keyboard-coding-system 'iso-8859-1)
  (setq default-buffer-file-coding-system 'iso-8859-1)
  (prefer-coding-system 'iso-8859-1)
  (set-language-environment "Latin-1")
  (setq file-buffer-coding 'iso-8859-1)
  (let ((mode (current-input-mode)))
    (setcar (cdr (cdr mode)) 8)
    (apply 'set-input-mode mode))

  (setq indent-tabs-mode nil)
;  (setq erlang-indent-level 2)

  (require 'whitespace)
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode t)

  (autopair-global-mode)
  ;; Autocomplete
  ;;(ac-distel-setup)
  ;;  (custom-set-faces
  ;;   '(my-tab-face            ((((class color)) (:background "#3f2020"))) t)
  ;;   '(my-trailing-space-face ((((class color)) (:background "#3f2020"))) t)
;;   '(my-long-line-face ((((class color)) (:background "#3f2020"))) t))
  ;;  (font-lock-add-keywords
  ;;   'erlang-mode
  ;;   '(("\t+" (0 'my-tab-face t))
  ;;     ("^.\\{81,\\}$" (0 'my-long-line-face t))
;;     ("[ \t]+$"      (0 'my-trailing-space-face t))))
;  (if (and (locate-library "my-erlang-flymake")
;	   buffer-file-truename)
;      (progn
;	(load "my-erlang-flymake")
;	(flymake-mode)
;	(erlang-flymake-only-on-save)
;	(local-set-key (kbd "M-'") 'erlang-flymake-next-error)))
  )
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(put 'narrow-to-page 'disabled nil)
