;; erlang-flymake.el --- Flymake support.

;; Author:   Mats Cronqvist
;; Version:  0.1
;; Keywords: erlang, flymake
;; Created:  2009-05-14

;; based on snippets stolen from 
;; http://www.emacswiki.org/emacs/FlyMake
;; and
;; http://www.emacswiki.org/emacs/FlymakeErlang

;; to enable, put something like this in your erlang-mode-hook
;;   (flymake-mode)
;;   (local-set-key (kbd "M-'") 'erlang-flymake-next-error)

(if (locate-library "flymake")
    (progn
      (require 'flymake)
      (defun erlang-flymake-tmp-filename(filename prefix)
        "Make a temp file name (in $HOME/.erlang-flymake)"
        (let* ((tmp-dir (concat (getenv "HOME") "/.erlang-flymake"))
               (tmp-name (file-name-nondirectory filename))
               (tmp-file (concat tmp-dir "/" tmp-name)))
          (flymake-log 3 "temp-file is: %s" tmp-file)
          tmp-file))
      (defun erlang-flymake-init ()
        "Set up the command used to parse our buffer"
        (let* ((erlang-dir 
                (file-name-directory (locate-library "erlang-flymake")))
               (temp-file (flymake-init-create-temp-buffer-copy
                           'erlang-flymake-tmp-filename)))
          (list (concat erlang-dir "flymaker.sh") 
                (list temp-file default-directory))))
      (defun erlang-flymake-next-error ()
        "Goto next error, if any. Display error in mini-buffer."
        (interactive)
        (flymake-goto-next-error)
        (let ((err (get-char-property (point) 'help-echo)))
          (when err
            (message err))))
      ;; add our init file to flymake's alist of filename regexps
      (add-to-list 
       'flymake-allowed-file-name-masks
       '(".+\\.erl$" erlang-flymake-init))))

(defun erlang-flymake-only-on-save ()
  "Trigger flymake only when the buffer is saved (disables syntax
check on newline and when there are no changes)."
  (interactive)
  ;; There doesn't seem to be a way of disabling this; set to the
  ;; largest int available as a workaround (most-positive-fixnum
  ;; equates to 8.5 years on my machine, so it ought to be enough ;-) )
  (setq flymake-no-changes-timeout most-positive-fixnum)
  (setq flymake-start-syntax-check-on-newline nil))
