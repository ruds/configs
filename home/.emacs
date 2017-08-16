;; Poor little emacs
(if window-system (tool-bar-mode -1))
(setq viper-mode t)
(require 'viper)

(prefer-coding-system 'utf-8)

(add-to-list 'load-path "~/.emacs.d/cc-mode/")
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'uniquify)
(require 'ffap)
(require 'clojure-mode)

(load "~/.emacs.d/local-functions.el")
;;(require 'rust-mode)
;;(defun my-haskell-mode-hook ()
;;  (setq indent-tabs-mode nil))
;;(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defadvice viper-maybe-checkout (around viper-svn-checkin-fix activate)
      "Advise viper-maybe-checkout to ignore svn files."
      (let ((file (expand-file-name (buffer-file-name buf))))
        (when (and (featurep 'vc-hooks)
                   (not (memq (vc-backend file) '(nil Git SVN Hg))))
          ad-do-it)))

(show-paren-mode 1)

(global-set-key "\C-q" 'fill-paragraph)
(global-set-key "\C-ct" 'ffap)
(global-set-key "\C-c\C-t" 'ffap-other-window)
(global-set-key "\C-c>" (lambda () (interactive) (indent-rigidly-n 2)))
(global-set-key "\C-c<" (lambda () (interactive) (indent-rigidly-n -2)))
(global-set-key "\C-cp" 'transpose-windows)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.clj\'" . clojure-mode))

(setq-default indent-tabs-mode nil)

;; c-mode stuffs
(c-add-style "my-c-style"
	     '((c-basic-offset . 2)
	       (c-offsets-alist . ((innamespace . 0)
                                   (case-label . +)
                                   (statement-case-intro . +)
				   (access-label . /)
				   (member-init-intro . ++)
				   (arglist-intro . ++)))))
(defun my-c-mode-hook ()
  (local-set-key "\C-c\C-f" 'compile)
  (local-set-key ";" 'self-insert-command)
  (local-set-key ":" 'self-insert-command)
  (local-set-key "<" 'self-insert-command)
  (local-set-key ">" 'self-insert-command)
  (local-set-key "(" 'self-insert-command)
  (local-set-key "(" 'self-insert-command)
  (local-set-key ")" 'self-insert-command)
  (local-set-key "," 'self-insert-command)
  (local-set-key "{" 'self-insert-command)
  (local-set-key "}" 'self-insert-command)
  (c-set-style "my-c-style"))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; python stuffs
(defun my-python-mode-hook ()
  (local-set-key "\C-c\C-f" 'compile))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-rust-mode-hook ()
  (local-set-key "\C-c\C-f" 'compile))
(add-hook 'rust-mode-hook 'my-rust-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-skip-threshold 2)
 '(compile-command "ant -find build.xml")
 '(dabbrev-case-fold-search nil)
 '(inhibit-startup-screen t)
 '(latex-run-command "pdflatex")
 '(make-backup-files nil)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix))))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(viper-want-ctl-h-help t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'color-theme)
(color-theme-initialize)
(color-theme-hober)
