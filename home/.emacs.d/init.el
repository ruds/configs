(setq custom-file (concat user-emacs-directory "custom.el"))
;; customizations must be loaded before we muck with packages
(load custom-file)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Custom commands
(defun quit-other-window (count)
  "Call quit-window in the other window"
  (interactive "p")
  (other-window count)
  (quit-window))

(defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
                 (file-exists-p filename))
        (delete-file filename))
      (kill-buffer buffer))))

(defun repeatify (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

;; Custom keybindings (not package-related)
(windmove-default-keybindings)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x 4 q") 'quit-other-window)
(global-set-key (kbd "M-S-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; Load packages
(require 'package)
(setq package-pinned-packages
      '((bind-key . "melpa")
        (diminish . "melpa")
        (use-package . "melpa")))
(package-initialize)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package quelpa)
(use-package quelpa-use-package)

;; Basic emacs stuff
(use-package delight)

(use-package dired-x
  :ensure nil
  :after dired)

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package helm
  :bind (("M-i" . helm-imenu)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :demand t
  :delight)

(use-package eldoc
  :delight)

(use-package which-key
  :delight)

(use-package org
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture))
  :hook ((org-shiftup-final . windmove-up)
         (org-shiftleft-final . windmove-left)
         (org-shiftdown-final . windmove-down)
         (org-shiftright-final . windmove-right)
         (org-mode . use-very-safe-yas-expand)
         (org-mode . display-fill-column-indicator-mode)
         (org-mode . auto-fill-mode)
         (org-src-mode . display-fill-column-indicator-mode))
  :preface
  ;; Recommended by org manual.
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
  (defun use-very-safe-yas-expand ()
    (make-variable-buffer-local 'yas/trigger-key)
    (setq yas/trigger-key [tab])
    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
    (define-key yas/keymap [tab] 'yas/next-field))
  :config
  (setq org-log-done 'note
        ;; Necessary until/unless my patch lands
        org-src-window-setup 'plain)
  :defer t)

(use-package keychain-environment
  :quelpa (keychain-environment :fetcher github :repo "tarsius/keychain-environment"))

;; General programming
(use-package magit
  :defer t)

(use-package kotlin-mode
  :defer t)

(use-package sml-mode
  :preface
  (defun my-sml-rules (orig kind token)
    (if (and (equal kind ':after) (equal token "struct")) 4
      (funcall orig kind token)))
  :config
  (advice-add 'sml-smie-rules :around 'my-sml-rules)
  :defer t)

(use-package smartparens
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :config (progn
            (require 'smartparens-config)
            (show-smartparens-global-mode t)))

;; LSP mode
(use-package helm-xref
  :after helm)

(use-package dap-mode
  :defer t)

(use-package yasnippet
  :delight yas-minor-mode)

(use-package company
  :defer t
  :delight " ©")

(use-package flymake
  :config
  (progn
    (defvar flymake-goto-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map goto-map)
        map))
    (bind-keys :map flymake-mode-map
               :prefix-map flymake-goto-map
               :prefix "M-g"
               ("f" . flymake-goto-next-error)
               ("M-f" . flymake-goto-next-error)
               ("F" . flymake-goto-prev-error)
               ("M-F" . flymake-goto-prev-error))
    (repeatify 'flymake-goto-map))
  :delight)

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1)
  :delight '(:eval (concat " P:" (projectile-project-name)))
  :config
  (use-package helm-projectile
    :demand t
    :config
    (helm-projectile-on)))

(use-package lsp-java
  :defer t)

(use-package lsp-mode
  :hook (((c-mode c++-mode java-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (require 'dap-cpptools)
  (setq read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil)
  (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list))

;; Viewing media
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
