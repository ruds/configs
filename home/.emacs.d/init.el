(setq custom-file (concat user-emacs-directory "custom.el"))
;; customizations must be loaded before we muck with packages
(load custom-file)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Custom commands
(defun rerun-compilation ()
  "Rerun the most recent compilation from the *compilation* buffer."
  (interactive)
  (let ((cbuf (get-buffer "*compilation*")))
    (when cbuf
      (save-current-buffer
        (set-buffer cbuf)
        (call-interactively 'recompile)))))

(defun quit-other-window (count)
  "Call `quit-window' in the other window indicated by COUNT."
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

;; From https://emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;; From https://emacswiki.org/emacs/IncrementNumber
(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defvar my/ediff-windows nil)
(defun my/ediff-before-setup-hook ()
  (setq my/ediff-windows (current-window-configuration)))
(defun my/ediff-quit-hook()
  (set-window-configuration my/ediff-windows))
(add-hook 'ediff-before-setup-hook #'my/ediff-before-setup-hook)
(add-hook 'ediff-quit-hook #'my/ediff-quit-hook)

(defun my-c-transpose-args--forward-to-argsep ()
  "Move to the end of the current c function argument.
Returns point."
  (interactive)
  (while (progn
           (comment-forward most-positive-fixnum)
           (looking-at "[^,)]"))
    (forward-sexp))
  (point))

(defun my-c-transpose-args--backward-to-argsep ()
  "Move to the beginning of the current c function argument.
Returns point."
  (interactive)
  (let ((pt (point))
        cur)
    (up-list -1)
    (forward-char)
    (while (progn
             (setq cur (point))
             (> pt (my-c-transpose-args--forward-to-argsep)))
      (forward-char))
    (goto-char cur)))

(defun my-c-transpose-args--direction (is_forward)
  "Transpose two arguments of a c-function.
The first arg is the one with point in it."
  (interactive)
  (let* ((pt-original (point)) ;; only different to pt when not 'is_forward'
         (pt (progn
               (when (not is_forward)
                 (goto-char (- (my-c-transpose-args--backward-to-argsep) 1))
                 (unless (looking-at ",")
                   (goto-char pt-original)
                   (user-error "Argument separator not found")))
               (point)))
         (b (my-c-transpose-args--backward-to-argsep))
         (sep (progn
                (goto-char pt)
                (my-c-transpose-args--forward-to-argsep)))
         (e (progn
              (unless (looking-at ",")
                (goto-char pt-original)
                (user-error "Argument separator not found"))
              (forward-char)
              (my-c-transpose-args--forward-to-argsep)))
         (ws-first (buffer-substring-no-properties
                    (goto-char b)
                    (progn
                      (skip-chars-forward "[[:space:]\n]")
                      (point))))
         (first (buffer-substring-no-properties (point) sep))
         (ws-second (buffer-substring-no-properties
                     (goto-char (1+ sep))
                     (progn
                       (skip-chars-forward "[[:space:]\n]")
                       (point))))
         (second (buffer-substring-no-properties (point) e)))

    (delete-region b e)
    (insert ws-first second "," ws-second first)

    ;; Correct the cursor location to be on the same character.
    (if is_forward
        (goto-char
         (+
          ;; word start.
          (- (point) (length first))
          ;; Apply initial offset within the word.
          (- pt b (length ws-first))))
      (goto-char
       (+
        b (length ws-first)
        ;; Apply initial offset within the word.
        (- pt-original (+ pt 1 (length ws-second))))))))

(defun my-c-transpose-args-forward ()
  (interactive)
  (my-c-transpose-args--direction t))
(defun my-c-transpose-args-backward ()
  (interactive)
  (my-c-transpose-args--direction nil))

;; Custom keybindings (not package-related)
(windmove-default-keybindings)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x 4 q") 'quit-other-window)
(global-set-key (kbd "M-S-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c r") 'rerun-compilation)

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

(use-package helm-org :defer)

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
  :init
  (require 'helm-org)
  :defer t)

(use-package exec-path-from-shell :defer)

;; General programming
(use-package keychain-environment
  :commands keychain-refresh-environment
  :quelpa (keychain-environment :fetcher github :repo "tarsius/keychain-environment"))

(use-package magit
  :init
  (keychain-refresh-environment))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package bnf-mode
  :defer t)

(use-package kotlin-mode
  :defer t)

(use-package sml-mode
  :commands (sml-mode emil-mode)
  :preface
  (defun my-sml-rules (orig kind token)
    (if (and (equal kind ':after) (equal token "struct")) 4
      (funcall orig kind token)))
  :config
  (advice-add 'sml-smie-rules :around 'my-sml-rules)
  (defalias 'emil-mode 'sml-mode)
  :defer t)

(use-package smartparens
  :hook (prog-mode . turn-on-smartparens-strict-mode)
  :config (progn
            (require 'smartparens-config)
            (show-smartparens-global-mode t)
            (sp-local-pair 'sml-mode "'" nil :actions nil)
            (sp-local-pair 'sml-mode "`" nil :actions nil)))

(use-package cc-mode
  :defer t
  :bind (:map c-mode-base-map
              ("C-M-t" . my-c-transpose-args-forward)))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package clang-format
  :defer t)

(use-package clang-format+
  :hook (c-mode-common . clang-format+-mode))

(use-package google-c-style
  :hook
  ((c-mode c++-mode) . google-set-c-style)
  (c-mode-common . google-make-newline-indent))

(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Emacs as IDE
(use-package helm-xref
  :after helm)

(use-package dap-mode
  :defer t
  :init
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-mode 1)
  (dap-auto-configure-mode 1)
  (dap-ui-controls-mode 1)
  :config (require 'dap-lldb))

(use-package yasnippet
  :delight yas-minor-mode)

(use-package company
  :defer t
  :delight " ©")

;; (use-package flymake
;;   :config
;;   (progn
;;     (defvar flymake-goto-map
;;       (let ((map (make-sparse-keymap)))
;;         (set-keymap-parent map goto-map)
;;         map))
;;     (bind-keys :map flymake-mode-map
;;                :prefix-map flymake-goto-map
;;                :prefix "M-g"
;;                ("f" . flymake-goto-next-error)
;;                ("M-f" . flymake-goto-next-error)
;;                ("F" . flymake-goto-prev-error)
;;                ("M-F" . flymake-goto-prev-error))
;;     (repeatify 'flymake-goto-map))
;;   :delight)

(use-package flycheck
  :commands (flycheck-mode flycheck-checker-executable-variable flycheck-select-checker)
  :hook (prog-mode . flycheck-mode)
  :delight)

(use-package helm-projectile
  :commands helm-projectile-on)

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1)
  :delight '(:eval (concat " P:" (projectile-project-name)))
  :config (helm-projectile-on))

(use-package cmake-ide
  :after (projectile flycheck-clangcheck)
  :hook (c++-mode . my/cmake-ide-find-project)
  :init (cmake-ide-setup)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-build-dir (concat (projectile-project-root) "build")))))

(use-package lsp-java :defer)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-ui :defer)

(use-package lsp-mode
  :hook (((c-mode c++-mode java-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (require 'dap-cpptools)
  (require 'lsp-ui)
  (setq read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil))

;; Viewing media
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
