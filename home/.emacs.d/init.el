(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(require 'package)
(setq package-pinned-packages
      '((bind-key . "melpa")
	(diminish . "melpa")
	(use-package . "melpa")))
(package-initialize)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package magit)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(global-set-key [remap list-buffers] 'ibuffer)
(windmove-default-keybindings)
