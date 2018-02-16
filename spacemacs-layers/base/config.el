(defgroup base-layer nil "Customization for the base layer" :group 'local)

(defcustom base/compile-func 'compile
  "The function used to compile stuff."
  :group 'base-layer
  :type 'function)

(defvar base/format-region-alist
  '((c++-mode . clang-format-region)
    (c-mode . clang-format-region)))

(defvar base/format-buffer-alist
  '((c++-mode . clang-format-buffer)
    (c-mode . clang-format-buffer)
    (go-mode . go-fmt)))

(add-to-list 'auto-mode-alist '("patch$" . diff-mode))

(defun base/c-mode-common-hook ()
  (modify-syntax-entry ?_ "w"))

(add-hook 'c-mode-common-hook 'base/c-mode-common-hook)
