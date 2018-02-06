(spacemacs/set-leader-keys
  "of" 'base/format-region-or-buffer
  "oF" 'base/compile
  "oc" 'comment-region
  "op" 'transpose-windows
  "ot" 'find-file-at-point
  "oT" 'ffap-other-window
  )

;; Common keys for all c-like modes
(dolist (mode '(c-mode c++-mode java-mode))
  (spacemacs/set-leader-keys-for-major-mode mode
    ">" (apply-to-region 'indent-code-rigidly 2)
    "<" (apply-to-region 'indent-code-rigidly -2)
  ))

;; Python
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  ">" (apply-to-region 'indent-code-rigidly 2)
  "<" (apply-to-region 'indent-code-rigidly -2)
  )
