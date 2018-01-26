;; save-emacs-state and save-emacs-timer are by
;; leadpipe@google.com (Luke Blanshard)
(when (configuration-layer/package-usedp 'desktop)
  (defun desktop/save-emacs-state ()
    "Saves the desktop as of right now."
    (interactive)
    (make-directory desktop/dirname t)
    (desktop-save desktop/dirname)
    (message "Save desktop %s" desktop/dirname)))

(defun desktop/set-base-dirname (SYM VAL)
  (set-default SYM VAL)
  (setq-default desktop/dirname (concat desktop/base-dirname server-name "/")))
