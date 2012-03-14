(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun ascend-to-build-xml (dir)
  "Find a directory between dir and '/' that contains 'build.xml' and return its name. If none exists, returns nil."
  (cond ((file-exists-p (expand-file-name "build.xml" dir))
         dir)
        ((equal dir "/") nil)
        (t (ascend-to-build-xml (expand-file-name "../" dir)))))

;; From http://www.emacswiki.org/emacs-ja/IndentRigidlyN
(defun indent-rigidly-n (n)
      "Indent the region, or otherwise the current line, by N spaces."
      (let* ((use-region (and transient-mark-mode mark-active))
             (rstart (if use-region (region-beginning) (point-at-bol)))
             (rend   (if use-region (region-end)       (point-at-eol)))
             (deactivate-mark "irrelevant")) ; avoid deactivating mark
        (indent-rigidly rstart rend n)))
