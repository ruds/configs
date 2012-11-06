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

(defun hg-change-repository (new-repo)
  "Loop through all open buffers; if they are under Mercurial and the same path exists under new-repo, kill the buffer and open a new one at the same path in new-repo."
  (interactive "Mrepo: ")
  (mapcar #'buffer-name
          (delq nil (loop for buf in (buffer-list)
                          collect (when (equalp 'Hg
                                                (vc-backend 
                                                 (buffer-file-name buf)))
                                    (hg-maybe-reopen-in-repo buf new-repo))))))

(defun hg-maybe-reopen-in-repo (buffer-or-name new-repo)
  "If the file visited in buffer-or-name is also in the HG repository new-repo, kill the buffer and visit the file in new-repo."
  (interactive "bbuffer: \nMrepo: ")
  (let* ((buf (get-buffer buffer-or-name))
         (f (expand-file-name (buffer-file-name buf)))
         (r (vc-find-root f ".hg")))
    (when r
      (let* ((root (expand-file-name r))
             (path (substring f (length root)))
             (new-path
              (expand-file-name (concat root "../" new-repo "/" path))))
        (when (file-readable-p new-path)
          (kill-buffer buf)
          (find-file new-path))))))

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
