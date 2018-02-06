(defun wide-frame ()
  (interactive)
  (set-frame-width (selected-frame) 120))

(defun narrow-frame ()
  (interactive)
  (set-frame-width (selected-frame) 80))

(defun reopen-modified-buffer (buf)
  "Reopens the given buffer if the buffer is not modified but its file is.

Returns the file name if the buffer was reopened, or nil otherwise."
  (let ((name (buffer-file-name buf)))
    (and name
         (file-readable-p name)
         (not (buffer-modified-p buf))
         (not (verify-visited-file-modtime buf))
         (save-excursion
           (set-buffer buf)
           (find-alternate-file name))
         name)))

;; Author: Steve Yegge (modified by me)
(defun reopen-all-buffers ()
  "Reopen all non-dirty buffers associated with a file."
  (interactive)
  (let* ((buffers (buffer-list))
         (count 0)
         (temp-buffer-show-function
          (lambda (tempbuf)
            (if (zerop count)
                (message "No files were reopened.")
              (save-excursion
                (set-buffer (get-buffer "*reopened*"))
                (insert (format "Reopened %d files\n" count)))
              (pop-to-buffer "*reopened*")))))
    (with-output-to-temp-buffer "*reopened*"
      (dolist (buf buffers)
        (let ((name (reopen-modified-buffer buf)))
          (when name
            (princ (format "reopened %s\n" (buffer-file-name buf)))
            (incf count)))))))

;; http://stackoverflow.com/questions/2238418/emacs-lisp-how-to-get-buffer-major-mode
(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion (set-buffer buffer-or-string) major-mode))

; Author: wyrick@google.com
;; This is basically the same as save-window-excursion,
;; except that we save current-frame information, rather than window info.
(when (not (fboundp 'save-frame-excursion))
  (defmacro save-frame-excursion (&rest body)
    "Execute body, preserving selected frame and mouse position."
    (let ((current-frame  (gensym 'current-frame))
          (current-mousep (gensym 'current-mousep)))
      `(let ((,current-frame (selected-frame))
             (,current-mousep (mouse-position)))
         (unwind-protect
             (progn ,@body)
           (select-frame-set-input-focus ,current-frame)
           (raise-frame)
           (set-mouse-position ,current-frame
                               (car (cdr ,current-mousep))
                               (cdr (cdr ,current-mousep))))))))

;; Make next-error move the error to the top of the window
(defun next-error-to-top ()
  (if (eq (selected-frame) (next-frame))
      (save-window-excursion
        (pop-to-buffer next-error-last-buffer nil t)
        (recenter 0))
    (save-frame-excursion
     (other-frame 1)
     (save-window-excursion
       (pop-to-buffer next-error-last-buffer nil t)
       (recenter 0)))))
(add-hook 'next-error-hook 'next-error-to-top)

(defun maximize-frame-vertically (frame)
  (when (display-graphic-p frame)
    (let* ((junk-height
            (+ 0 ;; fudge factor caused by Gnome/KDE bars (I *think*)
               (plist-get (symbol-plist 'title) 'x-frame-parameter)
               (plist-get (symbol-plist 'border-width) 'x-frame-parameter)
               (plist-get (symbol-plist 'internal-border-width)
                          'x-frame-parameter)
               (plist-get (symbol-plist 'menu-bar-lines) 'x-frame-parameter)
               (plist-get (symbol-plist 'tool-bar-lines) 'x-frame-parameter)))
           (target-height (- (display-pixel-height frame) junk-height))
           (target-rows (/ target-height (frame-char-height frame))))
      (set-frame-height frame target-rows))))

(defun maximize-vertically ()
  (interactive)
  (maximize-frame-vertically (selected-frame)))

;; Source: http://www.emacswiki.org/emacs/TransposeWindows
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

(defmacro apply-to-region (fun &rest args)
  "Return an interactive function that applies fun to the current region.

fun should take a start and end position as its first two
arguments; any other arguments given to apply-to-region will be
passed to fun after the start and end."
  `(lambda ()
     (interactive)
     (apply ,fun (region-beginning) (region-end) ',args)))

(defun base/format-buffer ()
  (interactive)
  (funcall (alist-get major-mode base/format-buffer-alist 'ignore)))

(defun base/format-region (b e)
  (interactive "r")
  (funcall (alist-get major-mode base/format-region-alist
                      (lambda (b e)
                        (alist-get major-mode base/format-buffer-alist
                                   'ignore)))
           b e))

(defun base/format-region-or-buffer (&optional arg)
  "Formats a region if called with an argument, or a buffer otherwise"
  (interactive "P")
  (if arg
      (base/format-region (region-beginning) (region-end))
    (base/format-buffer)))

(defun base/compile ()
  "Compile using base/compile-func"
  (interactive)
  (funcall base/compile-func))
