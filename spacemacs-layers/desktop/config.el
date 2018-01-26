(defgroup desktop-layer nil "Customization for the desktop layer" :group 'local)

(defvar desktop/dirname nil
  "The directory desktop data is stored in.

You should probably customize desktop/base-dirname instead.")

(defcustom desktop/base-dirname "~/.emacs.desktops/"
  "The base directory of the desktop files.

Each server will create a subdirectory of this directory to store desktop
data."
  :group 'desktop-layer
  :type 'directory
  :set 'desktop/set-base-dirname
  :initialize 'custom-initialize-reset)

(defvar desktop/save-emacs-timer nil
  "The timer controlling the periodic saving of emacs state.")

(when (configuration-layer/package-usedp 'desktop)
  (defvar desktop/save-emacs-timer
    (run-with-idle-timer 300 t 'desktop/save-emacs-state)))
