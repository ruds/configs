;;; packages.el --- base layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 Matt Rudary
;;
;; Author: Matt Rudary <matt@rudary.com>
;; URL: https://github.com/ruds/configs/spacemacs-layers/base/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Apache License version 1.1

(defconst base-packages
  '(ffap)
  "The list of Lisp packages required by the base layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun base/init-ffap () (use-package ffap))
