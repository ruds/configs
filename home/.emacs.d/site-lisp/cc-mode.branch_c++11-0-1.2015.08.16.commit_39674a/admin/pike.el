;;; pike.el -- Font lock definitions for Pike and other LPC files.
;;; $Id: pike.el,v 1.22 2001/02/13 14:11:55 mast Exp $
;;; Copyright (C) 1995, 1996, 1997, 1998, 1999 Per Hedbor.
;;; This file is distributed as GPL

;;; Keywords: Pike, LPC, uLPC, �LPC, highlight

;;; To use:
;;;
;;; (require 'pike)
;;;
;;; Older Emacs versions doesn't come with a Pike-aware CC Mode (M-x
;;; c-version should report 5.23 or later), so you might have to
;;; upgrade that (see http://www.python.org/emacs/cc-mode/). You
;;; probably want this too in that case:
;;;
;;; (setq auto-mode-alist
;;;   (append '(("\\.pike$" . pike-mode)) auto-mode-alist)

(require 'font-lock)
(require 'custom)

;; Added in later font-lock versions. Copied here for backward
;; compatibility.
(defvar font-lock-preprocessor-face 'font-lock-keyword-face
  "Don't even think of using this.")
(defvar pike-font-lock-refdoc-face 'pike-font-lock-refdoc-face)
(defvar pike-font-lock-refdoc-init-face 'pike-font-lock-refdoc-init-face)
(defvar pike-font-lock-refdoc-init2-face 'pike-font-lock-refdoc-init2-face)
(defvar pike-font-lock-refdoc-keyword-face 'pike-font-lock-refdoc-keyword-face)
(defvar pike-font-lock-refdoc-error-face 'pike-font-lock-refdoc-error-face)

(defconst pike-font-lock-keywords-1 nil
 "For consideration as a value of `pike-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst pike-font-lock-keywords-2 nil
 "For consideration as a value of `pike-font-lock-keywords'.
This adds highlighting of types and identifier names.")

(defconst pike-font-lock-keywords-3 nil
 "For consideration as a value of `pike-font-lock-keywords'.
Highlight some constructs differently")

(defgroup pike-faces nil
  "Faces used by the pike color highlighting mode."
  :group 'font-lock
  :group 'faces)

(defface pike-font-lock-refdoc-face
  '((t))
  "Face to use for normal text in Pike documentation comments. It's
overlaid over the `font-lock-comment-face'."
  :group 'pike-faces)
(defface pike-font-lock-refdoc-init-face
  '((t (:bold t)))
  "Face to use for the magic init char of Pike documentation comments. It's
overlaid over the `font-lock-comment-face'."
  :group 'pike-faces)
(defface pike-font-lock-refdoc-init2-face
  '((t))
  "Face to use for the comment starters Pike documentation comments. It's
overlaid over the `font-lock-comment-face'."
  :group 'pike-faces)
(defface pike-font-lock-refdoc-keyword-face
  '((t))
  "Face to use for markup keywords Pike documentation comments. It's
overlaid over the `font-lock-reference-face'."
  :group 'pike-faces)
(defface pike-font-lock-refdoc-error-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color)) (:foreground "hotpink"))
    (((background light)) (:foreground "white" :background "black"))
    (t (:foreground "black" :background "white")))
  "Face to use for invalid markup in Pike documentation comments."
  :group 'pike-faces)

(defconst pike-font-lock-type-regexp
  (concat "\\<\\("
"mixed\\|"
"float\\|"
"int\\|"
"program\\|"
"string\\|"
"function\\|"
"function(.*)\\|"
"array\\|"
"array(.*)\\|"
"mapping\\|"
"mapping(.*)\\|"
"multiset\\|"
"multiset(.*)\\|"
"object\\|"
"object(.*)\\|"
"void\\|"
"constant\\|"
"class"
	"\\)\\>)*")
  "Regexp which should match a primitive type.")


; Problems: We really should allow all unicode characters...
(let ((capital-letter "A-Z\300-\326\330-\337")
      (letter "a-zA-Z\241-\377_")
      (digit  "0-9")
      )

  (defconst pike-font-lock-identifier-regexp
    (concat "\\<\\([" letter "][" letter digit "]*\\)\\>")
    "Regexp which should match all Pike identifiers.")

  (defconst pike-font-lock-class-name-regexp
    (concat "\\<\\([" capital-letter "][" letter digit "]*\\)\\>")
    "Regexp which should match a class name.
The name is assumed to begin with a capital letter.")


  (defun pike-font-lock-hack-file-coding-system-perhaps ( foo )
    (interactive)
    (message "charset %s" (buffer-substring (match-beginning 2) (match-end 2)))
    (condition-case fel
	(if (and (fboundp 'set-buffer-file-coding-system)
		 (fboundp 'symbol-concat))
	    (let ((coding (buffer-substring 
			   (match-beginning 2) (match-end 2))))
	      (set-buffer-file-coding-system (symbol-concat coding))))
      (t nil)))


  (defconst pike-modifier-regexp
    (concat "\\<\\(public\\|inline\\|final\\|static\\|protected\\|"
	    "local\\|optional\\|private\\|nomask\\|variant\\)\\>"))
  (defconst pike-operator-identifiers
    (concat "`\\(->=?\\|+=?\\|==\\|\\[\\]=?\\|()\\|[!<>~]"
	    "\\|`?\\(<<\\|>>\\|[%&*+/^|-]\\)\\)"))

  ;; Basic font-lock support:
  (setq pike-font-lock-keywords-1
	(list
	 ;; Keywords:
	 (list        
	  (concat
	   "\\<\\("
	   "predef\\|"
	   "import\\|"
	   "default\\|"
	   "case\\|"
	   "class\\|"
	   "break\\|"
	   "continue\\|"
	   "do\\|"
	   "else\\|"
	   "for\\|"
	   "if\\|"
	   "return\\|"
	   "switch\\|"
	   "while\\|"
	   "lambda\\|"
	   "catch\\|"
	   "throw\\|"
	   "foreach\\|"
	   "inherit"
	   "\\)\\>")
	  1 'font-lock-keyword-face)
	 
	 ;; Modifiers:
	 (list pike-modifier-regexp 1 font-lock-preprocessor-face)

	 ;; Class names:
	 (list (concat "\\(\\<class\\>\\)\\s *" 
		       pike-font-lock-identifier-regexp)
	       (list 1 'font-lock-keyword-face)
	       (list 2 'font-lock-function-name-face))
 
	 ;; Methods:
	 (list (concat (concat "^\\s *\\("
			       pike-font-lock-type-regexp
			       "\\|"
			       pike-font-lock-class-name-regexp
			       "\\)")
		       "\\s *"
		       ;;"\\s *\\(\\[\\s *\\]\\s *\\)*"
		       (concat "\\("
			       pike-font-lock-identifier-regexp
			       "\\|"
			       pike-operator-identifiers
			       "\\)")
		       "\\s *(")
	       4
	       'font-lock-function-name-face)
	 ;; Case statements:
	 ;; Any constant expression is allowed.
	 '("\\<case\\>\\s *\\(.*\\):"
	   1 font-lock-reference-face)
	 ;; Labels in statements:
	 `(,(concat "\\<\\(break\\|continue\\)\\>\\s *\\("
		    pike-font-lock-identifier-regexp "\\)")
	   2 font-lock-reference-face)))

    ;; Types and declared variable names:
    (setq pike-font-lock-keywords-2
	  (append 
	   (list
	    '("^#!.*$" 0 font-lock-comment-face)

	    '("^#[ \t]*error\\(.*\\)$"
	      (1 font-lock-string-face))

	    ;; #charset char-set-name

	    ;; Defines the file charset. 

	    '("^\\(#[ \t]*charset\\)[ \t]*\\(.*\\)$"
	      (2 font-lock-keyword-face)
	      (pike-font-lock-hack-file-coding-system-perhaps
	       ))

	    ;; Fontify filenames in #include <...> as strings.
	    '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 
	      1 font-lock-string-face)

	    '("^#[ \t]*define[ \t]+\\(\\sw+\\)\("
	      1 font-lock-function-name-face)
	    '("^#[ \t]*define[ \t]+\\(\\sw+\\)"
	      1 font-lock-variable-name-face)
	    ;; Fontify symbol names in #if ...defined 
	    ;; etc preprocessor directives.
	    '("^#[ \t]*if\\>"
	      ("\\<\\(defined\\|efun\\|constant\\)\\>[ \t]*(?\\(\\sw+\\)?" 
	       nil nil
	       (1 font-lock-reference-face)
	       (2 font-lock-variable-name-face nil t)))

	    '("^\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(.*\\)?"
	      (1 font-lock-reference-face) 
	      (2 font-lock-variable-name-face nil t))
	    )

	   pike-font-lock-keywords-1

	   (list
	    ;; primitive types, can't be confused with anything else.
	    (list pike-font-lock-type-regexp
		  '(1 font-lock-type-face)
		  '(font-lock-match-pike-declarations
		    (goto-char (match-end 1))
		    (goto-char (match-end 0))
		    (0 font-lock-variable-name-face)))

	    ;; Declarations, class types and capitalized variables:
	    (list (concat
		   (concat "\\("	; 1
			   pike-font-lock-class-name-regexp ; 2
			   "\\|"
			   (concat
			    "object("
			    "\\(\\sw+\\.\\)*" ; 3
			    pike-font-lock-identifier-regexp ; 4
			    ")")
			   "\\)")
		   (concat "\\("
			   "[,:|\)]"
			   "\\|"
			   (concat
			    "\\(\\s *\\.\\.\\.\\)?"
			    "\\(\\s \\|/\\*\\([^*]\\|\\*[^/]\\)*\\*/\\)*"
			    "\\(`\\|\\<\\)")
			   "\\)"))
		  '(font-lock-match-pike-types
		    (goto-char (or (match-beginning 2)
				   (match-beginning 4)))
		    (goto-char (match-end 1))
		    (1 font-lock-type-face))
		  (list (concat "\\=" pike-font-lock-identifier-regexp
				"\\.")
			'(progn
			   (goto-char (or (match-beginning 2)
					  (match-beginning 4)))
			   (while (or (= (preceding-char) ?.)
				      (= (char-syntax (preceding-char)) ?w))
			     (backward-char)))
			'(goto-char (match-end 0))
			'(1 font-lock-reference-face)
			'(0 nil))	; Workaround for bug in XEmacs.
		  '(font-lock-match-pike-declarations
		    (goto-char (match-end 1))
		    nil
		    (1 font-lock-variable-name-face)))
	    )

	   ;; Labels. Do this after the type font locking above to
	   ;; avoid highlighting "int" in "mapping(int:string)" as a
	   ;; label.
	   `((pike-font-lock-find-label
	      0 font-lock-reference-face))
	   ))

    (setq pike-font-lock-keywords-1
	  (append pike-font-lock-keywords-1
		  `((pike-font-lock-find-label
		     0 font-lock-reference-face))))

  ;; Modifier keywords
  (setq pike-font-lock-keywords-3
	(append

	 (list
	   ;; Feature scoping:
	   ;; These must come first or the Modifiers from keywords-1 will
	   ;; catch them.  We don't want to use override fontification here
	   ;; because then these terms will be fontified within comments.
	  '("\\<public\\>"    0 font-lock-preprocessor-face)
	  '("\\<inline\\>"    0 font-lock-preprocessor-face)
	  '("\\<final\\>"     0 font-lock-preprocessor-face)
	  '("\\<static\\>"    0 font-lock-preprocessor-face)
	  '("\\<protected\\>" 0 font-lock-preprocessor-face)
	  '("\\<local\\>"     0 font-lock-preprocessor-face)
	  '("\\<private\\>"   0 font-lock-preprocessor-face)
	  '("\\<nomask\\>"    0 font-lock-preprocessor-face)

	  `(,(concat "^\\([^/]\\|/[^/]\\)*"
		     "\\(//[^.!|\n\r]\\|\\(//\\)\\([.!|]\\)\\([^\n\r]*\\)\\)")
	    (3 pike-font-lock-refdoc-init2-face prepend t)
	    (4 pike-font-lock-refdoc-init-face prepend t)
	    (5 pike-font-lock-refdoc-face prepend t)
	    ("\\(@\\(\\w+{?\\|\\[[^\]]*\\]\\|[@}]\\|$\\)\\)\\|\\(@.\\)"
	     (if (match-end 4) (goto-char (match-end 4)) (end-of-line))
	     nil
	     (1 font-lock-reference-face t t)
	     (1 pike-font-lock-refdoc-keyword-face prepend t)
	     (3 pike-font-lock-refdoc-error-face t t))
	    ((lambda (limit)
	       (if (looking-at "[ \t]*@\\(decl\\|member\\|index\\|elem\\)")
		   (progn
		     (put-text-property (match-end 0) limit 'face nil)
		     (goto-char limit)
		     t)))
	     (if (match-end 4) (goto-char (match-end 4)) (end-of-line))
	     nil)
	    )
	  )
	 pike-font-lock-keywords-2
	 )))

(defvar pike-font-lock-keywords pike-font-lock-keywords-1
  "Additional expressions to highlight in Pike mode.")

(defun font-lock-match-pike-types (limit)
  "Match and skip over types."
  (prog1
      (and (looking-at pike-font-lock-identifier-regexp)
	   (save-restriction
	     (narrow-to-region (save-excursion
				 (beginning-of-line)
				 (point))
			       limit)
	     (save-match-data
	       (let ((pos (point)))
		 (while (and (condition-case nil
				 (progn (up-list -1) t)
			       (error nil))
			     (eq (following-char) ?\()
			     (save-excursion
			       (skip-syntax-backward " ")
			       (skip-syntax-backward "w")
			       (looking-at pike-font-lock-type-regexp)))
		   (setq pos (point)))
		 (goto-char pos)))
	     (condition-case nil
		 (progn
		   (while (save-match-data
			    (forward-sexp)
			    (if (looking-at
				 (concat "\\s *|"
					 "\\(" pike-font-lock-class-name-regexp
					 "\\|" pike-font-lock-type-regexp
					 "\\s *\(?\\)"))
				(progn
				  (if (eq (char-after (1- (match-end 0))) ?\()
				      (forward-sexp))
				  t))))
		   (save-match-data
		     (looking-at "\\s *\\(`\\|\\<\\|$\\)")))
	       (error nil))))
    (goto-char limit)))

;; Match and move over any declaration/definition item after
;; point.  Does not match items which look like a type declaration
;; (primitive types and class names, i.e. capitalized words.)
;; Should the variable name be followed by a comma, we reposition
;; the cursor to fontify more identifiers.
(defun font-lock-match-pike-declarations (limit)
  "Match and skip over variable definitions."
  (while (and (looking-at ")")
	      (condition-case nil
		  (save-excursion
		    (up-list -1)
		    (skip-syntax-backward " ")
		    (skip-syntax-backward "w")
		    (looking-at pike-font-lock-type-regexp))
		(error nil)))
    (forward-char 1))
  (looking-at "\\(\\s *\\.\\.\\.\\)?\\(\\s \\|/\\*\\([^*]\\|\\*[^/]\\)*\\*/\\)*")
  (goto-char (match-end 0))
  (and
   (looking-at pike-font-lock-identifier-regexp)
   (save-match-data
     (not (string-match pike-font-lock-type-regexp
			(buffer-substring (match-beginning 1)
					  (match-end 1)))))
   (save-match-data
     (save-excursion
       (goto-char (match-beginning 1))
       (not (looking-at
	     (concat "\\(\\sw+\\.\\)*"
		     pike-font-lock-class-name-regexp
		     "\\s *\\(\\<\\||\\)")))))
   (save-match-data
     (let ((start (match-end 0)))
       (condition-case nil
	   (save-restriction
	     (narrow-to-region (point-min) limit)
	     (goto-char start)
	     ;; Note: Both `scan-sexps' and the second goto-char can
	     ;; generate an error which is caught by the
	     ;; `condition-case' expression.
	     (while (not (looking-at "\\s *\\(\\(,\\)\\|;\\|$\\)"))
	       (goto-char (or (scan-sexps (point) 1) (point-max))))
	     (goto-char (match-end 2))) ; non-nil
	 (error
	  (goto-char start)
	  (if (not (looking-at "\\s *("))
	      (goto-char limit)
	    t)))))))

(defun pike-font-lock-find-label (limit)
  (catch 'found
    (while (re-search-forward (concat pike-font-lock-identifier-regexp ":[^:]") limit t)
      (save-excursion
	;; Don't format identifiers as indexes in mapping constants.
	(condition-case nil (up-list -1) (error (throw 'found t)))
	(if (not (eq (char-after (point)) ?\[)) (throw 'found t))))))

;; XEmacs way.
(put 'pike-mode 'font-lock-defaults 
      '((pike-font-lock-keywords
     pike-font-lock-keywords-1 pike-font-lock-keywords-2
     pike-font-lock-keywords-3)
        nil nil ((?_ . "w")) beginning-of-defun
        (font-lock-mark-block-function . mark-defun)))

;; GNU Emacs way.
(if (and (boundp 'font-lock-defaults-alist)
	 (not (assq 'pike-mode font-lock-defaults-alist)))
    (setq font-lock-defaults-alist
	  (cons
	   (cons 'pike-mode
		 '((pike-font-lock-keywords pike-font-lock-keywords-1
		    pike-font-lock-keywords-2 pike-font-lock-keywords-3)
		   nil nil ((?_ . "w")) beginning-of-defun
		   (font-lock-mark-block-function . mark-defun)))
	   font-lock-defaults-alist)))

;; Autoload spec for older emacsen that doesn't come with a Pike aware
;; CC Mode. Doesn't do any harm in later emacsen.
(autoload 'pike-mode "cc-mode" "Major mode for editing Pike code.")

(provide 'pike)
