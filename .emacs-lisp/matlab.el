;;; matlab.el --- major mode for MATLAB dot-m files
;;
;; Author: Matt Wette <mwette@alumni.caltech.edu>
;;
;; Copyright (c) 1997, Matthew Wette All rights reserved.
;; 
;; Redistribution and use in source and binary forms, with or without 
;; modification, are permitted provided that the following conditions are 
;; met:
;; 
;;     * Redistributions of source code must retain the above copyright 
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright 
;;       notice, this list of conditions and the following disclaimer in 
;;       the documentation and/or other materials provided with the distribution
;;       
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
;; POSSIBILITY OF SUCH DAMAGE.
;; 
;; This major mode for GNU emacs provides support for editing MATLAB dot-m
;; files.  It automatically indents for block structures, line continuations
;; (e.g., ...), and comments.  The usual paren matching support is included.
;; Filling and auto-fill works for comment lines.
;;
;; You may wish to add something like the following to your ~/.emacs file:
;;   (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
;;   (setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
;;   (defun my-matlab-mode-hook ()
;;     (setq matlab-indent-function t)	; if you want function bodies indented
;;     (setq fill-column 76)		; where auto-fill should wrap
;;     (turn-on-auto-fill))
;;   (setq matlab-mode-hook 'my-matlab-mode-hook)
;;   (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
;;   (defun my-matlab-shell-mode-hook ()
;;	'())
;;   (setq matlab-mode-hook 'my-matlab-mode-hook)
;; Put this file in the emacs load-path so emacs can find it (check the manual).
;; To get font-lock try adding
;;     (font-lock-mode 1)
;; Or for newer versions of emacs
;;     (global-font-lock-mode t)
;; To get hilit19 support try adding
;;     (matlab-mode-hilit)
;;
;; Indent places the beginning of the entire comment.  If placing a new 
;; comment on the line use fill-prefix from previous line.  If comment
;; already exists keep its existing justification.
;;

;;; Code:

;;(setq debug-on-error t)

(defconst matlab-mode-version "1.10.1 1997"
  "Current version of matlab mode.")

;;; user-changeable variables =================================================

;; Variables which the user can change
(defvar matlab-indent-level 2
  "*The indentation in matlab-mode.")

(defvar matlab-cont-level 4
  "*Continuation indent.")

(defvar matlab-fill-code t
  "*If true, `auto-fill-mode' causes code lines to be automatically continued.")

(defvar matlab-comment-column 40
  "*The goal comment column in `matlab-mode' buffers.")

(defvar matlab-comment-line-s "% "
  "*String to start comment on line by itself.")

(defvar matlab-comment-on-line-s "% "
  "*String to start comment on line with code.")

(defvar matlab-comment-region-s "% $$$ "
  "*String inserted by \\[matlab-comment-region] at start of each line in \
region.")

(defvar matlab-indent-function nil
  "*If t, indent body of function.")

(defvar matlab-vers-on-startup t
  "*If non-nil, shows the version number on startup.")

(defvar matlab-shell-save-and-go-history '("()")
  "Keeps track of paramters passed to the Matlab shell.")

;;; matlab-mode variables =====================================================

;; syntax table
(defvar matlab-mode-syntax-table nil
  "The syntax table used in `matlab-mode' buffers")

(if matlab-mode-syntax-table
    ()
  ;; changes by Mats Bengtsson
  (setq matlab-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?_  "_" matlab-mode-syntax-table)
  (modify-syntax-entry ?%  "<" matlab-mode-syntax-table)
  (modify-syntax-entry ?\\ "." matlab-mode-syntax-table)
  (modify-syntax-entry ?\t ">" matlab-mode-syntax-table)
  (modify-syntax-entry ?\n ">" matlab-mode-syntax-table)
  (modify-syntax-entry ?+  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?-  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?*  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?'  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?/  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?=  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?<  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?>  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?&  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?|  "." matlab-mode-syntax-table)
  (set-syntax-table matlab-mode-syntax-table))

(defvar matlab-mode-special-syntax-table
  (let ((s (copy-syntax-table matlab-mode-syntax-table)))
    (modify-syntax-entry ?_  "w" s)
    s)
  "The syntax table used when navigating blocks.")

;; abbrev table
(defvar matlab-mode-abbrev-table nil
  "the abbrev table used in `matlab-mode' buffers")

(define-abbrev-table 'matlab-mode-abbrev-table ())

;; mode map
(defvar matlab-mode-map ()
  "The keymap used in `matlab-mode'")

(defvar matlab-help-map ()
  "The help key map for `matlab-mode' and `matlab-shell-mode'")

;; Here are some cool Matlab help commands!
(if matlab-help-map
    ()
  (setq matlab-help-map (make-sparse-keymap))
  (define-key matlab-help-map "r" 'matlab-shell-run-command)
  (define-key matlab-help-map "f" 'matlab-shell-describe-command)
  (define-key matlab-help-map "a" 'matlab-shell-apropo)
  (define-key matlab-help-map "v" 'matlab-shell-describe-variable)
  (define-key matlab-help-map "t" 'matlab-shell-topic-browser)
  )

(if matlab-mode-map
    ()
  (setq matlab-mode-map (make-sparse-keymap))
  (define-key matlab-mode-map "\r" 'matlab-return)
  (define-key matlab-mode-map "\C-j" 'matlab-linefeed)
  (define-key matlab-mode-map "\t" 'matlab-indent-line)
  (define-key matlab-mode-map "\M-;" 'matlab-comment)
  (define-key matlab-mode-map "\C-c;" 'matlab-comment-region)
  (define-key matlab-mode-map "\C-c\r" 'matlab-comment-return)
  (define-key matlab-mode-map "\C-c\C-f" 'matlab-fill-comment-line)
  (define-key matlab-mode-map "\C-c\C-h" matlab-help-map)
  (define-key matlab-mode-map "\C-c\C-j" 'matlab-justify-line)
  (define-key matlab-mode-map "\C-c\C-q" 'matlab-fill-region)
  (define-key matlab-mode-map "\C-c\C-s" 'matlab-shell-save-and-go)
  (define-key matlab-mode-map "\C-c\C-t" 'matlab-show-line-info)
  (define-key matlab-mode-map "\M-\r" 'newline)
  (define-key matlab-mode-map "\M-\C-f" 'matlab-forward-sexp)
  (define-key matlab-mode-map "\M-\C-b" 'matlab-backward-sexp)
  (substitute-key-definition 'comment-region 'matlab-comment-region
			     matlab-mode-map) ; global-map) ;torkel
  )

;; font-lock keywords
(defvar matlab-font-lock-keywords
  (list
   ;; String quote chars are also used as transpose, but only if directly
   ;; after a non-deliminating character.  To quote a quote, put two
   ;; in a row, thus we need an anchored first quote. (Eric Ludlam)
   '("\\([ \t(),;]\\|^\\)\\('\\([^\n']\\|''\\)*'\\)"
     2 font-lock-string-face)
   ;; Comments must occur after the string, that way we can check to see
   ;; if the comment start char has occurred inside our string. (EL)
   ;; (match-beginning 1) doesn't work w/ xemacs -- use 0 instead
   '("\\(%[^%\n]*\\)"
     1 (if (eq (get-text-property (match-beginning 0) 'face)
               font-lock-string-face) nil
         font-lock-comment-face) keep)
   '("\\(^\\|[;,]\\)[ \t]*\\(global\\|for\\|while\\|if\\|elseif\\|else\\|end\
\\|return\\|switch\\|case\\|otherwise\\)\\b"
     2 font-lock-keyword-face)
    ;; handle graphics cool stuff
    '("\\<\\(\\figure\\|axes\\|line\\|surface\\|patch\\|text\\|light\\|\
image\\)\\>"
      1 font-lock-type-face)
   )
   "Expressions to hightlight in Matlab mode.")

;; Eric Ludlam's gaudy font-lock keywords
(defvar matlab-gaudy-font-lock-keywords
  (append
   matlab-font-lock-keywords
   (list
   ;; defining a function, assigned variables, and function name
   '("^\\(function\\)\\s-+\\([^=]+\\)\\s-*=\\s-*\\(\\sw+\\)\\s-*(" 
     (1 font-lock-keyword-face) (2 font-lock-variable-name-face)
     (3 font-lock-function-name-face))
   '("^\\(function\\)\\s-+\\(\\sw+\\)\\s-*(" 
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
   ;; Anchor on the function keyword, hightlight params
   '("^function\\([^=]+=\\)?\\s-*\\(\\sw+\\)+\\s-*("
     ("\\s-*\\(\\sw+\\)\\s-*[,|)]" nil nil 
      (1 font-lock-variable-name-face)))
    ;; I like variables for FOR loops
    '("\\<\\(for\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*\\([^\n,]+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face) 
      (3 font-lock-reference-face))
    ;; Items after a switch statements are cool
    '("\\<\\(case\\|switch\\)\\s-+\\(.*\\)" 
      (1 font-lock-keyword-face) (2 font-lock-reference-face))
   ;; How about a few matlab constants such as pi, infinity, and sqrt(-1)?
   ;; The ^>> is in case I use this in an interactive mode someday
   '("\\<\\(pi\\|inf\\|Inf\\|NaN\\|nan\\|ans\\|i\\|j\\|^>>\\)\\>"
     1 font-lock-reference-face)
   ;; Define these as variables since this is about as close
   ;; as matlab gets to variables
   '("\\(set\\|get\\)\\s-*(\\s-*\\(\\w+\\)\\s-*\\(,\\|)\\)" 
     2 font-lock-variable-name-face)
   ))
  "Expressions to hightlight in Matlab mode.")


(defvar matlab-really-guady-font-lock-keywords
  (append
   matlab-gaudy-font-lock-keywords
   (list
    ;; Since it's a math language, how bout dem symbols?
    '("\\([<>~]=?\\|\\.[*^']\\|==\\|\\<xor\\>\\|[-!^&|*+\\/~:]\\)"
      1 font-lock-type-face)
    ;; How about the special help comments
    ;;'("function[^\n]+" 
    ;;  ("^%\\([^\n]+\\)\n" nil nil (1 font-lock-reference-face t)))
    ;; continuation elipses.  This will underline all valid elipses
    ;; according to matlab usage.
    '("[^.]\\(\\.\\.\\.\\)\\(\\s-*%\\|\n\\)" 1 'underline)
    ;; How about debugging statements?
    '("\\<\\(db\\sw+\\)\\>" 1 'bold)
    ;; Correct setting of the syntax table and other variables 
    ;; will automatically handle this 
    ;; '("%\\s-+.*" 0 font-lock-comment-face t)
    ))
  "Expressions to hightlight in Matlab mode.")

(defvar matlab-shell-font-lock-keywords
  (list
   ;; How about Errors?
   '("^\\(Error in\\|Syntax error in\\)\\s-+==>\\s-+\\(.+\\)$"
     (1 font-lock-comment-face) (2 font-lock-string-face))
   ;; and line numbers
   '("^\\(On line [0-9]+\\)" 1 font-lock-comment-face)
   ;; User beep things
   '("\\(\\?\\?\\?[^\n]+\\)" 1 font-lock-comment-face)
   ;; Useful user commands, but not useful programming constructs
   '("\\<\\(demo\\|whatsnew\\|info\\|subscribe\\|help\\|doc\\|lookfor\\|what\
\\|whos?\\|cd\\|clear\\|load\\|save\\)\\>"
     1 font-lock-keyword-face)
   ;; Various notices
   '(" M A T L A B (R) " 0 'underline)
   '("All Rights Reserved" 0 'italic)
   '("\\((c)\\s-+Copyright[^\n]+\\)" 1 font-lock-comment-face)
   '("\\(Version\\)\\s-+\\([^\n]+\\)"
     (1 font-lock-function-name-face) (2 font-lock-variable-name-face))
   )
  "Additional keywords used by matlab when reporting errors in interactive\
mode.")


;; hilit19 patterns
(defvar matlab-hilit19-patterns
  '(
    ("\\(^\\|[^%]\\)\\(%[ \t].*\\|%\\)$" 2 comment)
    ("\\(^\\|[;,]\\)[ \t]*\\(\
function\\|global\\|for\\|while\\|if\\|elseif\\|else\\|end\\|return\
\\|switch\\|case\\|otherwise\\)\\b" 2 keyword)))

(defvar matlab-imenu-generic-expression
  '((nil "^function\\(\\s-+[^=\n]+\\s-*=\\s-*\\|\\s-+\\)\\(\\sw+\\)\\s-*(" 
	 2))
  "Expressions which find function headings in Matlab M files.")


;;; matlab-mode entry point ==================================================

(defun matlab-mode ()
  "Matlab-mode is a major mode for editing MATLAB dot-m files.
This is version `matlab-mode-version'.  This version
    +   will run matlab-mode-hook if it is non-nil
    +   supports font-lock and hilit19

Special Key Bindings:
\\{matlab-mode-map}
Variables:
  matlab-indent-level		Level to indent blocks.
  matlab-comment-column         Goal column for on-line comments.
  fill-column			Column used in auto-fill.
  matlab-comment-line-s	        Sring to start comment line.
  matlab-comment-region-s	String to put comment lines in region.
  matlab-vers-on-startup	If t, show version on start-up.
  matlab-indent-function	If t, indents body of MATLAB functions.
  matlab-hilit19-patterns	Patterns for hilit19
  matlab-font-lock-keywords	Keywords for font-lock
  matlab-return-function	Function-variable to customize RET handling

Commands:
  matlab-mode                   Enter MATLAB major mode.
  matlab-return                 RET with post indenting.
  matlab-linefeed               RET with pre and post indent.
  matlab-comment-return		RET for next-line comment.
  matlab-indent-line            Indent line for structure.
  matlab-comment                Add comment to current line.
  matlab-comment-indent         Compute indent for comment.
  matlab-comment-region		Comment (with arg, uncomment) region.
  matlab-fill-region		Fill region (usually comments).
  matlab-justify-line		Delete space on end and justify.
  matlab-mode-hilit             Turn on hilit19.

To add automatic support put something like the following in your .emacs file:
  \(autoload 'matlab-mode \"matlab\" \"Enter Matlab mode.\" t\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.m$\" . matlab-mode\) \
auto-mode-alist\)\)
  \(defun my-matlab-mode-hook \(\)
    \(setq fill-column 76\)
    \(font-lock-mode 1\)    
    \(turn-on-auto-fill\)\)
  \(setq matlab-mode-hook 'my-matlab-mode-hook\)"
  (interactive)
  (kill-all-local-variables)
  (use-local-map matlab-mode-map)
  (setq major-mode 'matlab-mode)
  (setq mode-name "Matlab")
  (setq local-abbrev-table matlab-mode-abbrev-table)
  (set-syntax-table matlab-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'matlab-indent-line)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%\\s-+")
  (make-local-variable 'comment-column)
  (setq comment-column 'matlab-comment-column)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'matlab-comment-indent)
  (make-local-variable 'fill-column)
  (setq fill-column default-fill-column)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'matlab-auto-fill)
  (make-local-variable 'fill-prefix)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression matlab-imenu-generic-expression)
  ;; give each file it's own paramter history
  (make-local-variable 'matlab-shell-save-and-go-history)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-font-lock-keywords
			      matlab-gaudy-font-lock-keywords
			      matlab-really-guady-font-lock-keywords
			      )
			     t ; do not do string/comment highlighting
			     t ; keywords are case insensitive.
			     ;; This puts _ as a word constituant,
			     ;; simplifying our keywords significantly
			     ((?_ . "w"))))
  (if window-system (matlab-frame-init))
  (run-hooks 'matlab-mode-hook)
  (matlab-reset-vars)
  (if matlab-vers-on-startup (matlab-show-version)))


;;; regexps for MATLAB language ===============================================

;; "-pre" means "partial regular expression"

(defvar matlab-block-beg-pre-if "function\\|for\\|while\\|if\\|switch"
  "Key works which mark the beginning of an indented block.
Includes function.")
(defvar matlab-block-beg-pre-no-if "for\\|while\\|if\\|switch"
  "Key works which mark the beginning of an indented block.
Excludes function.")

(defvar matlab-block-beg-pre 
  (if matlab-indent-function
      matlab-block-beg-pre-if matlab-block-beg-pre-no-if)
  "Partial regular expression to recognize Matlab block-begin keywords")

(defconst matlab-block-mid-pre 
  "elseif\\|else"
  "Partial regular expression to recognize Matlab mid-block keywords")

(defconst matlab-block-end-pre-if
  "end\\|function"
  "partial regular expression to recognize Matlab block-end keywords")
(defconst matlab-block-end-pre-no-if
  "end"
  "Partial regular expression to recongnize Matlab block-end keyworks")

(defvar matlab-block-end-pre
  (if matlab-indent-function
      matlab-block-end-pre-if matlab-block-end-pre-no-if)
  "Partial regular expression to recongnize Matlab block-end keyworks")

;; Not used.
(defconst matlab-other-pre
  "function\\|return"
  "partial regular express to recognize Matlab non-block keywords")

(defconst matlab-endless-blocks
  "case\\|otherwise"
  "These keywords initialize new blocks, like a for or if, but don't have
thier own explicit ends (As such, are endless).  A new case or otherwise
will and a previous endless block, and and end will end this block, plus
any outside normal blocks.")

(defvar matlab-block-re
  (concat "\\(^\\|[;,]\\)[ \t]*\\("
	  matlab-block-beg-pre "\\|"
	  matlab-block-mid-pre "\\|"
	  matlab-block-end-pre "\\|"
	  matlab-endless-blocks "\\)\\b")
  "regular expression for keywords which begin Matlab blocks")

(defvar matlab-block-scan-re
  (concat "\\<\\("
	  matlab-block-beg-pre "\\|"
	  matlab-block-end-pre "\\)\\b")
  "Expression used to scan over matching pairs of begin/ends")

(defconst matlab-block-beg-re
  (concat "\\(" matlab-block-beg-pre "\\)"))

(defconst matlab-block-mid-re
  (concat "\\(" matlab-block-mid-pre "\\)"))

(defconst matlab-block-end-re
  (concat "\\(" matlab-block-end-pre "\\)"))

(defconst matlab-block-end-no-function-re
  (concat "\\<\\(" matlab-block-end-pre-no-if "\\)\\>"))

(defconst matlab-endless-blocks-re
  (concat "\\(" matlab-endless-blocks "\\)"))

(defconst matlab-cline-start-skip "[ \t]*%[ \t]*"
  "*The regular expression for skipping comment start.")

(defun matlab-reset-vars ()
  "Call this whenever any Matlab parsing expressions are changed."
  (interactive)
  (setq matlab-block-beg-pre 
	(if matlab-indent-function
	    matlab-block-beg-pre-if matlab-block-beg-pre-no-if)
	matlab-block-re
	(concat "\\(^\\|[;,]\\)[ \t]*\\("
		matlab-block-beg-pre "\\|"
		matlab-block-mid-pre "\\|"
		matlab-block-end-pre "\\|"
		matlab-endless-blocks "\\)\\b")
	matlab-block-end-pre
	(if matlab-indent-function
	    matlab-block-end-pre-if matlab-block-end-pre-no-if)
	matlab-block-end-re
	(concat "\\(" matlab-block-end-pre "\\)")))


;;; utilities =================================================================

(defun matlab-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "matlab-mode, version %s" matlab-mode-version))

(defun matlab-find-prev-line ()
  (if (= -1 (forward-line -1)) nil
    (if (matlab-ltype-empty) (matlab-find-prev-line) t)))

(defun matlab-prev-line ()
  "Go to the previous line of code.  Return nil if not found."
  (interactive)
  (let ((old-point (point)))
    (if (matlab-find-prev-line) t (goto-char old-point) nil)))

(defmacro matlab-navigation-syntax (&rest forms)
  "Set the current environment for syntax-navigation and execute FORMS"
  (list 'let '((oldsyntax (syntax-table)))
	(list 'unwind-protect
	      (list 'progn
		    '(set-syntax-table matlab-mode-special-syntax-table)
		    (cons 'progn forms))
	      '(set-syntax-table oldsyntax))))
(put 'matlab-navigation-syntax 'lisp-indent-function 0)
  

(defun matlab-backward-sexp (&optional autoend)
  "Go backwards one balenced set of Matlab expressions.
If optional AUTOEND, then pretend we are at an end."
  (interactive)
  (matlab-navigation-syntax
    (if (and (not autoend)
	     (save-excursion (backward-word 1)
			     (or (not (looking-at
				       matlab-block-end-no-function-re))
				 (matlab-cursor-in-string-or-comment))))
	;; Go backwards one simple expression
	(forward-sexp -1)
      ;; otherwise go backwards recursively across balanced expressions
      ;; backup over our end
      (if (not autoend) (forward-word -1))
      (let ((done nil))
	(while (not done)
	  (re-search-backward matlab-block-scan-re nil t)
	  (if (looking-at matlab-block-end-no-function-re)
	      (if (matlab-cursor-in-string-or-comment)
		  nil
		;; we must skip the expression and keep searching
		(forward-word 1)
		(matlab-backward-sexp))
	    (if (not (matlab-cursor-in-string-or-comment))
		(setq done t))))))))

(defun matlab-forward-sexp ()
  "Go forward one balenced set of Matlab expressions."
  (interactive)
  (matlab-navigation-syntax
    ;; skip over preceeding whitespace
    (skip-chars-forward "[ \t\n]+")
    (if (or (not (looking-at (concat "\\(\\s-\\|;\\|\\.\\)*\\("
				     matlab-block-beg-pre
				     "\\)\\>")))
	    (and (matlab-cursor-in-string-or-comment)
		 (not (looking-at "\\(\\s-\\|\\.\\)*$"))))
	;; Go forwards one simple expression
	(forward-sexp 1)
      ;; otherwise go forwards recursively across balanced expressions
      (forward-word 1)
      (let ((done nil))
	(while (not done)
	  (re-search-forward matlab-block-scan-re nil t)
	  (goto-char (match-beginning 0))
	  (if (looking-at matlab-block-beg-pre)
	      (if (matlab-cursor-in-string-or-comment)
		  (forward-word 1)
		;; we must skip the expression and keep searching
		(matlab-forward-sexp))
	    (forward-word 1)
	    (if (not (matlab-cursor-in-string-or-comment))
		(setq done t))))))))


;;; line types and attributes =================================================

(defun matlab-ltype-empty ()		; blank line
  "Returns t if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun matlab-ltype-comm ()		; comment line
  "Returns t if current line is a MATLAB comment line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*%[ \t].*$")))

(defun matlab-ltype-code ()		; line of code
  "Return t if current line is a MATLAB code line."
  (and (not (matlab-ltype-empty)) (not (matlab-ltype-comm))))

(defun matlab-lattr-comm ()		; line has comment
  "Returns t if current line contains a comment."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\(^\\|.*[^%]\\)%[ \t]")))

(defun matlab-lattr-cont ()		; line has continuation
  "Returns t if current line ends in ... and optional comment."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^; \t.][ \t]*\\.\\.+[ \t]*\\(%.*\\)?$"
		       (position-at-eol) t)))

(defun matlab-cursor-in-string-or-comment ()
  "Returns t if the cursor is in a valid Matlab comment or string"
  (save-restriction
    (narrow-to-region (save-excursion (beginning-of-line) (point))
		      (save-excursion (end-of-line) (point)))
    (let ((l 0) (rv nil))
      (or (save-excursion
	    ;; In a string if odd # quotes before point on line
	    (while (re-search-backward "'" nil t)
	      (setq l (1+ l)))
	    (= 1 (% l 2)))
	  (save-excursion
	    ;; In comment, if there is a % before us, and it is not in
	    ;; a string.  We already have a quote count, so use
	    ;; that to see if any % are outside of a string
	    (while (re-search-backward "%\\|'" nil t)
	      (if (looking-at "'")
		  (setq l (1- l))
		(if (= 1 (% l 2))
		    nil
		  (beginning-of-line)
		  (setq rv t))))
	    rv)
	  ))))

(defun matlab-cursor-in-param-block ()
  "Returns t if the cursor is inside a param block.
Useful for keywords appearing inside paramter lists as variables."
  
  )


;;; indent functions ==========================================================

(defun matlab-indent-line ()
  "Indent a line in matlab-mode."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (matlab-calc-indent))
    ;; If line contains a comment, format it.
    (if () (if (matlab-lattr-comm) (matlab-comment))))
  (skip-chars-forward " \t%"))

(defun matlab-calc-indent ()
  "Return the appropriate indentation for this line as an int."
  (interactive)
  (let ((indent 0))
    (save-excursion
      (if (matlab-prev-line)
	  (setq indent (+ (current-indentation) (matlab-add-to-next)))))
    (setq indent (+ indent (matlab-add-from-prev)))
    indent))

(defun matlab-add-to-next ()
  (car (cdr (matlab-calc-deltas))))

(defun matlab-add-from-prev ()
  (car (matlab-calc-deltas)))

(defun matlab-calc-deltas ()
  "This routine returns the list (add-from-prev add-to-next)."
  (let ((add-from-prev 0) (add-to-next 0) eol)
    (if (matlab-ltype-comm) (list 0 0)
      (save-excursion
	(matlab-navigation-syntax
	  (setq eol (position-at-eol))
	  ;; indentation for control structures
	  (beginning-of-line)
	  (while (re-search-forward matlab-block-re eol t)
	    (save-excursion
	      (goto-char (match-beginning 2))
	      (if (looking-at matlab-block-beg-re)
		  (progn
		    (setq add-to-next (+ add-to-next matlab-indent-level))
		    ;; In some circumstances a block begin is also a block
		    ;; end, notibly, FUNCTION will mark the end of a previous
		    ;; FUNCTION.  Theoretically when looking at a FUNCTION
		    ;; line I should verify backwards that we are also ending
		    ;; a funciton, but that might be too pesky.
		    (if (looking-at matlab-block-end-re)
			(setq add-from-prev
			      (- add-from-prev matlab-indent-level))))
		(if (> add-to-next 0)
		    (setq add-to-next (- add-to-next matlab-indent-level))
		  (setq add-from-prev (- add-from-prev matlab-indent-level)))
		(if (looking-at matlab-endless-blocks-re)
		    ;; With the introduction of switch statements, our current
		    ;; indentation is no-longer indicitive of the last opened
		    ;; block statement.  We must use the specialized forward/
		    ;; backward sexp to navigate over intervening blocks of
		    ;; code to learn our true indentation level.
		    (save-excursion
		      (let ((p (point)))
			(setq add-to-next (+ add-to-next matlab-indent-level))
			;; Ok, the fun is over, now for some unpleasant scanning
			(matlab-backward-sexp t)
			(if (and
			     (re-search-forward matlab-endless-blocks-re nil t)
			     (< p (point)))
			    (setq add-from-prev
				  (+ add-from-prev matlab-indent-level))))))
		(if (looking-at matlab-block-end-re)
		    (save-excursion
		      (forward-word 1)
		      (matlab-backward-sexp)
		      (if (looking-at "switch")
			  (setq add-from-prev
				(- add-from-prev matlab-indent-level)))))
		(if (looking-at matlab-block-mid-re)
		    (setq add-to-next (+ add-to-next matlab-indent-level))))))
	  ;; indentation for matrix expressions
	  (beginning-of-line)
	  (while (re-search-forward "[][]" eol t)
	    (save-excursion
	      (goto-char (match-beginning 0))
	      (if (matlab-cursor-in-string-or-comment)
		  nil
		(if (looking-at "\\[")
		    (setq add-to-next (+ add-to-next matlab-indent-level))
		  (setq add-to-next (- add-to-next matlab-indent-level))))))
	  ;; continuation lines
	  (if (matlab-lattr-cont)
	      (save-excursion
		(if (= 0 (forward-line -1))
		    (if (matlab-lattr-cont)
			()
		      (setq add-to-next (+ add-to-next matlab-cont-level)))
		  (setq add-to-next (+ add-to-next matlab-cont-level))))
	    (save-excursion
	      (if (= 0 (forward-line -1))
		  (if (matlab-ltype-comm) ()
		    (if (matlab-lattr-cont)
			(setq add-to-next (- add-to-next matlab-cont-level)))))))
	  )
	(list add-from-prev add-to-next)))))


;;; the return key ============================================================

(defvar matlab-return-function 'matlab-indent-end-before-ret
  "Function to handle return key.
Must be one of:
    'matlab-plain-ret
    'matlab-indent-after-ret
    'matlab-indent-end-before-ret
    'matlab-indent-before-ret")

(defun matlab-return ()
  "Handle carriage return in matlab-mode."
  (interactive)
  (funcall matlab-return-function))

(defun matlab-plain-ret ()
  "Vanila new line."
  (interactive)
  (newline))
  
(defun matlab-indent-after-ret ()
  "Indent after new line."
  (interactive)
  (newline)
  (matlab-indent-line))

(defun matlab-indent-end-before-ret ()
  "Indent line if block end, start new line, and indent again."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "[ \t]*\\(elseif\\|else\\|end\\|case\\|otherwise\\)\\b"))
      (matlab-indent-line))
  (newline)
  (matlab-indent-line))

(defun matlab-indent-before-ret ()
  "Indent line, start new line, and indent again."
  (interactive)
  (matlab-indent-line)
  (newline)
  (matlab-indent-line))

(defun matlab-linefeed ()
  "Handle linefeed in matlab-mode.
Has effect of `matlab-return' with (not matlab-indent-before-return)."
  (interactive)
  (matlab-indent-line)
  (newline)
  (matlab-indent-line))

(defun matlab-comment-return ()
  "Handle carriage return for Matlab comment line."
  (interactive)
  (cond
   ((matlab-ltype-comm)
    (matlab-set-comm-fill-prefix) (newline) (insert fill-prefix)
    (matlab-reset-fill-prefix) (matlab-indent-line))
   ((matlab-lattr-comm)
    (newline) (indent-to matlab-comment-column)
    (insert matlab-comment-on-line-s))
   (t
    (newline) (matlab-comment) (matlab-indent-line))))

(defun matlab-comm-from-prev ()
  "If the previous line is a comment-line then set up a comment on this line."
  (save-excursion
    ;; If the previous line is a comment-line then set the fill prefix from
    ;; the previous line and fill this line.
    (if (and (= 0 (forward-line -1)) (matlab-ltype-comm))
	(progn 
	  (matlab-set-comm-fill-prefix)
	  (forward-line 1) (beginning-of-line)
	  (delete-horizontal-space)
	  (if (looking-at "%") (delete-char 1))
	  (delete-horizontal-space)
	  (insert fill-prefix)
	  (matlab-reset-fill-prefix)))))

(defun matlab-comment ()
  "Add a comment to the current line."
  (interactive)
  (cond
   ((matlab-ltype-empty)		; empty line
    (matlab-comm-from-prev)
    (skip-chars-forward " \t%"))
   ((matlab-ltype-comm)			; comment line
    (matlab-comm-from-prev)
    (skip-chars-forward " \t%"))
   ((matlab-lattr-comm)			; code line w/ comment
    (beginning-of-line)
    (re-search-forward "[^%]%[ \t]")
    (forward-char -2)
    (if (< (current-column) matlab-comment-column)
	(indent-to matlab-comment-column))
    (skip-chars-forward "% \t"))
   (t					; code line w/o comment
    (end-of-line)
    (re-search-backward "[^ \t\n^]" 0 t)
    (forward-char)
    (delete-horizontal-space)
    (if (< (current-column) matlab-comment-column)
	(indent-to matlab-comment-column)
      (insert " "))
    (insert matlab-comment-on-line-s))))

(defun matlab-comment-indent ()
  "Indent a comment line in matlab-mode."
  (matlab-calc-indent))

(defun matlab-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts `matlab-comment-region-s' at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert matlab-comment-region-s)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert matlab-comment-region-s)))
      (let ((com (regexp-quote matlab-comment-region-s))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))


;;; filling ===================================================================

(defun matlab-set-comm-fill-prefix ()
  "Set the `fill-prefix' for the current (comment) line."
  (interactive)
  (setq fill-prefix
	(save-excursion
	  (beginning-of-line) 
	  (buffer-substring
	   (point)
	   (progn (re-search-forward "[ \t]*%[ \t]+") (point))))))

(defun matlab-set-code-fill-prefix ()
  "Set the `fill-prefix' for the current code line."
  (setq fill-prefix
	(save-excursion
	  (beginning-of-line) 
	  (buffer-substring
	   (point)
	   (progn (re-search-forward "[ \t]*") (point))))))

(defun matlab-reset-fill-prefix ()
  "Reset the fill-prefix."
  (setq fill-prefix nil))

(defun matlab-auto-fill ()
  "Do filling."
  (interactive)
  (if (> (current-column) fill-column)
      (cond
       ((matlab-ltype-comm)
	(matlab-set-comm-fill-prefix) (do-auto-fill)
	(matlab-reset-fill-prefix))
       ((and 
	 (and (matlab-ltype-code) (not (matlab-lattr-comm))) matlab-fill-code)
	(save-excursion
	  (while (> (current-column) fill-column) (forward-char -1))
	  (re-search-backward "[ \t]+") (delete-horizontal-space)
	  (insert " ...\n") (matlab-indent-line))))))

(defun matlab-join-comment-lines ()
  "Join current comment line to previous, deleting space and comment mark."
  (interactive)
  (beginning-of-line)
  (forward-char -1) (delete-char 1)	; delete newline
  (delete-horizontal-space)
  (delete-char 1)			; delete "%"
  (delete-horizontal-space)
  (insert " "))

(defun matlab-wrap-line () nil)

(defun matlab-fill-region (beg-region end-region &optional justify-flag)
  "Fill the region between BEG-REGION and END-REGION.
Non-nil JUSTIFY-FLAG means justify commment lines as well."
  (interactive "*r\nP")
  (let ((end-reg-mk (make-marker)))
    (set-marker end-reg-mk end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (while (< (save-excursion (forward-line 1) (point)) end-reg-mk)
      (if (save-excursion (= (forward-line 1) 0))
	  (progn 
	    (cond
	     ((matlab-ltype-comm)
	      (while (matlab-fill-comment-line))
	      (if justify-flag (justify-comment-line))))
	    (forward-line 1))))))

(defun matlab-fill-comment-line ()
  "Fill the current comment line."
  (interactive)
  (let ((prev-indent-col 0))
    (beginning-of-line)
    (re-search-forward matlab-cline-start-skip)
    (setq prev-indent-col (current-column))
    ;;(matlab-set-comm-fill-prefix)
    (if (/= (forward-line 1) 0)
	()
      (beginning-of-line)
      (re-search-forward matlab-cline-start-skip)
      (if (/= prev-indent-col (current-column))
	  (progn (forward-line -1) ())
	(matlab-join-comment-lines)
	(if (matlab-wrap-line)
	    (save-excursion
	      (forward-line 1)
	      (beginning-of-line)
	      (insert matlab-comment-line-s)
	      t))))))

(defun matlab-justify-line ()
  "Delete space on end of line and justify."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)
    (justify-current-line)))


;;; M File path stuff =========================================================

(defun matlab-mode-determine-mfile-path ()
  "Create the path in `matlab-mode-install-path'"
  (let ((path (file-name-directory matlab-shell-command)))
    ;; if we don't have a path, find the Matlab executable on our path.
    (if (not path)
	(let ((pl exec-path))
	  (while (and pl (not path))
	    (if (file-exists-p (concat (car pl) "/" matlab-shell-command))
		(setq path (car pl)))
	    (setq pl (cdr pl)))))
    ;; When we find the path, we need to massage it to identify where
    ;; the M files are that we need for our completion lists.
    (if (string-match "bin$" path)
	(setq path (substring path 0 (match-beginning 0))))
    ;; Everything stems from toolbox (I think)
    (setq path (concat path "toolbox/"))
    path))

;; I haven't determined a use for this yet...  -EML
;(defvar matlab-mode-install-path (matlab-mode-determine-mfile-path)
;  "Base path pointing to the locations of all the m files used by matlab.
;Usually there are no m files here, but directories to other toolboxes.")


;;; V19 stuff =================================================================

(defun matlab-mode-hilit ()
  "Set up hilit19 support for matlab-mode."
  (interactive)
  (cond (window-system
	 (setq hilit-mode-enable-list  '(not text-mode)
	       hilit-background-mode   'light
	       hilit-inhibit-hooks     nil
	       hilit-inhibit-rebinding nil)
	 (require 'hilit19)
	 (hilit-set-mode-patterns 'matlab-mode matlab-hilit19-patterns))))

(defvar matlab-mode-menu-keymap nil
  "Keymap used in Matlab mode to provide a menu.")

(defun matlab-frame-init ()
  (interactive)
  ;;(modify-frame-parameters (selected-frame) '((menu-bar-lines . 2)))
  ;; make a menu keymap
  (if matlab-mode-menu-keymap
      nil
    (setq matlab-mode-menu-keymap (make-sparse-keymap "Matlab"))
    (define-key matlab-mode-map [menu-bar matlab]
      (cons "Matlab" matlab-mode-menu-keymap))
    (define-key matlab-mode-map [menu-bar matlab topicbrowse]
      '("Topic Browser" . matlab-shell-topic-browser))
    (define-key matlab-mode-map [menu-bar matlab saveandgo] 
      '("Save and Go" . matlab-shell-save-and-go))
    (define-key matlab-mode-map [menu-bar matlab version] 
      '("Version" . matlab-show-version))
    ))


;;; Matlab shell from Eric Ludlam <eludlam@mathworks.com> =====================

(defvar matlab-shell-mode-map ()
  "Keymap used in matlab-shell-mode.")

(defvar matlab-shell-font-lock-keywords-1
  (append matlab-font-lock-keywords matlab-shell-font-lock-keywords)
  "Keyword symbol used for fontlock mode.")

(defvar matlab-shell-font-lock-keywords-2
  (append matlab-shell-font-lock-keywords-1 matlab-gaudy-font-lock-keywords)
  "Keyword symbol used for gaudy font-lock symbols.")

(defvar matlab-shell-font-lock-keywords-3
  (append matlab-shell-font-lock-keywords-2 
	  matlab-really-guady-font-lock-keywords)
  "Keyword symbol used for really guady font-lock symbols.")

(defvar matlab-shell-command "matlab"
  "*The name of the command to be run which will start the Matlab process.")

(defvar matlab-shell-command-switches ""
  "*The switches to `matlab-shell-command'.")

(defvar matlab-shell-buffer-name "Matlab"
  "Name used to create `matlab-shell' mode buffers.
This name will have *'s surrounding it.")

(defun matlab-shell ()
  "Create a buffer with matlab running as a subprocess."
  (interactive)
  (require 'comint)
  ;; Build keymap here in case someone never uses comint mode
  (if matlab-shell-mode-map
      ()
    (if (string-match "XEmacs" emacs-version)
	(progn
	  (setq matlab-shell-mode-map (make-keymap))
	  (set-keymap-name matlab-shell-mode-map 'matlab-shell-mode-map)
	  (set-keymap-parents matlab-shell-mode-map (list comint-mode-map)))
      (setq matlab-shell-mode-map
	    (nconc (make-sparse-keymap) comint-mode-map)))
    (substitute-key-definition 'next-error 'matlab-shell-last-error
			       matlab-shell-mode-map global-map)
    (define-key matlab-shell-mode-map "\C-c\C-h" matlab-help-map)
    ;; C-{up,down,return}, up and down from Phil Miller <miller@kirk.lmco.com>
    (define-key matlab-shell-mode-map [(control up)]
      'comint-previous-matching-input-from-input)
    (define-key matlab-shell-mode-map [(control down)]
      'comint-next-matching-input-from-input)
    (define-key matlab-shell-mode-map [up]
      'comint-previous-matching-input-from-input)
    (define-key matlab-shell-mode-map [down]
      'comint-next-matching-input-from-input)
    (define-key matlab-shell-mode-map [(control return)] 'comint-kill-input)
    (if (string-match "XEmacs" emacs-version)
	()
      (define-key matlab-shell-mode-map [menu-bar completion]
	(copy-keymap (lookup-key comint-mode-map [menu-bar completion])))
      (define-key matlab-shell-mode-map [menu-bar matshell] 
	(cons "Matlab" (make-sparse-keymap "Matlab")))
      (define-key matlab-shell-mode-map [menu-bar matshell exit]
	'("Exit" . matlab-shell-exit))
      (define-key matlab-shell-mode-map [menu-bar matshell close]
	'("Close Figures" . matlab-shell-close-figures))
      (define-key matlab-shell-mode-map [menu-bar matshell delfig]
	'("Close Current Figure" . matlab-shell-close-current-figure))
      (define-key matlab-shell-mode-map [menu-bar matshell demos]
	'("Demos" . matlab-shell-demos))
      (define-key matlab-mode-map [menu-bar matshell topicbrowse]
	'("Topic Browser" . matlab-shell-topic-browser))
      (define-key matlab-shell-mode-map [menu-bar matshell lasterr]
	'("Goto last error" . matlab-shell-last-error))
      ))
  (switch-to-buffer
   (make-comint matlab-shell-buffer-name matlab-shell-command 
		nil matlab-shell-command-switches))
  (matlab-shell-mode))

(defun matlab-shell-mode ()
  "Run matlab as a subprocess in an emacs buffer.

This mode will allow standard emacs shell commands/completion to occur
with matlab running as an inferior process.  Additionally, this shell
mode is integrated with `matlab-mode', a major mode for editing M
code.

>From an M file buffer:
\\<matlab-mode-map>
\\[matlab-shell-save-and-go] - Save the current M file, and run it in a \
Matlab shell.

>From Shell mode:
\\<matlab-shell-mode-map>
\\[matlab-shell-last-error] - find location of last matlab runtime error \
in the offending M file.

>From an M file, or from Shell mode:
\\[matlab-shell-describe-variable] - Show variable contents in a popup buffer.
\\[matlab-shell-describe-command] - Show online documentation for a command\
in a popup buffer.
\\[matlab-shell-apropo] - Show output from LOOKFOR command in a popup buffer."
  (comint-mode)
  (setq major-mode 'matlab-shell-mode
	mode-name "M-Shell"
	comint-prompt-regexp "^>> *"
	comint-delimiter-argument-list [ 59 ] ; semi colon
	comint-dynamic-complete-functions '(comint-replace-by-expanded-history)
	comint-process-echoes t
	)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (use-local-map matlab-shell-mode-map)
  (set-syntax-table matlab-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-shell-font-lock-keywords-1
			      matlab-shell-font-lock-keywords-2
			      matlab-shell-font-lock-keywords-3)
			     t t ((?_ . "w"))))
  (run-hooks 'matlab-shell-mode-hook)
  (matlab-show-version)
  )

(defun matlab-shell-run-command (command)
  "Run COMMAND and display result in a buffer.
This command requires an active matlab shell."
  (interactive "sMatlab Variable: ")
  (let ((doc (matlab-shell-collect-command-output command)))
    (with-output-to-temp-buffer "*Matlab Help*"
      (princ doc))))

(defun matlab-shell-describe-variable (variable)
  "Get the contents of VARIABLE and display them in a buffer.
This uses the WHOS (Matlab 5) command to find viable commands.
This command requires an active matlab shell."
  (interactive "sMatlab Variable: ")
  (let ((doc (matlab-shell-collect-command-output (concat "whos " variable))))
    (with-output-to-temp-buffer "*Matlab Help*"
      (princ doc))))

(defun matlab-shell-describe-command (command)
  "Describe COMMAND textually by fetching it's doc from the Matlab shell.
This uses the lookfor command to find viable commands.
This command requires an active matlab shell."
  (interactive "sMatlab Command: ")
  (let ((doc (matlab-shell-collect-command-output (concat "help " command))))
    (with-output-to-temp-buffer "*Matlab Help*"
      (princ doc))))

(defun matlab-shell-apropo (matlabregex)
  "Look for any active commands in MATLAB matching MATLABREGEX.
This uses the lookfor command to find viable commands."
  (interactive "sMatlab Command Subexpression: ")
  (let ((ap (matlab-shell-collect-command-output
	     (concat "lookfor " matlabregex))))
    (with-output-to-temp-buffer "*Matlab Apropo*"
      (princ ap))))
  
(defun matlab-on-prompt-p ()
  "Return t if we are sitting at an empty prompt line."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat comint-prompt-regexp "$"))))

(defun matlab-shell-collect-command-output (command)
  "If there is a matlab shell, run the matlab COMMAND and return it's output.
It's output is returned as a string with no face properties.  The text output
of the command is removed from the matlab buffer so there will be no
indication that it ran."
  (let ((msbn (concat "*" matlab-shell-buffer-name "*"))
	(pos nil)
	(str nil))
    (if (not (get-buffer msbn))
	(error "You need to run the command `matlab-shell' to do that!"))
    (save-excursion
      (set-buffer msbn)
      (if (not (matlab-on-prompt-p))
	  (error "Matlab shell must be non-busy to do that."))
      ;; We are done error checking, run the command.
      (setq pos (point))
      (comint-send-string (get-buffer-process (current-buffer))
			  (concat command "\n"))
      (message "Matlab ... Executing command.")
      (sit-for 1)
      (goto-char (point-max))
      (while (or (>= pos (point)) (not (matlab-on-prompt-p)))
	(accept-process-output (get-buffer-process (current-buffer)))
	(goto-char (point-max))
	(message "Matlab reading..."))
      (message "Matlab reading...done")
      (save-excursion
	(goto-char pos)
	(beginning-of-line)
	(setq str (buffer-substring-no-properties (save-excursion
						    (goto-char pos)
						    (beginning-of-line)
						    (forward-line 1)
						    (point))
						  (save-excursion
						    (goto-char (point-max))
						    (beginning-of-line)
						    (point))))
	(delete-region pos (point-max)))
      str)))

(defun matlab-shell-save-and-go ()
  "Save this M file, and evaluate it in a Matlab shell."
  (interactive)
  (if (not (eq major-mode 'matlab-mode))
      (error "Save and go is only useful in a matlab buffer!"))
  (let ((fn-name (file-name-sans-extension
		  (file-name-nondirectory (buffer-file-name))))
	(msbn (concat "*" matlab-shell-buffer-name "*"))
	(param ""))
    (save-buffer)
    ;; Do we need parameters?
    (if (save-excursion
	  (goto-char (point-min))
	  (end-of-line)
	  (forward-sexp -1)
	  (looking-at "([a-zA-Z]"))
	(setq param (read-string "Paramters: "
				 (car matlab-shell-save-and-go-history)
				 'matlab-shell-save-and-go-history)))
    ;; No buffer?  Make it!
    (if (not (get-buffer msbn)) (matlab-shell))
    ;; Ok, now fun the function in the matlab shell
    (if (get-buffer-window msbn)
	(select-window (get-buffer-window msbn))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*")))
    (comint-send-string (get-buffer-process (current-buffer))
			(concat fn-name " " param "\n"))))

(defun matlab-shell-last-error ()
  "In the Matlab interactive buffer, find the last Matlab error, and go there.
To reference old errors, put the currsor just after the error text."
  (interactive)
  (let (eb el)
    (save-excursion
      (if (not (re-search-backward 
		(concat "\\(Error\\|Syntax error\\) in ==> "
			"\\([-.a-zA-Z_0-9/]+\\).*\nOn line "
			"\\([0-9]+\\) ") nil t))
	  (error "No errors found!"))
      (setq eb (buffer-substring-no-properties
		(match-beginning 2) (match-end 2))
	    el (buffer-substring-no-properties
		(match-beginning 3) (match-end 3))))
    (find-file-other-window eb)
    (goto-line (string-to-int el))))

(defun matlab-shell-demos ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "demo\n"))
(defun matlab-shell-close-figures ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "close all\n"))
(defun matlab-shell-close-current-figure ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "delete(gcf)\n"))
(defun matlab-shell-exit ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "exit\n"))


;;; matlab-shell based Topic Browser ==========================================

(defvar matlab-shell-topic-current-topic nil
  "The currently viewed topic in a Matlab shell topic buffer.")

(defun matlab-shell-topic-browser ()
  "Create a topic browser by querying an active Matlab shell using HELP.
Maintain state in our topic browser buffer."
  (interactive)
  ;; Reset topic browswer if it doesn't exist.
  (if (not (get-buffer "*Matlab Topic*"))
      (setq matlab-shell-topic-current-topic nil))
  (let ((b (get-buffer-create "*Matlab Topic*")))
    (switch-to-buffer b)
    (if (string= matlab-shell-topic-current-topic "")
	nil
      (matlab-shell-topic-browser-mode)
      (matlab-shell-topic-browser-create-contents ""))))

(defvar matlab-shell-topic-map nil
  "Map for browsing topics using Matlab.")

(if matlab-shell-topic-map
    nil
  (setq matlab-shell-topic-map (make-keymap "Matlab Topic"))
  (define-key matlab-shell-topic-map " " 'scroll-up)
  (define-key matlab-shell-topic-map "\C-?" 'scroll-down)
  (define-key matlab-shell-topic-map "\r" 'matlab-shell-topic-choose)
  (define-key matlab-shell-topic-map "t" 'matlab-shell-topic-browser)
  (define-key matlab-shell-topic-map "q" 'bury-buffer)
  (if (string-match "XEmacs" emacs-version)
      (define-key matlab-shell-topic-map '(button2) 'matlab-shell-topic-click)
    (define-key matlab-shell-topic-map [mouse-2] 'matlab-shell-topic-click)
    
    (define-key matlab-shell-topic-map [menu-bar mattopic]
      (cons "Matlab-Topic" (make-sparse-keymap "MatlabTopic")))
    (define-key matlab-shell-topic-map [menu-bar mattopic toptopic]
      '("Top Level Topics" . matlab-shell-topic-browser))
    (define-key matlab-shell-topic-map [menu-bar mattopic selecttopic]
      '("Select This Topic" . matlab-shell-topic-choose))
    ))

(defvar matlab-shell-topic-font-lock-keywords
  '(("^[^:\n]+:$" 0 font-lock-keyword-face)
    ;; These are subtopic fields...
    ("^\\(\\w+/\\w+\\)[ \t]+-" 1 font-lock-reference-face)
    ;; These are functions...
    ("^[ \t]+\\(\\w+\\)[ \t]+-" 1 font-lock-function-name-face)
    ;; Here is a See Also line...
    ("[ \t]+See also "
     ("\\(\\w+\\)\\([,.]\\| and\\) *" nil nil (1 font-lock-reference-face)))
    ;; These are subheadings...
    ("^[ \t]+\\([^.\n]+[a-zA-Z.]\\)$" 1 'underline)
    )
  "Keywords useful for highlighting a Matlab TOPIC buffer.")

(defun matlab-shell-topic-browser-mode ()
  "Major mode for browsing matlab HELP topics.
The output of the Matlab command HELP with no parameters creates a listing
of known help topics at a given installation.  This mode parses that listing
and allows selecting a topic and getting more help for it.

\\{matlab-shell-topic-map}"
  (kill-all-local-variables)
  (setq major-mode 'matlab-shell-topic-browser-mode
	mode-name "M-Topic"
	)
  (use-local-map matlab-shell-topic-map)
  (make-variable-buffer-local 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-shell-topic-font-lock-keywords)
			     t t ((?_ . "w"))))
  (run-hooks 'matlab-shell-topic-hooks)
  )

(defun matlab-shell-topic-browser-create-contents (subtopic)
  "Fill in a topic browser with the output from SUBTOPIC"
  (toggle-read-only -1)
  (erase-buffer)
  (insert (matlab-shell-collect-command-output (concat "help " subtopic)))
  (goto-char (point-min))
  (forward-line 1)
  (delete-region (point-min) (point))
  (setq matlab-shell-topic-current-topic subtopic)
  (toggle-read-only 1)
  )

(defun matlab-shell-topic-click (e)
  "Click on an item in a Matlab topic buffer we want more information on.
Must be bound to event E."
  (interactive "e")
  (mouse-set-point e)
  (matlab-shell-topic-choose))

(defun matlab-shell-topic-choose ()
  "Choose the topic to expand on that is under the cursor.
This can fill the topic buffer with new information.  If the topic is a
command, use `matlab-shell-describe-command' instead of changing the topic
buffer."
  (interactive)
  (let ((topic nil) (fun nil) (p (point)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^\\w+/\\(\\w+\\)[ \t]+-")
	  (setq topic (match-string 1))
	(if (looking-at "^[ \t]+\\(\\(\\w\\|_\\)+\\)[ \t]+-")
	    (setq fun (match-string 1))
	  (if (not (looking-at "^[ \t]+See also"))
	      (error "You did not click on a subtopic, function or reference.")
	    (goto-char p)
	    (forward-word -1)
	    (if (not (looking-at "\\(\\(\\w\\|_\\)+\\)[.,]"))
		(error "You must click on a reference.")
	      (setq topic (match-string 1)))))))
    (message "Opening item %s..." (or topic fun))
    (if topic
	(matlab-shell-topic-browser-create-contents (downcase topic))
      (matlab-shell-describe-command fun))
    ))


;;; matlab-mode debugging =====================================================

(defun matlab-show-line-info ()
  "Display type and attributes of current line.  Used in debugging."
  (interactive)
  (let ((msg "line-info:") (deltas (matlab-calc-deltas)))
    (cond
     ((matlab-ltype-empty)
      (setq msg (concat msg " empty")))
     ((matlab-ltype-comm)
      (setq msg (concat msg " comment")))
     (t
      (setq msg (concat msg " code"))))
    (setq msg (concat msg " add-from-prev="
		      (int-to-string (car deltas))))
    (setq msg (concat msg " add-to-next="
		      (int-to-string (car (cdr deltas)))))
    (setq msg (concat msg " indent="
		      (int-to-string (matlab-calc-indent))))
    (if (matlab-lattr-cont)
	(setq msg (concat msg " w/cont")))
    (if (matlab-lattr-comm)
	(setq msg (concat msg " w/comm")))
    (message msg)))

(defun matlab-clear-vars ()
  (interactive)
  (makunbound 'matlab-indent-level)
  (makunbound 'matlab-cont-level)
  (makunbound 'matlab-comment-line-s)
  (makunbound 'matlab-comment-on-line-s)
  (makunbound 'matlab-comment-region-s)
  (makunbound 'matlab-indent-function)
  (makunbound 'matlab-matlab-mode-syntax-table)
  (makunbound 'matlab-matlab-mode-abbrev-table)
  (makunbound 'matlab-matlab-mode-map)
  (makunbound 'matlab-matlab-block-beg-pre)
  (makunbound 'matlab-matlab-block-mid-pre)
  (makunbound 'matlab-matlab-block-end-pre)
  (makunbound 'matlab-matlab-other-pre)
  (makunbound 'matlab-matlab-block-re)
  (makunbound 'matlab-matlab-block-beg-re)
  (makunbound 'matlab-matlab-block-end-re)
  (makunbound 'matlab-cline-start-skip)
  (makunbound 'matlab-matlab-font-lock-keywords))


;;; stuff which belongs elsewhere =============================================

(defun position-at-eol ()		; return point for end-of-line
  (interactive)
  (save-excursion
    (end-of-line)
    (point)))

(defun justify-comment-line ()
  "Add spaces to comment line point is in, so it ends at fill-column."
  (interactive)
  (save-excursion
    (save-restriction
      (let (ncols beg)
	(beginning-of-line)
	(forward-char (length fill-prefix))
	(skip-chars-forward " \t")
	(setq beg (point))
	(end-of-line)
	(narrow-to-region beg (point))
	(goto-char beg)
	(while (re-search-forward "   *" nil t)
	  (delete-region
	   (+ (match-beginning 0)
	      (if (save-excursion
		    (skip-chars-backward " ])\"'")
		    (memq (preceding-char) '(?. ?? ?!)))
		  2 1))
	   (match-end 0)))
	(goto-char beg)
	(while (re-search-forward "[.?!][])""']*\n" nil t)
	  (forward-char -1)
	  (insert " "))
	(goto-char (point-max))
	(setq ncols (- fill-column (current-column)))
	(if (search-backward " " nil t)
	    (while (> ncols 0)
	      (let ((nmove (+ 3 (% (random) 3))))
		(while (> nmove 0)
		  (or (search-backward " " nil t)
		      (progn
			(goto-char (point-max))
			(search-backward " ")))
		  (skip-chars-backward " ")
		  (setq nmove (1- nmove))))
	      (insert " ")
	      (skip-chars-backward " ")
	      (setq ncols (1- ncols))))))))


(provide 'matlab)

;;; matlab.el ends here
