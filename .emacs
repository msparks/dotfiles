(setq load-path  (cons (expand-file-name "~/.emacs-lisp/") load-path))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-netscape-program "opera")
 '(browse-url-xterm-program "term.sh")
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(delete-old-versions t)
 '(global-font-lock-mode t nil (font-lock))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(guess-style-guesser-alist (quote ((indent-tabs-mode . guess-style-guess-tabs-mode))))
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2)))
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(swbuff-clear-delay 3)
 '(swbuff-status-window-layout (quote adjust))
 '(tab-width 2)
 '(tool-bar-mode t)
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(xterm-mouse-mode t))

(require 'color-theme)
(defun color-theme-wombat ()
  (interactive)
  (color-theme-install
   '(color-theme-midnight
     ((font . "fixed")
      (width . 130)
      (height . 50)
      (background-color . "#202020")
      (foreground-color . "#cccccc")
      (background-mode . dark)
      (mouse-color . "grey85")
      (cursor-color . "grey85"))
     (default ((t (nil))))
     (diff-added ((t (:foreground "green"))))
     (diff-context ((t nil)))
     (diff-file-header ((((class color) (min-colors 88) (background dark))
                         (:foreground "RoyalBlue1"))))
     (diff-function ((t (:foreground "#00bbdd"))))
     (diff-header ((((class color) (min-colors 88) (background dark)) (:foreground
                                                                       "RoyalBlue1"))))
     (diff-hunk-header ((t (:foreground "#fbde2d"))))
     (diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
     (diff-refine-change ((((class color) (min-colors 88) (background dark))
                           (:background "#182042"))))
     (diff-removed ((t (:foreground "#de1923"))))
     (font-lock-comment-face ((t (:italic t :foreground "#c0bc6c"))))
     (font-lock-string-face ((t (:foreground "#95e454"))))
     (font-lock-keyword-face ((t (:foreground "#87afff"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (font-lock-constant-face ((t (:foreground "#bbbbbb"))))
     (font-lock-type-face ((t (:foreground "#bbbbbb"))))
     (font-lock-variable-name-face ((t (:foreground "#caeb82"))))
     (font-lock-function-name-face ((t (:foreground "#caeb82"))))
     (font-lock-builtin-face ((t (:foreground "SkyBlue"))))
     (modeline ((t (:foreground "#caeb82" :background "#444444"))))
     (modeline-buffer-id ((t (:foreground "#87afff" :background "#222222"))))
     (modeline-mousable ((t (:foreground "#f6f3e8" :background "#444444"))))
     (highline-face ((t (:background "grey12"))))
     (setnu-line-number-face ((t (:background "Grey15" :foreground "White" :bold t))))
     (show-paren-match-face ((t (:background "grey30"))))
     (region ((t (:background "grey30"))))
     (highlight ((t (:background "blue"))))
     (secondary-selection ((t (:background "navy"))))
     (widget-field-face ((t (:background "navy"))))
     (widget-single-line-field-face ((t (:background "royalblue")))))) )

(color-theme-wombat)

(require 'cc-mode)
(defun my-c-mode-common-hook ()
  (setq c-basic-offset tab-width))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(require 'linum)
(global-linum-mode)

(require 'tabbar)
(tabbar-mode)
(global-set-key (kbd "M-p") 'tabbar-backward)
(global-set-key (kbd "M-n") 'tabbar-forward)

(global-set-key (kbd "C-c c") 'compile)

(setq tabbar-buffer-groups-function
      (lambda ()
        (list "All Buffers")))

(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (find (aref (buffer-name buffer) 0) " *"))
         (buffer-list))))

;; Add a buffer modification state indicator in the tab label,
;; and place a space around the label to make it look less crowded.
(defadvice tabbar-buffer-tab-label (after fixup activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " " (concat ad-return-value "[+] "))
          (concat " " (concat ad-return-value " ")))))
;; called each time the modification state of the buffer changed
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
;; first-change-hook is called BEFORE the change is made
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)
;; this doesn't work for revert
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(setq tabbar-home-button (quote (("[Home]") "[x]")))
(setq tabbar-separator (quote (" ")))

(require 'smooth-scrolling)

;; Coding style preferences
(require 'google-coding-style)

;; This causes massive CPU usage when scrolling with lots of colors
;;(require 'whitespace)
;;(global-whitespace-mode)
;;(setq whitespace-style '(tabs trailing lines-tail empty))

;; set indent style to 'k&r' style for everything but java
;; http://en.wikipedia.org/wiki/Indent_style
(setq c-default-style
      '((java-mode . "java") (other . "k&r")))

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Disable menu bar
(menu-bar-mode -1)

;; Fill columns
(setq-default fill-column 79)

;; Fill mode for text mode by default
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Fill mode by default for all major modes
(setq-default auto-fill-function 'do-auto-fill)

;; Make Text mode the default mode for new buffers
(setq default-major-mode 'text-mode)

;; Show region after marking
(setq-default transient-mark-mode t)

;; Periodically reload buffers from disk.
(global-auto-revert-mode)

;; Display time
(setq display-time-24hr-format t)
(display-time)

;; Enable time-stamp updating on save.
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S")  ; date format
(add-hook 'write-file-hooks 'time-stamp)             ; update when saving

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Make bells less visible. The visible bell (terminal flash) is annoying.
(setq ring-bell-function (lambda () (message "*beep*")))

(defun font-lock-set-up-width-warning (width)
  "In the current buffer, make text beyond column `width' appear in
`font-lock-warning-face'."
  (require 'font-lock)
  (font-lock-mode 1)
  (make-local-variable 'font-lock-keywords)
  (font-lock-add-keywords
   nil
   `((,(format "^.\\{%d\\}\\(.+\\)" width) 1 font-lock-warning-face t))))

;; cperl-mode is preferred to perl-mode.
(defalias 'perl-mode 'cperl-mode)

;; Matlab major mode.
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")

(add-hook 'c-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'c++-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'css-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'java-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'js2-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'cperl-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'python-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'rst-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'text-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'matlab-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))

;; Look for spelling mistakes in code comments and strings.
(add-hook 'c-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'c++-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'css-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'java-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'js2-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'cperl-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'emacs-mode-hook
          '(lambda () (flyspell-prog-mode)))
(add-hook 'matlab-mode-hook
          '(lambda () (flyspell-prog-mode)))

;; Look for spelling mistakes in all text.
(add-hook 'text-mode-hook
          '(lambda () (flyspell-mode)))
(add-hook 'rst-mode-hook
          '(lambda () (flyspell-mode)))

;; git-commit major mode.
(require 'git-commit)

;; Javascript major mode.
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; CSS major mode.
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; Markdown major mode.
(autoload 'markdown-mode "markdown-mode.el" "Mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; reStructuredText major mode.
(autoload 'rst-mode "rst" nil t)
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

;; Arduino code is C++.
(add-to-list 'auto-mode-alist '("\\.pde$" . c++-mode))

;; Diff mode settings.
(setq diff-default-read-only t)  ;; open diffs in RO mode

;; Load in smart tabs (indent with hard tabs, indent with spaces) when we need
;; to edit code written by evil people.
(require 'smarttabs)

;; Use guess-style.el to automatically figure out the indentation settings of
;; the file we're editing. The customized guess-style-guesser-alist setting
;; above determines what settings to discover.
(require 'guess-style)
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
(add-hook 'c-mode-hook 'guess-style-guess-all)
(add-hook 'c++-mode-hook 'guess-style-guess-all)
(add-hook 'java-mode-hook 'guess-style-guess-all)
(add-hook 'cperl-mode-hook 'guess-style-guess-all)
(add-hook 'python-mode-hook 'guess-style-guess-all)
(add-hook 'emacs-lisp-mode-hook 'guess-style-guess-all)
(add-hook 'text-mode-hook 'guess-style-guess-all)
(add-hook 'matlab-mode-hook 'guess-style-guess-all)
(global-guess-style-info-mode 1)

;; More indentation preferences.
(add-hook 'css-mode-hook
          '(lambda () (setq css-indent-offset tab-width)))

;; Prefer a dark background.
(setq frame-background-mode 'dark)

;; Define a new face for hard tabs.
(defface hard-tabs-face
  '((t (:foreground "gray30" :underline t))) "Used for hard tabs.")

;; Draw hard tabs with the custom face.
(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\t" 0 'hard-tabs-face prepend)))))

(add-hook 'sh-mode-hook
     (lambda ()
       (auto-fill-mode nil)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:background "purple"))))
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "#989973"))))
 '(tabbar-button ((t (:background "#202020" :foreground "#202020"))))
 '(tabbar-default ((t (:background "#202020" :foreground "white"))))
 '(tabbar-selected ((t (:background "#222222" :foreground "Pink"))))
 '(trailing-whitespace ((t (:background "#aaaaaa")))))
