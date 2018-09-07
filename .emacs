;; Add a hook to display the startup time for profiling.
;;
;; Source: https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract
                               after-init-time before-init-time)))
                     gcs-done)))

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-netscape-program "opera")
 '(browse-url-xterm-program "term.sh")
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(default-input-method "rfc1345")
 '(delete-old-versions t)
 '(display-time-24hr-format t)
 '(fill-column 80)
 '(global-linum-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(guess-style-guesser-alist (quote ((indent-tabs-mode . guess-style-guess-tabs-mode))))
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold nil)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2)))
 '(require-final-newline t)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(sml/col-number-format "%02c]")
 '(sml/line-number-format "[%3l")
 '(sml/modified-char "×")
 '(sml/numbers-separator ",")
 '(sml/pos-id-separator ")")
 '(sml/pos-minor-modes-separator "")
 '(sml/position-percentage-format "%p")
 '(sml/pre-id-separator " (")
 '(sml/pre-minor-modes-separator "")
 '(standard-indent 2)
 '(swbuff-clear-delay 3)
 '(swbuff-status-window-layout (quote adjust))
 '(tab-width 2)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(version-control t)
 '(xterm-mouse-mode t))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Load custom theme.
(load-theme 'wombat-ms t)

;; Set up smart-mode-line.
(use-package smart-mode-line
  :config
  (sml/setup)
  (sml/apply-theme 'respectful))

(use-package magit
  :ensure t
  :demand t
  :bind (("C-x g" . magit-status)))
(setq git-commit-fill-column 72)

;; Set up projectile.
(use-package projectile
  :defer 2
  :config
  (projectile-global-mode))

;; flx-ido for better matching with projectile and ido.
(use-package flx-ido
  :defer 2)

;; ag for search.
(use-package ag
  :defer 2)

;; Irony mode.
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(use-package irony
  :init
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :hook ((c-mode
           c++-mode
           objc-mode-hook) . irony-mode))

(use-package company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(global-company-mode)

(require 'cc-mode)
(defun my-c-mode-common-hook ()
  (setq c-basic-offset tab-width))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Go mode.
(use-package go-mode
  :defer t)

(use-package smooth-scrolling
  :defer 2)

;; Dynamically calculate linum-format based on the number of lines in the file.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; Tab bar.
(use-package tabbar)
(tabbar-mode)
;; Move between tabs within a group with M-p and M-n.
(global-set-key (kbd "M-p") 'tabbar-backward-tab)
(global-set-key (kbd "M-n") 'tabbar-forward-tab)
;; Move between tab groups with C-M-p and C-M-n.
(global-set-key (kbd "C-M-p") 'tabbar-backward-group)
(global-set-key (kbd "C-M-n") 'tabbar-forward-group)

;; From "Xah Lee".
(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
 This function is a custom function for tabbar-mode's
 tabbar-buffer-groups.  This function group all buffers into 3
 groups: Those Dired, those user buffer, and those emacs buffer.
 Emacs buffer are those starting with '*'."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (string-equal "TAGS" (buffer-name)))
     "Emacs Buffer")
    ((eq major-mode 'dired-mode)
     "Dired")
    (t "User Buffer"))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

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

(global-set-key (kbd "C-c C-c C-c") 'compile)

;; Only prompt for a compile command if a prefix argument is given.
(setq compilation-read-command nil)

;; Ido mode for magic autocompleting.
(ido-mode)

;; Ido mode in M-x.
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; Coding style preferences.
(require 'custom-google-coding-style)

;; set indent style to 'k&r' style for everything but java
;; http://en.wikipedia.org/wiki/Indent_style
(setq c-default-style
      '((java-mode . "java") (other . "k&r")))

;; Enable backup files.
(setq make-backup-files t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Put autosave files (i.e., #foo#) in ~/.emacs_autosaves.
(defvar autosave-dir (concat "~/.emacs_autosaves/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Fill mode for text mode by default
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Fill mode by default for most major modes.
(setq-default auto-fill-function 'do-auto-fill)
(add-hook 'sh-mode-hook (lambda () (auto-fill-mode nil)))
(add-hook 'go-mode-hook (lambda () (auto-fill-mode nil)))

;; Make Text mode the default mode for new buffers
(setq default-major-mode 'text-mode)

;; Periodically reload buffers from disk.
(global-auto-revert-mode)

;; Enable time-stamp updating on save.
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S")  ; date format
(add-hook 'write-file-hooks 'time-stamp)             ; update when saving

;; Disable splash screen.
(setq inhibit-splash-screen t)
;; And startup message.
(eval-after-load "startup" '(fset
                             'display-startup-echo-area-message (lambda ())))

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
(use-package matlab
  :ensure matlab-mode
  :defer t
  :init
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

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

;; Use 72-column fills in mail mode.
(add-hook 'mail-mode-hook
          (lambda () (set-fill-column 72)))
(add-hook 'mail-mode-hook
          '(lambda () (font-lock-set-up-width-warning 72)))

;; Dart looks a little like C++.
(add-to-list 'auto-mode-alist '("\\.dart$" . c++-mode))

;; Markdown major mode.
(use-package markdown-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; reStructuredText major mode.
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

;; Arduino code is C++.
(add-to-list 'auto-mode-alist '("\\.pde$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))

;; Protocol buffer definitions are best edited as C.
(add-to-list 'auto-mode-alist '("\\.proto$" . c-mode))

;; BUILD files in python-mode.
(add-to-list 'auto-mode-alist '("\/BUILD$" . python-mode))

;; TypeScript files in javascript-mode.
(add-to-list 'auto-mode-alist '("\\.ts$" . javascript-mode))

;; SCons files in python-mode.
(add-to-list 'auto-mode-alist '("SConstruct$" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript$" . python-mode))

;; Diff mode settings.
(setq diff-default-read-only t)  ;; open diffs in RO mode

;; Goto-last-change. Bound to 'C-x \'.
(use-package goto-last-change
  :defer 1
  :config
  (global-set-key (kbd "C-x \\") 'goto-last-change))

;; For finding and updating TAGS files.
(use-package etags-table
  :defer 2
  :config
  (setq etags-table-search-up-depth 10))

(use-package ctags-update
  :defer 2
  :config
  (add-hook 'c-mode-common-hook 'turn-on-ctags-auto-update-mode))

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

;; Language-cpecific indentation tweaks.
;; https://dougie.io/emacs/indentation/
(setq-default css-indent-offset tab-width)     ;; CSS
(setq-default python-indent-offset tab-width)  ;; Python
(setq-default js-indent-level tab-width)       ;; Javascript

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:background "purple"))))
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "#989973"))))
 '(sml/col-number ((t (:foreground "#9A75A6"))))
 '(sml/git ((t (:foreground "#CC6666"))))
 '(sml/line-number ((t (:foreground "#9A75A6" :weight bold))))
 '(sml/minor-modes ((t (:foreground "#81A2BE"))))
 '(sml/modes ((t (:foreground "#5F819D" :weight normal))))
 '(sml/numbers-separator ((t (:foreground "#aaa"))))
 '(sml/position-percentage ((t (:foreground "#B294BB" :weight normal))))
 '(tabbar-button ((t (:background "#202020" :foreground "#202020"))))
 '(tabbar-default ((t (:background "#202020" :foreground "white"))))
 '(tabbar-selected ((t (:background "#222222" :foreground "Pink"))))
 '(trailing-whitespace ((t (:background "#aaaaaa")))))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1024 1024))
