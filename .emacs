(add-to-list 'load-path "~/.emacs.d/lisp/")

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
 '(deft-extension "md")
 '(deft-text-mode (quote markdown-mode))
 '(delete-old-versions t)
 '(global-font-lock-mode t nil (font-lock))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(guess-style-guesser-alist (quote ((indent-tabs-mode . guess-style-guess-tabs-mode))))
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2)))
 '(require-final-newline t)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(swbuff-clear-delay 3)
 '(swbuff-status-window-layout (quote adjust))
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode t)
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(xterm-mouse-mode t))

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not (require 'git-commit-mode nil t))
  (package-install 'git-commit-mode))

;; Disable mouse wheel.
(setq mouse-wheel-mode nil)

(require 'cc-mode)
(defun my-c-mode-common-hook ()
  (setq c-basic-offset tab-width))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Go mode.
(require 'go-mode-load)

(require 'linum)
(global-linum-mode)

;; Tab bar.
(require 'tabbar)
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

;; Helper for compilation. Close the compilation window if there was no error at
;; all.
;; http://emacswiki.org/emacs/ModeCompile
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; Only prompt for a compile command if a prefix argument is given.
(setq compilation-read-command nil)

(require 'smooth-scrolling)

;; Enable deft for note-taking.
(require 'deft)

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

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Disable menu bar
(menu-bar-mode -1)

;; Fill columns
(setq-default fill-column 80)

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
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
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

;; Use 72-column fills in git-commit mode.
(add-hook 'git-commit-mode-hook
          (lambda () (set-fill-column 72)))
(add-hook 'git-commit-mode-hook
          '(lambda () (font-lock-set-up-width-warning 72)))

;; Use 72-column fills in mail mode.
(add-hook 'mail-mode-hook
          (lambda () (set-fill-column 72)))
(add-hook 'mail-mode-hook
          '(lambda () (font-lock-set-up-width-warning 72)))

;; Javascript major mode.
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; CSS major mode.
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; Dart looks a little like C++.
(add-to-list 'auto-mode-alist '("\\.dart$" . c++-mode))

;; Markdown major mode.
(autoload 'markdown-mode "markdown-mode.el" "Mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; reStructuredText major mode.
(autoload 'rst-mode "rst" nil t)
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

;; OpenSCAD files.
(autoload 'scad-mode "scad-mode" "Major mode for editing SCAD code." t)
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))

;; Diff mode settings.
(setq diff-default-read-only t)  ;; open diffs in RO mode

;; Goto-last-change. Bound to 'C-x \'.
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
(global-set-key (kbd "C-x \\") 'goto-last-change)

;; For finding and updating TAGS files.
(require 'etags-table)
(setq etags-table-search-up-depth 10)

(autoload 'turn-on-ctags-auto-update-mode
  "ctags-update" "turn on ctags-auto-update-mode'." t)
(add-hook 'c-mode-common-hook 'turn-on-ctags-auto-update-mode)

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
