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
 '(indent-tabs-mode nil)
 '(inhibit-splash-screen t)
 '(large-file-warning-threshold nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(show-paren-mode t nil (paren))
 '(swbuff-clear-delay 3)
 '(swbuff-status-window-layout (quote adjust))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode t)
 '(uniquify-buffer-name-style nil nil (uniquify))
 '(xterm-mouse-mode t)
)

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
  (setq tab-width 2)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil))  ;; force only spaces for indentation
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(require 'linum)
(global-linum-mode)

(require 'tabbar)
(tabbar-mode 1)
(global-set-key (kbd "M-p") 'tabbar-backward)
(global-set-key (kbd "M-n") 'tabbar-forward)

(require 'smooth-scrolling)

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

;; fill columns
(setq-default fill-column 79)

;; fill mode for text mode by default
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Make Text mode the default mode for new buffers
(setq default-major-mode 'text-mode)

;; Show region after marking
(transient-mark-mode)

;; Display time
(setq display-time-24hr-format t)
(display-time)

(add-hook 'sh-mode-hook
     (lambda ()
       (auto-fill-mode nil)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "#989973")))))
