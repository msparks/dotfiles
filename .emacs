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
 '(uniquify-buffer-name-style nil nil (uniquify)))

;;
;; Wombat theme ported from Vim
;;
(set-background-color "#242424")
(set-foreground-color "#d8d8d8")
(set-cursor-color "#656565")
(set-face-foreground 'font-lock-comment-face "#99968b")
(set-face-italic-p 'font-lock-comment-face t)
(set-face-foreground 'font-lock-doc-face "#99968b")
(set-face-italic-p 'font-lock-doc-face t)
(set-face-foreground 'font-lock-constant-face "#e5786d")
(set-face-foreground 'font-lock-string-face "#95e454")
(set-face-italic-p 'font-lock-string-face t)
(set-face-foreground 'font-lock-variable-name-face "#cae682")
(set-face-foreground 'font-lock-function-name-face "#cae682")
(set-face-foreground 'font-lock-type-face "#cae682")
(set-face-foreground 'font-lock-builtin-face "#8ac6f2")
(set-face-foreground 'font-lock-keyword-face "#8ac6f2")
(set-face-foreground 'font-lock-preprocessor-face "#e5786d")
(set-face-foreground 'font-lock-negation-char-face "#e7f6da")
(set-face-foreground 'link "#8ac6f2")
(set-face-bold-p 'link t)
(set-face-underline-p 'link t)
(set-face-foreground 'show-paren-match "#f6f3e8")
(set-face-background 'show-paren-match "#857b6f")
(set-face-bold-p 'show-paren-match t)
(set-face-foreground 'region "#f6f3e8")
(set-face-background 'region "#444444")
(set-face-foreground 'lazy-highlight "black")
(set-face-background 'lazy-highlight "yellow")
(set-face-background 'modeline "#505050")
(set-face-foreground 'modeline "#f0f0f0")

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
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "M-k") 'tabbar-forward)

(require 'smooth-scrolling)

(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(tabs trailing lines-tail empty))

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

;; To include .reminders as a shell mode
(setq auto-mode-alist
      (cons '(".reminders$" . shell-script-mode) auto-mode-alist))

(add-hook 'sh-mode-hook
     (lambda ()
       (auto-fill-mode nil)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "#989973")))))
