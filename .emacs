(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(TeX-DVI-via-PDFTeX t)
 '(TeX-PDF-mode t)
 '(TeX-view-style (quote (("^a4\\(?:dutch\\|paper\\|wide\\)?\\|sem-a4$" "%(o?)kdvi %dS -paper a4 %d") ("^a5\\(?:comb\\|paper\\)?$" "%(o?)kdvi %dS -paper a5 %d") ("^b5paper$" "%(o?)kdvi %dS -paper b5 %d") ("^letterpaper$" "%(o?)kdvi %dS -paper us %d") ("^legalpaper$" "%(o?)kdvi %dS -paper legal %d") ("^executivepaper$" "%(o?)kdvi %dS -paper 7.25x10.5in %d") ("^landscape$" "%(o?)kdvi %dS -paper a4r -s 0 %d") ("." "%(o?)kdvi %dS %d"))))
 '(browse-url-netscape-program "opera")
 '(browse-url-xterm-program "term.sh")
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(delete-old-versions t)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(show-paren-mode t nil (paren))
 '(swbuff-clear-delay 3)
 '(swbuff-status-window-layout (quote adjust))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode t nil (tool-bar))
 '(uniquify-buffer-name-style nil nil (uniquify)))

(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(cursor ((t (:background "white"))))
 '(font-latex-doctex-documentation-face ((t (:background "#eeeeee"))))
 '(font-latex-doctex-preprocessor-face ((t (:inherit (list font-latex-doctex-documentation-face bold)))))
 '(font-latex-sedate-face ((((class color) (background light)) (:foreground "cyan"))))
 '(show-paren-match-face ((((class color)) (:underline t))))
 '(widget-inactive-face ((((class grayscale color) (background dark)) (:foreground "gray")))))

(require 'cc-mode)
;;(defun my-build-tab-stop-list (width)
;;  (let ((num-tab-stops (/ 80 width))
;;	(counter 1)
;;	(ls nil))
;;    (while (<= counter num-tab-stops)
;;      (setq ls (cons (* width counter) ls))
;;      (setq counter (1+ counter)))
;;    (set (make-local-variable 'tab-stop-list) (nreverse ls))))
(defun my-c-mode-common-hook ()
  (setq tab-width 4)
;;  (my-build-tab-stop-list tab-width)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil)) ;; force only spaces for indentation
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; line by line scrolling
(setq scroll-step 1)

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

;; ===== Make Text mode the default mode for new buffers =====
(setq default-major-mode 'text-mode)

;; To include .reminders as a shell mode 
  (setq auto-mode-alist
        (cons '(".reminders$" . shell-script-mode) auto-mode-alist))

(add-hook 'sh-mode-hook
     (lambda ()
       (auto-fill-mode nil)))

;; TeX stuff
;; C-c C-f to compile, C-c C-v to view
(setq tex-dvi-view-command "kdvi")
;(load "/usr/share/emacs/site-lisp/site-gentoo")
;(setq TeX-auto-save t)
;(setq TeX-parse-self t)

(setq load-path  (cons (expand-file-name "~/.emacs-lisp/") load-path))

(require 'swbuff-x)
(global-set-key (kbd "ESC O c") 'swbuff-switch-to-next-buffer) ; ctrl-left
(global-set-key (kbd "ESC O d")   'swbuff-switch-to-previous-buffer) ; ctrl-right
(global-set-key (kbd "ESC [ 3 ^") 'swbuff-kill-this-buffer) ; ctrl-delete
; makes the buffers be displayed while cycling
(setq swbuff-display-intermediate-buffers t)