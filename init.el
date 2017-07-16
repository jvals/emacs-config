;;; package --- Summary
;;; Commentary:
;;; Code:

;; Enable mouse mode, so you can use the mouse if need be
;; (xterm-mouse-mode)

;; Change the backup directory so your local dir doesn't get cluttered
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq vc-make-backup-files t)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq auto-save-default t)
(setq delete-old-versions t)
(setq kept-new-versions 10)
(setq kept-old-versions 10)
(setq version-control t)

(defun force-backup-of-buffer ()
  "Make a special 'per session' backup at the first save of each Emacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; Enable line numbers
;;(global-linum-mode t)


;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ------------------------------------------------------------
;; Fix environment
;; ------------------------------------------------------------
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ------------------------------------------------------------
;; Themes
;; ------------------------------------------------------------
(use-package monokai-theme
  :ensure t
  :defer t)

(use-package material-theme
  :ensure t
  :defer t)


;; ------------------------------------------------------------
;; Not my shit
;; ------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(swiper-match-face-2 ((t (:background "DarkOrchid4")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asm-comment-char 35)
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(package-hidden-regexps (quote ("\\`@available")))
 '(package-selected-packages
   (quote
    (vue-mode projectile-mode yaml-mode projectile try org-bullets expand-region all-the-icons xclip whole-line-or-region unbound twig-mode super-save smooth-scroll smex rainbow-mode php-auto-yasnippets openwith opencl-mode linum-relative ido-vertical-mode hc-zenburn-theme guide-key goto-chg google-c-style golden-ratio god-mode ggtags flymake-google-cpplint flymake-cursor flycheck-google-cpplint flx-ido fill-column-indicator exec-path-from-shell cuda-mode company-web company-php company-c-headers company-anaconda cmake-mode bison-mode autopair auctex aggressive-indent ace-jump-mode))))




;; ------------------------------------------------------------
;; Python
;; ------------------------------------------------------------

;; Ipython as default interpreter
;; (defvar python-shell-interpreter "ipython")
(defvar python-shell-interpreter "python3")


(use-package company
  :ensure t
  :defer t)
;; Activate anaconda, eldoc and company for python
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'company-mode)


;; Add anaconda to company
(eval-after-load "company"
 '(progn
    (add-to-list 'company-backends 'company-anaconda)
    (add-to-list 'company-backends 'company-c-headers)))

;; Global company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Company completion
;;(global-set-key "\t" 'company-complete-common)
(setq company-idle-delay 0.1)

;; ------------------------------------------------------------
;; C-mode
;; ------------------------------------------------------------

;; (require 'cc-mode)
;; (eval-after-load 'cc-mode
;;   (setq-default c-basic-offset 4
;; 		indent-tabs-mode nil))


;; ------------------------------------------------------------
;; Projectile
;; ------------------------------------------------------------
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

;; ------------------------------------------------------------
;; IDO
;; ------------------------------------------------------------
(use-package ido-vertical-mode
  :ensure t
  :defer t)
;; Activate ido-mode
(ido-mode t)
(setq ido-everywhere t)
;; Fuzzy matching in ido-mode
(use-package flx-ido
  :ensure t
  :defer t)
(flx-ido-mode 1)
;; Vertical ido-mode
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; ------------------------------------------------------------
;; Ace Jump
;; ------------------------------------------------------------
(use-package ace-jump-mode
  :ensure t
  :defer t)
;; (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
;; (global-set-key (kbd "C-c c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "M-s") 'ace-jump-char-mode)
;; (global-set-key (kbd "C-c c c SPC") 'ace-jump-line-mode)

;; ------------------------------------------------------------
;; Flycheck
;; ------------------------------------------------------------

;; (require 'flycheck)
(use-package flycheck
  :ensure t
  :defer t)
(use-package flycheck-google-cpplint
  :ensure t
  :defer t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     (flycheck-add-next-checker 'c/c++-gcc
                                '(warning . c/c++-googlelint))))

;;(setq-default flycheck-disabled-checkers '(c/c++-clang))

;; ------------------------------------------------------------
;; OpenWith
;; ------------------------------------------------------------

(use-package openwith
  :ensure t
  :defer t)
(openwith-mode t)
;; 'open' is a built-in command on osx, which opens a file with the default program
(setq openwith-associations '(("\\.pdf\\'" "open -a /Applications/Skim.app " (file))))

;; ------------------------------------------------------------
;; God mode
;; ------------------------------------------------------------

(use-package god-mode
  :ensure t)
  ;;:defer t)
;; Doesn't really work in iterm
(global-set-key (kbd "<escape>") 'god-mode-all)
;; (global-set-key (kbd "^[ godmode") 'god-mode-all) ;; magic word set in iterm2 settings
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "z") 'repeat)

;; ------------------------------------------------------------
;; Magit
;; ------------------------------------------------------------

(use-package magit
  :ensure t
  :defer t)
(global-set-key (kbd "C-x g") 'magit-status)

;; ------------------------------------------------------------
;; Neotree
;; ------------------------------------------------------------
(use-package neotree
  :ensure t
  :defer t)
(global-set-key [f8] 'neotree-toggle)
;; When the neotree window is opened, find current file and jump to node.
(setq neo-smart-open t)

;; ------------------------------------------------------------
;; All The Icons
;; ------------------------------------------------------------
(use-package all-the-icons
  :ensure t)

;; ------------------------------------------------------------
;; hl-line
;; ------------------------------------------------------------
;; Highlight current line
(use-package hl-line
  :ensure t
  :defer t)
(if (display-graphic-p)
    (global-hl-line-mode 1))

;; ------------------------------------------------------------
;; expand-region
;; ------------------------------------------------------------
(use-package expand-region
  :ensure t
  :defer t)
(global-set-key (kbd "C-æ") 'er/expand-region)

;; ------------------------------------------------------------
;; which-key
;; ------------------------------------------------------------
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; ------------------------------------------------------------
;; org-mode
;; ------------------------------------------------------------
(use-package org-bullets
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ------------------------------------------------------------
;; Try
;; ------------------------------------------------------------
(use-package try
  :ensure t
  :defer t)

;; ------------------------------------------------------------
;; Swiper
;; ------------------------------------------------------------
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;; ------------------------------------------------------------
;; Transparency
;; ------------------------------------------------------------
(defun set-transparency (value)
  "Set the transparency of the frame window."
  (interactive "nSet the transparency value 0 - 100: ")
  (set-frame-parameter (selected-frame) 'alpha value))
(global-set-key (kbd "C-c C-t") 'set-transparency)

;; ------------------------------------------------------------
;; Rainbow
;; ------------------------------------------------------------
(use-package rainbow-mode
  :ensure t
  :defer t)

;; ------------------------------------------------------------
;; clang-format
;; ------------------------------------------------------------
(use-package clang-format
  :ensure t
  :defer t
  :config
  (setq clang-format-style "google"))

;; ------------------------------------------------------------
;; Misc
;; ------------------------------------------------------------


(defun insert-time ()
  "Insert the current time."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%H:%M)")))
(global-set-key (kbd "C-c t") 'insert-time)


(defun insert-current-date ()
  "Insert the current date."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(global-set-key (kbd "C-c d") 'insert-current-date)


;; Kill this buffer instead of prompting
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; winner-mode remembers window configs
;; to switch, use C-c left and C-c right
(winner-mode 1)

;; No truncating lines
;; (set-default 'truncate-lines t)

;; Disable silent newline
;; (setq mode-require-final-newline 0)

;; workaround: ansi-term + yasnippet + tab = Wrong type argument: characterp, tab
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

;; Automatically update files that are changed externally
(global-auto-revert-mode 1)

;; Auto save files whenever focus is lost
(use-package super-save
  :ensure t
  :defer t)
(super-save-mode +1)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (auto-fill-mode 1)
                             (setq-default fci-rule-column 80)))


(bind-key* "C-c RET" 'compile)

;; Undo key
(global-set-key (kbd "C--") 'undo)

;; No scroll-bars
(scroll-bar-mode -1)

(use-package autopair
  :ensure t
  :defer t)
;; Pair up parenthesis and brackets
(autopair-global-mode 1)
;; Surround a region with parenthesis, brackets, etc.
(defvar autopair-autowrap t)

;; Commands that work on regions now work on the current line if there is no region
(use-package whole-line-or-region
  :ensure t
  :defer t)
(whole-line-or-region-mode 1)

;; Relative line numbers
;; (global-relative-line-numbers-mode)

;; Set scroll step to 1
(setq scroll-step 1)

;; Tramp
(defvar tramp-default-method "ssh")

;; Deactivate menubar
(if (display-graphic-p) nil
    (menu-bar-mode 0))

;; (use-package golden-ratio
;;   :ensure t
;;   :defer t)
;; (golden-ratio-mode 1)

;; (use-package desktop
;;   :ensure t
;;   :defer t)
;; ;; Save desktop
;; (desktop-save-mode 1)
;; (setq desktop-restore-frames t)
;; (setq desktop-restore-in-current-display t)
;; (setq desktop-restore-forces-onscreen nil)

;; Activate markdown-preview-mode
;; (add-hook 'markdown-mode-hook 'markdown-preview-mode)
;; Color theme for markdown preview
;;(defvar markdown-preview-style "http://thomasf.github.io/solarized-css/solarized-light.css")

;; Guide-key
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r"))
;; (guide-key-mode 1)
;; (setq guide-key/recursive-key-sequence-flag t)
;; (setq guide-key/popup-window-position 'bottom)

;; yasnippet
(use-package yasnippet
  :ensure t
  :defer t)
(add-hook 'after-init-hook (yas-global-mode 1))

;; Iedit
(use-package iedit
  :ensure t
  :defer t)
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; Flymake
;; (defun flymake_google_init ()
;; 	(require 'flymake-google-cpplint)
;; 	(custom-set-variables
;; 		'(flymake-google-cpplint-command "/usr/local/Cellar/python3/3.5.1/Frameworks/Python.framework/Versions/3.5/bin/cpplint"))
;; 	(flymake-google-cpplint-load)
;; )
;; (add-hook 'c-mode-hook 'flymake_google_init)
;; ; start google-c-style with emacs
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)

;; Set shell
(defvar explicit-shell-file-name "/usr/local/bin/bash")

;; Set right option key as meta, and left to normal osx option-key
;; These bindings only work in the cocoa/GUI version of emacs
(setq mac-right-option-modifier 'meta)
(setq mac-option-modifier nil)
(setq mac-right-command-modifier 'meta)

;; Switch windows with C-o
;; (global-set-key (kbd "C-o") 'other-window)
(use-package bind-key
  :ensure t
  :defer t)

;; swindow cycles windows on all frames.
(defun swindow()
  (interactive)
  (other-window 1 t)
  (select-frame-set-input-focus (selected-frame)))
(bind-key* "C-o" 'swindow)

;; Smex: Fuzzy Command pallette like sublime text
(use-package smex
  :ensure t
  :defer t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Column mode
(setq column-number-mode t)

;; Disable the Toolbar in GUI emacs
(tool-bar-mode -1)

;; Use arrow keys for window movement
;; (windmove-default-keybindings)
(global-set-key (kbd "<left>")  'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>")    'windmove-up)
(global-set-key (kbd "<down>")  'windmove-down)


;; Bind C-x C-k to kill buffer
(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)

;; Use ibuffer when listing all buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Highlight parenthesis
(show-paren-mode 1)

;; Set font (only works in GUI)
(set-face-attribute 'default nil :family "source code pro" :height 90)

(load-theme 'monokai)

;; Region color
(custom-set-faces '(region ((t (:inherit highlight :background "dark olive green")))))


(provide 'init.el)
;;; init.el ends here
