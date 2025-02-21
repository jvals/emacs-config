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
;; (package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


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
;; Python
;; ------------------------------------------------------------

;; Ipython as default interpreter
(setq python-shell-interpreter "ipython"
  python-shell-interpreter-args "--simple-prompt -i")
;; (defvar python-shell-interpreter "python3")

(use-package company
  :ensure t
  :bind ("C-;" . company-complete-common)
  :config
  (setq company-idle-delay 0.1))

;; Global company mode
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
  '(progn
  (add-to-list 'company-backends '(company-dabbrev-code company-gtags company-clang company-etags company-keywords))))
  ;; (add-to-list 'company-backends '(company-dabbrev-code company-gtags company-clang company-etags company-keywords company-abbrev company-dabbrev))))

(setq company-dabbrev-downcase nil)


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
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

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
(global-set-key (kbd "C-c j") 'ace-jump-word-mode)
(global-set-key (kbd "C-c c") 'ace-jump-char-mode)
(global-set-key (kbd "C-c l") 'ace-jump-line-mode)

;; ------------------------------------------------------------
;; Flycheck
;; ------------------------------------------------------------

;; (require 'flycheck)
(use-package flycheck
  :ensure t
  :defer t
  :config (setq flycheck-python-pycompile-executable "python3"))



(add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq-default flycheck-disabled-checkers '(c/c++-clang))
;; (setq-default flycheck-c/c++-gcc-executable "/usr/local/bin/gcc-7")
;; (setq-default flycheck-gcc-openmp 1)
;; (setq-default flycheck-gcc-pedantic 1)
;; (setq-default flycheck-gcc-warnings '("all"))
;; (setq-default flycheck-gcc-language-standard "c11")

;; (use-package flycheck-google-cpplint
;;   :ensure t
;;   :defer t)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-google-cpplint)
;;      (flycheck-add-next-checker 'c/c++-gcc
;;                                 '(warning . c/c++-googlelint))))

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
;; (global-set-key (kbd "<escape>") 'god-mode-all)
;; (global-set-key (kbd "^[ godmode") 'god-mode-all) ;; magic word set in iterm2 settings
;; (define-key god-local-mode-map (kbd ".") 'repeat)
;; (define-key god-local-mode-map (kbd "z") 'repeat)

;; ------------------------------------------------------------
;; Magit
;; ------------------------------------------------------------

(use-package magit
  :ensure t
  :defer t)
(global-set-key (kbd "C-x g") 'magit-status)

;; Use standard git path
(setq magit-git-executable "/usr/bin/git")

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
  :defer t
  :bind ("C-'" . er/expand-region))
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
  :bind (("C-s" . swiper))
  :config (set-face-attribute 'swiper-match-face-2 nil :background "DarkOrchid4"))

;; ------------------------------------------------------------
;; Transparrency
;; ------------------------------------------------------------
(defun set-transparrency (value)
  "Set the transparrency of the frame window."
  (interactive "nSet the transparrency value 0 - 100: ")
  (set-frame-parameter (selected-frame) 'alpha value))
(global-set-key (kbd "C-c C-t") 'set-transparrency)

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
;; multiple-cursors
;; ------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-7" . mc/edit-lines)))


;; ------------------------------------------------------------
;; kill other buffers
;; ------------------------------------------------------------
(defun only-current-buffer ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))


;; ------------------------------------------------------------
;; company try hard
;; ------------------------------------------------------------
(use-package company-try-hard
  :ensure t)

;; ------------------------------------------------------------
;; my split function (modified split-window-sensibly)
;; ------------------------------------------------------------
(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it vertically disregarding
             ;; the value of `split-height-threshold'.
             (let ((split-height-threshold 0))
               (when (window-splittable-p window)
                 (with-selected-window window
                   (split-window-below))))))))

(setq split-window-preferred-function #'my-split-window-sensibly)


;; ------------------------------------------------------------
;; Web-mode
;; ------------------------------------------------------------
(use-package web-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; ------------------------------------------------------------
;; Scala-mode
;; ------------------------------------------------------------
(use-package scala-mode
  :ensure t)

;; ------------------------------------------------------------
;; LSP
;; ------------------------------------------------------------
(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook #'lsp))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp :ensure t)

;; ------------------------------------------------------------
;; eshell
;; ------------------------------------------------------------
(defun shell-history ()
  (interactive)
  (let ((command (with-temp-buffer
                   (insert-file-contents-literally "~/.histfile")
                   (let ((history-list (reverse(split-string (buffer-string) "\n" t))))
                     (ivy-read "Command: " history-list)))))
    (when command
      (insert command))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "M-r") 'shell-history)))

;; ------------------------------------------------------------
;; Vterm
;; ------------------------------------------------------------
;; (require 'vterm)
;; (define-key vterm-mode-map (kbd "C-c C-y") 'vterm-yank)

;; ------------------------------------------------------------
;; Misc
;; ------------------------------------------------------------

;; Redirect customize to separate file
(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file :noerror)

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
(bind-key* "<C-left>" 'winner-undo)
(bind-key* "<C-right>" 'winner-redo)

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
;; Current line shows the absolute line number
(setq linum-relative-current-symbol "")

;; Set scroll step to 1
(setq scroll-step 1)

;; Tramp
(defvar tramp-default-method "ssh")

;; Deactivate menubar
(if (display-graphic-p) nil
  (menu-bar-mode 0))
(menu-bar-mode 0)

;; (use-package golden-ratio
;;   :ensure t
;;   :defer t)
;; (golden-ratio-mode 1)

(use-package desktop
  :ensure t)
;; Save desktop
(desktop-save-mode 0)
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display nil)
(setq desktop-restore-forces-onscreen nil)

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


;; Set shell
(cond
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (defvar explicit-shell-file-name "/usr/local/bin/zsh")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (defvar explicit-shell-file-name "/bin/bash"))))


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
(defun sswindow()
  (interactive)
  (other-window -1 t)
  (select-frame-set-input-focus (selected-frame)))
(if (display-graphic-p)
    (progn
      (keyboard-translate ?\C-i ?\H-i)
      (bind-key* "H-i" 'sswindow)))


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
;; (global-set-key (kbd "<left>")  'windmove-left)
;; (global-set-key (kbd "<right>") 'windmove-right)
;; (global-set-key (kbd "<up>")    'windmove-up)
;; (global-set-key (kbd "<down>")  'windmove-down)


;; Bind C-x C-k to kill buffer
(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)

;; Use ibuffer when listing all buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Highlight parenthesis
(show-paren-mode 1)

;; Set font (only works in GUI)
(when window-system
(cond
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (if (> (x-display-pixel-width) 1680)
        (set-face-attribute 'default nil :family "source code pro" :height 100)
      (set-face-attribute 'default nil :family "source code pro" :height 100))))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (set-face-attribute 'default nil :family "Source Code Pro" :height 100))))
    ;; (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 75))))
)
;; (cond
;;  ((string-equal system-type "gnu/linux")
(progn
  (setq monokai-background "#1c1c1c")
  (setq monokai-256-background "#1c1c1c"))

(load-theme 'monokai t)

;; Region color
(set-face-attribute 'region nil :background "dark olive green")

(use-package glsl-mode
  :ensure t)

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;; might mess with grep
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Scons
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))
;; hs-minor-mode
(add-hook 'c++-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c f t") 'hs-toggle-hiding)   ;; code fold toggle
(global-set-key (kbd "C-c f s a") 'hs-show-all)      ;; code fold show all
(global-set-key (kbd "C-c f h a") 'hs-hide-all)      ;; code fold hide all
(global-set-key (kbd "C-c f h l") 'hs-hide-level)    ;; code fold hide level
(global-set-key (kbd "C-c f h b") 'hs-hide-block)    ;; code fold hide block
(global-set-key (kbd "C-c f s b") 'hs-show-block)    ;; code fold show block

;; pop tags
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; markerpen
;; (add-to-list 'load-path "~/.emacs.d/markerpen")
;; (load-library "markerpen")

;; (defun markred ()
;;   (interactive)
;;   (markerpen-mark-region 1))

;; (defun markyellow ()
;;   (interactive)
;;   (markerpen-mark-region 3))

;; (defun markblue ()
;;   (interactive)
;;   (markerpen-mark-region 4))

;; (defun markgreen ()
;;   (interactive)
;;   (markerpen-mark-region 11))

;; (defun markclear ()
;;   (interactive)
;;   (markerpen-clear-region))

;; Display time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode 1)

;; Tab width
(setq-default tab-width 2)
(setq-default c-basic-offset 2
							tab-width 2
							indent-tabs-mode nil)
(c-set-offset 'substatement-open 0)
(c-set-offset 'cpp-macro 0)

;; Smart-dash
;; (add-to-list 'load-path "~/.emacs.d/smart-dash")
;; (require 'smart-dash)
;; (add-hook 'c++-mode-hook 'smart-dash-mode)

;; Resize windows
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (global-set-key (kbd "C-S-;") 'shrink-window-horizontally)
    (global-set-key (kbd "C-S-#") 'enlarge-window-horizontally)
    (global-set-key (kbd "C-S-[") 'shrink-window)
    (global-set-key (kbd "C-S-'") 'enlarge-window))))

;; Move to line
(global-set-key (kbd "M-g") 'goto-line)

;; Remove trailing whitespace in c mode on save
(add-hook 'c-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Markdown mode
(use-package markdown-mode
  :ensure t)

;; Indent html
(setq sgml-basic-offset 2)

;; C comment style
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end "")))

;; open this file
(defun open-init ()
    (interactive)
    (find-file user-init-file))

;; Enable upcase-region
(put 'upcase-region 'disabled nil)

;; Restart emacs
(use-package restart-emacs
  :ensure t)

;; Keybinding for bury-buffer
(global-set-key (kbd "C-c q") 'bury-buffer)

;; Overwrite regions
(delete-selection-mode 1)

;; join lines
(defun join-lines () (interactive) (let ((fill-column 999999)) (fill-paragraph nil)))

;; ignore escape key
;; (global-set-key (kbd "<escape>") #'ignore)

;; Fix environment on macos so that GUI emacs inherits the path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; Set dired size to human readable
(setq dired-listing-switches "-alh")

(use-package yaml-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

;; start server
(server-start)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(provide 'init.el)
;;; init.el ends here
(put 'erase-buffer 'disabled nil)
