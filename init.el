
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
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
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
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(c-tab-always-indent t)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "dbb643699e18b5691a8baff34c29d709a3ff9787f09cdae58d3c1bc085b63c25" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" default)))
 '(fci-rule-color "#3E3D31")
 '(flycheck-c/c++-gcc-executable "gcc-6")
 '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint")
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy jade javascript-eslint javascript-jshint javascript-gjslint javascript-jscs javascript-standard json-jsonlint json-python-json less lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-mdl rst-sphinx rst ruby-rubocop ruby-rubylint ruby ruby-jruby rust-cargo rust sass scala scala-scalastyle scss-lint scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby c/c++-googlelint)))
 '(flycheck-clang-include-path
   (quote
    ("/Users/jorgenvalstad/Dropbox/Datateknikk/9_semester/TDT4295/MCU/ytelse/usbhost/src/")))
 '(flycheck-gcc-include-path
   (quote
    ("/usr/local/Cellar/libusb/1.0.20/include/libusb-1.0/")))
 '(flycheck-gcc-includes
   (quote
    ("/usr/local/Cellar/libusb/1.0.20/include/libusb-1.0/libusb.h")))
 '(flycheck-gcc-language-standard "c11")
 '(flycheck-gcc-openmp t)
 '(flycheck-gcc-pedantic t)
 '(flycheck-gcc-pedantic-errors t)
 '(flycheck-googlelint-filter
   "-legal/copyright, -readability/casting, -readability/todo, -runtime/threadsafe_fn")
 '(flycheck-googlelint-verbose 5)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(hl-sexp-background-color "#efebe9")
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\\\.DS_Store")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (which-key opencl-mode spacemacs-theme ggtags moe-theme cuda-mode writeroom-mode neotree git-gutter zenburn-theme xclip whole-line-or-region web-mode use-package undo-tree unbound twig-mode theme-changer super-save smooth-scrolling smooth-scroll smex slime relative-line-numbers php-auto-yasnippets openwith move-text monokai-theme material-theme markdown-preview-mode magit linum-relative leuven-theme iedit ido-vertical-mode hippie-expand-slime guide-key goto-chg google-c-style golden-ratio god-mode gitconfig-mode flymake-google-cpplint flymake-cursor flycheck-google-cpplint flx-ido fill-column-indicator django-snippets django-mode company-web company-php company-c-headers company-anaconda cmake-mode bison-mode autopair auctex aggressive-indent ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(same-window-buffer-names (quote ("\"*compile*\"")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))




;; ------------------------------------------------------------
;; Python
;; ------------------------------------------------------------

;; Ipython as default interpreter
(defvar python-shell-interpreter "ipython")

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
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "C-c c c SPC") 'ace-jump-line-mode)

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
     (flycheck-add-next-checker 'c/c++-cppcheck
                                '(warning . c/c++-googlelint))))

(setq-default flycheck-disabled-checkers '(c/c++-clang))

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
;; (set-face-background 'hl-line "#3e4446")
;; Nice hl-line for white background #ffb3a5
;; (require 'color)
;; (defun set-hl-line-color-based-on-theme ()
;;   "Sets the hl-line face to have no foregorund and a background
;;     that is 10% darker than the default face's background."
;;   (set-face-attribute 'hl-line nil
;;                       :foreground nil
;;                       :background (color-darken-name (face-background 'default) 10)))
;; (add-hook 'global-hl-line-mode-hook 'set-hl-line-color-based-on-theme)

;; ------------------------------------------------------------
;; expand-region
;; ------------------------------------------------------------
(use-package expand-region
  :ensure t
  :defer t)
(global-set-key (kbd "C-i") 'er/expand-region)

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
  :config (progn
	    (global-set-key "\C-s" 'swiper)))

;; ------------------------------------------------------------
;; Misc
;; ------------------------------------------------------------

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
(menu-bar-mode 0)

;; (use-package golden-ratio
;;   :ensure t
;;   :defer t)
;; (golden-ratio-mode 1)

(use-package desktop
  :ensure t
  :defer t)
;; Save desktop
(desktop-save-mode 1)
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
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
  :ensure t)
(yas-global-mode 1)

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

;; Recenter the screen when up or down is hit
;; (setq recenter-redisplay nil)
;; (global-set-key (kbd "<up>") (lambda () (interactive) (previous-line) (scroll-down-line)))
;; (global-set-key (kbd "<down>") (lambda () (interactive) (next-line) (scroll-up-line)))
;; Might not be as useful as I thought.

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
(global-set-key (kbd "<left>")  'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>")    'windmove-up)
(global-set-key (kbd "<down>")  'windmove-down)
;; This also is not particularly useful.. The arrow keys are hard to reach

;; Bind C-x C-k to kill buffer
(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)

;; Use ibuffer when listing all buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use hippie expand instead of dabbrev expand
;;(global-set-key (kbd "M-/") 'hippie-expand)

;; Highlight parenthesis
(show-paren-mode 1)

;; Set font (only works in GUI)
(set-face-attribute 'default nil :family "source code pro" :height 110)

(load-theme 'monokai)

;; (defvar calendar-location-name "Trondheim, Sør-Trøndelag")
;; (defvar calendar-longitude 10.4)
;; (defvar calendar-latitude 63.4)
;; (add-to-list 'load-path "~/.emacs.d/elpa/theme-changer-20130725.1919/")
;; (use-package theme-changer
;;   :ensure t)
;; Change color based on window system and time of day
;; (defun change_theme_if_no_theme()
;;   (if (display-graphic-p)
;;       (change-theme 'material-light 'monokai)
;;     (load-theme 'monokai)))
;; ;; FIX: This line needs to do the inverse of what it's doing now
;; (add-hook 'desktop-after-read-hook 'change_theme_if_no_theme)

;; ;; Helper for compilation. Close the compilation window if
;; ;; there was no error at all.
;; (defun compilation-exit-autoclose (status code msg)
;;   ;; If M-x compile exists with a 0
;;   (when (and (eq status 'exit) (zerop code))
;;     ;; then bury the *compilation* buffer, so that C-x b doesn't go there
;;     (bury-buffer)
;;     ;; and delete the *compilation* window
;;     (delete-window (get-buffer-window (get-buffer "*compilation*"))))
;;   ;; Always return the anticipated result of compilation-exit-message-function
;;   (cons msg code))
;; ;; Specify my function (maybe I should have done a lambda function)
;; (setq compilation-exit-message-function 'compilation-exit-autoclose)



(provide 'init.el)
;;; init.el ends here
