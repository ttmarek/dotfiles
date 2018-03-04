;;; Commentary:

(require 'package)

;; --------------------------------------------------
;; Installing all necessary fonts
;; 1. https://github.com/domtronn/all-the-icons.el#installing-fonts
;; 2. https://github.com/source-foundry/Hack#package-managers

;; --------------------------------------------------
;; Highlight the current line
(global-hl-line-mode +1)
;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Hollow cursor
(setq-default cursor-type 'hollow)
;; No tabs
(setq-default indent-tabs-mode nil)
;; Set folder for emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs-backup")))
;; Maximize the emacs window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Line Spacing
(setq-default line-spacing 10)
;; Don't show startup screen
(setq inhibit-startup-screen t)
;; Highlight matching parens
(show-paren-mode 1)
;; Enable line numbers
(global-linum-mode t)
;; Hide the toolbar
(tool-bar-mode -1)
;; Hide the scrollbar
(toggle-scroll-bar -1)
;; Hide the menubar
(menu-bar-mode -1)
;; Set font face and size
(defun font-exists (font)
  "Check if FONT exists."
  (if (null (x-list-fonts font)) nil t))

(if (font-exists "hack")
    (set-face-attribute 'default nil :font "hack" :height 140)
  (if (font-exists "inconsolata")
      (set-face-attribute 'default nil :font "hack" :height 150)))

;; Set path to ispell
(setq ispell-program-name "/usr/local/bin/ispell")

;; Packages
;; --------------------------------------------------
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Activate all the packages
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package css-mode
  :init
  (setq css-indent-offset 2))

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-on))

(use-package projectile
  :ensure t
  :init
  (projectile-mode))

(use-package term
  :init
  (setq term-scroll-to-bottom-on-output t)
  (setq term-suppress-hard-newline t))

(use-package org-mode
  :init
  (setq org-startup-folded 'showall))

(use-package less-css-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :init
  (defun ts-mode-hooks ()
    "Hooks for typescript-mode"
    (setq truncate-lines t)
    (setq hscroll-margin 0)
    (setq hscroll-step 1)
    (setq typescript-indent-level 2))
  (add-hook 'typescript-mode-hook 'ts-mode-hooks))

(use-package thesaurus
  :ensure t
  :config
  (setq thesaurus-bhl-api-key "65e09b66ad5c7c6dfc204d58e122e5da"))

(use-package bufshow
  :ensure t
  :bind
  ([right] . bufshow-next)
  ([left] . bufshow-prev))

(use-package zerodark-theme
  :ensure t
  :config
  (load-theme 'zerodark t))

(use-package all-the-icons
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x l" . magit-log-all))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package smex :ensure t)

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(use-package smooth-scrolling
  :ensure t
  :config
  (setq scroll-conservatively 10)
  (setq scroll-margin 5))

(use-package multi-term
  :ensure t
  :bind
  ("C-x t" . multi-term))

(use-package prettier-js
  :ensure t)

(use-package add-node-modules-path
  :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.js\\'"
  :init
  (defun web-mode-hooks ()
    "Hooks for web-mode"
    (add-node-modules-path)
    (prettier-js-mode)
    (setq web-mode-content-type "jsx")
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq truncate-lines t)
    (setq hscroll-margin 0)
    (setq hscroll-step 1))
  (add-hook 'web-mode-hook 'web-mode-hooks)
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

(use-package flycheck
  :ensure t
  :init
  (setq-default flycheck-indication-mode nil)
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (global-flycheck-mode))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode)
         ("\\.babelrc\\'" . json-mode))
  :init
  (defun json-mode-hooks ()
    "Hooks for json-mode"
    (make-local-variable 'js-indent-level)
    (setq truncate-lines t)
    (setq hscroll-margin 0)
    (setq hscroll-step 1)
    (setq js-indent-level 2))
  (add-hook 'json-mode-hook 'json-mode-hooks))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :init
  (defun markdown-mode-hooks()
    "Hooks for markdown mode"
    (setq fill-column 80)
    (setq left-margin-width 5)
    (setq right-margin-width 5)
    (linum-mode -1)
    (visual-line-mode))
  (add-hook 'markdown-mode-hook 'markdown-mode-hooks))

;; Custom Functions
;; --------------------------------------------------
(defun presenting-increase-font ()
  "Increase font size for making presentations"
  (interactive)
  (set-face-attribute 'default nil :height 250))

(defun presenting-reset-font ()
  "Reset font size after making presentations"
  (interactive)
  (set-face-attribute 'default nil :font "inconsolata" :height 150))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun copy-file-path()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode use-package undo-tree typescript-mode thesaurus solarized-theme smex smartparens scss-mode rjsx-mode rainbow-mode multiple-cursors markdown-mode magit less-css-mode json-mode helm-mt flow-minor-mode doom-themes counsel-projectile color-theme-solarized centered-window-mode bufshow aurora-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
