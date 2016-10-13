(require 'package)

;; --------------------------------------------------
;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)
;; No tabs
(setq-default indent-tabs-mode nil)
;; Set folder for emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs-backup")))
;; Maximize the emacs window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Line Spacing
(setq-default line-spacing 7)
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
  "check if font exists"
  (if (null (x-list-fonts font)) nil t))

(if (font-exists "inconsolata")
    (set-face-attribute 'default nil :font "inconsolata" :height 140))

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

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package smex :ensure t)

(use-package multi-term
  :ensure t
  :bind
  ("C-x t" . multi-term))

(use-package js2-mode
  :ensure t
  :init
  (defun js2-mode-hooks ()
    (setq js2-global-externs
          '("describe"
            "it"
            "expect"
            "beforeEach"
            "afterEach"
            "jest"))
    (setq js2-strict-trailing-comma-warning nil)
    (setq js2-include-node-externs t))
  (add-hook 'js2-minor-mode-hook 'js2-mode-hooks))

(use-package web-mode
  :ensure t
  :mode "\\.js\\'"
  :init
  (defun web-mode-hooks ()
    "Hooks for web-mode"
    (js2-minor-mode)
    (setq web-mode-content-type "jsx")
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2))
  (add-hook 'web-mode-hook 'web-mode-hooks))

(use-package json-mode
  :ensure t
  :init
  (defun json-mode-hooks ()
    "Hooks for json-mode"
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))
  (add-hook 'json-mode-hook 'json-mode-hooks))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; Custom Functions
;; --------------------------------------------------
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
