;; MELPA
;; http://melpa.org/#/getting-started (keep at the top of init)
;; -------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Activate all the packages
(package-initialize)

;; Fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Packages
;; -------------------------------------------------------------------
(setq to-install-if-needed '(helm
                             helm-mt
                             magit
                             json-mode
                             yaml-mode
                             scss-mode
                             markdown-mode
                             web-mode
                             aurora-theme
                             smartparens ))

;; Loop through the list of packages and install any if they haven't
;; been already
(dolist (pkg to-install-if-needed)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; RANDOM
;; -------------------------------------------------------------------
;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; NO TABS
(setq-default indent-tabs-mode nil)

;; Set folder for emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs-backup")))

;; Maximize the emacs window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Line Spacing
(setq-default line-spacing 6)

;; Random Functions
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

;; SMARTPARENS
;; -------------------------------------------------------------------
(smartparens-global-mode t)

;; YAML
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; SASS
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; HELM
;; -------------------------------------------------------------------
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(setq helm-ff-newfile-prompt-p nil)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(semantic-mode 1)

;; Multiple Terminal Management
;; -------------------------------------------------------------------
(global-set-key (kbd "C-x t") 'helm-mt)

;; Highlight Matching Parenthesis
;; -------------------------------------------------------------------
(show-paren-mode 1)

;; JavaScript
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; Adds JSX support in .js files
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js\\'")))

(defun web-mode-hooks ()
  "Hooks for web-mode"
  ;; indentation
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2))

(add-hook 'web-mode-hook 'web-mode-hooks)

;; CSS
;; -------------------------------------------------------------------
(defun css-mode-hooks ()
  "Hooks for css-mode"
  (setq css-indent-offset 2))

;; JSON
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; MARKDOWN
;; -------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; APPEARANCE
;; -------------------------------------------------------------------
(setq inhibit-startup-screen t)

(global-linum-mode t)                   ; enable line #s

(tool-bar-mode -1)                      ; hide toolbar

(toggle-scroll-bar -1)                  ; hide scrollbar

(menu-bar-mode -1)                      ; hide menubar

(load-theme 'aurora t)
