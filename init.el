;; MELPA
;; http://melpa.org/#/getting-started (keep at the top of init)
;; -------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; RANDOM
;; -------------------------------------------------------------------

;; NO TABS
(setq-default indent-tabs-mode nil)

;; Set folder for emacs backup files
(setq backup-directory-alist `(("." . "~/.emacs-backup")))

;; Maximize the emacs window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Line Spacing
(setq-default line-spacing 5)

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

;; HELM
;; -------------------------------------------------------------------
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(semantic-mode 1)

;; Multiple Terminal Management
;; -------------------------------------------------------------------
(global-set-key (kbd "C-x t") 'helm-mt)

;; Highlight Matching Parenthesis
;; -------------------------------------------------------------------
(show-paren-mode 1)

;; JAVASCRIPT
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))


(setq js2-highlight-level 3)
(setq-default js2-basic-offset 2)       ;two spaced indent
;; customisations in custom.el
(add-hook 'js2-mode-hook          
	  (lambda ()
	    ;; Hide code blocks
	    (hs-minor-mode t)
	    ;; js-doc.el hooks:
	    (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
	    (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; (setq sgml-attribute-offset 2)          ;two spaced indent for jsx tags

;; Node syntax highlighting
(setq js2-include-node-externs t)
;; Airbnb stylguide says they are better for git diffs
(setq js2-strict-trailing-comma-warning nil)
;; allow missing colon on one-liners
(setq js2-missing-semi-one-line-override t)

(global-set-key (kbd "<C-return>") 'hs-toggle-hiding)


;; JSX
;; (defun web-mode-custom-indent ()
;;   "Adjust indents in web-mode from 4 to 2 spaces."
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))

;; (add-hook 'web-mode-hook 'web-mode-custom-indent)

;; JSON
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; APPEARANCE
;; -------------------------------------------------------------------

(setq inhibit-startup-screen t)

(global-linum-mode t)                   ; enable line #s

(tool-bar-mode -1)                      ; hide toolbar

(toggle-scroll-bar -1)                  ; hide scrollbar

(menu-bar-mode -1)                      ; hide menubar

(load-theme 'aurora t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "667e296942c561382fe0a8584c26be0fe7a80416270c3beede8c6d69f2f77ccc" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "8122f00211dbaf973fbe5831f808af92387c8fc1a44f0c6bcc9b22c16997c9dd" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:background "gray12" :foreground "gray12"))))
 '(term-color-blue ((t (:background "blue" :foreground "blue"))))
 '(term-color-cyan ((t (:background "cyan" :foreground "cyan"))))
 '(term-color-green ((t (:background "green" :foreground "green"))))
 '(term-color-magenta ((t (:background "dark magenta" :foreground "dark magenta"))))
 '(term-color-red ((t (:background "orange red" :foreground "orange red"))))
 '(term-color-white ((t (:background "white smoke" :foreground "white smoke"))))
 '(term-color-yellow ((t (:background "yellow" :foreground "yellow")))))
