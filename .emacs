(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs essentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-default-font "Ubuntu Mono-13")
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(global-linum-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro for running shell (C-c s)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key
 (kbd "C-c s")
 (lambda ()
   (interactive)
   (let ((current-prefix-arg '(4)))
     (call-interactively #'term))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Balanced parentheses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode 1)  
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA package archive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacemacs-dark theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package spacemacs-theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (helm yasnippet dap-mode lsp-ui flycheck which-key lsp-java company-emacs-eclim eclim company-web company company-irony spacemacs-theme spaceline irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spaceline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-support-shift-select t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete with company
;; For company to work you need clang, cmake and libclang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))
 
(use-package company-irony
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  
(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete for HTML with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'company-backends 'company-web-html)

(defun my-sgml-insert-gt ()
  "Inserts a `>' character and calls 
`my-sgml-close-tag-if-necessary', leaving point where it is."
  (interactive)
  (insert ">")
  (save-excursion (my-sgml-close-tag-if-necessary)))

(defun my-sgml-close-tag-if-necessary ()
  "Calls sgml-close-tag if the tag immediately before point is
an opening tag that is not followed by a matching closing tag."
  (when (looking-back "<\\s-*\\([^</> \t\r\n]+\\)[^</>]*>")
    (let ((tag (match-string 1)))
      (unless (and (not (sgml-unclosed-tag-p tag))
           (looking-at (concat "\\s-*<\\s-*/\\s-*" tag "\\s-*>")))
    (sgml-close-tag)))))

(eval-after-load "sgml-mode"
  '(define-key sgml-mode-map ">" 'my-sgml-insert-gt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP-Java, IDE package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'java-mode-hook 'yas-global-mode)
  
  (add-hook 'java-mode-hook 'which-key-mode)
  (add-hook 'java-mode-hook 'flycheck-mode)
  ;; Java has different indentation, the code below fixes that
  (add-hook 'java-mode-hook (lambda ()
			      (setq c-basic-offset 4
				    tab-width 4
				    indent-tabs-mode t)))

  ;; Turns on Flycheck errors list at the buttom
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.15)))

  (use-package lsp-mode :ensure t
    :bind ("M-RET" . lsp-execute-code-action))
  (use-package lsp-ui
    :ensure t
    :config
    (setq lsp-prefer-flymake nil
	  lsp-ui-doc-delay 5.0
	  lsp-ui-sideline-enable nil
	  lsp-ui-sideline-show-symbol nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm
  :ensure t
  :init
  (defun tkj-list-buffers()
    (interactive)
    (let ((helm-full-frame t))
      (helm-mini)))
  
  :bind ("C-x C-b" . 'tkj-list-buffers)

  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-display-header-line nil)
  (set-face-attribute 'helm-source-header nil :height 0.1)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 25)
  (setq helm-autoresize-min-height 25)
  (helm-mode 1))
