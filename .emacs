;; Will maximize screen when starting emacs
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs essentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-frame-font "Ubuntu Mono-13")
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(global-linum-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro for running shell (C-c s)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-term (name _action)
  "Determine whether NAME names a `term-mode' buffer."
  (with-current-buffer name
    (derived-mode-p #'term-mode)))

(defun my-terminal ()
  "Start Bash in a terminal emulator.
Like `term', but respect buffer display actions."
  (interactive)
  (let ((switch-to-buffer-obey-display-actions t))
    (term "/bin/bash")))

(add-to-list 'display-buffer-alist
	     '(my-term () (inhibit-same-window . t)
		       (display-buffer-below-selected)
		       (window-height . 0.20)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(highlight-indent-guides s pyvenv highlight-indentation lsp-python-ms elpy treemacs helm yasnippet dap-mode lsp-ui flycheck which-key lsp-java company-emacs-eclim eclim company-web company company-irony spacemacs-theme spaceline irony))
 '(python-indent-offset 2))
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
  (require 'dap-java)
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
	  lsp-ui-sideline-show-symbol nil))
  (use-package treemacs
    :ensure t
    :bind
    ("C-c t" . 'treemacs)
    ("C-c C-t" . 'treemacs-select-window)
    :config
    (add-hook 'treemacs-mode-hook
	      (lambda () (treemacs-resize-icons 15)))
    (setq treemacs-is-never-other-window t)))

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
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-display-header-line nil)
  (set-face-attribute 'helm-source-header nil :height 0.1)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 25)
  (setq helm-autoresize-min-height 25)
  (helm-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'highlight-indent-guides)
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (elpy-mode . (lambda ()
	       (setq c-basic-offset 4
		     tab-width 4
		     python-indent-offset 4)))
  :config
  (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
  (setq highlight-indent-guides-method 'bitmap)
  
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "#A47CB8")
  (set-face-background 'highlight-indent-guides-even-face "#A47CB8")
  (set-face-foreground 'highlight-indent-guides-character-face "#A47CB8"))
