(package-initialize)

;; Emacs essentials
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-default-font "Ubuntu Mono-13")
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(global-linum-mode t)
;;------------------------------------------------------------------

;; Macro for running shell (C-c s)
(global-set-key
 (kbd "C-c s")
 (lambda ()
   (interactive)
   (let ((current-prefix-arg '(4)))
     (call-interactively #'term))))
;;------------------------------------------------------------------

;; Balanced parentheses
(electric-pair-mode 1)  
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))
;;------------------------------------------------------------------


;; MELPA package archive
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Spacemacs-dark theme
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
    (company-web company company-irony spacemacs-theme spaceline irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;------------------------------------------------------------------

;; Spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; Org-mode
(setq org-support-shift-select t)

;; Auto-complete with company
;; For company to work you need clang, cmake and libclang (good luck getting everything to work. xD)
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
;;------------------------------------------------------------------

;; Auto-complete for HTML with company
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
;;------------------------------------------------------------------

