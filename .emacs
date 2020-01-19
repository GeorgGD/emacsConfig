
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Emacs essentials
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-default-font "Ubuntu Mono-13")
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(global-linum-mode t)

;; Macro for running shell (C-c s)
(global-set-key
 (kbd "C-c s")
 (lambda ()
   (interactive)
   (let ((current-prefix-arg '(4)))
     (call-interactively #'shell))))


;; MELPA package archive
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/"))

;; Spacemaps color there and Spaceline-package
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (irony-eldoc company-irony irony yasnippet spacemacs-theme spaceline which-key use-package monokai-theme ivy haskell-mode evil-surround evil-magit evil-leader evil-escape company cider auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)

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


;; Org-mode
(setq org-support-shift-select t)
