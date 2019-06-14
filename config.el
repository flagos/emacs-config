
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))


(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    lsp-mode
    lsp-ui
    company-lsp
    elpy ;; for highlight-indentation-mode
    material-theme
    ;;spacemacs-theme
    flycheck
    py-autopep8
    lua-mode
    markdown-mode
    vlf
    smex
    magit
    cl-lib
    js2-mode
    web-mode
    which-key
    groovy-mode
    multiple-cursors
    iedit
    projectile
    docker
))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)



(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
;;(load-theme 'spacemacs-dark t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(tool-bar-mode -1)
(menu-bar-mode -1)

(add-hook 'after-init-hook 'global-company-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'lsp-mode-hook (lambda () (highlight-indentation-mode 1)))

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)

(add-to-list 'load-path (file-name-directory load-file-name))
(load "robot-mode.el")

;;(load-file (concatenate (file-name-directory load-file-name) "/robot-mode.el"))
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))

(setq mouse-yank-at-point nil)
(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; ecb configuration
'(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
'(ecb-tip-of-the-day nil)
)

(scroll-bar-mode -1)

 (global-auto-revert-mode 1)

(require 'vlf-setup)

(smex-initialize) ;
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(electric-pair-mode t)
(ido-mode t)


;; javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; pretty print xml
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; which-key
(which-key-mode)

;; multiple-cursors
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-N") 'mc/mark-all-like-this)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

