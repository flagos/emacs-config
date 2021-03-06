;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-check-signature nil)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    lsp-mode
    lsp-ui
    elpy ;; for highlight-indentation-mode
    material-theme
    flycheck
    py-autopep8
    python-black
    py-isort
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
    use-package
    dockerfile-mode
    docker-compose-mode
    yasnippet
    forge
    python-pytest
    counsel
    lsp-ivy
    counsel-projectile
    pycoverage
    counsel-spotify
    sql-indent
    csv-mode
    python-docstring
    restclient
    quelpa
))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; install quelpa packages
(require 'quelpa)

(quelpa '(change-case :fetcher git :url "git@gist.github.com:e8a10244aac6308de1323d1f6685658b.git"))


(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
;;(load-theme 'spacemacs-dark t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globallye
(setq linum-format "%d ")

(tool-bar-mode -1)
(menu-bar-mode -1)

;; window config
(windmove-default-keybindings 'meta)
(winner-mode 1)

(setq calendar-week-start-day 1)

(add-hook 'after-init-hook 'global-company-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'lsp-mode-hook (lambda () (highlight-indentation-mode 1)))

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(use-package lsp-mode
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  :hook
    ((python-mode . lsp)))

(use-package python-black
  :demand t
  :after python)

(global-flycheck-mode)

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
(require 'multiple-cursors)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-N") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(add-to-list 'mc/unsupported-minor-modes 'flyspell-mode)
(add-to-list 'mc/unsupported-minor-modes 'linum-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


;; screen stuff
(define-key input-decode-map "\e[1;2D" [S-left])
(define-key input-decode-map "\e[1;2C" [S-right])
(define-key input-decode-map "\e[1;2B" [S-down])
(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;2F" [S-end])
(define-key input-decode-map "\e[1;2H" [S-home])


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 113 :width normal))))
 '(region ((t (:background "black" :inverse-video t)))))


;; magit-popup
(setq transient-enable-popup-navigation t)


;; yasnippet
(yas-reload-all)
(add-hook 'python-mode-hook #'yas-minor-mode)


;; forge
(with-eval-after-load 'magit
  (require 'forge))

(setq auth-sources '("~/.authinfo"))

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")


(counsel-projectile-mode)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)

;; Use C-j for immediate termination with the current value, and RET
;; for continuing completion for that directory. This is the ido
;; behaviour.
;; (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
;; (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)


;; pycoverage
(require 'linum)
(require 'pycoverage)

(defun my-coverage ()
  (interactive)
  (when (derived-mode-p 'python-mode)
    (progn
      (linum-mode)
      (pycoverage-mode))))

(load-file "/home/vincent/project/emacs-config/private.el")

;;restclient
(require 'restclient)

;; docker
(use-package docker
  :ensure t
    :bind ("C-c d" . docker))

(provide 'config)
;;; config.el ends here
