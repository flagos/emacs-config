;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
;; (setq package-archive-priorities '(("melpa"    . 5)
;;                                    ("jcs-elpa" . 0))

(setq package-check-signature nil)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    bnf-mode
    cl-lib
    code-review
    counsel
    counsel-projectile
    counsel-spotify
    csv-mode
    dap-mode
    dash
    djangonaut
    difftastic
    docker
    docker-compose-mode
    dockerfile-mode
    elisp-format
    elpy ;; for highlight-indentation-mode
    emacsql
    flycheck
    forge
    git-auto-commit-mode
    git-link
    gitlab-pipeline
    groovy-mode
    hackernews
    ht
    iedit
    js2-mode
    json-mode
    load-env-vars
    lsp-ivy
    lsp-mode
    lsp-pyright
    lsp-sonarlint
    lsp-ui
    lua-mode
    magit
    markdown-mode
    material-theme
    move-dup
    multiple-cursors
    org-journal
    org-roam
    pinentry
    poetry
    projectile
    py-autopep8
    py-isort
    pycoverage
    python-black
    python-docstring
    python-pytest
    quelpa
    restclient
    smex
    sql-indent
    sqlite3
    terraform-mode
    use-package
    vlf
    vterm
    web-mode
    which-key
))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; install quelpa packages
;; (require 'quelpa)

;; (quelpa '(change-case :fetcher git :url "git@gist.github.com:e8a10244aac6308de1323d1f6685658b.git"))

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
;;(load-theme 'spacemacs-dark t) ;; load material theme
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (global-display-line-numbers-mode) ;; enable line numbers globallye


(tool-bar-mode -1)
(menu-bar-mode -1)

;; backup config
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
)


;; elisp
(require 'elisp-format)

;; window config
(windmove-default-keybindings 'meta)
(winner-mode 1)

(setq calendar-week-start-day 1)

(add-hook 'after-init-hook 'global-company-mode)

(setq projectile-sort-order 'recently-active)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;; sonarlint
(use-package lsp-sonarlint
  :custom
  ;; Allow sonarlint to download and unzip the official VSCode extension
  ;; If nil, you'll have to do that yourself. See also `lsp-sonarlint-download'
  ;; `lsp-sonarlint-download-url' and `lsp-sonarlint-download-dir'
  (lsp-sonarlint-auto-download t)

  ;; Choose which analyzers you want enabled. By default all are enabled
  ;; See command `lsp-sonarlint-available-analyzers' for the full list.
  (lsp-sonarlint-enabled-analyzers '("python" "text")))


;; lsp
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.env\\'"))
(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'lsp-mode-hook (lambda () (highlight-indentation-mode 1)))

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config (setq lsp-pyright-venv-directory "lsp"
                lsp-file-watch-threshold 20000
                lsp-pyright-auto-import-completions t)
  )  ; or lsp-deferred

(use-package dap-mode
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  )


;; vterm
(use-package vterm
    :ensure t)

;; lsp performances
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; poetry
(use-package poetry
 :ensure t)

(add-hook 'before-save-hook 'py-isort-before-save)

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))


;; .env files bricolage.
(defvar @-dotenv-file-name ".env"
  "The name of the .env file."
  )

(defun @-find-env-file ()
  "Find the closest .env file in the directory hierarchy."

  (let* ((env-file-directory (locate-dominating-file "." @-dotenv-file-name))
        (file-name (concat env-file-directory @-dotenv-file-name)))
    (when (file-exists-p file-name)
        file-name))
  )

(defun @-set-project-env ()
  "Export all environment variables in the closest .env file."

  (let ((env-file (@-find-env-file)))
    (when env-file
      (load-env-vars env-file)))
  )


(defun @-set-env-vars-hooks ()
  "Load env file in a project."

  (use-package load-env-vars)
  (add-hook 'projectile-mode-hook #'@-set-project-env)
  (add-hook 'projectile-after-switch-project-hook #'@-set-project-env)
  (add-hook 'comint-exec-hook #'@-set-project-env)
  (add-hook 'lsp-mode-hook #'@-set-project-env)
  (add-hook 'vterm-mode-hook #'@-set-project-env)
  )
(@-set-env-vars-hooks)

;; (global-djangonaut-mode)
(setq python-shell-extra-pythonpaths '("/home/vincent/work/oper-product/"))
(setq python-shell-process-environment '("DJANGO_SETTINGS_MODULE=oper.settings"))


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

;; Reverse colors for the border to have nicer line
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "gray")

;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))


(global-auto-revert-mode 1)

(require 'vlf-setup)

(smex-initialize) ;
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(electric-pair-mode t)
;; electric-pair only for whilelist modes
(defvar my-electic-pair-modes '(python-mode emacs-lisp-mode json-mode yaml-mod))

(defun my-inhibit-electric-pair-mode  (char)
  (not (member major-mode my-electic-pair-modes)))
(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

(ido-mode t)


;; markdown
(custom-set-variables
  '(markdown-command "pandoc"))

;; javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)

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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


;; screen stuff
;; (define-key input-decode-map "\e[1;2D" [S-left])
;; (define-key input-decode-map "\e[1;2C" [S-right])
;; (define-key input-decode-map "\e[1;2B" [S-down])
;; (define-key input-decode-map "\e[1;2A" [S-up])
;; (define-key input-decode-map "\e[1;2F" [S-end])
;; (define-key input-decode-map "\e[1;2H" [S-home])


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 113 :width normal))))
 '(region ((t (:background "black" :inverse-video t)))))


;; magit-popup
(setq transient-enable-popup-navigation t)


;; forge
(with-eval-after-load 'magit
  (require 'forge))

(setq auth-sources '("~/.authinfo.gpg"))

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(setq projectile-completion-system 'ivy)


;; Use C-j for immediate termination with the current value, and RET
;; for continuing completion for that directory. This is the ido
;; behaviour.
;; (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
;; (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)


;; pycoverage
(require 'pycoverage)

(defun my-coverage ()
  (interactive)
  (when (derived-mode-p 'python-mode)
    (progn
      (pycoverage-mode))))

;;restclient
(require 'restclient)

;; impostman
;; (Require 'impostman)

;; docker
(use-package docker
  :ensure t
    :bind ("C-c d" . docker))

;; slack
;; (el-get-bundle slack)

;; (load-file "/home/vincent/project/emacs-config/private.el")

;; automate release note
(defun generate-yaml-note ()
      (interactive)
      (let ((default-directory "/home/vincent/work/datapred/docs/release-notes")
            (explicit-shell-file-name "bash"))
        (message "Launching generate yaml")
        (message (shell-command-to-string "python generate_yaml_note.py"))))

(setq create-lockfiles nil)

(setq ghub-use-workaround-for-emacs-bug 'force)
(setq code-review-gitlab-base-url "gitlab.com") ;;; default value
(setq code-review-fill-column 80)

;; lark
 (load-file "~/project/emacs-config/lark-mode.el")


(use-package move-dup
  :bind (("S-M-<up>"   . move-dup-move-lines-up)
         ("C-M-<up>" . move-dup-duplicate-up)
         ("S-M-<down>"   . move-dup-move-lines-down)
         ("C-M-<down>" . move-dup-duplicate-down)))

;; org-roam
;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/Documents/notes/"))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

;; org-agenda
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))

;; org-journal
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-type  (quote monthly)
        org-journal-date-format "%A, %d %B %Y"))


;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
      ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Ctrl-K with no kill
(defun delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
)
(global-set-key (kbd "C-k") 'delete-line-no-kill)

;; pinentry
(setq epa-pinentry-mode 'loopback)
(setq epa-file-encrypt-to "vincent.lagorsse@opercredits.com")
(pinentry-start)

;; diffstatic
(require 'difftastic)

;; Add commands to a `magit-difftastic'
(eval-after-load 'magit-diff
  '(transient-append-suffix 'magit-diff '(-1 -1)
     [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
      ("S" "Difftastic show" difftastic-magit-show)]))
(add-hook 'magit-blame-read-only-mode-hook
          (lambda ()
            (keymap-set magit-blame-read-only-mode-map
                        "D" #'difftastic-magit-show)
            (keymap-set magit-blame-read-only-mode-map
                        "S" #'difftastic-magit-show)))


;; safe variables
(add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))
(add-to-list 'safe-local-variable-values '(gac-automatically-add-new-files-p . t))

(provide 'config)
;;; config.el ends here
