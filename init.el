;;; INIT.el --- Initialization file for Emacs.
;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; use-package
(eval-when-compile (require 'use-package))
(require 'bind-key)

;; (setq use-package-always-defer t)
(setq use-package-always-ensure t)

;; use-package configurations
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-background nil)
  )

(use-package ag
  :ensure t
  :hook (ag-mode . wgrep-ag-setup)
  :config
  (setq ag-highlight-search t)
  (setq ag-arguments (cons "-W 256" ag-arguments)))

(use-package auctex :ensure t :defer t)

(require 'artist)
(eval-after-load "artist"
   '(define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation))

(use-package browse-kill-ring :ensure t)

(use-package column-enforce-mode
  :ensure t
  :defines column-enforce-mode-column
  :config (setq column-enforce-mode-column 80)
  :hook ((prog-mode . column-enforce-mode)
         (html-mode . column-enforce-mode)
         (sql-mode . (lambda () (column-enforce-mode -1)))))

(use-package company :ensure t)

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode))

(use-package dumb-jump
  :after evil
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package elixir-mode
  :ensure t
  :bind (:map elixir-mode-map
              ("C-c C-c f" . elixir-format))
  :hook
  (elixir-mode . (lambda ()
                   (setq column-enforce-column 80)
                   (column-enforce-mode))))

(use-package elm-mode
  :ensure t
  :config (setq elm-indent-offset 2))

(use-package erlang :defer t :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global (kbd "C-]") 'evil-goto-definition)
  (global-set-key (kbd "<f4>") 'evil-mode)
  (defalias #'forward-evil-WORD #'forward-evil-symbol))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme
               '(textobjects insert navigation additional shift todo))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-magit
  :after (evil magit)
  :ensure t)

(use-package evil-matchit
  :ensure t
  :after evil
  :config (global-evil-matchit-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package flx-ido :ensure t)
  ;; (ido-mode 1)
  ;; (flx-ido-mode 1)
  ;; (setq ido-everywhere t)
  ;; (setq ido-enable-flex-matching t)
  ;; (setq ido-use-faces nil)
  ;; (setq ido-use-filename-at-point 'guess)
  ;; (setq ido-use-url-at-point t))
(use-package flycheck
  :ensure t
  :hook
  (ruby-mode . (lambda ()
                 (setq-local flycheck-command-wrapper-function
                    (lambda (command)
                        (append '("bundle" "exec") command)))))
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  (global-flycheck-mode))

;; control the flycheck list errors buffer
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.10)))

;; (use-package flycheck-elixir :ensure t)

(use-package flyspell
  :ensure t
  :hook
  (text-mode . flyspell-mode)
  (html-mode . (lambda() (flyspell-mode -1))))

(use-package gnutls
  :ensure t
  :config
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

(use-package groovy-mode :ensure t)

;; ivy-immediate-done C-M-j
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :bind
  (("C-c r" . ivy-resume)
   ("C-c s" . counsel-rg)
   ("C-s"   . swiper))
  :config
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (counsel-ag . ivy--regex-plus)
	  (counsel-rg . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; (setq counsel-rg-base-command "rg -S -M 512 --no-heading --line-number --color never %s .")
  (setq counsel-ag-base-command "ag -W 256 --nocolor --nogroup %s"))

(use-package json-mode
  :ensure t
  :hook (json-mode . (lambda()(setq js-indent-level 2))))

(use-package kotlin-mode :ensure t)

(use-package lsp-java :ensure t)

(defun cond-add-elixir-credo ()
  "Add elixir-credo to lsp next checker if 'major-mode' is elixir-mode."
  (when (and (eq major-mode 'elixir-mode)
             (not (member 'elixir-credo (flycheck-get-next-checkers 'lsp))))
    (flycheck-add-next-checker 'lsp 'elixir-credo)))

(use-package lsp-mode
  :ensure t
  :init
  (add-to-list 'exec-path (concat user-emacs-directory "elixir-ls"))
  (add-to-list 'exec-path (concat user-emacs-directory "kotlin-ls/bin"))
  :config
    (setq lsp-log-io nil)
    (setq lsp-enable-file-watchers nil)
    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook
    (java-mode . lsp)
    (elixir-mode . lsp)
    (elm-mode . lsp)
    (lsp-diagnostics-updated . cond-add-elixir-credo)
  :commands (lsp))

(use-package magit
  :ensure t
  :after (evil)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

(use-package markdown-mode
  :ensure t
  :config (setq tab-width 2)
  :hook
  (markdown-mode . auto-fill-mode)
  (markdown-mode . display-line-numbers-mode)
  (markdown-mode . (lambda() (setq-local fill-column 80)))
  (markdown-mode . company-mode))

(use-package midnight)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; org-mode
(require 'org)
(add-hook 'org-mode-hook 'auto-revert-mode)
(add-to-list 'org-modules 'org-habit)
(setq org-directory "~/gtd")
(setq org-default-notes-file "~/gtd/inbox.org")
(setq org-agenda-files "~/.emacs.d/org-agenda-files")
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))
(org-add-link-type "airmail" 'org-airmail-open)

(defun org-airmail-open (url)
  "Visit the Airmail message referenced by URL.
URL should be a vaid Airmail message url retrieved from Airmail with
'Copy Message Link'."
  (shell-command
   ;; Note: org strips "airmail:" from the link URL
   (concat "open -a '/Applications/Airmail.app' airmail:"
           (shell-quote-argument url))))

(require 'org-capture)
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates `(("t" "TODO" entry (file "inbox.org")
                               ,(concat "* TODO %?\n"
                                        "  %U\n"
                                        "  %a"))
                              ("h" "Habit" entry (file "habits.org")
                               ,(concat "* TODO %?\n"
                                        "  SCHEDULED: %t\n"
                                        "  :PROPERTIES:\n"
                                        "  :STYLE: habit\n"
                                        "  :END:"))))

(require 'org-clock)
(setq org-clock-idle-time 10)
(require 'org-agenda)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-todo-ignore-scheduled 'future)

(use-package org-present
  :ensure t
  :after evil
  :hook
  ((org-present-mode . (lambda ()
			      (evil-mode 0)
			      (hide-mode-line-mode 1)
			      (org-present-big)
			      (org-display-inline-images)
			      (org-present-hide-cursor)
			      (org-present-read-only)))
  (org-present-mode-quit . (lambda ()
			     (evil-mode 1)
			     (hide-mode-line-mode 0)
			     (org-present-small)
			     (org-remove-inline-images)
			     (org-present-show-cursor)
			     (org-present-read-write)))))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode)
  (projectile-tags-exclude-patterns))

(use-package restclient
  :ensure t
  :config
  (setq restclient-inhibit-cookies t))

(use-package ripgrep :ensure t)

(use-package rjsx-mode
  :after (js2-mode js-mode)
  :ensure t)
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(use-package rubocop :ensure t)

(use-package spaceline
  :ensure t
  :config
  (setq powerline-default-separator 'contour)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package string-inflection :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.erb\\'"
  :config (setq web-mode-markup-indent-offset 2))

(use-package wgrep :ensure t)

(use-package wgrep-ag
  :ensure t
  :after wgrep)

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . display-line-numbers-mode))

(use-package yari :ensure t)

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

;; (use-package zoom-window
;;   :ensure t
;;   :bind ("C-x C-z" . zoom-window-zoom)
;;   :config (setq zoom-window-mode-line-color nil))

;; END OF USE-PACKAGE
(global-set-key (kbd "C-x C-z") 'maximize-window)

;; file mode associations
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
(fset 'perl-mode 'cperl-mode)

(defun copy-project-relative-file-name ()
  "Copy the current buffer file name relative to project root."
  (interactive)
  (let ((filename (file-relative-name
                  buffer-file-name (projectile-project-root))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard" filename)))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; shell shortcuts
(defun shell1 () "Switch to or create *shell-1."
       (interactive) (shell "*shell-1*"))
(defun shell2 () "Switch to or create *shell-2."
       (interactive) (shell "*shell-2*"))
(defun shell3 () "Switch to or create *shell-3."
       (interactive) (shell "*shell-3*"))

(global-set-key (kbd "C-1") 'shell1)
(global-set-key (kbd "C-2") 'shell2)
(global-set-key (kbd "C-3") 'shell3)
(global-set-key (kbd "M-1") 'shell1)
(global-set-key (kbd "M-2") 'shell2)
(global-set-key (kbd "M-3") 'shell3)

(global-set-key (kbd "M-g") 'goto-line) ; For Simon
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

;; general hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'ruby-mode-hook 'rubocop-mode)

(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

(defalias 'yes-or-no-p 'y-or-n-p)
(setenv "PAGER" "cat")

;; emacs client / server
(load "server")
(unless (server-running-p) (server-start))

(setenv "EDITOR" "emacsclient")

;; look and feel
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(setq column-number-mode t)
(show-paren-mode 1)
(add-hook 'after-init-hook 'toggle-frame-maximized)
(add-hook 'after-init-hook 'powerline-reset)
(setq show-trailing-whitespace t)
(menu-bar-mode -1)
(setq use-dialog-box nil)
;; (set-frame-font "Source Code Pro 14" nil t)
;; (add-to-list 'default-frame-alist '(height . 120))
;; (add-to-list 'default-frame-alist '(width . 80))

;;If this is nil, split-window-sensibly is not allowed to split a window vertically.
(setq split-height-threshold nil)
(setq split-width-threshold 200)

;; tags
(setq tags-add-tables nil)

;; ediff
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; ispell
(setq ispell-program-name "/usr/local/bin/ispell")

;; recentf
(recentf-mode 1)

;; shell
(setq shell-file-name "/bin/bash")
(add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))

;; indentation
(electric-indent-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default css-indent-offset 2)
(setq-default sh-basic-offset 2)
(setq-default cperl-indent-level 2)
;; backups
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; lock files
(setq create-lockfiles nil)

(add-to-list 'display-buffer-alist
              '("*shell-?*" (display-buffer-reuse-window
                              display-buffer-same-window)))

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(setq inhibit-startup-screen t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default)))
 '(default-input-method "latin-prefix")
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (kotlin-mode nix-mode column-marker evil-matchit browse-kill-ring java-imports zoom-window dumb-jump gtags groovy-mode ripgrep web-mode yari ctags-update spaceline wget evil-collection wgrep-ag use-package string-inflection json-mode evil-surround rg counsel-projectile evil-magit rjsx-mode js2-mode hide-mode-line org-present yaml-mode evil-org ivy-hydra hydra counsel ivy rubocop haskell-mode ws-butler markdown-mode alchemist ag ace-window zenburn-theme evil-snipe column-enforce-mode flx-ido company yasnippet yasnippet-snippets meghanada projectile flycheck exec-path-from-shell restclient erlang evil)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values (quote ((column-enforce-column . 120))))
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))
(put 'dired-find-alternate-file 'disabled nil)
