;;; INIT.el --- Initialization file for Emacs.

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

;; (setq use-package-always-defer t)
(setq use-package-always-ensure t)

;; use-package configurations
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

(use-package ace-window
  :defer t
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-background nil)
  )

(use-package ag
  :defer t
  :ensure t
  :hook (ag-mode . wgrep-ag-setup)
  :config
  (setq ag-highlight-search t)
  (setq ag-arguments (cons "-W 256" ag-arguments)))

(use-package auctex :defer t :ensure t)

(use-package avy
  :defer t
  :ensure t
  :config
  (avy-setup-default)
  (setq avy-timeout-seconds 0.2)
  :bind
  ("C-," . avy-pop-mark)
  ("C-;" . avy-goto-char-timer)
  ("C-c C-;" . avy-goto-line))

(use-package browse-kill-ring :defer t :ensure t)

(use-package company
  :defer t
  :ensure t
  :config (setq company-idle-delay 0.500))

(use-package consult
  :defer t
  :ensure t
  :config
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root)))
    (setq consult-narrow-key "<")
    (setq recentf-max-menu-items 100)
    (setq recentf-max-saved-items 100)
    (setq xref-show-xrefs-function 'consult-xref)
  :bind (("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("C-c b" . consult-project-buffer)
         ("C-s"   . consult-line)
         ("C-c s" . consult-ripgrep)
         ("C-c C-1"  . consult-flymake)
         ("C-x r b" . consult-bookmark)
         ))

(use-package csv-mode
  :defer t
  :ensure t
  )

(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode))

(use-package direnv
  :ensure t
  :init
  :config
  (setq direnv-show-paths-in-summary nil)
  (setq direnv-always-show-summary t)
  (direnv-mode))

;; (use-package dumb-jump
;;   :defer t
;;   :after (evil)
;;   :ensure t
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package elixir-mode
  :defer t
  :ensure t
  :bind (:map elixir-mode-map ("C-c C-c f" . elixir-format))
  :init (add-hook 'elixir-mode-hook
                  (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '(elixir-mode "elixir-ls"))
;;   :hook
;;   ;; (python-mode . (lambda() (flycheck-mode -1))) ;; eglot uses flymake
;;   ;; (python-mode . eglot-ensure)
;;   (elixir-mode . eglot-ensure)
;;   (go-mode . eglot-ensure))


(use-package emacs
  :init
  ;; vertico settings
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-to-list 'image-types 'svg)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; look and feel
  (set-frame-font "Menlo-14" nil t)
  ;; (load-theme 'gruvbox-light-soft t)
  (load-theme 'gruvbox-dark-soft t)

  (setq ring-bell-function 'ignore)
  (scroll-bar-mode -1)
  (setq column-number-mode t)
  (show-paren-mode 1)
  ;; (add-hook 'after-init-hook 'toggle-frame-maximized)
  (add-to-list 'default-frame-alist '(height . 100))
  (add-to-list 'default-frame-alist '(width . 120))
  ;; (add-hook 'after-init-hook 'powerline-reset)
  (setq show-trailing-whitespace t)
  (menu-bar-mode -1)
  (setq use-dialog-box nil)

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
  ;; (add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))

  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; indentation
  (electric-indent-mode 1)
  (setq-default standard-indent 2)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default c-basic-offset 2)
  (setq-default js-indent-level 2)
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

  (setq undo-strong-limit 1048576)
  (setq undo-limit 1048576)

  (put 'dired-find-alternate-file 'disabled nil)
  (tool-bar-mode -1)
  :hook (python-mode . hs-minor-mode)
  )

(use-package embark
  :ensure t

  :bind
  (("C-'" . embark-act)         ;; pick some comfortable binding
   ("C-<return>" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package elm-mode
  :defer t
  :ensure t
  :config (setq elm-indent-offset 2))

(use-package erlang :defer t :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global (kbd "C-]") 'evil-goto-definition)
  (global-set-key (kbd "<f4>") 'evil-mode)
  (defalias #'forward-evil-WORD #'forward-evil-symbol))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-collection-want-find-usages-bindings t)
  :config
  (evil-collection-init))

;; (use-package evil-org
;;   :defer t
;;   :ensure t
;;   :after (org)
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;;             (lambda ()
;;               (evil-org-set-key-theme
;;                '(textobjects insert navigation additional shift todo))))
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

(use-package evil-matchit
  :ensure t
  :after evil-collection
  :config (global-evil-matchit-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :init
  :config
  (setq exec-path-from-shell-variables
   '("PATH" "MANPATH" "IN_NIX_SHELL" "NIX_PROFILES" "NIX_PATH" "NIX_SSL_CERT_FILE")))

;; (use-package flymake
;;   :bind ("C-c C-2"   . flymake-goto-next-error))

(use-package flx-ido :ensure t)
  ;; (ido-mode 1)
  ;; (flx-ido-mode 1)
  ;; (setq ido-everywhere t)
  ;; (setq ido-enable-flex-matching t)
  ;; (setq ido-use-faces nil)
  ;; (setq ido-use-filename-at-point 'guess)
  ;; (setq ido-use-url-at-point t))

(use-package flycheck
  :defer t
  :ensure t
  :hook
  (ruby-mode . (lambda ()
                 (setq-local flycheck-command-wrapper-function
                    (lambda (command)
                        (append '("bundle" "exec") command)))))
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  ;; (global-flycheck-mode))

  ;; control the flycheck list errors buffer
  (add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.10)))

  ;; (use-package flycheck-elixir :ensure t)
  )

(use-package flyspell
  :defer t
  :ensure t
  :hook
  (text-mode . flyspell-mode)
  (html-mode . (lambda() (flyspell-mode -1))))

;; (use-package git-gutter
;;   :ensure t
;;   :defer t
;;   :init (git-gutter:linum-setup)
;;   :config (global-git-gutter-mode))


(use-package git-link :ensure t :defer t)

(use-package go-mode :ensure t :defer t)

(use-package gnutls
  :defer t
  :ensure t
  :config
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem"))

(use-package graphql-mode :ensure t :defer t)

(use-package groovy-mode :ensure t :defer t)

(use-package gruvbox-theme :ensure t :defer t)

(use-package hl-todo :ensure t :defer t :config (global-hl-todo-mode))

(use-package jq-mode :ensure t :defer t)

(use-package json-mode
  :defer t
  :ensure t
  :hook (json-mode . (lambda()(setq js-indent-level 2))))

(use-package kotlin-mode
  :defer t
  :ensure t
  :config (setq kotlin-tab-width 2))

(use-package lsp-java
  :defer t
  :ensure t
  :config
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication"))
  (setq lsp-java-import-gradle-version "6.8.1"))

(defun cond-add-elixir-credo ()
  "Add elixir-credo to lsp next checker if 'major-mode' is elixir-mode."
  (when (and (eq major-mode 'elixir-mode)
             (not (member 'elixir-credo (flycheck-get-next-checkers 'lsp))))
    (flycheck-add-next-checker 'lsp 'elixir-credo)))

(use-package lsp-mode
  :defer t
  :ensure t
  ;; :after (lsp-ui)
  :init
    (setq lsp-elixir-server-command '("language_server.sh"))
    (setq gc-cons-threshold 100000000)
    (setq read-process-output-max (* 1024 1024)) ;; 1mb
    ;; (add-to-list 'exec-path (concat user-emacs-directory "kotlin-ls/bin"))
  :config
    (setq lsp-idle-delay 0.500)
    (setq lsp-log-io nil)
    (setq lsp-headerline-breadcrumb-enable t)
    (setq lsp-enable-file-watchers nil)
    (setq lsp-elixir-suggest-specs nil)
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-modeline-diagnostics-enable t)
    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook
    (kotlin-mode . lsp)
    (elixir-mode . (lambda() (direnv-update-environment) (lsp)))
    (elixir-ts-mode . (lambda() (direnv-update-environment) (lsp)))
    (elm-mode    . lsp)
    (java-mode   . lsp)
    (tsx-ts-mode . (lambda() (message "running lsp hook tsx-ts-mode") (lsp)))
    (tsx-mode . (lambda() (message "running lsp hook tsx-mode") (lsp)))
    (typescript-ts-base-mode . (lambda() (message "running lsp hook typescript-ts-base-mode") (lsp)))
    (typescript-mode . lsp)
    (python-mode . lsp)
    (python-ts-mode . lsp)
    (lsp-mode    . lsp-enable-which-key-integration)
    (lsp-mode . (lambda () (evil-local-set-key 'normal (kbd "gr") 'lsp-find-references)))
    (lsp-diagnostics-updated . cond-add-elixir-credo)
  :commands (lsp))

(use-package lsp-ui
  :ensure t)

(use-package lsp-pyright
  :defer t
  :ensure t
  :config
  (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))


(use-package marginalia
  :defer t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package magit
  :defer t
  :ensure t
  :after (evil)
  :config (setq magit-list-refs-sortby "-committerdate")
  :hook
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

(use-package magit-todos :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t
  :config (setq tab-width 2)
  :hook
  (markdown-mode . auto-fill-mode)
  (markdown-mode . display-line-numbers-mode)
  (markdown-mode . (lambda() (setq-local fill-column 80)))
  (markdown-mode . company-mode))

(use-package midnight :ensure t)

(use-package evil-multiedit
  :after (evil) :ensure t :config (evil-multiedit-default-keybinds))

(use-package nix-mode
  :defer t
  :ensure t
  :mode "\\.nix\\'")

(use-package orderless
  :ensure t
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; org-mode
;; (require 'org)
;; (add-hook 'org-mode-hook 'auto-revert-mode)
;; (add-to-list 'org-modules 'org-habit)
;; (setq org-directory "~/gtd")
;; (setq org-default-notes-file "~/gtd/inbox.org")
;; (setq org-agenda-files "~/.emacs.d/org-agenda-files")
;; (setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-use-outline-path 'file)
;; (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
;; (setq org-todo-keywords
;;       '((sequence "TODO" "IN-PROGRESS" "DONE")))

;; (require 'org-capture)
;; (global-set-key "\C-cc" 'org-capture)
;; (setq org-capture-templates `(("t" "TODO" entry (file "inbox.org")
;;                                ,(concat "* TODO %?\n"
;;                                         "  %U\n"
;;                                         "  %a"))
;;                               ("h" "Habit" entry (file "habits.org")
;;                                ,(concat "* TODO %?\n"
;;                                         "  SCHEDULED: %t\n"
;;                                         "  :PROPERTIES:\n"
;;                                         "  :STYLE: habit\n"
;;                                         "  :END:"))))

;; (require 'org-clock)
;; (setq org-clock-idle-time 10)

;; (require 'org-agenda)
;; (global-set-key "\C-ca" 'org-agenda)
;; (setq org-agenda-window-setup 'current-window)
;; (setq org-agenda-todo-ignore-scheduled 'future)

;; (use-package org-present
;;   :ensure t
;;   :after evil
;;   :hook
;;   ((org-present-mode . (lambda ()
;; 			      (evil-mode 0)
;; 			      (hide-mode-line-mode 1)
;; 			      (org-present-big)
;; 			      (org-display-inline-images)
;; 			      (org-present-hide-cursor)
;; 			      (org-present-read-only)))
;;   (org-present-mode-quit . (lambda ()
;; 			     (evil-mode 1)
;; 			     (hide-mode-line-mode 0)
;; 			     (org-present-small)
;; 			     (org-remove-inline-images)
;; 			     (org-present-show-cursor)
;; 			     (org-present-read-write)))))

(use-package projectile
  :defer t
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'auto)
  (projectile-mode)
  (projectile-tags-exclude-patterns))

(use-package protobuf-mode :defer t :ensure t)

(defvar last-qtest-buffer nil
  "Store the last buffer used for 'qtest' command.")

(defun qtest (buffer)
  "Run `make qtest` in the selected or the last-used shell buffer."
  (interactive
   (list (completing-read
          "Select shell buffer: "
          (mapcar (lambda (buf)
                    (buffer-name buf))
                  (seq-filter (lambda (buf)
                                (with-current-buffer buf
                                  (derived-mode-p 'shell-mode)))
                              (buffer-list)))
          nil
          t
          (if (bufferp last-qtest-buffer)
              (buffer-name last-qtest-buffer)
            "*shell-1*"))))
  (let ((test-path (buffer-file-name)))
    (pop-to-buffer buffer)
    (goto-char (point-max))
    (insert (concat "make qtest t=" test-path))
    (comint-send-input)
    (setq last-qtest-buffer (get-buffer buffer))))

(global-set-key (kbd "C-c q") 'qtest)

(use-package restclient
  :defer t
  :ensure t
  :config
  (setq restclient-inhibit-cookies t)
  (autoload 'jq-set-var "jq-mode.el")
  :mode ("\\.http\\'" . restclient-mode))

(use-package ripgrep :ensure t :defer t)

(use-package rubocop :ensure t :defer t)


;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (setq powerline-default-separator 'contour)
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   (spaceline-spacemacs-theme))

(use-package swift-mode :ensure t :defer t)

(use-package string-inflection :ensure t :defer t)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;; deprecated - use typescript-ts-mode
;; (use-package typescript-mode
;;   :defer t
;;   :ensure t
;;   :config (setq typescript-indent-level 2))

;; (use-package undo-fu :ensure t)
;; (use-package undo-tree
;;   :ensure t
;;   :config (global-undo-tree-mode))

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package savehist
  :init
  (savehist-mode))

(use-package solarized-theme
             :defer t
             :ensure t)

(use-package web-mode
  :defer t
  :ensure t
  :mode (("\\.erb\\'" . web-mode) ("\\.html\\'" . web-mode))
  :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
  )

(use-package wgrep :ensure t :defer t)

(use-package wgrep-ag
  :defer t
  :ensure t
  :after wgrep)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package yaml-mode
  :defer t
  :ensure t
  :hook (yaml-mode . display-line-numbers-mode))

(use-package yari :ensure t :defer t)

(use-package zenburn-theme
             :defer t
             :ensure t
             :init
             ;; use variable-pitch fonts for some headings and titles
            (setq zenburn-use-variable-pitch t)
            ;; scale headings in org-mode
            (setq zenburn-scale-org-headlines t)
            ;; scale headings in outline-mode
            (setq zenburn-scale-outline-headlines t))

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

(defun copy-project-relative-path ()
  "Copy the current buffer file name relative to project root."
  (interactive)
  (let ((filename (file-relative-name
                  buffer-file-name (projectile-project-root))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard" filename)))


(defun create-python-import ()
  (interactive)
  (let ((import (concat (concat "from " (replace-regexp-in-string "/" "."
                   (replace-regexp-in-string "\.py" ""
                   (file-relative-name
                  buffer-file-name (projectile-project-root))))) " import")))
    (kill-new import)
    (message "Copied '%s' to the clipboard" import)))

(defun copy-path ()
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
;; (global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

;; general
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(setq-default fill-column 80)
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
 '(custom-safe-themes
   '("d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(protobuf-mode diff-hl-mode git-gutter csv-mode csv treesit-auto evil-multiedit hl-todo embark-consult hl-todo-modo zenburn-theme yari yaml-mode ws-butler which-key wgrep-ag web-mode swift-mode string-inflection spaceline solarized-theme rubocop ripgrep restclient projectile org-present orderless nix-mode marginalia lsp-ui lsp-pyright lsp-origami lsp-java kotlin-mode json-mode jq-mode gruvbox-theme groovy-mode graphql-mode go-mode git-link flycheck flx-ido exec-path-from-shell evil-surround evil-org evil-matchit evil-collection erlang elm-mode elixir-ts-mode elixir-mode dumb-jump direnv consult company browse-kill-ring auto-package-update auctex ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
