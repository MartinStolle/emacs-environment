;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org>

;;; Code:
;; Milkypostman’s Emacs Lisp Package Archive

;; straight.el package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; Turn off mouse interface early in startup to avoid momentary display
(tool-bar-mode -1)
;; Hide scrollbar
(menu-bar-mode -1)
;; Hide scrollbar
(scroll-bar-mode -1)
;; Display the current column with
(setq column-number-mode t)

;; Recentf is a minor mode that builds a list of recently opened files.
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Purpose: When you visit a file, point goes to the last place where it was when you previously visited the same file.
(save-place-mode 1)
;; Auto revert after you changed the file in Emacs
(global-auto-revert-mode 1)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Do ding !
(setq visible-bell t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; The variable indent-tabs-mode controls whether tabs are used for indentation.
(setq-default indent-tabs-mode nil)

;; ido-mode allows many operations (like buffer switching and file navigation) to be enhanced with
;; instant feedback among the completion choices
(ido-mode t)
(setq ido-enable-flex-matching t)

;; The uniquify library makes it so that when you visit two files with the same name in different directories,
;; the buffer names have the directory name appended to them
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; The saveplace library saves the location of the point when you kill a buffer and returns to it next time
(require 'server)
(unless (server-running-p)
  (server-start))

;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
(require 'saveplace)
(setq-default save-place t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; show-paren-mode highlights the matching pair when the point is over parentheses.
(show-paren-mode 1)

;; supercool multiline edit
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; mouse scroll
(global-set-key (kbd "<mouse-4>") 'scroll-up)
(global-set-key (kbd "<mouse-5>") 'scroll-down)

(straight-use-package 'use-package)

;; Auto update packages - still not sure if settings will work in corporate environment
;; (use-package auto-package-update
;;   :straight t
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00"))

;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :straight t
  :bind ("C-c SPC" . ace-jump-mode))

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window))

;; dont automatically save and restore undo-tree history along with buffer
(use-package undo-tree
  :straight t
  :init
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

;; https://github.com/emacs-helm/
(use-package helm
  :straight t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

;; git integration
(use-package magit
  :commands magit-status)

;; In split copy command copies files automatically to other window
(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  (setq dired-dwim-target t))

;; Org Mode add ons
(use-package org
  :straight (:type built-in)
  :custom
  (load "~/custom/custom.el")
  (org-add-link-type "gsep" 'make-gsep-link)
  (org-add-link-type "team" 'make-team-link)
  (org-add-link-type "jira" 'make-jira-link)
  (org-add-link-type "asam" 'make-asam-link)
  ;; auto indent mode for org mode just works against me!
  (electric-indent-mode -1))

;; minor syntax highlighting for eshell
(use-package eshell-syntax-highlighting
  :straight t
  :after eshell-mode
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

;; Load my nice theme
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-nord t))

;;
;; IDE
;;
;; which-key is a minor dependency for lsp-mode
(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; Language Server Protocol
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :straight t
  :after lsp)

;; https://github.com/10sr/with-venv-el
;; search for suitable venv directory for current evironment
(straight-use-package 'with-venv)

;; Debug Adapter Protocol https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :straight t
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Temporal fix
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))
  )

;; requires previous pip install 'python-lsp-server[all]' https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;; dap mode requires pip install "ptvsd>=4.2" https://emacs-lsp.github.io/dap-mode/page/configuration/#python
(use-package python-mode
  :straight t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python.exe")
  (dap-python-executable "python.exe")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :straight t
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package company
  :straight t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(provide 'init)
;;; init.el ends here
