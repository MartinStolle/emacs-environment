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
;; default directory where all my projects are
(defvar default-project-directory "e:/git/")

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
;; Setting this variable to a small integer will reduce the size of repositories. This variable affects all packages, even those whose versions are locked.
(setq straight-vc-git-default-clone-depth 1)

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; Display the current column with
(setq column-number-mode t)

;; Set up defaults for the Latin-1 character set, which supports most of the languages of Western Europe.
(set-language-environment "Latin-1")

;; Recentf is a minor mode that builds a list of recently opened files.
(use-package recentf
  :straight (:type built-in)
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25))

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

;; use single emacs instance
(require 'server)
(unless (server-running-p)
  (server-start))

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

;; Behold, the mighty hydra, https://github.com/abo-abo/hydra
(use-package hydra
  :straight t
  :defer t
  )

;; add hydra to ibuffer
;; https://github.com/abo-abo/hydra/wiki/Ibuffer
(use-package ibuffer
  :straight (:type built-in)
  :config
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ↑   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    ↓   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)
    ("k" ibuffer-backward-line)

    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)

    ("D" ibuffer-do-delete)
    ("S" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("s" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)

    ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
    ("q" quit-window "quit ibuffer" :color blue)
    ("." nil "toggle hydra" :color blue))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
  )

;; https://github.com/joaotavora/yasnippet type an abbreviation and automatically expand it into function templates
(use-package yasnippet
  :straight t
  :after python
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  )

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
  ;; ls parameters:
  ;; -l     use a long listing format
  ;; -A, --almost-all
  ;;        do not list implied . and ..
  ;; -h, --human-readable
  ;;        with -l and -s, print sizes like 1K 234M 2G etc.
  ;; -p, --indicator-style=slash
  ;;        append / indicator to directories
  (dired-listing-switches "-lAhp --group-directories-first")
  (setq dired-dwim-target t)
  :config
  ;; Dired in single buffer (prevent dired from opening a lot of buffers)
  (put 'dired-find-alternate-file 'disabled nil)
  (defhydra hydra-dired (:hint nil :color pink)
    "
^Mark^                   ^Actions^         ^Control^                 ^Movement^       ^Search^
^^^^^^^------------------------------------------------------------------------------------------------
_m_: mark                _+_: create dir   _(_: hide details         _p_: up dir      _S_: search files
_u_: unmark              _R_: rename       _g_: Refresh buffer       _n_: down dir    _I_: search content
_U_: unmark all          _C_: copy         _s_: toggle sort by date  _i_: view subdir ^ ^
_t_: toggle marks        _M_: chmod        _$_: summary              _$_: hide subdir ^ ^
_t_: mark using regexp   _Z_: compress     _<_: undo                 ^ ^              ^ ^
_/_: mark all dirs       _D_: delete       _v_: view                 ^ ^              ^ ^
^ ^                      _=_: diff         ^ ^                       ^ ^              ^ ^
"
    ("m" dired-mark)
    ("u" dired-unmark)
    ("U" dired-unmark-all-marks)
    ("t" dired-toggle-marks)
    ("E" dired-mark-files-regexp)
    ("/" dired-mark-directories)
    ("+" dired-create-directory)
    ("R" dired-do-rename)
    ("C" dired-do-copy)
    ("Z" dired-do-compress)
    ("D" dired-do-delete)
    ("M" dired-do-chmod)
    ("=" dired-diff)
    ("(" dired-hide-details-mode)
    ("g" revert-buffer)
    ("s" dired-sort-toggle-or-edit)
    ("?" dired-summary)
    ("<" dired-undo)
    ("v" dired-view-file)
    ("p" dired-tree-up)
    ("n" dired-tree-down)
    ("i" dired-maybe-insert-subdir)
    ("$" dired-hide-subdir)
    ("S" dired-isearch-filenames)
    ("I" dired-do-isearch)
    ("q" quit-window "quit" :color blue)
    ("." nil "toggle hydra" :color blue))
  (define-key dired-mode-map "." 'hydra-dired/body)
  )

;; Prerequisite for a few packages (e.g. all-the-icons-dired)
;; "M-x all-the-icons-install-fonts" to install fonts at the first time.
(straight-use-package 'all-the-icons)
;; https://github.com/jtbm37/all-the-icons-dired dired with nice icons
(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package python
  :straight (:type built-in)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :bind
  ( :map python-mode-map
    ("C-c r" . python-indent-shift-right)
    ("C-c l" . python-indent-shift-left))
  :hook
  ;; NOTE: these hooks runs in reverse order
  ;; move recently and frequently used candidates to the top of the completions list, but otherwise leave candidate ordering alone.
  (python-mode . (lambda () (setq-local company-prescient-sort-length-enable nil)))
  (python-mode . lsp-deferred)
  ;; (python-mode . fk/activate-pyvenv)
  )

(use-package pyvenv
  :straight t
  :after python
  )

;;
;; IDE
;;

;; https://docs.projectile.mx/ Projectile is a project interaction library
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path '((default-project-directory . 2)))
  (add-to-list 'projectile-globally-ignored-directories "^\\.venv$")
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

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
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :straight t
  :after lsp)

;; Debug Adapter Protocol https://github.com/emacs-lsp/dap-mode
;; requires previous pip install 'python-lsp-server[all]' https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;; dap mode also need pip install debugpy
(use-package dap-mode
  :straight t
  :init
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
  :custom
  (lsp-enable-dap-auto-configure nil)
  :hook
  (python-mode . (lambda () (require 'dap-python)))
  :config
  (dap-ui-mode 1)
  ;; we use hydra, these dont work anyway
  (dap-ui-controls-mode -1)
  :commands dap-debug)

;; http://company-mode.github.io/
;; text completion framework
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

;; Load my nice theme
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-nord t))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner "~/.emacs.d/static/GnuHead.png")
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  )

;; A better, more detailed help buffer.
(use-package helpful
  :straight t
  :custom
  ;; Use helpful in `helm-apropos'
  (helm-describe-function-function 'helpful-function)
  (helm-describe-variable-function 'helpful-variable)
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

;; add additional init files
(push (expand-file-name "custom" user-emacs-directory) load-path)

(require 'init-org)
(require 'init-eshell)

(provide 'init)
;;; init.el ends here
