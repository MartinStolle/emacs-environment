;; Milkypostman’s Emacs Lisp Package Archive
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; ido-mode allows many operations (like buffer switching and file navigation) to be enhanced with
;; instant feedback among the completion choices
(ido-mode t)
(setq ido-enable-flex-matching t)

;; The uniquify library makes it so that when you visit two files with the same name in different directories,
;; the buffer names have the directory name appended to them
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; The saveplace library saves the location of the point when you kill a buffer and returns to it next time
(server-start)

;; you visit the associated file.
(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; show-paren-mode highlights the matching pair when the point is over parentheses.
(show-paren-mode 1)

;; set font
(set-default-font "12")
(set-face-attribute 'default nil :font "Ubuntu Mono-12")

;; Python IDE
;;(add-hook 'after-init-hook #'global-flycheck-mode)

(elpy-enable)

(setq-default flycheck-flake8-maximum-line-length 120)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "885ef8634f55df1fa067838330e3aa24d97be9b48c30eadd533fde4972543b55" "e24180589c0267df991cf54bf1a795c07d00b24169206106624bb844292807b9" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "e02ba8d4d8165ae7d9f3f8da11483c3273c8d794fddbf798caf381c6941a6142" "3164a65923ef23e0f3dff9f9607b4da1e07ef1c3888d0f6878feef6c28357732" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'twilight)
;;(setq exec-path (cons "D:/Tools/Go/" exec-path))

;; everything go related
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'after-save-hook 'gofmt)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c i") 'go-goto-imports)))
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") 'godef-jump)))

(add-to-list 'load-path "~/.emacs.d/modes")
;; golang autocomplete
(require 'go-autocomplete)
(require 'auto-complete-config)
(setq ac-auto-start nil)
; M-/ still triggers emacs-style autocomplete, TAB gives a fancy menu
(ac-set-trigger-key "TAB")
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'go-mode)

(require 'go-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;(setq exec-path (append exec-path '(getenv "GOBIN")))
(require 'gofmt-replace)

(provide 'init)
;;; init.el ends here
