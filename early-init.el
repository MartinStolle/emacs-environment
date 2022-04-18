;;; early-init.el --- Emacs early-init.el            -*- lexical-binding: t; -*-
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; 10GB GC threshold during init
(setq gc-cons-threshold (* 10000 1024 1024))
(setq load-prefer-newer t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Load time: %s (with %d garbage collections)."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
                                                        before-init-time)))
                     gcs-done)))

;; Turn off mouse interface early in startup to avoid momentary display
(tool-bar-mode -1)
;; Hide scrollbar
(menu-bar-mode -1)
;; Hide scrollbar
(scroll-bar-mode -1)

(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)
(setq custom-safe-themes t)

(setq package-enable-at-startup nil)
(setq package-native-compile t)

(provide 'early-init)
;;; early-init.el ends here
