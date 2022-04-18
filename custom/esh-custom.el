;; esh-custom.el from https://gist.github.com/jav-solo/d8a0805c04a5fcb90e1fdbc7ba8fbaa2
;; Emacs custom shell that I used to customize the eshell prompt on my Mac running macOS Big Sur
;; I copied this file in my .emacs.d/eshell/ directory and load the file in my profile file in the same directory
;; Note: You must install packages `dash`, `s`, `magit`, and `all-the-icons` and then run `M-x all-the-icons-install-fonts`
;; I did so using MELPA and it worked fine
;; Please see https://github.com/domtronn/all-the-icons.el for usage of icons

(require 'dash)
(require 's)
(require 'magit)
(require 'all-the-icons)

;; Use 'prepend for the NS and Mac ports or Emacs will crash.
(set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'prepend)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                    (concat esh-section-delim ,FORM)
                    (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

(esh-section esh-dir
             nil
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "gold"))

(esh-section esh-git
             (all-the-icons-alltheicon "git")
             (magit-get-current-branch)
             '(:foreground "pink"))

(esh-section esh-clock
             (all-the-icons-alltheicon "terminal")
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

(esh-section esh-python
             (all-the-icons-alltheicon "python")
             pyvenv-virtual-env-name)

;; Separator between esh-sections
(setq esh-sep " | ")

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header "\n┌─")

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp "└─>")
(setq eshell-prompt-string "└─>")

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-clock esh-python))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)
