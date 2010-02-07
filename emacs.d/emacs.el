;;; Main Emacs configuration file

; Add the .emacs.d directory to the load path
(setq load-path (cons "~/.emacs.d/" load-path))

; Enable Viper
(setq viper-mode t)
(setq viper-custom-file-name "~/.emacs.d/viper-config.el")
(require 'viper) 

; Scroll line-by-line
(setq scroll-margin 3
      scroll-step 1 
      scroll-conservatively 10000)

; No startup message or beep
(setq inhibit-startup-message t)
(setq visible-bell t)

; Title bar text
(setq frame-title-format "Emacs: %b")

; Highlight matching parens
(show-paren-mode)

; Window management
(windmove-default-keybindings)
(mouse-wheel-mode t)

; Store customizations in a different file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load "~/.emacs.d/emacs-custom.el")

; Show line numbers
(setq line-number-mode t)
(global-linum-mode 1)
(set-face-attribute 'linum nil :inherit 'shadow :background "black" :foreground "red3")

; Enable ido
(require 'ido)
(ido-mode t)

;; Syntax highlighting
(global-font-lock-mode t)
(require 'color-theme)
(color-theme-initialize)
(color-theme-hober)

;; Add some font-lock-mode keywords for lisp macros
(font-lock-add-keywords 'lisp-mode
  '(("(\\(\\(define-\\|do-\\|with\\|fn\\|bind\\)\\(\\s_\\|\\w\\)*\\)" 
   1 font-lock-keyword-face)))

;; Make dabbrev ignore $ and . characters
(setq dabbrev-abbrev-skip-leading-regexp "\\(\\$\\.\\)")


;; Code folding
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(define-key global-map "\C-ch" 'hs-hide-all)
(define-key global-map "\C-cz" 'hs-toggle-hiding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Common Lisp       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up SLIME
(add-to-list 'load-path "/usr/share/common-lisp/source/slime/")
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     C, C++, Java      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load JDEE
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site/jde/lisp"))
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site/cedet/common"))
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site/elib"))
;; (load-file (expand-file-name "~/.emacs.d/site/cedet/common/cedet.el"))

;; Compilation and testing
(defun run-compile ()
  (interactive)
  (cond 
   ((eq major-mode 'c-mode) (compile "make -s"))
   (t (compile "make -s"))))

(defun run-program ()
  (interactive)
  (cond 
   ((eq major-mode 'c-mode) (shell-command "~/programming/Lang/bin/ionno ~/programming/Lang/input.ion"))
   (t (shell-command "bin/ionno input.ion"))))


;; Lisp functions
(defun header-guard (name)
  " Wrap the current buffer in a C preproccesor #ifndef guard "
  (interactive "sName: ")
  (let ((buf (current-buffer)))
    (princ (format "#ifndef %s\n#define %s\n\n#endif /* %s */" name name name) buf)
    (goto-line 3)))

(defun ls ()
  " Shows contents of working directory. "
  (interactive)
  (shell-command "ls"))

;; Define keymaps
(define-key global-map "\C-ca" 'pattern-command)
(define-key global-map "\C-cq" 'run-compile)
(define-key global-map "\C-cw" 'run-program)
(define-key global-map "\C-x\M-c" 'butterfly)

; I like butterflies!
(defun butterfly (butterfly)
  (interactive "sButterfly: ")
  (princ "Butterfly!"))

