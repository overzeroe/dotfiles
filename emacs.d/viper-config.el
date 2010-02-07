;; Viper customization file

; No startup message
(setq viper-inhibit-startup-message 't)

; Allow complete merging of Emacs and Vi
(setq viper-expert-level '5)

; :set ignorecase
(setq viper-case-fold-search t)

; Map C-e to end-of-line, not scroll-up
(put 'viper-scroll-up-one 'disabled t)


