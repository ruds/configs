(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '3)
(setq viper-want-ctl-h-help t)
(setq viper-fast-keyseq-timeout 0)
(setq viper-no-multiple-ESC t)
(defun viper-translate-all-ESC-keysequences () t)

(define-key viper-vi-global-user-map "\C-v" 'scroll-up)
