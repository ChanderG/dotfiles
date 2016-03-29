(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Due to conflict with dwm -> swap meta and super
(setq x-meta-keysym 'super)
(setq x-super-keysym 'meta)

;; Add a closing time stamp to TODO entries
(setq org-log-done 'time)

;; Change backups file name
(defun make-backup-file-name (filename)
  (expand-file-name
    (concat "." (file-name-nondirectory filename) "~")
    (file-name-directory filename)))

;; font and size
(set-default-font "Monospace 18")

;; set browser to use for opening
(setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "opera")

;; for pomodoro in orgmode clocks
(defun pomodoro-start ()
  "Starts and automatically clocks out a Pomodoro unit of 20 minutes."
  (interactive)
  (org-clock-in)
  (message "Starting pomodoro cycle of 20 minutes.")
  (set-process-sentinel (start-process "sleep" nil "sleep" "20m") 'pomodoro-end)
)

(defun pomodoro-end (process event)
  (org-clock-out)
  (message "Stopping pomodoro cycle of 20 minutes.")
  (start-process "slock" nil "slock")
)

(global-set-key '[f4] 'pomodoro-start)

;; simplifying clock-in / clock-out
(global-set-key '[f5] 'org-clock-in)
(global-set-key '[f6] 'org-clock-out)

;; export to html; don't open
(global-set-key '[f7] (lambda () 
			(interactive)
			(org-export-dispatch '("h" "h"))))

;;syntax highlight code blocks
(setq org-src-fontify-natively t)
