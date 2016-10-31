(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "40c66989886b3f05b0c4f80952f128c6c4600f85b1f0996caa1fa1479e20c082" "9ab634dcc9131f79016c96c4955298409649f6538908c743a8a9d2c6bc8321ef" default)))
 '(fci-rule-color "#14151E")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "mediumspringgreen")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "goldenrod")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "mediumspringgreen")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "goldenrod")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "mediumspringgreen"))))
 '(vc-annotate-very-old-color nil))

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
            browse-url-generic-program "firefox")

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

;; diabled for now
;(global-set-key '[f4] 'pomodoro-start)

;; simplifying clock-in / clock-out
(global-set-key '[f5] 'org-clock-in)
(global-set-key '[f6] 'org-clock-out)

;; export to html; don't open
(global-set-key '[f7] (lambda () 
			(interactive)
			(org-export-dispatch '("h" "h"))))

;;syntax highlight code blocks
(setq org-src-fontify-natively t)

;; ctrl arrow based buffer navigation
(global-set-key '[C-left] 'next-buffer)
(global-set-key '[C-right] 'previous-buffer)

;; quick buffer specific todos
;; list todos with keyword TODO (assumed first in TODOS property)
;; use column view
(global-set-key '[f8] (lambda () 
			(interactive)
			(org-agenda-set-restriction-lock "file")
			(org-todo-list 1)
			(org-agenda-columns)
			))

;; dismiss custom todos view
(global-set-key '[f9] (lambda ()
			(interactive)
			(execute-kbd-macro (kbd "q"))
			(execute-kbd-macro (kbd "q"))
			))

;; for melpa
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
		'("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; add post save hook to update dynamic blocks
(add-hook 'before-save-hook 'org-update-all-dblocks)
