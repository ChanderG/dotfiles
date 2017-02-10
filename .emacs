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
(global-set-key '[f4] (lambda () 
		       (interactive)
		       (org-columns)
		       ))

;; simplifying clock-in / clock-out
(global-set-key '[f5] 'org-clock-in)
(global-set-key '[f6] 'org-clock-out)

;;syntax highlight code blocks
(setq org-src-fontify-natively t)

;; ctrl arrow based buffer navigation
(global-set-key '[C-left] 'next-buffer)
(global-set-key '[C-right] 'previous-buffer)

;; quick buffer specific todos
;; list waiting stuff in column view
(global-set-key '[f7] (lambda () 
			(interactive)
			(org-agenda-set-restriction-lock "file")
			(org-todo-list 1)
			(org-agenda-columns)
			))

;; list todos with keyword TODO (assumed second in TODOS property)
;; use column view
(global-set-key '[f8] (lambda () 
			(interactive)
			(org-agenda-set-restriction-lock "file")
			(org-todo-list 2)
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

;; add post save hook to update dynamic blocks and clocks
(add-hook 'before-save-hook 'org-update-all-dblocks)
(add-hook 'before-save-hook 'org-table-recalculate-buffer-tables)

;; do the same on autosaves;
(add-hook 'auto-save-hook 'org-update-all-dblocks)
(add-hook 'auto-save-hook 'org-table-recalculate-buffer-tables)

;; save automatically 
;; runs on tab switch etc
(add-hook 'focus-out-hook (lambda ()
			    (interactive)
			    (save-buffer)
				  ))

(load (expand-file-name "~/Documents/Dabblings/CL/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(require 'evil)
(evil-mode 1)

;Use space to toggle between normal mode and emacs mode
(define-key evil-normal-state-map " " 'evil-emacs-state)
(define-key evil-emacs-state-map " " 'evil-exit-emacs-state)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)
   (lisp . t)
   (emacs-lisp . nil)
   ))

;; disable confirmation upon C-c C-c in org-babel
(setq org-confirm-babel-evaluate nil)

;; zoom in and out of headings in orgmode
(global-set-key (kbd "C-<next>") (lambda ()
				   (interactive)
				   (outline-next-visible-heading 0)
				   (org-narrow-to-subtree)))

(global-set-key (kbd "C-<prior>") (lambda ()
				    (interactive)
				    (widen)
				    (outline-up-heading 1)
				    (org-narrow-to-subtree)))

(defun read-clocks ()
  (with-current-buffer (find-file "~/orgmode/time.org")
    (let* ((entries (nth 2 (org-clock-get-table-data "~/orgmode/time.org" nil)))
	   (no (position "Sprints" entries :test (lambda (x y) (string= x (nth 1 y)))))
	   (e (nthcdr no entries)))
      (with-temp-file "~/orgmode/clockinfo"
	(insert (mapconcat
		 (lambda (x) (concat (number-to-string (nth 3 x)) "," (nth 1 x)))
		 e
		 "\n"))))))


