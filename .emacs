;;; -*- lexical-binding: t -*-
; for evil key binding

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
;; toggle clock in and out
(global-set-key '[f5] (lambda () 
			(interactive)
			(if (org-clocking-p)
			  (org-clock-out)
			  (org-clock-in)
			)))

(global-set-key '[f6] 'pomodoro-start)

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

;(load (expand-file-name "~/Documents/Dabblings/CL/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; disable menu bar
(menu-bar-mode 0)

(require 'evil)
(evil-mode 1)

; helm
(require 'helm-config)
(helm-mode 1)

;Use space to toggle between normal mode and emacs mode
(define-key evil-normal-state-map " " 'evil-emacs-state)
(define-key evil-emacs-state-map " " 'evil-exit-emacs-state)

;; easy to access commands
(define-key evil-normal-state-map (kbd ";") 'helm-M-x)
(define-key evil-emacs-state-map (kbd ";") 'helm-M-x)

;; normal map single key translations for orgmode
; life saver
(define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
(define-key evil-normal-state-map (kbd "<") 'org-metaleft)
(define-key evil-normal-state-map (kbd ">") 'org-metaright)
(define-key evil-normal-state-map (kbd "O") 'org-insert-heading)

;; multi key translation setup in normal mode

;; Note: lexical-binding must be t in order for this to work correctly.
(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
  key-from translates to key-to, else key-from translates to itself.  translate-keys-p
  takes key-from as an argument. "
  (define-key key-translation-map key-from
	      (lambda (prompt)
		(if (funcall translate-keys-p key-from) key-to key-from))))

(defun my-translate-keys-p (key-from)
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and
    ;; Only allow a non identity translation if we're beginning a Key Sequence.
    (equal key-from (this-command-keys))
    (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))))

;(define-key evil-normal-state-map "c" nil)
;(define-key evil-motion-state-map "cu" 'universal-argument)
;(make-conditional-key-translation (kbd "ch") (kbd "C-h") 'my-translate-keys-p)
(make-conditional-key-translation (kbd "g") (kbd "C-x") 'my-translate-keys-p)
;(make-conditional-key-translation (kbd ";") (kbd "M-x")'my-translate-keys-p)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)
   (lisp . t)
   (emacs-lisp . t)
   ))

;; disable confirmation upon C-c C-c in org-babel
(setq org-confirm-babel-evaluate nil)

;; notify after execution of source blocks finish
(add-hook 'org-babel-after-execute-hook (lambda ()
					  (interactive)
					  (start-process "Notification" nil "notify-send" "Emacs" "Evaluation of src block finished.")
						))

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


;; no confirm shell links for nice button like behaviour
(setq org-confirm-shell-link-function nil)
;; agenda view sizes
(setq org-agenda-window-frame-fractions '(0.25 . 0.40))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/orgmode/time.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
