;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(defconst MY-FONT "Monospace-9")
(set-default-font MY-FONT)
(menu-bar-mode -1)                       ;; show the menu...
(mouse-avoidance-mode 'jump)             ;; mouse ptr when cursor is too close
(tool-bar-mode -1)                       ;; turn-off toolbar
(setq-default indent-tabs-mode nil)

(setq auto-save-default nil)             ;; disable auto save
(setq make-backup-files nil)             ;; disable creating backup files

(setq cua-enable-cua-keys nil)           ;; only for rectangles
(cua-mode t)

(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0               ;; ... are very ...
  scroll-down-aggressively 0             ;; ... annoying
  scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v 

(global-visual-line-mode 1)              ;; enable line wrapping
;; (global-linum-mode 1)                    ;; show line number

(scroll-bar-mode nil)                    ;; hide scroll bar

(setq fringe-mode '(1 . 0))              ;; emacs 22+
(delete-selection-mode 1)                ;; delete the sel with a keyp

(setq x-select-enable-clipboard t        ;; copy-paste should work ...
  interprogram-paste-function            ;; ...with...
  'x-cut-buffer-or-selection-value)      ;; ...other X clients

(setq search-highlight t                 ;; highlight when searching... 
  query-replace-highlight t)             ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no 

(setq initial-scratch-message
  ";; scratch buffer created -- happy hacking\n")

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

(put 'narrow-to-region 'disabled nil)    ;; enable...
(put 'erase-buffer 'disabled nil)        ;; ... useful things
(file-name-shadow-mode t)                ;; be smart about filenames in mbuf

(setq inhibit-startup-message t          ;; don't show ...    
  inhibit-startup-echo-area-message t)   ;; ... startup messages
(setq require-final-newline t)           ;; end files with a newline

;; Set default font and frame size
(setq default-frame-alist
      `(
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (tool-bar-lines . 0)
        (fullscreen . maximized)
        (font . ,MY-FONT)
        ))

;; slick-copy: make copy-past a bit more intelligent
;; from: http://www.emacswiki.org/emacs/SlickCopy
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively witand type RET to get full documentation.h no active region, copy a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (message "Copied line")
      (list (line-beginning-position)
               (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))
        

;; key board / input method settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal

        
;; set spliting window horizontally by default
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

;; default directory
(setq default-directory "~/Projects/AdobeSPLC/adobe_splc/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
;; 
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path "~/.emacs.d")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some handy packages
;;
;; dired-details
(require 'dired-details)
(dired-details-install)

;; hl-line: highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default


;; yasnippet
(add-to-list 'load-path
                 "~/.emacs.d/yasnippet-0.6.1c")
   (require 'yasnippet) ;; not yasnippet-bundle
   (yas/initialize)
   (yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")

;; (require 'yasnippet-bundle)             

;; org mode
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(require 'org-install)

;; auto-complete mode
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Tabbar
;; (require 'tabbar)
;; (tabbar-mode)

;; Icicles Mode (minibuffer completion)
;; (add-to-list 'load-path "~/.emacs.d/icicles/")
;; (require 'icicles)
;; (icy-mode 1)

;; ido mode
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-histroy nil)
(setq ido-confirm-unique-completion nil)
(setq ido-show-dot-for-dired t)
(setq ido-use-filename-at-point t)

;; Numberring windows
(require 'window-number)
(window-number-mode t)
(window-number-meta-mode t)

;; Autopair mode
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming languages

;; haml mode
(add-to-list 'load-path "~/.emacs.d/haml-mode")
(require 'haml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My key bindings

;; switch to next window
(global-set-key "\M-`" 'other-window)
(global-set-key "\C-x\C-f"
                (lambda ()
                  (interactive)
                  (setq default-directory my-current-project-path)
                  (my-create-new-window)
                  (ido-find-file)
                  ))

;; sensitivelly adjust current window
(global-set-key [f2]
	(lambda ()
		(interactive)
		(let ((buf (window-buffer))
					(win (selected-window))
					(largest-win (get-largest-window)))
			(unless (equal win largest-win)
        (let* ((left (nth 0 (window-inside-pixel-edges largest-win)))
              (top (nth 1 (window-inside-pixel-edges largest-win)))
              (right (nth 2 (window-inside-pixel-edges largest-win)))
              (bottom (nth 3 (window-inside-pixel-edges largest-win)))
              (width (- right left))
              (height (- bottom top)))
				(if (> width height)
						(progn
						(select-window (split-window largest-win nil t)))     ;; split horizontally
					(progn
						(select-window (split-window largest-win nil nil))))  ;; split vertically
        (set-window-buffer (selected-window) buf)
        (delete-window win))))))

;; my feature
;; create a new window and move current buffer to
(global-set-key [f5]
								(lambda ()
									(interactive)
									(let ((buf (window-buffer))
												(win (selected-window)))
                      (let* ((left (nth 0 (window-inside-pixel-edges win)))
                             (top (nth 1 (window-inside-pixel-edges win)))
                             (right (nth 2 (window-inside-pixel-edges win)))
                             (bottom (nth 3 (window-inside-pixel-edges win)))
                             (width (- right left))
                             (height (- bottom top)))
                        (if (> width height)
                            (progn
                              (select-window (split-window win nil t)))     ;; split horizontally
                          (progn
                            (select-window (split-window win nil nil))))  ;; split vertically
                        (set-window-buffer (selected-window) buf)
                        (set-window-buffer win (other-buffer))))))
                        


;; ==========================================================
      
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming
(autoload 'linum-mode "linum" "mode for line numbers" t) 
(global-set-key (kbd "C-<f5>") 'linum-mode)                 ;; line numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program shortcuts
(defun .emacs ()
  (interactive)
  (find-file "~/.emacs"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tab-width 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my rails project browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my-rails-project "~/Projects/AdobeSPLC/adobe_splc/")
(setq my-rails-project-app (concat my-rails-project "app/"))
(setq my-rails-project-controllers (concat my-rails-project "app/controllers/"))
(setq my-rails-project-views (concat my-rails-project "app/views/"))
(setq my-rails-project-models (concat my-rails-project "app/models/"))
(setq my-rails-project-config (concat my-rails-project "config/"))
(setq my-rails-project-db (concat my-rails-project "db/"))
(setq my-rails-project-migrations (concat my-rails-project "db/migrate/"))
(setq my-rails-project-javascripts (concat my-rails-project "public/javascripts"))
(setq my-rails-project-stylesheets (concat my-rails-project "public/stylesheets"))

;; dired my project
(defun .rproject ()
	(interactive)
	(setq default-directory my-rails-project)
	(call-interactively 'find-file))

;; dired my app
(defun .rapp ()
	(interactive)
	(setq default-directory my-rails-project-app)
	(call-interactively 'find-file))

;; dired my controller
(defun .rc ()
	(interactive)
	(setq default-directory my-rails-project-controllers)
	(call-interactively 'find-file))

;; dired my config
(defun .rcf ()
	(interactive)
	(setq default-directory my-rails-project-config)
	(call-interactively 'find-file))

;; dired my db
(defun .rdb ()
	(interactive)
	(setq default-directory my-rails-project-db)
	(call-interactively 'find-file))

;; open environment.rb
(defun .renvironment.rb ()
	(interactive)
	(find-file (concat my-rails-project-config "environment.rb")))

;; dired my javascripts
(defun .rjavascripts ()
	(interactive)
	(setq default-directory my-rails-project-javascripts)
	(call-interactively 'find-file))

;; dired my models
(defun .rm ()
	(interactive)
	(setq default-directory my-rails-project-models)
	(call-interactively 'find-file))

;; dired my migrations
(defun .rmi ()
	(interactive)
	(setq default-directory my-rails-project-migrations)
	(call-interactively 'find-file))

;; open environment.rb
(defun .rroutes.rb ()
	(interactive)
	(find-file (concat my-rails-project-config "routes.rb")))

;; dired my views
(defun .rviews ()
	(interactive)
	(setq default-directory my-rails-project-views)
	(call-interactively 'find-file))

;; dired my stylesheets
(defun .rstylesheets ()
	(interactive)
	(setq default-directory my-rails-project-stylesheets)
	(call-interactively 'find-file))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My new features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; navigate file and open in another window
(global-set-key "\M-o" 'my-explorer)

;; my current project path
(defvar my-current-project-path "~/Projects/MagooshExam/magoosh_exam/")
(defvar my-current-window-config)

;; set my current project path
(defun my-set-current-project-path (path)
	(interactive "sNew project path: ")
	(setq my-current-project-path path))

;; my save windows configuration
(defun my-save-windows-config ()
  (setq my-current-window-config (current-window-configuration))
  )

;; my restore saved windows configuration
(defun my-restore-windows-configuration ()
  (set-window-configuration my-current-window-config)
  )

;; my project path concatenation
(defun my-concat-with-project-path (path)
  (concat my-current-project-path path)
  )

;; my initial completion list 
(setq my-completion-list nil)
(add-to-list 'my-completion-list (cons "controllers" (my-concat-with-project-path "app/controllers/")))
(add-to-list 'my-completion-list (cons "views" (my-concat-with-project-path "app/views/")))
(add-to-list 'my-completion-list (cons "models" (my-concat-with-project-path "app/models/")))
(add-to-list 'my-completion-list (cons "user.rb" (my-concat-with-project-path "app/models/user.rb")))

;; add a shortcut to completion list
(defun my-add-a-shortcut (alias path)
	(interactive "sAlias: \nsPath: ")
	(add-to-list 'my-completion-list (cons alias (my-concat-with-project-path path))))

;; remove a shortcut
(defun my-remove-a-shortcut (alias)
	(interactive "sShortcut Alias: ")
	(setq my-completion-list (assq-delete-all alias my-completion-list)))

;; my spliting window properly
(defun my-split-window (w)
	(let* ((left (nth 0 (window-inside-pixel-edges w)))
				 (top (nth 1 (window-inside-pixel-edges w)))
				 (right (nth 2 (window-inside-pixel-edges w)))
				 (bottom (nth 3 (window-inside-pixel-edges w)))				 
				 (width (- right left))
				 (height (- bottom top)))
		
		(if (> width height)
				(split-window w nil t)     ;; split horizontally
			(split-window w nil nil)))  ;; split vertically
)

;; my creating new window
(defun my-create-new-window ()
  (select-window (my-split-window (get-largest-window))))
											
;; display the initial completion list and input prompt
(defun my-navigator ()
  ;; display the completion list
  (interactive)
  (let (selection path)
    (setq w (get-largest-window))
    (setq selection (ido-completing-read "Select: " my-completion-list))
    (setq path (cdr (assoc selection my-completion-list)))

    (if (file-directory-p path)
        (progn
          (setq default-directory path)
          (ido-find-file))
      (progn
				(my-split-window w)
        (find-file path)
        )
       )
    )
  )

(defun my-explorer (request)
	(interactive "sWhat do you want? ")
  (let (path)
    (cond ((string-match "\\(.+\\) model$" request)
           (setq path (concat my-current-project-path "app/models/" (match-string 1 request) ".rb"))
           (my-create-new-window)
           (find-file path))
          
          ((string-match "\\(.+\\) controller$" request)
           (setq path (concat my-current-project-path "app/controllers/" (match-string 1 request) ".rb"))
           (my-create-new-window)
           (find-file path))
          
          ((string-match "^\\(models\\|views\\|controllers\\)$" request)
           (setq path (concat my-current-project-path "app/" (match-string 1 request)))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))
          
          ((string-match "^plugins$" request)
           (setq path (concat my-current-project-path "vendor/" (match-string 1 request)))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ((string-match "^prj$" request)
           (setq path my-current-project-path)
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ((string-match "^config$" request)
           (setq path (concat my-current-project-path "config/"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ((string-match "^db$" request)
           (setq path (concat my-current-project-path "db/"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; other commands
          ((string-match "^.emacs$" request)
           (setq path "~/.emacs")
           (my-create-new-window)
           (find-file path))

          ((string-match "^scratch$" request)
           (my-create-new-window)
           (switch-to-buffer "*scratch*"))

          ((string-match "^shell$" request)
           (setq default-directory my-current-project-path)
           (my-create-new-window)
           (eshell))
          
          ((string-match "^save wins$" request)
           (my-save-windows-config))

          ((string-match "^restore wins$" request)
           (my-restore-windows-configuration))

          ((string-match "^del win$" request)
           (delete-window))

          ((string-match "^make frame$" request)
           (setq buf (window-buffer))
           (delete-window)
           (switch-to-buffer-other-frame buf))
          
          ((string-match "^pwd$" request)
           (message "Your current project path: %s" my-current-project-path))

          ((string-match "^change prj$" request)
           (message "change prj")
           (call-interactively 'my-set-current-project-path))
          
          )
    )
  )

