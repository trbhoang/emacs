;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(menu-bar-mode  t)                       ;; show the menu...
(mouse-avoidance-mode 'jump)             ;; mouse ptr when cursor is too close
(tool-bar-mode -1)                       ;; turn-off toolbar 

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

(toggle-scroll-bar nil)                  ;; hide scroll bar

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

(setq indent-tabs-mode nil)

;; set spliting window horizontally by default
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

;; default directory
(setq default-directory "d:/Document/Projects/")

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
;; (require 'dired-details)
;; (dired-details-install)

;; dired-single
;; (require 'dired-single)                  

;; haml mode
(add-to-list 'load-path "~/.emacs.d/haml-mode")
(require 'haml-mode)

;; hl-line: highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default

;; org mode
(add-to-list 'load-path "d:/Document/Projects/Emacs/org-mode/lisp")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; (require 'yasnippet-bundle)
;; (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
;; (setq yas/prompt-functions '(yas/dropdown-prompt yas/x-prompt)) 

;; auto complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; Show 0.8 second later
(setq ac-auto-show-menu 0.4)


;; Tabbar
;; (require 'tabbar)
;; (tabbar-mode)

;; Ido
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-last-directory-history nil)
(setq ido-confirm-unique-completion nil) ;; wait for RET, even for unique?
(setq ido-show-dot-for-dired t) ;; put . as the first item
(setq ido-use-filename-at-point t) ;; prefer file names near point

;; Auto pair
(add-to-list 'load-path "/path/to/autopair") ;; comment if autopair.el is in standard load path 
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers 

;; Numberring window mode
(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My key bindings

;; switch to next window
(global-set-key "\M-`" 'other-window)

;; navigate file and open in another window
(global-set-key "\M-o" 'my-explorer)

;; my feature
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
                        
(global-set-key [f6] 'previous-buffer)
(global-set-key [f7] 'next-buffer)


(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line

(defun .emacs ()
  (interactive)
  (find-file "~/.emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming
(autoload 'linum-mode "linum" "mode for line numbers" t) 
(global-set-key (kbd "C-<f5>") 'linum-mode)                 ;; line numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; program shortcuts
(global-set-key (kbd "C-c E") ;; .emacs
                (lambda()(interactive)(find-file "~/.emacs")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-width 2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my rails project browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my-rails-project "d:/Document/Projects/Adobe_SPLC/adobe_splc/")
(setq my-rails-project-app (concat my-rails-project "app/"))
(setq my-rails-project-controllers (concat my-rails-project "app/controllers/"))
(setq my-rails-project-views (concat my-rails-project "app/views/"))
(setq my-rails-project-models (concat my-rails-project "app/models/"))

;; dired my project
(defun rproject ()
  (interactive)
  (setq default-directory my-rails-project)
  (call-interactively 'find-file))

;; dired my app
(defun rapp ()
  (interactive)
  (setq default-directory my-rails-project-app)
  (call-interactively 'find-file))

;; dired my controller
(defun rcontrollers ()
  (interactive)
  (setq default-directory my-rails-project-controllers)
  (call-interactively 'find-file))

;; dired my views
(defun rviews ()
  (interactive)
  (setq default-directory my-rails-project-views)
  (call-interactively 'find-file))

;; dired my models
(defun rmodels ()
  (interactive)
  (setq default-directory my-rails-project-models)
  (call-interactively 'find-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My new features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my current project path
(setq my-current-project-path "d:/Document/Projects/Adobe_SPLC/adobe_splc/")

;; set my current project path
(defun my-set-current-project-path (path)
	(interactive)
	(setq my-current-project-path path))

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
  (my-split-window (get-largest-window)))
											
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
  (cond ((string-match "\\(.+\\) model" request)
         (find-file-other-window (concat my-current-project-path "app/models/" (match-string 1 request) ".rb")))
        ((string-match "\\(.+\\) controller" request)
         (my-create-new-window)
         (find-file-other-window (concat my-current-project-path "app/controllers/" (match-string 1 request) "s_controller.rb")))
        ((string-match "\\(models\\|views\\|controllers\\)" request)
         (my-create-new-window)
         (dired-other-window (concat my-current-project-path "app/" request))
         )
		)  
  )

