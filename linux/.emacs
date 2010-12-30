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
(setq default-directory "~/Projects/")

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
;; (require 'window-number)
;; (window-number-mode t)
;; (window-number-meta-mode t)

;; autopair mode
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; etags select
(require 'etags-select)
(global-set-key "\M-." 'etags-select-find-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming languages

;; haml mode
(add-to-list 'load-path "~/.emacs.d/haml-mode")
(require 'haml-mode)

;; ==========================================================
      
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming
(autoload 'linum-mode "linum" "mode for line numbers" t) 
(global-set-key (kbd "C-<f5>") 'linum-mode)                 ;; line numbers


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My new features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;; Key bindings

(global-set-key "\M-`" 'other-window)
(global-set-key "\M-1" 'delete-window)
(global-set-key "\M-2" 'my-move-current-buffer-to-other-window)
(global-set-key "\M-o" 'my-explorer)

(global-set-key [f2] 'my-zooming)
(global-set-key [f5] 'my-refresh-buffer)


;;;;; My configurations

(defvar my-configurations-path "/home/hoangtran/Projects/EmacsConfigurations/")
(defvar my-project-configuration-path (concat my-configurations-path "conf"))
(defvar my-projects-path "~/Projects/")
(defvar my-saved-window-config nil)
(defvar my-current-zoom nil)
(defvar my-completion-list nil)
(defvar my-tags-table-dir "/home/hoangtran/Projects/Tags/")

(defvar my-configurations nil)

;; following vars will be loaded from file
(defvar my-current-project-path nil)
(defvar my-project-language nil)
(defvar my-tagging-command nil)
(defvar my-tags-table-list nil)


;;;;; Implementation of my features

(defun my-initializing ()
  ; load configurations from file
  (my-load-configurations-from-file my-project-configuration-path)
  ; load configurations into corresponding vars
  (my-load-configurations-to-vars)
  ; update tags table list
  (setq tags-table-list my-tags-table-list)
  ; tagging code
  (if my-tagging-command
      (my-tagging)
    )
  )

;; this function load all neccessary configurations from a file
(defun my-load-configurations-from-file (config-file)
  "Return a list of arguments correspond to lines of tag-config-file"
  ;; (message config-file)
  (let (config-list config key value hash-of-configs)
    (if (file-exists-p config-file)
      (progn
        (with-temp-buffer
          (insert-file-contents config-file)
          (setq config-list (split-string (buffer-string) "[ \t]*\n"))
          )
        (while config-list
          (setq config (car config-list))
          (setq config-list (cdr config-list))
          (if (> (length config) 0)
              (progn
                (setq pos (string-match "=" config))
                (if pos
                    (progn
                      (setq key (substring config 0 pos))
                      (setq value (substring config (+ 1 pos)))
                      (add-to-list 'hash-of-configs (cons key value))
                      (setq my-configurations hash-of-configs)
                      )
                  (message "Invalid configuration: %s" config)
                  )
                )
            )
          )
        )
      (message "Config file doesn't exist!")
    )
  )
  )

(defun my-load-configurations-to-vars ()
  "This function loads configs saved in 'my-configurations' hash to corresponding vars"
  (let (tags-table-list)
    ; load project path
    (setq my-current-project-path (cdr (assoc "project-path" my-configurations)))
   
    ; load tagging command 
    (setq my-tagging-command (cdr (assoc "tagging-command" my-configurations)))
    
    ; load tags table list
    (setq tags-table-list (cdr (assoc "tags-table-list" my-configurations)))
    (if tags-table-list
        (setq my-tags-table-list (mapcar '(lambda (x) (concat my-tags-table-dir x)) (split-string tags-table-list)))
      )
    
    ; load project language
    (setq my-project-language (cdr (assoc "language" my-configurations)))
    )
  )


; change project and reload configurations
(defun my-change-project (project-name)
  (interactive "sProject name: ")
  (if (file-exists-p (concat my-configurations-path project-name))
      (progn
        (setq my-project-configuration-path (concat my-configurations-path project-name))
        ; reinitializing
        (my-initializing)
        )
    )
  )

;; set my current project path
(defun my-set-current-project-path (path)
  (interactive "sNew project path: ")
  (setq my-current-project-path path))

;; my save windows configuration
(defun my-save-windows-config ()
  (setq my-saved-window-config (current-window-configuration))
  )

;; my restore saved windows configuration
(defun my-restore-windows-configuration ()
  (if my-saved-window-config
      (set-window-configuration my-saved-window-config)
    )
  )

;; my reset saved windows configuration
(defun my-reset-saved-window-config ()
  (setq my-saved-window-config nil)
  )

;; my project path concatenation
(defun my-concat-with-project-path (path)
  (concat my-current-project-path path)
  )

;; sensitivelly adjust current window
(defun my-sensitively-adjust-current-window ()
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
        (delete-window win)))))

;; create a new window and move current buffer to
(defun my-move-current-buffer-to-other-window ()
  (interactive)
  (let ((buf (window-buffer))
        (win (selected-window)))
        (set-window-buffer (my-create-new-window) buf)
        (set-window-buffer win (other-buffer))
        )
  )


(defun my-zooming ()
  (interactive)
  (if (eq (selected-window) (next-window))
      (setq my-current-zoom t)
    (setq my-current-zoom nil)
    )
  (if my-current-zoom
      (progn
        (my-restore-windows-configuration)
        (setq my-current-zoom nil)
        (my-reset-saved-window-config) ;; reset window config
        )
    (progn
      (my-save-windows-config) 
      (delete-other-windows)
      (setq my-current-zoom t)
      )
    ))


(defun my-refresh-buffer ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t)
  )


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

;; my tagging system
(defun my-tagging ()
  "This function make TAGS file by looking into current project dir and parsing all .rb file to generate tags of methods"
  ; (call-process "/bin/bash" nil nil nil "-c" "touch test.txt")
  (message "Tagging........")
  (call-process "/bin/bash" nil nil nil "-c" my-tagging-command)
  (message "Tagging finished!")
  )


(defun my-explorer (request)
  (interactive "sWhat do you want? ")
  (let (path)
    (cond ((string-match "^m \\(.+\\)$" request)  ;; goto specific model
           (setq path (concat my-current-project-path "app/models/" (match-string 1 request) ".rb"))
           (my-create-new-window)
           (find-file path))

          ;; goto specific controller 
          ((string-match "^c \\(.+\\)$" request)
           (setq path (concat my-current-project-path "app/controllers/" (match-string 1 request) "s_controller.rb"))
           (my-create-new-window)
           (find-file path))

          ;; goto models dir 
          ((string-match "^ms$" request)
           (setq path (concat my-current-project-path "app/models"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto views dir 
          ((string-match "^vs$" request)
           (setq path (concat my-current-project-path "app/views"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto controllers dir 
          ((string-match "^cs$" request)
           (setq path (concat my-current-project-path "app/controllers"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto helpers dir 
          ((string-match "^hs$" request)
           (setq path (concat my-current-project-path "app/helpers"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto plugins dir
          ((string-match "^plugs$" request)
           (setq path (concat my-current-project-path "vendor/plugins"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto project dir
          ((string-match "^prj$" request)
           (setq path my-current-project-path)
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto config dir
          ((string-match "^conf$" request)
           (setq path (concat my-current-project-path "config/"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto directory db/
          ((string-match "^db$" request)
           (setq path (concat my-current-project-path "db/"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto database.yml
          ((string-match "^db conf$" request)
           (setq path (concat my-current-project-path "config/database.yml"))
           (my-create-new-window)
           (find-file path))

          ;; goto javascripts dir
          ((string-match "^js$" request)
           (setq path (concat my-current-project-path "public/javascripts/"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ;; goto stylesheets dir
          ((string-match "^stls$" request)
           (setq path (concat my-current-project-path "public/stylesheets/"))
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          
          ;; goto notes
          ((string-match "^notes$" request)
           (setq path "~/Projects/Notes/")
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))
          
          ;; goto todos
          ((string-match "^todos$" request)
           (setq path "~/Projects/Notes/Tasks.org")
           (my-create-new-window)
           (find-file path))

          ;; other commands
          ((string-match "^.emacs$" request)
           (setq path "~/.emacs")
           (my-create-new-window)
           (find-file path))

          ((string-match "^blank$" request)
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
          
          ((string-match "^pd$" request)
           (message "Your current project path: %s" my-current-project-path))

          ((string-match "^pwd$" request)
           (message "Your buffer file name: %s" (buffer-file-name)))

          ((string-match "^change prj$" request)
           (call-interactively 'my-change-project))

          ((string-match "^prjs$" request)
           (setq path my-projects-path)
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          ((string-match "^~$" request)
           (setq path "~/")
           (my-create-new-window)
           (setq default-directory path)
           (ido-find-file)
           (setq default-directory my-current-project-path))

          
          ((string-match "^shortcuts$" request)
           (setq buf (get-buffer-create "*my shortcuts*"))
           (set-buffer buf)
           (setq buffer-read-only nil)
           ;; add shortcut descriptions to this buffer
           (insert "1.  prj\n")
           (insert "2.  models\n")
           (insert "3.  views\n")
           (insert "4.  controllers\n")
           (insert "5.  config\n")
           (insert "6.  db\n")
           (insert "7.  plugins\n")
           (insert "8.  xxx model\n")
           (insert "9.  xxx controller\n")
           (insert "10. change prj\n\n")

           (insert "11. notes\n")
           (insert "12. todos\n")
           (insert "13. .emacs\n")
           (insert "14. scratch\n")
           (insert "15. shell\n")
           (insert "16. save wins\n")
           (insert "17. restore wins\n")
           (insert "18. del win\n")
           (insert "19. make frame\n")
           (insert "20. pwd\n")
           (insert "21. shortcuts\n")
           
           (setq buffer-read-only t)
           (my-create-new-window)
           (switch-to-buffer buf)
           )

          ((string-match "^rem$" request)
           (setq path "~/Projects/Notes/Remember.txt")
           (my-create-new-window)
           (find-file path))
          
          )
    )
  )

;;;;; Initializations

(my-initializing)
