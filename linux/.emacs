;;
;; general settings
;;
(set-default-font "Monospace-9")
(menu-bar-mode -1)	                 ;; hide menu
(tool-bar-mode -1)   	                 ;; hide tool bar 
(scroll-bar-mode -1)	                 ;; hide scroll bar
(setq auto-save-default -1)              ;; disable auto save
(setq make-backup-files -1)              ;; disable creating backup files
(global-visual-line-mode 1)              ;; line wrapping 
(global-hl-line-mode t)                  ;; highlight current line

(setq initial-scratch-message
  ";; scratch buffer created -- happy hacking\n")

(setq inhibit-startup-message t          ;; don't show ...    
  inhibit-startup-echo-area-message t)   ;; ... startup messages

(setq x-select-enable-clipboard t)       ;; make emacs copy/paste available out of emacs

(setq make-backup-files nil)

;; set the default basic indentation width for all c-mode-derived languages
(setq-default c-basic-offset 4)

;; default directory 
(setq default-directory "~/Projects/")

(add-to-list 'load-path "~/.emacs.d")


;;
;; some handy packages
;;

;; 'anything' package
;; (add-to-list 'load-path "~/.emacs.d/anything-config")
;; (require 'anything-config)

;; 'anything-ipython'
;; (require 'anything-ipython)
;; (add-hook 'python-mode-hook #'(lambda ()
                                ;; (local-set-key (kbd "M-<tab>") 'anything-ipython-complete)))
;;				(define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
;; (add-hook 'ipython-shell-hook #'(lambda ()
;;                                 (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))

;; (setq debug-on-quit 1)  ;; for debugging



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun mp-add-python-keys ()
;;   (local-set-key (kbd "C-c q") 'shell))
 
;; (add-hook 'python-mode-hook 'mp-add-python-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; yasnippet
(add-to-list 'load-path
                 "~/.emacs.d/yasnippet-0.6.1c")
   (require 'yasnippet) ;; not yasnippet-bundle
   (yas/initialize)
   (yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")

;; org mode
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(require 'org-install)

;; auto-complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

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

;; autopair mode
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; haml mode
(add-to-list 'load-path "~/.emacs.d/haml-mode")
(require 'haml-mode)

;; php mode
(require 'php-mode)

;; web template mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; 
;; my stuffs
;; 

;; my goto newline and indent from abitrary position from current line
(defun my-open-newline ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  )

;; kill to start of line
(defun kill-to-start-of-line ()
  "kill from point to start of line"
  (interactive)
  (kill-line 0)
  )

;; put ; at the end of line, go to next line and indent
(defun add-semicolon-and-go-next ()
  (interactive)
  (delete-trailing-whitespace)
  (end-of-line)
  (insert ";")
  (newline-and-indent)
  )

;; put , at the end of line, go to next line and indent
(defun add-colon-and-go-next ()
  (interactive)
  (delete-trailing-whitespace)
  (end-of-line)
  (insert ",")
  (newline-and-indent)
  )


(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line


(autoload 'linum-mode "linum" "mode for line numbers" t) 
(global-set-key (kbd "C-<f5>") 'linum-mode)                 ;; line numbers

;; hook to css mode
(defvar hexcolour-keywords-1
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background 
                     (match-string-no-properties 0)))))))

(defvar hexcolour-keywords-2
  '(("#[abcdef[:digit:]]\\{3\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background 
                     (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords-1)
  (font-lock-add-keywords nil hexcolour-keywords-2))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My new features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;; Key bindings
(global-set-key "\M-`" 'other-window)
(global-set-key [C-S-left] 'shrink-window-horizontally)
(global-set-key [C-S-right] 'enlarge-window-horizontally)
(global-set-key [C-S-down] 'shrink-window)
(global-set-key [C-S-up] 'enlarge-window)
(global-set-key "\M-1" 'delete-window)
(global-set-key "\M-2" 'my-move-current-buffer-to-other-window)
(global-set-key "\M-o" 'my-explorer)
(global-set-key "\C-o" 'my-open-newline)

(global-set-key (kbd "C-S-x") 'my-zooming)
(global-set-key (kbd "C-S-s") 'scroll-bar-mode)
(global-set-key (kbd "C-S-o") 'split-window-horizontally)
(global-set-key (kbd "C-S-e") 'split-window-vertically)
(global-set-key [f5] 'my-refresh-buffer)
(global-set-key "\M-3" 'toggle-selective-display)
(global-set-key "\M-4" 'toggle-selective-display-1)
(global-set-key (kbd "C-.") 'kill-to-start-of-line)
(global-set-key (kbd "C-;") 'add-semicolon-and-go-next)
(global-set-key (kbd "C-,") 'add-colon-and-go-next)


;;;;; My configurations

(defvar my-projects-path "~/Projects/")
(defvar my-configurations-path (concat my-projects-path "Editor/ProjectsNavigationOnEmacs/"))
(defvar my-project-configuration-path (concat my-configurations-path "conf"))
(defvar my-saved-window-config nil)
(defvar my-current-zoom nil)
(defvar my-completion-list nil)
(defvar my-tags-table-dir (concat my-projects-path "Tags/"))

(defvar my-configurations nil)
 
;; following vars will be loaded from file
(defvar my-current-project-path nil)
(defvar my-project-language nil)
(defvar my-project-type nil)  ;; Ex: ror, android, c++,...
(defvar my-tagging-command nil)
(defvar my-tags-table-list nil)

;; config vars for android project
(defvar my-android-package nil)  ;; Ex: com/example/sudoku (com.example.sudoku)


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
  ; set default directory
  (if my-current-project-path
      (setq default-directory my-current-project-path)
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

    ; load project type
    (setq my-project-type (cdr (assoc "type" my-configurations)))

    ; load android project package 
    (setq my-android-package (cdr (assoc "android-package" my-configurations)))
    
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


;; my goto a project's file
(defun my-goto-project-file (input-pattern dir-to-find extension-pattern)
  (let (path)
    (setq path (concat my-current-project-path dir-to-find (match-string 1 input-pattern) extension-pattern))
    (my-create-new-window)
    (find-file path)
    )
  )

;; my goto a project's dir
(defun my-goto-project-dir (dir)
  (let (path)
    (setq path (concat my-current-project-path dir))
    (my-create-new-window)
    (setq default-directory path)
    (ido-find-file)
    (setq default-directory my-current-project-path)
    )
  )

;; my goto a normal dir
(defun my-goto-normal-dir (dir)
  (my-create-new-window)
  (setq default-directory dir)
  (ido-find-file)
  ;; (setq default-directory my-current-project-path)
  ;; my-current-project-path could be 'nil' in some case which cause error!!!
  )

;; my goto a normal file
(defun my-goto-normal-file (file)
  (my-create-new-window)
  (find-file file)
  )


(defun my-explorer (request)
  (interactive "sWhat do you want? ")
  (cond

   ;;======================================================================
   ;; For RoR projects
   ;;======================================================================

   ;; goto a model
   ((string-match "^m \\(.+\\)$" request)  
    (my-goto-project-file request "app/models/" ".rb"))

   ;; goto a controller 
   ((string-match "^c \\(.+\\)$" request)
    (my-goto-project-file request "app/controllers/" "s_controller.rb"))

   ;; goto database.yml
   ((string-match "^db conf$" request)
    (my-goto-project-file "" "config/" "database.yml"))
   
   ;; goto models dir 
   ((string-match "^ms$" request)
    (my-goto-project-dir "app/models"))
   
   ;; goto views dir 
   ((string-match "^vs$" request)
    (my-goto-project-dir "app/views"))

   ;; goto controllers dir 
   ((string-match "^cs$" request)
    (my-goto-project-dir "app/controllers"))

   ;; goto helpers dir 
   ((string-match "^hs$" request)
    (my-goto-project-dir "app/helpers"))

   ;; goto plugins dir
   ((string-match "^plugs$" request)
    (my-goto-project-dir "vendor/plugins"))

   ;; goto project dir
   ((string-match "^prj$" request)
    (my-goto-project-dir ""))

   ;; goto config dir
   ((string-match "^conf$" request)
    (my-goto-project-dir "config/"))   

   ;; goto directory db/
   ((string-match "^db$" request)
    (my-goto-project-dir "db/"))

   ;; goto public dir
   ((string-match "^pub$" request)
    (my-goto-project-dir "public/"))

   ;; goto javascripts dir
   ((string-match "^js$" request)
    (my-goto-project-dir "public/javascripts/"))

   ;; goto stylesheets dir
   ((string-match "^stls$" request)
    (my-goto-project-dir "public/stylesheets/"))

   ;; goto test dir
   ((string-match "^test$" request)
    (my-goto-project-dir "test/"))

   ;; goto spec dir
   ((string-match "^sp$" request)
    (my-goto-project-dir "spec/"))

   ;; goto spec/models
   ((string-match "^sp m$" request)
    (my-goto-project-dir "spec/models/"))

   ;; goto spec/controllers
   ((string-match "^sp c$" request)
    (my-goto-project-dir "spec/controllers/"))

   ;; goto spec/views
   ((string-match "^sp v$" request)
    (my-goto-project-dir "spec/views/"))
   

   ;;======================================================================
   ;; For Android projects
   ;;======================================================================

   ;; goto AndroidManifest.xml
   ((string-match "^mf$" request)
    (my-goto-project-file "" "" "AndroidManifest.xml"))

   ;; goto strings.xml
   ((string-match "^strings$" request)
    (my-goto-project-file "" "res/values/" "strings.xml"))

   ;; goto R.java
   ((string-match "^R$" request)
    (my-goto-project-file "" (concat "gen/" my-android-package "/") "R.java"))

   ;; goto a layout
   ((string-match  "^l \\(.+\\)$" request)
    (my-goto-project-file request "res/layout/" ".xml"))   

   ;; goto a layout-land
   ((string-match  "^ll \\(.+\\)$" request)
    (my-goto-project-file request "res/layout-land/" ".xml"))   

   ;; goto a values file
   ((string-match  "^v \\(.+\\)$" request)
    (my-goto-project-file request "res/values/" ".xml"))   

   ;; goto src dir
   ((string-match "^src$" request)
    (my-goto-project-dir (concat "src/" my-android-package)))
   
   ;; goto res dir
   ((string-match "^res$" request)
    (my-goto-project-dir "res/"))

   ;; goto res/values dir
   ((string-match "^vals$" request)
    (my-goto-project-dir "res/values"))

   ;; goto res/layout dir
   ((string-match "^layout$" request)
    (my-goto-project-dir "res/layout"))

   ;; goto res/layout-land dir
   ((string-match "^llayout$" request)
    (my-goto-project-dir "res/layout-land"))
   
   ;;======================================================================
   ;; Regular shortcuts
   ;;======================================================================
   
   ;; goto notes
   ((string-match "^notes$" request)
    (my-goto-normal-dir "~/Documents/Notes"))

   ;; goto 'must read'
   ((string-match "^read$" request)
    (my-goto-normal-file "~/Documents/Notes/Reads.org"))

   ;; goto 'questions'
   ((string-match "^?$" request)
    (my-goto-normal-file "~/Documents/Notes/Questions.org"))
   
   ;; goto todos
   ((string-match "^todos$" request)
    (my-goto-normal-file "~/Documents/Notes/Tasks.org"))

   ;; goto .emacs
   ((string-match "^.emacs$" request)
    (my-goto-normal-file "~/.emacs"))

   ;; goto *scratch* buffer
   ((string-match "^blank$" request)
    (my-create-new-window)
    (switch-to-buffer "*scratch*"))

   ;; open eshell 
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

   ;; display current buffer on another new frame
   ((string-match "^make frame$" request)
    (setq buf (window-buffer))
    (delete-window)
    (switch-to-buffer-other-frame buf))

   ;; display current project path
   ((string-match "^pd$" request)
    (message "Your current project path: %s" my-current-project-path))

   ;; display current buffer file path 
   ((string-match "^pwd$" request)
    (message "Your buffer file name: %s" (buffer-file-name)))

   ;; switch to other project which has fixed name in configurations dir
   ((string-match "^change prj$" request)
    (call-interactively 'my-change-project))

   ;; goto dir of store all project 
   ((string-match "^prjs$" request)
    (my-goto-normal-dir my-projects-path))

   ;; goto home dir
   ((string-match "^~$" request)
    (my-goto-normal-dir "~"))

   ;; goto rails yasnippets dir
   ((string-match "^yas$" request)
    (my-goto-normal-dir "/home/hoangtran/.emacs.d/yasnippet-0.6.1c/snippets/text-mode/ruby-mode"))

   ;; goto remember list 
   ((string-match "^rem$" request)
    (my-goto-normal-file "~/Documents/Notes/Remember.txt"))

   ;; view my snippets note
   ((string-match "^snips$" request)
    (my-goto-normal-file "~/Documents/Notes/Snippet.txt"))

   ;; view shortcuts
   ((string-match "^help$" request)
    (my-goto-normal-file "~/Documents/Notes/ProjectShortcuts.org"))

   ) ;; end cond
  )  ;; end defun



;;;;; Initializations

(my-initializing)


;; testing


;(custom-set-variables


 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.


;;  '(py-python-command-args nil)
;;  '(py-shell-name "ipython")
;;  '(python-python-command "ipython"))
;; (custom-set-faces
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 
 ;)
