(defun jecs/toggle-quote-lines (beg end)
  "Toggle wrapping all items in region with double quotes."
  (interactive (list (mark) (point)))
  (unless (region-active-p)
    (user-error "No region to wrap"))
  (let ((deactivate-mark nil)
        (replacement (string-join
                      (mapcar (lambda (item)
                                (if (string-match-p "^\".*\"$" item)
                                    (string-trim item "\"" "\"")
                                  (format "\"%s\"" item)))
                              (split-string (buffer-substring beg end) "\n"))
                      "\n")))
    (delete-region beg end)
    (insert replacement)))

(defun jecs/stare ()
  "Opens random image from predefined list."
  (interactive)
  (let ((image-list '("~/Pictures/o_o/Gregory.png"
                      "~/Pictures/o_o/hamsterStare.png"
                      "~/Pictures/o_o/monkaStare.png"
                      "~/Pictures/o_o/Stare.png"
                      "~/Pictures/o_o/crunchy-cat-luna.png"
                      "~/Pictures/o_o/mikeStare.png")))
    (ffap (nth (random (length image-list)) image-list)))
  (image-transform-fit-to-window))

(defun jecs/listening (arg)
  "Opens listening gif. When ARG is non-nil, will open not-listening gif."
  (interactive "P")
  (ffap
   (if arg
       "~/Pictures/library/not-listening.gif"
     "~/Pictures/library/listening.gif"))
  (image-transform-fit-to-window)
  (image-toggle-animation)
  (message "%s" arg))

;; custom keybinds
(map! :leader
      (:prefix-map ("j" . "jecs")
       :desc "Stare" "s" #'jecs/stare
       :desc "Listening" "l" #'jecs/listening
       :desc "Open Phone" "p" #'scrcpy-open))

;; global keybinds
(map! :leader
      (:prefix-map ("x" . "global")
       :desc "Duplicate Line/Region" "d" #'duplicate-dwim))

;; scroll the compilation buffer
(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

(setq user-full-name "Joshua Carrasco"
      user-mail-address "mail@joshuacarrasco.com")

(setq-default delete-by-moving-to-trash t)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")

;; frame title
(set-frame-name "emacs btw")

;; dont ask me
(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

(setq-default tab-width 4)

;; so cross program copy paste will add to kill ring
(setq save-interprogram-paste-before-kill t)

;; default font was "Source Code Pro"
;; font installed from https://github.com/protesilaos/iosevka-comfy
(set-frame-font "Iosevka Comfy Wide 11" nil t)
;; (set-frame-font "Source Code Pro 11" nil t)

;; set initial size
(when window-system
  (set-frame-size (selected-frame) 136 50))

(map! :desc "Repeat" :g "C-." #'repeat)

;; enable profiler when ran emacs with
;; emacs --debug-init
(when init-file-debug
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

(set-frame-parameter nil 'alpha-background 98)
(add-to-list 'default-frame-alist '(alpha-background . 98))

(let ((banners '(("           ▓█▓███████████▓███▓▓▓▓▓████▓▓▓▓█▓█▓███▓▓█▓▓█▓▓█▓▒░▒▒▒▓█▓▓▒▓████    
           ██▒███████████▓██▓▓▓█▒▒▓████▓▓▓▓▓▓████▓▓██▓▓█▒░░░░░░░░░▓█▓▓▓███░   
          ▒██▓▓██████████▓██▓▓▓▓▓█▓▒▒▓▓▓▒▓▒▓▓█████▓███▓░░▒▓█████▒░▒█▓▓▓███▒   
          ███▓▓█████████████▓▓█▓▓████▓▓▓▒▒█▓▓█████▓██▒░▒██▓▒▒████▒██▒▓▓███▒   
         ▒███▓████████████▓█▓▓█▓▓█████████▓█▓▓█████▓█░▓█▒░░░▒███████▒▓▓███▓   
        ░███▓▓▓███████████▓█▓▓█▓▓█▓▓▓▓▓▓▓████▓▓████▓▓▓█░▒░░░░░▒░████▒▓▓▓███   
        ▓███▓▒▓▓██████████▓█▒███▓█▓██▓▓▓███████▓███▓██▒░░░▒▓░░▓▓████▓▓▓▓███   
      ░▓██▓▓▒▓▓▓██████████▓▓▒████▒░░░░░░░▒██████▓▓██▓█▒▒▒▒▒▒▒▓▒█████▓▒▓▓███   
     ▒██▓▓▓▒▒▒▓▓███████████▒▓██▒░░▒▒▓▓▓▓▒▒▒██████▓▓█▓██▓▒▒▒▒▒▓▓▓█████▒▓▓█▓█░  
            ▓▓▓▓███████████▒▓▒░░▓███▓▒▒▓███████████▓▓████████████████▒▓▓█▓█░  
            ▓▒▓▒███████████▓░░▓███▒░░░███████████████████████████████▓▒▓█▓█░  
            ▒▓▒▓▓█████████▓▓░▓██▓░▒░░▒▒▓▒▓████████████████████████████▒▓▓▓█░  
           ░▒▓░▓▓████████▓▓▓▓██▓░▒░░▒▒░▒▒▓████████████████████████████▒▓▓▓█░  
            ██▒▒▓█████████▓▓▓██░░░░▒▒▒▓▒▒████████████▓▓███████████████▓▒▓▓█▒  
           ░█▓▓▒▒███████████▓▓█░░░▒▒▓▓▒▒▓████████████▓▓▓███████████████▒▓▓█▒  
           ░█▓▓▒▒████████████▓█▒▒▓▓▒▒▓████████████████▓▓███████████████▒▓▓▓▒  
           ▒▓▓▓▒▒▓███████████▓████▓▓██████████████████▓▓███████████████▒▓▓▓▒  
           ▒▒█▓▒▒▓████████████▓███▓████████████████████████████████████▒▓▓▓░  
           ▒█▓▓▒▓▓▓████████▓███▓██████████████████████████████████████▒▓▓▓█░  
          ░▓█▓▓▒▓▓▒████████▓███▓█████████████████████████████████████▓▒▓▒▓█░  
          ▒██▓▓▒▓▓▒▓███████▓▓██▓█████████████████████████████████████▒▓▓▒▓█   
         ░▓██▓▒▓▓▓▒▓████████▓███▓███████████████████████████▓███████▒▓▓▓▒▒▓   
         ▒▓█▓▓▒▓▓▓▒▒████████▓▓██▓██████████████████████▓▓▓▓▓▓▓█████▓▒▓▓▓▒▒▓░  
         ▓██▓▓▒▓▓▓▒▓▓███████▓▓██▓▓█████████████████▓▓▓▓███████████▓▒▓▓▓▓▒▒▓▒░ 
        ▒▓██▓▒▓▓▓▒▓▓▒█▓██████▓▓██▓███████████▓█▓██▓███████████████▒▓▓▓▒▒▒▒▒█░ 
        ▓▓██▓▒▓▓▓▒▓▓▒▓▓▓█████▓▓██▓███████████▓▓█▓▓███████████████▓▒▒▒▒▒▒▒░▓█▓ 
        ▓██▓▓▒▓▓▓▒▓▓▓▒█▓█████▓▓▓██▓██████████▓██████████████████▓█▓▒▒▒▒▒░▒▓█▓▒
       ▒▓██▓▒▒▓▓▓▒▓▓▓▒▓▓▓███▓▓▓▓▓▓▓████████████████████████████▓██▓▒▒▒▒▒▒▓▓██▓
       ▒▓██▓▒▓▓▓▓▒▓▓▓▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓██████████████████████████▒███▓▒▒▒▒▒▓▓▓██▓
      ░▓███▓▒▓▓▓▓▒▒▒▓██▓▓▓▓▓▓▓▓▓▓▓▓▒▓████████████████████████▒▒███▓▒▒▒▒▒▓▒███▓
      ▒▓▓▓▓▓▒▓▒▓▓░░████▓▓▓▓▓▓▓▓▓▓▓▓▓░▒▓████████████████████▓░▒▒████▒▒▒▒▒█▒███▓
      ▒▓▓▓▓▓▒▒▓▒░░░░████▓▓▓▓▓▓▓▓▓▓▒▓░░░▒▓█████████████████▒░▒▒▒████░▒▒▒▒█▓██▓▒
") ("            ████████        ████████  ████                                  
            ██▓▓▒▒▓▓████  ██        ██    ██              ██████████        
            ██▒▒████▓▓▓▓██    ░░    ░░      ██  ██████████▒▒▒▒▓▓▒▒██        
            ██▓▓██░░████  ░░░░░░░░░░░░░░      ██▓▓▓▓▒▒▒▒▒▒▒▒████▒▒██        
              ████░░██  ░░████████████░░░░░░░░  ▓▓▓▓▓▓▓▓████░░██▓▓██        
                ████  ░░▓▓▓▓▓▓▓▓████▓▓▓▓██▓▓░░░░▒▒██▓▓██▒▒▒▒░░██▓▓▓▓        
                  ██░░▓▓▒▒▓▓▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓░░░░░░██░░░░  ██▓▓▓▓          
                ██  ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓████░░░░██░░░░██▓▓██          
                ██░░██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓██░░██░░██▓▓██            
                  ██▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓██░░██▓▓██              
                ██▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▒▒▒▒▓▓██░░██████              
                ██▒▒▒▒▒▒▓▓▓▓▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓██▓▓▓▓██              
                ██▒▒▒▒▒▒▓▓██▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▓▓▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              
                ██▒▒▒▒▒▒▓▓██▒▒▒▒▒▒▓▓▓▓▒▒▒▒▒▒▓▓▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              
                ██▒▒▒▒▓▓████▒▒▒▒▒▒▓▓██▒▒▒▒▓▓▓▓▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              
                  ██████░░░░██▒▒▒▒▓▓██▒▒▒▒▓▓██▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              
                ██▓▓██▓▓▓▓▓▓░░██████░░██████▓▓▒▒▒▒▓▓▓▓▓▓▓▓▓▓██              
                ██▓▓██  ▒▒▒▒░░░░░░░░▓▓▓▓▓▓▒▒██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██              
                ██▓▓▓▓░░░░░░░░░░░░░░  ▒▒▒▒░░▓▓▒▒▒▒▓▓▓▓▓▓▓▓██                
                ██▒▒██░░          ░░░░░░░░██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██                
                ██▒▒▓▓██    ▓▓░░        ░░██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██                
                  ██▒▒██                ██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██                  
                  ██▒▒██░░    ▒▒▒▒      ░░████▓▓▓▓▓▓▓▓██                    
                    ██▓▓██    ░░░░    ░░██░░██▒▒▓▓▓▓██                      
                    ██▒▒▓▓▓▓▓▓      ░░▓▓▒▒▓▓▒▒▒▒████                        
                  ██  ██▓▓██░░████████░░░░██████                            
                ██  ░░░░████  ██░░██    ░░░░██                              
              ██    ░░▓▓░░░░████░░░░██    ██░░▓▓██    ██████                
              ██  ▒▒░░▓▓░░▓▓▓▓▓▓██▓▓██▓▓▓▓░░░░░░░░▓▓██░░░░░░██              
            ██  ░░░░▓▓░░▓▓▓▓▓▓██▓▓██▓▓▓▓██░░░░░░░░░░██  ░░░░██              
              ██  ░░██░░██████░░██▓▓▓▓████░░░░  ░░██    ░░░░░░██            
            ██▓▓██░░██░░░░░░░░░░░░██▓▓██░░░░    ░░██  ░░░░████              
") ("                                           ░░░                                
                                       ░░░░ ░░░░░                             
                                      ░░░░░░░░░░░░                            
                                     ░░░░░▒▒▒░░░░░░░░░                        
                                    ░░░░▒▒▒▒░░░░░░░  ░                        
                                   ░░░░░▒▒▒░▒░░▒░░░░                          
                                   ▒░░░░░▒░▒░░░▒▒░▒░▒░░                       
                               ░▒▒░░  ░░░░░░░░░▒░▒▒▒░▒░                       
                               ░░░░   ░  ░░░▒░░░░░░▒░▒▒                       
                            ▒▓▓▓▓▓▓▒  ░  ░░░░░░░░░░░▒░▒                       
                          ░▓▓▓▓▓▓▓▓▒▒   ░░░░░  ▒ ░░░▒░▒░                      
                         ░▓▓▓▓▓▓▓▓▓▒    ░▒░░░░ ▒ ░ ░░░░░                      
                         ▓▓▓▒▒▓▓▓▓▓░    ▒▓░░▒░░░░░░ ░░░░                      
                        ▓▓▓▓▒  ░░▒▒  ░░ ▒▒▒▓▓▒░ ░▒     ░                      
                       ░▓▓▓▒   ░░▒░ ░░░░▒▓▓▓▓▓░▒░                             
                       ▓▓▓▓   ░ ░░  ▒░▒░▓▓▓▓▓▓▓▒░░                            
                      ▒▓▓▓▒     ░  ░░▒▒░▓▓▓▓▓▓▓▓░                             
                     ░▓▓▓▓      ░  ░░ ▒▒▓▓▓▓▓▓▓▓▒                             
                     ▓▓▓▓▒     ░░  ░  ▒▒▓▓▓▒░▒▓▓░                             
                    ▒▓▓▓▓     ░░   ░  ▒▒▓▓▓▓▒▓▓▒   ░                          
                    ▓▓▓▓▒    ░        ▒▒▒▓▓▓▓▓▒    ▒                          
                   ▒▓▓▓▓░    ░       ░▒▒▒▒▒▓▒  ░░░ ░                          
                   ▓▓▓▓▒    ░        ▒▒▒▒▒░   ░▒░░  ░                         
                  ▒▓▓▓▓░   ░░       ▒▒▒▒▒▒░   ░▒░ ░                           
                  ▓▓▓▓▓ ░ ░░░▒▓    ▒▓▓▒▒▒▒░   ░▒░ ░                           
                 ▒▓▓▓▓▒▓▓▓▓▓▓▓▓ ░  ░▓▓▒▒▒▒▒░  ░▒░                             
                 ▓▓▓▓▓▓▓▓▓▓▓▓▓▓ ░   ▓▓▓▓▒▒▒▒▒ ░▒                              
                ░▓▓▓▓▒▓▓▓▓▓▓▓▓▓ ▒░  ░▓▓▓▓▓▓▓▒░ ░░                             
                ▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓ ░▓░  ░▓▓▓▓▓▓▒▒ ░▒▓▒                           
                 ▒▓▓▓▓▓▓▓▓▒▒▒▒▒░░▓▓▒  ░▓▓▓▓▓░▒ ░░▓▓▒                          
                  ░░░░░░░░░░░▒▒░▒▒▓▓▓░░░▓▓▓▓░▒░ ░▒▓▓                          
                ░░░░   ░     ░▒▒░▓▓▓▓▓▓░▒▓▓▓░▒▓░ ░▓▓▓                         
              ░░░           ░░▒▓▓▒▓▓▓▓▓▓░▒▓▓░░▓▓░ ░▓▓░                        
"))))
(defvar jecs/banners banners)
)

(setq +doom-dashboard-ascii-banner-fn
      (lambda ()
        (let* ((banner (nth (random (length jecs/banners)) jecs/banners))
               (longest-line (apply #'max (mapcar #'length banner))))
          (put-text-property
           (point)
           (dolist (line banner (point))
             (insert (+doom-dashboard--center
                      +doom-dashboard--width
                      (concat line (make-string (max 0 (- longest-line (length line))) 32)))
                     "\n"))
           'face 'doom-dashboard-banner))))

(defun doom-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Loaded %d packages in %.03fs"
           (- (length load-path) (length (get 'load-path 'initial-value)))
           doom-init-time))

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded))
        ;; doom-dashboard-widget-footer

(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2))

;; disable hl line on dashboard
(add-hook! '+doom-dashboard-functions
  (setq hl-line-mode nil)
  (hide-mode-line-mode 1))

;; override function to remove newlines at start
(defun doom-dashboard-widget-loaded ()
  "Overrides doom dashboard loaded function."
  (when doom-init-time
    (insert
     ""
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       (doom-display-benchmark-h 'return))
      'face 'doom-dashboard-loaded)
     "\n")))

(setq doom-theme
      ;; 'doom-one
      ;; 'doom-old-hope
      ;; 'doom-1337
      'darktooth)

(custom-theme-set-faces! 'doom-1337
  '(mode-line :background "#476685")
  '(mode-line-inactive :background "#242628")
  '(hl-line :background "#252526"))

(custom-theme-set-faces! 'darktooth
  ;; all colours come from darktooh theme unless specified

  ;; so that (text-scale-adjust) scales properly
  '(line-number :inherit default :foreground "#7C6F64" "#767676")
  ;; same colour for current line
  ;; '(line-number-current-line :inherit default :foreground "#7C6F64" "#767676")
  ;; slightly different colour for current line 
  '(line-number-current-line :inherit default :foreground "#A89984" "#767676")

  ;; tab bar
  '(tab-bar-tab :background "#3a3a3a" "#3a3a3a")
  '(tab-bar-tab-inactive :background "#1c1c1c" "#1c1c1c")
  '(tab-bar :background "#1c1c1c" "#1c1c1c")

  ;; org mode
  '(org-block-begin-line :inherit 'org-block :extend t :foreground "#928374" "#8a8a8a")
  '(org-block-end-line :inherit 'org-block :extend t :foreground "#928374" "#8a8a8a")
  '(org-block :background "#232323" "#262626" :extend t)

  ;; modeline icons not bold
  '(success :foreground "#B8BB26" "#73AF00" :bold nil)
  '(error :foreground "#FB4933" "#d75f5f" :bold nil)
  '(warning :foreground "#FABD2F" "#ffaf00" :bold nil))

(use-package! tree-sitter
  :hook
  ((prog-mode . global-tree-sitter-mode)
   (tree-sitter-after-on . tree-sitter-hl-mode)))

(map!
 :after tree-sitter
 :leader
 :desc "Tree Sitter Highlight" :g "t h" #'tree-sitter-hl-mode)

(after! treemacs
  (treemacs-follow-mode 1))

(map! :leader :desc "Select Treemacs" :g "TAB" #'treemacs-select-window)

(after! company
  (setq company-minimum-prefix-length 1))

(use-package! gptel
  :defer t
  :bind
  (("C-c o g" . #'gptel))
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (setq gptel-model "llama3:latest"
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '("llama3:latest"))))

(after! corfu
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  (setq corfu-auto-prefix 0))

(use-package! platformio-mode
  :custom
  (platformio-mode-keymap-prefix (kbd "C-c l p"))
  :init
  ;; you may be required to make a .projectile file in project root.
  (add-hook 'c-mode-hook (lambda ()
                           (lsp-deferred)
                           (platformio-conditionally-enable)))
  (add-hook 'c++-mode-hook (lambda ()
                             (lsp-deferred)
                             (platformio-conditionally-enable)))

  :config
  (which-key-add-keymap-based-replacements platformio-mode-map
   "C-c l p" '("PlatformIO" . platformio-command-map)
   "C-c l p b" '("Build Project" . platformio-build)
   "C-c l p c" '("Clean Compile Objects" . platformio-clean)
   "C-c l p u" '("Build and Upload" . platformio-upload)
   "C-c l p p" '("Programmer Upload" . platformio-programmer-upload)
   "C-c l p s" '("SPIFFS Upload" . platformio-spiffs-upload)
   "C-c l p d" '("Update" . platformio-update)
   "C-c l p m" '("Device Monitor" . platformio-device-monitor)
   "C-c l p l" '("List Boards" . platformio-boards)
   "C-c l p i" '("Update Workspace" . platformio-init-update-workspace)))

;; (after! tramp
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; (setq tramp-verbose 10)
;; (setq tramp-default-method "plink")
;; (setq tramp-remote-path '("/mnt/c/Program Files/PuTTY"))

(load! "scrcpy.el")

(add-hook 'org-mode-hook 'yas-minor-mode-on)

(after! ox-hugo
  :defer t
  (setq org-hugo-base-dir
        (concat org-directory "hugo")))

(use-package! org-roam
  :defer t
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:\n")
      :unnarrowed t)
     ("m" "math" plain "#+STARTUP: latexpreview\n%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Math\n")
      :unnarrowed t)
     ("p" "programming" plain "#+STARTUP: latexpreview\n%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Programming\n")
      :unnarrowed t))))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(map!
 :after org-roam-ui
 :desc "Open Web UI" :leader "n r w" #'org-roam-ui-open)

(map! :g "C-x b" #'consult-buffer)

(after! god-mode
  (god-mode-all -1)
  (map! :desc "Toggle God Mode" :g "M-m" #'god-local-mode)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "C-x C-1") #'delete-other-windows)
  (define-key god-local-mode-map (kbd "C-x C-2") #'split-window-below)
  (define-key god-local-mode-map (kbd "C-x C-3") #'split-window-right)
  (define-key god-local-mode-map (kbd "C-x C-0") #'delete-window)

  (after! ace-window
    (define-key god-local-mode-map (kbd "C-x C-o") #'ace-window)))

(map! :after tab-bar :desc "Toggle Tab Bar Mode" :g "C-x t C-0" #'tab-bar-mode)

(use-package! ws-butler
  :config
  (add-to-list 'ws-butler-global-exempt-modes 'text-mode))

(use-package! mu4e
  :defer 60
  :config
  (set-email-account! "info@joshuacarrasco.com"
                      '((mu4e-sent-folder       . "/info/Sent Mail")
                        (mu4e-drafts-folder     . "/info/Drafts")
                        (mu4e-trash-folder      . "/info/Trash")
                        (mu4e-refile-folder     . "/info/All Mail")
                        (smtpmail-smtp-user     . "info@joshuacarrasco.com")
                        (mu4e-compose-signature . "---\nRegards\nJoshua"))
                      t)
  (set-email-account! "mail@joshuacarrasco.com"
                      '((mu4e-sent-folder       . "/mail/Sent Mail")
                        (mu4e-drafts-folder     . "/mail/Drafts")
                        (mu4e-trash-folder      . "/mail/Trash")
                        (mu4e-refile-folder     . "/mail/All Mail")
                        (smtpmail-smtp-user     . "mail@joshuacarrasco.com")
                        (mu4e-compose-signature . "---\nRegards\nJoshua"))
                      t)
  
  (setq +mu4e-gmail-accounts '(("mail@joshuacarrasco.com" . "/mail")
                               ("info@joshuacarrasco.com" . "/info")))
  (setq mu4e-update-interval 60)
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq mu4e-modeline-show-global nil))

(use-package! indent-bars
  :hook ((prog-mode . indent-bars-mode))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.5)
  (indent-bars-display-on-blank-lines t))

;; (after! lsp-mode
;;   (setq lsp-inlay-hint-enable t))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))

(after! python
  (setq! lsp-pylsp-plugins-black-enabled t))

;; debugging
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(setq realgud:pdb-command-name "python3 -m pdb"
      gud-pdb-command-name "python3 -m pdb")

(after! lsp-mode
  (setq lsp-pyright-multi-root nil))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(wgsl-mode . "wgsl"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "wgsl_analyzer")
                    :major-modes '(wgsl-mode)
                    :server-id 'wgsl-ls)))

;; (after! rustic
;;   (setq lsp-inlay-hint-enable t))

;; (after! lsp-mode
;;    (setq lsp-rust-analyzer-server-command "emacs-lsp-booster rust-analyzer"))

(after! dap-mode
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  ;; (require 'dap-lldb)
  (require 'dap-cpptools)
  ;; (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  ;; (dap-gdb-lldb-setup)
  (dap-cpptools-setup)
  (dap-register-debug-template "Rust::CppTools Run Configuration"
                               (list :type "cppdbg"
                                     :request "launch"
                                     :name "Rust::Run"
                                     :MIMode "gdb"
                                     :miDebuggerPath "rust-gdb"
                                     :environment []
                                     :program "${workspaceFolder}/target/debug/image2ascii"
                                     :cwd "${workspaceFolder}"
                                     :console "external"
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}")))

(setq c-basic-offset 2)

(after! (tramp lsp-mode)
  (lsp-register-client ;; c/c++ remote LSP
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

(after! (tramp lsp-mode)
  (lsp-register-client ;; c-sharp remote LSP
   (make-lsp-client :new-connection (lsp-tramp-connection "csharp-ls")
                    :major-modes '(csharp-mode)
                    :remote? t
                    :server-id 'csharp-ls-remote)))

(after! (tramp lsp-mode)
  (lsp-register-client ;; javascript remote LSP
   (make-lsp-client :new-connection (lsp-tramp-connection "typescript-language-server --stdio")
                    :major-modes '(rjsx-mode js2-mode typescript-mode)
                    :remote? t
                    :server-id 'ts-ls-remote)))



(when (eq system-type 'windows-nt)
  (progn
    ;; use vs code font
    (set-frame-font "Cascadia Code 12" nil t)

    ;; vterm doesnt work on windows. use eshell instead in packages.el!
    (use-package window
      :custom
      (display-buffer-alist
       ;; bottom slot -1
       '(("\\*doom:eshell-popup:.*\\*"
          (display-buffer-in-side-window)
          (window-height . 0.25)
          (side . bottom)
          (slot . -1)
          (window-parameters . ((no-delete-other-windows . t)))))))))
