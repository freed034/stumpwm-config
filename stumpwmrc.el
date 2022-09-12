;; Created by フランク <@freed034> at Caen - France, February 2015
;; Last modification made at Orsay - France, December 2021
;; E-mail: freed034@pm.me

;; -*-lisp-*-
(in-package :stumpwm) ;;(setf *default-package* :stumpwm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load some modules. First define the module directory.
(set-module-dir "~/Packages/stumpwm-contrib/")
(load-module "battery-portable")
(load-module "disk")
(load-module "mem")
(load-module "net")
(load-module "wifi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup message: display the machine's name or display some phrase
;;(setf *startup-message* (machine-instance)) 
(setf *startup-message* "フランク, may the source be with you!")
;;(setf *startup-message* nil) ;; Disable welcome message

;; Change the prefix key
(set-prefix-key (kbd "s-a")) 

;; Set the lang variable for use english language on stumwpm
(setf (getenv "LC_ALL") "en_US.UTF-8")
(setf (getenv "LANG") "en_US.UTF-8")
(setf (getenv "LANGUAGE") "en_US.UTF-8")

;; Bugfix for scrolling doesn't work with an external mouse in GTK+3 apps.
(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

;;Set variables for use some look
(setf (getenv "DE") "stumpwm")
(setf (getenv "XDG_CURRENT_DESKTOP") "stumpwm")
(setf (getenv "XDG_SESSION_DESKTOP") "stumpwm")

;; Execute some commands at the login
(run-shell-command "xset b off")
(run-shell-command "xset s off")
(run-shell-command "xset -dpms")
(run-shell-command "setxkbmap latam")
(run-shell-command "xrdb -load ~/.Xresources")

;; Startup apps
;; (run-shell-command "stalonetray")
;; (run-shell-command "nm-applet")
;; (run-shell-command "dropbox start -i")
;; (run-shell-command "gnome-power-manager")
;; (run-shell-command "gnome-volume-control-applet")
;; (run-shell-command "system-config-printer-applet")

;;(setf *frame-number-map* "1234567890")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUPS CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(grename "W1") ;;Work1
(gnewbg "W2") ;;Work2
(gnewbg "R1") ;;Read1
(gnewbg "R2") ;;Read2
(gnewbg "P1") ;;Perso1
(gnewbg "P2") ;;Perso2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defvar color1 "#886666")
;;(defvar color2 "#222222")
;;(defvar color3 "#555555")
;;(defvar mycolor "#4169E1") ;; royalblue color
;;(defvar mycolor "#539DC2") ;; carolina blue color
(defvar mycolor1 "#9BC4E2") ;; cerulean blue color
(defvar mycolor2 "grey20")
(defvar mycolor3 "grey10")
(defvar mycolor4 "dimgray")

;;(defparameter FOREGROUND-COLOR "green")
(defparameter FOREGROUND-COLOR mycolor1)
(defparameter BACKGROUND-COLOR mycolor2)
(defparameter BORDER-COLOR mycolor3)
(defparameter FOCUS-COLOR mycolor4) ;;mycolor1
(defparameter UNFOCUS-COLOR mycolor3) ;;white

;; Window border colors.
(set-focus-color FOCUS-COLOR)
(set-unfocus-color UNFOCUS-COLOR)

;; Input box colors.
(set-fg-color FOREGROUND-COLOR)
(set-bg-color BACKGROUND-COLOR)
(set-win-bg-color BACKGROUND-COLOR)
(set-border-color BORDER-COLOR)

;; Modeline colors.
(setf *mode-line-foreground-color* FOREGROUND-COLOR)
(setf *mode-line-background-color* BACKGROUND-COLOR)
(setf *mode-line-border-color* BORDER-COLOR)

;; Background.
(run-shell-command (concatenate 'string "xsetroot -solid " BACKGROUND-COLOR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MESSAGE AND INPUT BAR CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Width of the message bar border
(set-msg-border-width 1)

(setf *message-window-padding* 1)
(setf *message-window-gravity* :center)
(setf *timeout-wait* 10);;30)
(setf *input-window-gravity* :bottom-right)

;; Use xorg-xlsfonts to see the options in my machine
(set-font "-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso10646-1")

;; Others fonts
;;(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")
;;(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-7")
;;(set-font "-xos4-terminus-medium-r-normal--0-0-72-72-c-0-iso8859-7")
;;(set-font "-*-dina-medium-r-normal-*-*-*-*-*-*-*-*-*")
;;(set-font "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*")
;;(set-font "-artwiz-smoothansi-medium-r-normal--13-130-75-75-m-60-iso8859-1")
;;(set-font "-adobe-courier-bold-r-normal--14-100-100-100-m-90-iso8859-1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOWS CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *maxsize-border-width* 1)
(set-frame-outline-width 1)
(set-normal-gravity :center)
(setf *normal-border-width* 1)
(setf *window-border-style* :tight) ;; thick,thin,none,tight

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE LINE CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on the modeline
;;(enable-mode-line (current-screen) (current-head) t)
(if (not (head-mode-line (current-head))) (toggle-mode-line (current-screen) (current-head)))

(setf *mode-line-border-width* 1)
(setf *mode-line-pad-x* 1)
(setf *mode-line-pad-y* 1)
(setf *mode-line-timeout* 2) ;; Update every 2 sec
(setf *mode-line-position* :top)

(setf *time-modeline-string* "^B[%H:%M][%a %b %e %Y]^b");;:%2s") ;; %a = name of the day, %b = name of the month %e = day %Y = year %y = short year
(setf *window-format* "^B[%s]%n·(%1c)^b") ;;<%9t>;;(setf *window-format* "^B%m%n^b %5t") %c = classe %t = title %n = number
(setf *window-info-format* "%h %n (%5t | 1%c)") ;;%wx
(setf *screen-mode-line-format* (list "[^1^B%n^b^n]|%W^>|%M|%D|%7l:^3%I^n|%B|^6%d"))
(setf wifi:*wifi-modeline-fmt* "%e %p")
(setf disk:*disk-modeline-fmt* "%m:%a")

;;(setf *group-format* "[%n·%2t%s]")

;; Basic and usufull mode-line
;;(setf *screen-mode-line-format* (list "%g | %v ^>^7 | %d "))

;; Other modeline
;;(setf *screen-mode-line-format* (list "^6*::^n  %g  ^6*::^n%w^6* :: ^8*CPU: ^n%f %C ^3*%t ^6*:: ^n^r%M ^6*:: ^n%b ^6*:: ^n %l ^6*::^8* - " '(:eval (run-shell-command "date +\"%T %d-%m-%Y\" | tr -d '[:cntrl:]'" t))))

;; Show time, cpu usage and network traffic in the modeline
;;(setf *screen-mode-line-format* (list '(:eval (run-shell-command "date '+%R, %F %a'|tr -d [:cntrl:]" t)) " | %t | %c| %l | [^B%n^b] %W"))
;;(setf *screen-mode-line-format* (list "%h | %g | %v ^>^7 | " '(:eval (run-shell-command "date" t))))

;;(defvar *vol-status-command-new* "amixer get Master | grep \"[[:digit:]]\\+%\" -o | tr -d \"\\n\"")

;;(setf *screen-mode-line-format*
;;      (list "[^B%n^b]|%W^> | "     
;;       '(:eval (run-shell-command *battery-status-command* t))
;;       " | Vol. "
;;       '(:eval (run-shell-command *vol-status-command* t))
;;       " | %d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOUSE STUFFS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *mouse-focus-policy* :click)
(setf *mouse-follows-focus* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Old Emacs configuration
;;(defvar *emacs-command* nil "Start an emacsclient frame. Starts an emacs daemon if necessary.")
;;(setf *emacs-command* "bash -c -i 'emacsclient -c -a emacs'") ;;(setf *emacs-command* "bash -c -i 'emacsclient -c -a \"\"'")
;;(defcommand emacs () () (run-shell-command (concat "exec " *emacs-command*)))

;; Emacs
(setf *emacs-command* "/usr/bin/emacsclient -c -a /usr/bin/emacs")
(defcommand my_emacs () ()
  "run emacs"
  (run-shell-command (concat "exec " *emacs-command*)))

;; Xterm
(defcommand terminal () ()
  "run xterm"
  (run-or-raise "xterm" '(:instance "Xterm")))

;; Xfce-terminal
(defcommand terminal-xfce () ()
  "run xfce-terminal"
  (run-or-raise "xfce4-terminal" '(:instance "Terminal")))

;; Mate-terminal
(defcommand terminal-mate () ()
  "run mate-terminal"
  (run-or-raise "mate-terminal" '(:instance "Terminal")))

;; Konsole
(defcommand konsole () ()
  "run konsole"
  (run-or-raise "konsole" '(:instance "Konsole")))

;; Firefox
(defcommand firefox-browser () ()
  "run firefox"
  (run-or-raise "firefox" '(:instance "Firefox")))

;; Luakit
(defcommand luakit-browser () ()
  "run luakit"
  (run-or-raise "luakit" '(:instance "Luakit")))

;; Konqueror
(defcommand konqueror-browser () ()
  "run konqueror"
;;  (run-shell-command "exec konqueror"))
  (run-or-raise "konqueror" '(:instance "Konqueror")))

;; Midori
(defcommand midori-browser () ()
  "run midori"
  (run-or-raise "midori" '(:instance "Midori")))

;; Chromium
(defcommand chromium-browser () ()
  "run chromium"
  (run-or-raise "chromium" '(:instance "Chromium")))

;; fff
(setf *fff-command* "xterm fff")
(defcommand fff () ()
  "run fff"
  (run-shell-command (concat "exec " *fff-command*)))

;; Caja
(defcommand caja () ()
  "run caja"
  (run-or-raise "caja" '(:instance "Caja")))

;; Dolphin
(defcommand dolphin () ()
  "run dolphin"
  (run-or-raise "dolphin" '(:instance "Dolphin")))

;; Thunar
(defcommand thunar () ()
  "run thunar"
  (run-or-raise "thunar" '(:instance "Thunar")))

;; Gnome-search-tool
(defcommand search-tool () ()
  "run gnome-search-tool"
  (run-or-raise "gnome-search-tool" '(:instance "Gnome-search-tool")))

;; Spotify
(defcommand spotify () ()
  "run spotify"
  (run-or-raise "spotify" '(:instance "Spotify")))

;; Shortcut to lock the screen using xscreensaver
(defcommand lock-screen-xscreen () ()
  "lock the screen"
  (run-or-raise "xscreensaver-command -lock" '(:instance "xscreensaver")))

;; Shortcut to lock the screen using xtrlock
(defcommand lock-screen () ()
  "lock the screen"
  ;;(run-shell-command "exec xflock4"))
  (run-shell-command "xtrlock")) ;; the last function is instend (define-key *root-map* (kbd "l") "exec xtrlock -b") ;; lock screen

(defcommand suspend-machine () ()
  "suspend the machine"
  (run-shell-command "systemctl suspend"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLEANING KEYBINDING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(undefine-key *root-map* (kbd "c"))
(undefine-key *root-map* (kbd "C-c"))
(undefine-key *root-map* (kbd "e"))
(undefine-key *root-map* (kbd "C-e"))
(undefine-key *root-map* (kbd "C-b"))
(undefine-key *root-map* (kbd "C-a"))
(undefine-key *root-map* (kbd "C-g"))
(undefine-key *root-map* (kbd "C-m"))
;;(undefine-key *root-map* (kbd "G"))
(undefine-key *root-map* (kbd "F1"))
(undefine-key *root-map* (kbd "F2"))
(undefine-key *root-map* (kbd "F3"))
(undefine-key *root-map* (kbd "F4"))
(undefine-key *root-map* (kbd "F5"))
(undefine-key *root-map* (kbd "F6"))
(undefine-key *root-map* (kbd "F7"))
(undefine-key *root-map* (kbd "F8"))
(undefine-key *root-map* (kbd "F9"))
(undefine-key *root-map* (kbd "F10"))

(undefine-key *group-root-map* (kbd "C-u"))
(undefine-key *group-root-map* (kbd "M-n"))
(undefine-key *group-root-map* (kbd "M-p"))
(undefine-key *group-root-map* (kbd "C-RET"))
(undefine-key *group-root-map* (kbd "w"))
(undefine-key *group-root-map* (kbd "C-w"))
(undefine-key *group-root-map* (kbd "DEL"))
(undefine-key *group-root-map* (kbd "C-k"))
(undefine-key *group-root-map* (kbd "K"))
(undefine-key *group-root-map* (kbd "\""))
(undefine-key *group-root-map* (kbd "C-N"))
(undefine-key *group-root-map* (kbd "F11"))

(undefine-key *tile-group-root-map* (kbd "C-n"))
(undefine-key *tile-group-root-map* (kbd "C-M-n"))
(undefine-key *tile-group-root-map* (kbd "SPC"))
(undefine-key *tile-group-root-map* (kbd "C-SPC"))
(undefine-key *tile-group-root-map* (kbd "C-p"))
(undefine-key *tile-group-root-map* (kbd "C-M-p"))
(undefine-key *tile-group-root-map* (kbd "M-t"))
(undefine-key *tile-group-root-map* (kbd "C-0"))
(undefine-key *tile-group-root-map* (kbd "C-1"))
(undefine-key *tile-group-root-map* (kbd "C-2"))
(undefine-key *tile-group-root-map* (kbd "C-3"))
(undefine-key *tile-group-root-map* (kbd "C-4"))
(undefine-key *tile-group-root-map* (kbd "C-5"))
(undefine-key *tile-group-root-map* (kbd "C-6"))
(undefine-key *tile-group-root-map* (kbd "C-7"))
(undefine-key *tile-group-root-map* (kbd "C-8"))
(undefine-key *tile-group-root-map* (kbd "C-9"))
(undefine-key *tile-group-root-map* (kbd "TAB"))
(undefine-key *tile-group-root-map* (kbd "M-TAB"))
(undefine-key *tile-group-root-map* (kbd "M-Up"))
(undefine-key *tile-group-root-map* (kbd "M-Down"))
(undefine-key *tile-group-root-map* (kbd "M-Left"))
(undefine-key *tile-group-root-map* (kbd "M-Right"))
(undefine-key *tile-group-root-map* (kbd "l"))
(undefine-key *tile-group-root-map* (kbd "C-l"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MY KEYS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key *root-map* (kbd "e") "my_emacs") ;; emacs
(define-key *root-map* (kbd "c") "terminal") ;; default terminal
(define-key *root-map* (kbd "s-x") "lock-screen") ;; lock screen
(define-key *root-map* (kbd "s-f") "firefox-browser") ;; firefox
(define-key *root-map* (kbd "s-o") "midori-browser") ;; midori
(define-key *root-map* (kbd "s-l") "luakit-browser") ;; luakit
;;(define-key *root-map* (kbd "s-k") "konqueror-browser") ;; konqueror
;;(define-key *root-map* (kbd "s-c") "midori-browser") ;; midori
;;(define-key *root-map* (kbd "s-c") "chromium-browser") ;; chrome
(define-key *root-map* (kbd "s-b") "thunar") ;; thunar
;;(define-key *root-map* (kbd "s-b") "fff") ;; fast file manager
;;(define-key *root-map* (kbd "s-s") "search-tool") ;; gnome-search-tool

(define-key *root-map* (kbd "s-m") "mode-line")
(define-key *root-map* (kbd "w") "windowlist")
(define-key *root-map* (kbd "s-a") "pull-hidden-next")
(define-key *root-map* (kbd "s-w") "grouplist")
(define-key *root-map* (kbd "s-TAB") "gnext")
(define-key *root-map* (kbd "s-z") "fullscreen")
(define-key *root-map* (kbd "s-g") "abort")
(define-key *root-map* (kbd "s-s") "suspend-machine")
(define-key *root-map* (kbd "s-Up") "move-window up")
(define-key *root-map* (kbd "s-Left") "move-window left")
(define-key *root-map* (kbd "s-Down") "move-window down")
(define-key *root-map* (kbd "s-Right") "move-window right")
(define-key *root-map* (kbd "s-DEL") "repack-window-numbers")

;;(define-key *top-map*  (kbd "XF86Launch9") "spotify")

;; KEY DEFINITION - Key code symbol table
;; note: certain keyboards have this different. use xev to find yours if these don't work
(setf *key-codes*
      '((162 . "XF86AudioPlay")		   ; handled by amarok (or other mp3 players)
	(164 . "XF86AudioStop")		   
	(144 . "XF86AudioPrev")
	(153 . "XF86AudioNext")
	(160 . "XF86AudioMute")
	(174 . "XF86AudioLowerVolume")	   ; we use amixer (alsa mixer) to  handle this
	(176 . "XF86AudioRaiseVolume")))

;; Others: "XF86Battery";;"XF86MonBrightnessUp";;"XF86MonBrightnessDown"

;; Map keycodes to keysyms
(mapcar (lambda (pair)
	  (let* ((keycode (car pair))
		 (keysym  (cdr pair))
		 (format-dest nil)
		 (format-dest (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)))
	    (format format-dest "xmodmap -e 'keycode ~d = ~a'" keycode keysym)
	    (run-shell-command format-dest)
	  format-dest))
	*key-codes*)

;; Volume control
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer set Master 5%-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer set Master 5%+")

;; Mute
(define-key *top-map* (kbd "XF86AudioMute") "exec amixer set Master toggle")

;; Alternatives keybinding
;; (define-key *top-map* (kbd "M-Tab") "pull-hidden-next")
;; (define-key *top-map* (kbd "M-ISO_Left_Tab") "pull-hidden-previous") ;; shift
;; (define-key *top-map* (kbd "M-]") "pull-hidden-next")
;; (define-key *top-map* (kbd "M-[") "pull-hidden-previous")
;; (define-key *top-map* (kbd "s-Tab") "fnext")
;; (define-key *top-map* (kbd "s-]") "gnext")
;; (define-key *top-map* (kbd "s-[") "gprev")
;; (define-key *top-map* (kbd "s-b") "move-focus left")
;; (define-key *top-map* (kbd "s-n") "move-focus down")
;; (define-key *top-map* (kbd "s-p") "move-focus up")
;; (define-key *top-map* (kbd "s-f") "move-focus right")
;; (define-key *top-map* (kbd "M-F4") "kill")
;; (define-key *top-map* (kbd "M-F2") "exec")
;; (define-key *top-map* (kbd "s-SPC") "exec")
;; (define-key *root-map* (kbd "M-F") "fullscreen")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MY WINDOW PLACEMENT RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clear rules
(clear-window-placement-rules)

;; (define-frame-preference "Default"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t   t :title "emacs")
;;   (0 t   t :class "XTerm"))

;; (define-frame-preference "Web"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t   t :class "Firefox"))

;; (define-frame-preference "Media"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t   t :instance "aumix")
;;   (0 t   t :class "MPlayer")
;;   (0 t   t :class "Avidemux"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MY OTHERS FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-stumpwm-command "safequit" ()
  "Checks if any windows are open before quitting."
  (let ((win-count 0))
    ;; Count the windows in each group
    (dolist (group (screen-groups (current-screen)))
      (setq win-count (+ (length (group-windows group)) win-count)))

    ;; Display the number of open windows or quit
    (if (= win-count 0)
        (run-commands "quit")
        (message (format nil "You have ~d ~a open" win-count (if (= win-count 1) "window" "windows"))))
 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECT A RANDOM BACKGROUND IMAGE FROM A GIVEN FOLDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *background-image-path* "~/Repos/GLab/Linux/stumpwm/background/")
(defun select-random-background-image ()
  "Select a random image"
  (let ((file-list (directory (concatenate 'string *background-image-path* "*.png")))
        (*random-state* (make-random-state t)))
    (namestring (nth (random (length file-list)) file-list))))

;;(run-shell-command (concatenate 'string "display -background black -backdrop -window root " (select-random-background-image)))
;;(run-shell-command (concatenate 'string "display -background black -window root " (select-random-background-image)))

(run-shell-command (concatenate 'string "feh --bg-fill " (select-random-background-image)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPOSITOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-shell-command "picom --config ~/.config/picom.conf")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDING NAMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *root-map*
;;   (kbd "c")   "exec xterm"
;;   (kbd "C-c") "exec xterm"
;;   (kbd "e")   "emacs"
;;   (kbd "C-e") "emacs"
;;   (kbd "C-b") "banish"
;;   (kbd "C-a") "time"
;;   (kbd "C-m") "lastmsg"
;;   (kbd "G")   "vgroups"
;;   (kbd "x")   '*exchange-window-map*
;;   (kbd "F1")  "gselect 1"
;;   (kbd "F2")  "gselect 2"
;;   (kbd "F3")  "gselect 3"
;;   (kbd "F4")  "gselect 4"
;;   (kbd "F5")  "gselect 5"
;;   (kbd "F6")  "gselect 6"
;;   (kbd "F7")  "gselect 7"
;;   (kbd "F8")  "gselect 8"
;;   (kbd "F9")  "gselect 9"
;;   (kbd "F10") "gselect 10"

;; *group-root-map*
;;   (kbd "C-u") "next-urgent"
;;   (kbd "M-n")     "next"
;;   (kbd "M-p")     "prev"
;;   (kbd "C-RET")   "expose"
;;   (kbd "w")   "windows"
;;   (kbd "C-w") "windows"
;;   (kbd "DEL") "repack-window-numbers"
;;   (kbd "C-k") "delete"
;;   (kbd "K")   "kill"
;;   (kbd "\"")  "windowlist"
;;   (kbd "C-N") "number"
;;   (kbd "F11") "fullscreen"

;; *tile-group-root-map*
;;   (kbd "C-n")     "pull-hidden-next"
;;   (kbd "C-M-n")   "next-in-frame"
;;   (kbd "SPC")     "pull-hidden-next"
;;   (kbd "C-SPC")   "pull-hidden-next"
;;   (kbd "C-p")     "pull-hidden-previous"
;;   (kbd "C-M-p")   "prev-in-frame"
;;   (kbd "M-t")     "other-in-frame"
;;   (kbd "C-0")     "pull 0"
;;   (kbd "C-1")     "pull 1"
;;   (kbd "C-2")     "pull 2"
;;   (kbd "C-3")     "pull 3"
;;   (kbd "C-4")     "pull 4"
;;   (kbd "C-5")     "pull 5"
;;   (kbd "C-6")     "pull 6"
;;   (kbd "C-7")     "pull 7"
;;   (kbd "C-8")     "pull 8"
;;   (kbd "C-9")     "pull 9"
;;   (kbd "TAB")     "fnext"
;;   (kbd "M-TAB")   "fother"
;;   (kbd "M-Up")    "move-window up"
;;   (kbd "M-Down")  "move-window down"
;;   (kbd "M-Left")  "move-window left"
;;   (kbd "M-Right") "move-window right"
;;   (kbd "l")       "redisplay"
;;   (kbd "C-l")     "redisplay"
