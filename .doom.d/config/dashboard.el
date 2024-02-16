
;; -------------------- Customize Doom Dashboard --------------------

(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2))

(defvar jecs/banners
  '(

    (
     "           ▓█▓███████████▓███▓▓▓▓▓████▓▓▓▓█▓█▓███▓▓█▓▓█▓▓█▓▒░▒▒▒▓█▓▓▒▓████    "
     "           ██▒███████████▓██▓▓▓█▒▒▓████▓▓▓▓▓▓████▓▓██▓▓█▒░░░░░░░░░▓█▓▓▓███░   "
     "          ▒██▓▓██████████▓██▓▓▓▓▓█▓▒▒▓▓▓▒▓▒▓▓█████▓███▓░░▒▓█████▒░▒█▓▓▓███▒   "
     "          ███▓▓█████████████▓▓█▓▓████▓▓▓▒▒█▓▓█████▓██▒░▒██▓▒▒████▒██▒▓▓███▒   "
     "         ▒███▓████████████▓█▓▓█▓▓█████████▓█▓▓█████▓█░▓█▒░░░▒███████▒▓▓███▓   "
     "        ░███▓▓▓███████████▓█▓▓█▓▓█▓▓▓▓▓▓▓████▓▓████▓▓▓█░▒░░░░░▒░████▒▓▓▓███   "
     "        ▓███▓▒▓▓██████████▓█▒███▓█▓██▓▓▓███████▓███▓██▒░░░▒▓░░▓▓████▓▓▓▓███   "
     "      ░▓██▓▓▒▓▓▓██████████▓▓▒████▒░░░░░░░▒██████▓▓██▓█▒▒▒▒▒▒▒▓▒█████▓▒▓▓███   "
     "     ▒██▓▓▓▒▒▒▓▓███████████▒▓██▒░░▒▒▓▓▓▓▒▒▒██████▓▓█▓██▓▒▒▒▒▒▓▓▓█████▒▓▓█▓█░  "
     "            ▓▓▓▓███████████▒▓▒░░▓███▓▒▒▓███████████▓▓████████████████▒▓▓█▓█░  "
     "            ▓▒▓▒███████████▓░░▓███▒░░░███████████████████████████████▓▒▓█▓█░  "
     "            ▒▓▒▓▓█████████▓▓░▓██▓░▒░░▒▒▓▒▓████████████████████████████▒▓▓▓█░  "
     "           ░▒▓░▓▓████████▓▓▓▓██▓░▒░░▒▒░▒▒▓████████████████████████████▒▓▓▓█░  "
     "            ██▒▒▓█████████▓▓▓██░░░░▒▒▒▓▒▒████████████▓▓███████████████▓▒▓▓█▒  "
     "           ░█▓▓▒▒███████████▓▓█░░░▒▒▓▓▒▒▓████████████▓▓▓███████████████▒▓▓█▒  "
     "           ░█▓▓▒▒████████████▓█▒▒▓▓▒▒▓████████████████▓▓███████████████▒▓▓▓▒  "
     "           ▒▓▓▓▒▒▓███████████▓████▓▓██████████████████▓▓███████████████▒▓▓▓▒  "
     "           ▒▒█▓▒▒▓████████████▓███▓████████████████████████████████████▒▓▓▓░  "
     "           ▒█▓▓▒▓▓▓████████▓███▓██████████████████████████████████████▒▓▓▓█░  "
     "          ░▓█▓▓▒▓▓▒████████▓███▓█████████████████████████████████████▓▒▓▒▓█░  "
     "          ▒██▓▓▒▓▓▒▓███████▓▓██▓█████████████████████████████████████▒▓▓▒▓█   "
     "         ░▓██▓▒▓▓▓▒▓████████▓███▓███████████████████████████▓███████▒▓▓▓▒▒▓   "
     "         ▒▓█▓▓▒▓▓▓▒▒████████▓▓██▓██████████████████████▓▓▓▓▓▓▓█████▓▒▓▓▓▒▒▓░  "
     "         ▓██▓▓▒▓▓▓▒▓▓███████▓▓██▓▓█████████████████▓▓▓▓███████████▓▒▓▓▓▓▒▒▓▒░ "
     "        ▒▓██▓▒▓▓▓▒▓▓▒█▓██████▓▓██▓███████████▓█▓██▓███████████████▒▓▓▓▒▒▒▒▒█░ "
     "        ▓▓██▓▒▓▓▓▒▓▓▒▓▓▓█████▓▓██▓███████████▓▓█▓▓███████████████▓▒▒▒▒▒▒▒░▓█▓ "
     "        ▓██▓▓▒▓▓▓▒▓▓▓▒█▓█████▓▓▓██▓██████████▓██████████████████▓█▓▒▒▒▒▒░▒▓█▓▒"
     "       ▒▓██▓▒▒▓▓▓▒▓▓▓▒▓▓▓███▓▓▓▓▓▓▓████████████████████████████▓██▓▒▒▒▒▒▒▓▓██▓"
     "       ▒▓██▓▒▓▓▓▓▒▓▓▓▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓██████████████████████████▒███▓▒▒▒▒▒▓▓▓██▓"
     "      ░▓███▓▒▓▓▓▓▒▒▒▓██▓▓▓▓▓▓▓▓▓▓▓▓▒▓████████████████████████▒▒███▓▒▒▒▒▒▓▒███▓"
     "      ▒▓▓▓▓▓▒▓▒▓▓░░████▓▓▓▓▓▓▓▓▓▓▓▓▓░▒▓████████████████████▓░▒▒████▒▒▒▒▒█▒███▓"
     "      ▒▓▓▓▓▓▒▒▓▒░░░░████▓▓▓▓▓▓▓▓▓▓▒▓░░░▒▓█████████████████▒░▒▒▒████░▒▒▒▒█▓██▓▒"
     "     ░▒▓▓▓▓▒▒▒▓▓░░░░░████▓▓▓▓▓▓▓▓▓▒▓▓  ▓▓▒▒▒▓█████████▓▓▒░ ░▒▒▒████▒▒▒▒▓████▓▒"
     "     ▒▓▓▓▓▓▒▓▒▓▓▒░░░░▒███▓▓▓▓▓▓▓▓▓▓▒▓░ ░▓▓▓▓▓▒▒▒▒▓▒▒▒▒▒░░ ░░▒▒▒████▒▒▒▒▓████▓▒"
     "     ▒▓▓▓▓▓▒▒▒▒▒▓█░░░░▒███▓▓▒▓▓▓▓▓▓▒▒▓ ░▒▓▓▓▒░░▓▓▓▓▓▓▒░   ░░▒▒▒████▒▒▒▒█████▓▒"
     "     ▒▓▓▓▓▓▒▒▒▒▓███░░░░▒███▓▓▓▓▓▓▓▓▓ ▓░░░░▒▓░░░░▓▓▓▓▓░ ░░░░░▒▒▒████▒▒▒▒▓████▓▓"
     )

    (
     "            ████████        ████████  ████                                  "
     "            ██▓▓▒▒▓▓████  ██        ██    ██              ██████████        "
     "            ██▒▒████▓▓▓▓██    ░░    ░░      ██  ██████████▒▒▒▒▓▓▒▒██        "
     "            ██▓▓██░░████  ░░░░░░░░░░░░░░      ██▓▓▓▓▒▒▒▒▒▒▒▒████▒▒██        "
     "              ████░░██  ░░████████████░░░░░░░░  ▓▓▓▓▓▓▓▓████░░██▓▓██        "
     "                ████  ░░▓▓▓▓▓▓▓▓████▓▓▓▓██▓▓░░░░▒▒██▓▓██▒▒▒▒░░██▓▓▓▓        "
     "                  ██░░▓▓▒▒▓▓▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓░░░░░░██░░░░  ██▓▓▓▓          "
     "                ██  ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓████░░░░██░░░░██▓▓██          "
     "                ██░░██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓██░░██░░██▓▓██            "
     "                  ██▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓██░░██▓▓██              "
     "                ██▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▒▒▒▒▓▓██░░██████              "
     "                ██▒▒▒▒▒▒▓▓▓▓▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▒▒▒▒▒▒▓▓▓▓██▓▓▓▓██              "
     "                ██▒▒▒▒▒▒▓▓██▒▒▒▒▒▒▒▒▓▓▒▒▒▒▒▒▓▓▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              "
     "                ██▒▒▒▒▒▒▓▓██▒▒▒▒▒▒▓▓▓▓▒▒▒▒▒▒▓▓▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              "
     "                ██▒▒▒▒▓▓████▒▒▒▒▒▒▓▓██▒▒▒▒▓▓▓▓▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              "
     "                  ██████░░░░██▒▒▒▒▓▓██▒▒▒▒▓▓██▒▒▒▒▒▒▓▓▓▓▓▓▓▓██              "
     "                ██▓▓██▓▓▓▓▓▓░░██████░░██████▓▓▒▒▒▒▓▓▓▓▓▓▓▓▓▓██              "
     "                ██▓▓██  ▒▒▒▒░░░░░░░░▓▓▓▓▓▓▒▒██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██              "
     "                ██▓▓▓▓░░░░░░░░░░░░░░  ▒▒▒▒░░▓▓▒▒▒▒▓▓▓▓▓▓▓▓██                "
     "                ██▒▒██░░          ░░░░░░░░██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██                "
     "                ██▒▒▓▓██    ▓▓░░        ░░██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██                "
     "                  ██▒▒██                ██▒▒▒▒▓▓▓▓▓▓▓▓▓▓██                  "
     "                  ██▒▒██░░    ▒▒▒▒      ░░████▓▓▓▓▓▓▓▓██                    "
     "                    ██▓▓██    ░░░░    ░░██░░██▒▒▓▓▓▓██                      "
     "                    ██▒▒▓▓▓▓▓▓      ░░▓▓▒▒▓▓▒▒▒▒████                        "
     "                  ██  ██▓▓██░░████████░░░░██████                            "
     "                ██  ░░░░████  ██░░██    ░░░░██                              "
     "              ██    ░░▓▓░░░░████░░░░██    ██░░▓▓██    ██████                "
     "              ██  ▒▒░░▓▓░░▓▓▓▓▓▓██▓▓██▓▓▓▓░░░░░░░░▓▓██░░░░░░██              "
     "            ██  ░░░░▓▓░░▓▓▓▓▓▓██▓▓██▓▓▓▓██░░░░░░░░░░██  ░░░░██              "
     "              ██  ░░██░░██████░░██▓▓▓▓████░░░░  ░░██    ░░░░░░██            "
     "            ██▓▓██░░██░░░░░░░░░░░░██▓▓██░░░░    ░░██  ░░░░████              "
     "          ██▒▒▓▓▓▓██    ░░░░░░░░░░░░████░░░░░░░░░░██  ░░██▓▓▓▓██            "
     "          ██▒▒▓▓██        ░░░░░░░░░░░░░░░░░░░░░░██    ██▓▓▓▓▓▓▓▓██          "
     )

))

(defun jecs/dashboard-ascii-banner ()
  (let* ((banner (nth (random (length jecs/banners)) jecs/banners))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'jecs/dashboard-ascii-banner)
(setq +doom-dashboard-functions
      '(
        doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        ;; doom-dashboard-widget-footer
        ))