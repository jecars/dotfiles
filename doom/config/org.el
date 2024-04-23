(after! ox-hugo
  (setq org-hugo-base-dir
        (concat org-directory "hugo")))

(use-package! org-roam
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:\n")
      :unnarrowed t)
     ("m" "math" plain "#+STARTUP: latexpreview\n%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Math\n")
      :unnarrowed t))))
