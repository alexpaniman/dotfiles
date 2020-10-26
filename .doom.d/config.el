;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;        _                             _
;;   __ _| | _____  ___ __   __ _ _ __ (_)_ __ ___   __ _ _ __
;;  / _` | |/ _ \ \/ / '_ \ / _` | '_ \| | '_ ` _ \ / _` | '_ \
;; | (_| | |  __/>  <| |_) | (_| | | | | | | | | | | (_| | | | |
;;  \__,_|_|\___/_/\_\ .__/ \__,_|_| |_|_|_| |_| |_|\__,_|_| |_|
;;                   |_|
;;
;; GitHub: https://github.com/alexpaniman


;; Some functionality uses this to identify you
(setq user-full-name "Alexander Paniman"
      user-mail-address "alexpaniman@gmail.com")

;; Set theme. It's similar to doom-one
(setq doom-theme 'doom-monokai-pro)

;; Directory for org files
(setq org-directory "~/org/")

;; Use relative line numbers
(setq display-line-numbers-type t
      doom--line-number-style 'relative)

;; Change doom logo to black hole
;; (setq fancy-splash-image "~/.doom.d/black-hole.png")

(defun fira-code (size)
  (font-spec :family "Fira Code"
             :size size))

;; Set fonts
(setq doom-font (fira-code 18)
      doom-big-font (fira-code 22)
      doom-variable-pitch-font (fira-code 18))

;; Path that projectile uses to discover projects
(setq projectile-project-search-path
      '("~/projects/"))

;; SVG screenshots
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;; ---> Org mode <---

(require 'org)

;; Enable org-mode to export org file with cyrillic to pdf
(setq org-latex-packages-alist
      '(("utf8" "inputenc" t)
        ("T2A" "fontenc" t)
        ("russian,english" "babel" t)))

;; Hide org-mode markers like *bold* or ~code~
(setq org-hide-emphasis-markers t)

;; Make LaTeX previews in org-mode bigger
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))

;; Enable graphviz dot support inside org-mode source block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;; Show images when opening a file.
(setq org-startup-with-latex-preview t
      org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; ---> Flyspell <---

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (rainbow-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
