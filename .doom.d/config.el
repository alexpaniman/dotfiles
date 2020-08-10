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
      doom-big-font (fira-code 24)
      doom-variable-pitch-font (fira-code 18))

;; Path that projectile uses to discover projects
(setq projectile-project-search-path
      '("~/projects/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2a2e38" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#bbc2cf"])
 '(custom-safe-themes
   (quote
    ("e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" default)))
 '(fci-rule-color "#62686E")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(objed-cursor-color "#ff665c")
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#242730"))
 '(rustic-ansi-faces
   ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#bbc2cf"])
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
