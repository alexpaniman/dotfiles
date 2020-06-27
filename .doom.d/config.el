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
(setq doom-theme 'doom-vibrant)

;; Directory for org files
(setq org-directory "~/org/")

;; Use relative line numbers
(setq display-line-numbers-type t
      doom--line-number-style 'relative)

;; Change doom logo to black hole
(setq fancy-splash-image "~/.doom.d/black-hole.png")

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
