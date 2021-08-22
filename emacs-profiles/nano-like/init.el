(setq inhibit-startup-message t) ; Disable message at startup

(add-to-list 'load-path "/home/alex/emacs-profiles/nano-like/libs/")

(require 'nano-layout)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(scroll-bar-mode -1) ; Disable scroll bar
(tool-bar-mode -1)   ; Disable tool bar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Add margin before text

(menu-bar-mode -1)   ; Disable menu bar

;; (require 'nano-theme-dark)
;; (require 'nano-theme-monokai-pro)

;; (require 'nano-base-colors)
;; (require 'nano-faces)
;; (nano-faces)

;; (require 'nano-theme)
;; (nano-theme)

;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-light t)
;;   (let ((line (face-attribute 'mode-line :underline)))
;;     (set-face-attribute 'mode-line          nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :underline  line)
;;     (set-face-attribute 'mode-line          nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; (require 'nano-modeline)
;; (nano-modeline)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering)
(require 'no-littering)

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)))

(use-package quelpa-use-package)

(use-package moody
  :custom (moody-mode-line-height 25)
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config (minions-mode 1))

(use-package magit)

(use-package cmake-mode)

(use-package vterm)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-mode 1))

(column-number-mode)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(dolist (mode '(term-mode-hook
                org-mode-hook
                pdf-view-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.7))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy-rich
  :after ivy
  :custom (ivy-virtual-abbreviate 'full
                                  ivy-rich-switch-buffer-align-virtual-buffer t
                                  ivy-rich-path-style 'abbrev)
  :config (progn
            (ivy-set-display-transformer 'ivy-switch-buffer
                                         'ivy-rich-switch-buffer-transformer)
            (ivy-rich-mode)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-w-delete nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (setq evil-undo-system 'undo-tree))

(evil-mode 1)

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package hydra)

(defun text-scale-reset ()
  "Reset text scaling."
  (interactive)
  (text-scale-increase 0))

(defhydra hydra-text-scale (global-map "<f5>" :timeout 2)
  "Scale text"
  ("j" text-scale-increase    "in")
  ("k" text-scale-decrease   "out")
  ("0" text-scale-reset    "reset")
  ("RET" nil "finished"    :exit t))

;; Recetf mode
(recentf-mode 1)

(define-key evil-normal-state-map (kbd "SPC SPC") #'counsel-recentf)

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (:map undo-tree-visualizer-mode-map
              ([remap evil-next-visual-line    ] . undo-tree-visualize-redo)
              ([remap evil-previous-visual-line] . undo-tree-visualize-undo)
              ([remap evil-forward-char        ] . undo-tree-visualize-switch-branch-right)
              ([remap evil-backward-char       ] . undo-tree-visualize-switch-branch-left))
  :config
  (defun undo-tree-split-side-by-side (original-function &rest args)
    "Split undo-tree side-by-side"
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply original-function args)))

  (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)

  (setq undo-tree-auto-save-history t))

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

(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "/home/alex/org/exported-org-files")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))

(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

(require 'org)

(setq org-startup-indented t
      org-hide-leading-stars t)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("extarticle"
                 "\\documentclass{extarticle}"

                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-preview-latex-default-process 'dvisvgm
      org-image-actual-width nil)

;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))

;; Enable org-mode to export org file with cyrillic to pdf
(setq org-latex-packages-alist
      '(("utf8" "inputenc" t)
        ("T2A" "fontenc" t)
        ("english,russian" "babel" t)

        ("" "multicol" f)))

;; Show images when opening a file.
(setq org-startup-with-latex-preview t
      org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(use-package auctex
    :mode ("\\.tex\\'" . latex-mode))

(use-package cdlatex
  :after (:any org-mode LaTeX-mode))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(use-package company-math
    :after (:any org-mode TeX-mode)
    :config
    (add-to-list 'company-backends 
     '(company-math-symbols-latex company-math-latex-commands)))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/var/autosaves/\\1" t)))

;; Create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/var/autosaves/" t)

(setq create-lockfiles nil)

;; Superior scrolling
(use-package good-scroll
  :init (good-scroll-mode 1))

(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
              ("\\" . hydra-pdftools/body)
              ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
              ("g"  . pdf-view-first-page)
              ("G"  . pdf-view-last-page)
              ("l"  . image-forward-hscroll)
              ("h"  . image-backward-hscroll)
              ("j"  . pdf-view-next-page)
              ("k"  . pdf-view-previous-page)
              ("e"  . pdf-view-goto-page)
              ("u"  . pdf-view-revert-buffer)
              ("al" . pdf-annot-list-annotations)
              ("ad" . pdf-annot-delete)
              ("aa" . pdf-annot-attachment-dired)
              ("am" . pdf-annot-add-markup-annotation)
              ("at" . pdf-annot-add-text-annotation)
              ("y"  . pdf-view-kill-ring-save)
              ("i"  . pdf-misc-display-metadata)
              ("s"  . pdf-occur)
              ("b"  . pdf-view-set-slice-from-bounding-box)
              ("r"  . pdf-view-reset-slice)
              ([remap image-next-line] . hydra-pdftools/body)))

(defhydra hydra-pdftools (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↓^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  > _W_ <   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↑^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("al" pdf-annot-list-annotations)
        ("ad" pdf-annot-delete)
        ("aa" pdf-annot-attachment-dired)
        ("am" pdf-annot-add-markup-annotation)
        ("at" pdf-annot-add-text-annotation)
        ("y"  pdf-view-kill-ring-save)
        ("+" pdf-view-enlarge :color red)
        ("-" pdf-view-shrink :color red)
        ("0" pdf-view-scale-reset)
        ("H" pdf-view-fit-height-to-window)
        ("W" pdf-view-fit-width-to-window)
        ("P" pdf-view-fit-page-to-window)
        ("n" pdf-view-next-page-command :color red)
        ("p" pdf-view-previous-page-command :color red)
        ("d" pdf-view-dark-minor-mode)
        ("b" pdf-view-set-slice-from-bounding-box)
        ("r" pdf-view-reset-slice)
        ("g" pdf-view-first-page)
        ("G" pdf-view-last-page)
        ("e" pdf-view-goto-page)
        ("o" pdf-outline)
        ("s" pdf-occur)
        ("i" pdf-misc-display-metadata)
        ("u" pdf-view-revert-buffer)
        ("F" pdf-links-action-perfom)
        ("f" pdf-links-isearch-link)
        ("B" pdf-history-backward :color red)
        ("N" pdf-history-forward :color red)
        ("l" image-forward-hscroll :color red)
        ("h" image-backward-hscroll :color red))

(use-package haskell-mode)

(use-package cc-mode)

(defun my-c-mode-common-hook ()
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
 (c-set-offset 'substatement-open 0)
 ;; other customizations can go here

 (setq c++-tab-always-indent t)
 (setq c-basic-offset 4)            ;; Default is 2
 (setq c-indent-level 4)            ;; Default is 2

 (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 4)
 (setq indent-tabs-mode nil))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defvar custom/lsp-ui-doc-active
  "Is lsp doc active now" nil)

(defun custom/lsp-ui-doc-toggle ()
  "Toggles lsp ui doc."
  (interactive)
  (if custom/lsp-ui-doc-active
      (lsp-ui-doc-hide)
    (lsp-ui-doc-show))
  (setq custom/lsp-ui-doc-active
	(not custom/lsp-ui-doc-active)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t)
  (setq lsp-idle-delay 0.500)

  :custom
  (lsp-ui-doc-enable    nil)
  (lsp-ui-doc-max-width 100)

  (lsp-modeline-code-actions-segments '(count icon name))

  :hook ((haskell-mode . lsp)
         (c-mode       . lsp)
	 (c++-mode     . lsp)
         (lsp-mode     . lsp-enable-which-key-integration))

  :commands lsp)

(define-key lsp-mode-map
  [remap evil-lookup] #'custom/lsp-ui-doc-toggle)

(use-package projectile)

(defun compile-project (arg)
  "Compile project and move cursor to compilation buffer."
  (interactive "P")
  (projectile-compile-project arg)
  (switch-to-buffer-other-window "*compilation*"))

(define-key evil-normal-state-map (kbd "C-c c") #'compile-project)

(defun doom-apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))

(with-eval-after-load 'compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; if you are ivy user
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package treemacs)
(use-package treemacs-evil)

(define-key evil-normal-state-map (kbd "C-;") #'treemacs)

;; (use-package dap-mode)

;; (require 'dap-gdb-lldb)
;; (dap-gdb-lldb-setup)

(use-package lsp-haskell)

(use-package all-the-icons)

;; Customize LSP code actions look
(require 'dash)
(require 'lsp-ui)

(define-key lsp-mode-map
  [remap evil-goto-definition] #'lsp-ui-peek-find-definitions)

(define-key evil-normal-state-map
  (kbd "g r") #'lsp-ui-peek-find-references)

(defun lsp-ui-sideline--code-actions (actions bol eol)
  "Show code ACTIONS."
  (let ((inhibit-modification-hooks t))
    (when lsp-ui-sideline-actions-kind-regex
      (setq actions (seq-filter (-lambda ((&CodeAction :kind?))
				  (or (not kind?)
				      (s-match lsp-ui-sideline-actions-kind-regex kind?)))
				actions)))
    (setq lsp-ui-sideline--code-actions actions)
    (lsp-ui-sideline--delete-kind 'actions)
    (let* ((titles (cl-loop for action in actions
			    collect (->> (lsp:code-action-title action)
				      (replace-regexp-in-string "[\n\t ]+" " ")
				      (concat (unless lsp-ui-sideline-actions-icon
						lsp-ui-sideline-code-actions-prefix)))))
	   (max-length (if (> (length titles) 0) (-max (--map (length it) titles)))))
      (cl-loop for action in actions
	       for  title in titles
	       do (-let* ((image (lsp-ui-sideline--code-actions-image))
			  (margin (- max-length (length title)))
			  (keymap (let ((map (make-sparse-keymap)))
				    (define-key map [down-mouse-1] (lambda () (interactive)
								     (save-excursion
								       (lsp-execute-code-action action))))
				    map))
			  (len (length title))
			  (title (progn (add-face-text-property 0 len 'lsp-ui-sideline-global nil title)
					(add-face-text-property 0 len 'lsp-ui-sideline-code-action nil title)
					(add-text-properties 0 len `(keymap ,keymap mouse-face highlight) title)
					title))
			  (string (concat (propertize " " 'display `(space :align-to
									   (- right-fringe
									      ,(lsp-ui-sideline--align
										(+ len (length image)) margin))))
					  image
					  (propertize title 'display (lsp-ui-sideline--compute-height))))
			  (pos-ov (lsp-ui-sideline--find-line (+ 1 (length title) (length image)) bol eol t))
			  (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
		    (when pos-ov
		      (overlay-put ov 'after-string string)
		      (overlay-put ov 'before-string " ")
		      (overlay-put ov 'kind 'actions)
		      (overlay-put ov 'position (car pos-ov))
		      (push ov lsp-ui-sideline--ovs)))))))

(defun new-cpp-olimp-prog (file-name)
  "Create NAME.cpp file in current directory with content of ~/.emacs.d/template.cpp"
  (interactive (list (read-file-name "Quick task: " nil default-directory nil)))
  (when (not (file-exists-p file-name))
    (switch-to-buffer (create-file-buffer file-name))
    (insert-file-contents "~/.emacs.d/template.cpp")
    (c++-mode)
    (write-file file-name)))

(define-key evil-normal-state-map (kbd "C-x C-l") #'new-cpp-olimp-prog)

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package rainbow-mode)

(use-package flycheck)

(use-package shell-pop
  :custom
  (shell-pop-shell-type                        '("vterm" "*vterm-pop*"
						(lambda nil (vterm))))
  (shell-pop-window-position                                 "bottom")
  (shell-pop-cleanup-buffer-at-process-exit                         t))

(define-key evil-normal-state-map (kbd "C-t") 'shell-pop)
(define-key evil-insert-state-map (kbd "C-t") 'shell-pop)

(require 'rainbow-mode)
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 0)

(use-package quelpa)

(use-package gdb-mi :quelpa (gdb-mi :fetcher git
                                    :url "https://github.com/weirdNox/emacs-gdb.git"
                                    :files ("*.el" "*.c" "*.h" "Makefile"))
  :init
  (fmakunbound 'gdb)
  (fmakunbound 'gdb-enable-debug))


(set-face-attribute 'default nil
                    :font "Fira Code"
                    :height 140)

;; (use-package ivy-posframe
;;   :config (ivy-posframe-mode 1)
;;   :custom
;;   (ivy-posframe-display-function-alist '((t . ivy-postframe-display)))
;;   (ivy-posframe-parameters '(( left-fringe . 8)
;; 			     (right-fringe . 8))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-tree)
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(visual-fill-column visial-fill-column visial-fill-column-mode projectile vterm cmake-mode magit dired+ minions helm doom-themes solarized-theme moody gdb-mi quelpa realgud shell-pop c++-mode flycheck ivy-posframe ivy-postframe all-the-icons lsp-haskell haskell-mode dap-mode lsp-treemacs lsp-ivy lsp-ui lsp-mode treemacs-evil treemacs org-pdfview pdf-tools good-scroll auctex tex latex company-math cdlatex undo-tree hydra evil-collection evil helpful ivy-rich counsel which-key rainbow-delimiters rainbow-delimeters use-package ivy)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error ((t (:foreground "indian red" :weight normal))))
 '(error ((t (:foreground "indian red" :weight normal))))
 '(flycheck-error-list-error ((t (:foreground "indian red" :width normal))))
 '(flycheck-fringe-error ((t (:foreground "indian red" :weight normal))))
 '(lsp-ui-peek-filename ((t (:foreground "white" :weight bold))))
 '(lsp-ui-peek-header ((t (:background "#434C5E" :foreground "black"))))
 '(lsp-ui-peek-highlight ((t (:background "#434C5E"))))
 '(lsp-ui-peek-line-number ((t (:foreground "white"))))
 '(lsp-ui-peek-list ((t (:background "#272A36"))))
 '(lsp-ui-peek-peek ((t (:background "#272A36"))))
 '(lsp-ui-peek-selection ((t (:background "#434C5E"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "gold" :weight normal)))))
(put 'dired-find-alternate-file 'disabled nil)
