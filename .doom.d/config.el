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

;; Superior scrolling
(require 'good-scroll)
(good-scroll-mode 1)

;; Fix emacs' $PATH envvar
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" ""
                          (shell-command-to-string
                           "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


(defun what-face ()
  "Check face under cursor."
  (interactive)
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

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

;; ---> LSP <---

;; Customize LSP code actions look
(require 'dash)
(require 'lsp-ui)
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

(define-key! lsp-mode-map
  [remap +lookup/definition] #'lsp-ui-peek-find-definitions
  [remap +lookup/references] #'lsp-ui-peek-find-references)

;; ---> Org mode <---

(require 'org)
(require 'cl-lib)

;; Org parallel enviroment
(defun org-special-block-extras--parallel (backend _ contents __)
  "Return CONTENTS exported to BACKEND and CONTENTS.
This adds to orgmode block that enables user to write text side by side.

Text in CONTENTS should be separated with org-like rules."

  (if (eq backend 'latex)
      (let* ((rule-string "\\noindent\\rule{\\textwidth}{0.5pt}")

             (rule-length (length rule-string))
             (content-length (length contents))

             (rules (cl-loop for search-in = contents then (substring search-in
                                                                      (+ search-index
                                                                         rule-length))
                             for search-index = (cl-search rule-string search-in)
                             until (null search-index)
                             collect (+ search-index
                                        (- content-length
                                           (length search-in))))))
        (if (= 0 (length rules))
            contents

          (concat "\\vspace{1em}"
                  (cl-loop for begin = 0 then (+ rule rule-length 1)
                           for rule in rules
                           concat (format (concat page-format "\\hspace{0.025\\textwidth}\\vrule\\hspace{0.025\\textwidth}")
                                          (substring contents begin (1- rule))) into result
                           finally return
                           (concat result (format (concat page-format "\\vspace{1em}")
                                                  (substring contents (+ 1 rule rule-length))))
                           with page-format = (format "\n\\begin{minipage}{%.3f\\textwidth}\n%%s\\end{minipage}"
                                                      (- (/ 1.0 (1+ (length rules))) 0.05))))))))

(advice-add #'org-latex-special-block
            :before-until
            (apply-partially #'org-special-block-extras--parallel
                             'latex))

;; Enable org-mode to export org file with cyrillic to pdf
(setq org-latex-packages-alist
      '(("utf8" "inputenc" t)
        ("T2A" "fontenc" t)
        ("english,russian" "babel" t)

        ("" "multicol" f)))

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

;; --------------------> SCIMAX-INKSCAPE <--------------------
(defcustom scimax-inkscape-thumbnail-width 300
  "Width of thumbnails in pts."
  :group 'scimax-inkscape)

(defcustom scimax-inkscape-template-svg
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"6in\"
   height=\"4in\"
   viewBox=\"0 100 152.4 201.6\"
   version=\"1.1\"
   id=\"svg8\"
   inkscape:version=\"0.92.2 (5c3e80d, 2017-08-06)\"
   sodipodi:docname=\"drawing.svg\">
  <defs
     id=\"defs2\" />
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#ffffff\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"0.0\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"1\"
     inkscape:cx=\"400\"
     inkscape:cy=\"214.9\"
     inkscape:document-units=\"in\"
     inkscape:current-layer=\"layer1\"
     showgrid=\"false\"
     units=\"in\"
     inkscape:window-width=\"1080\"
     inkscape:window-height=\"675\"
     inkscape:window-x=\"0\"
     inkscape:window-y=\"78\"
     inkscape:window-maximized=\"0\"
     inkscape:lockguides=\"true\"
     fit-margin-top=\"0\"
     fit-margin-left=\"0\"
     fit-margin-right=\"0\"
     fit-margin-bottom=\"0\" />
  <metadata
     id=\"metadata5\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
        <dc:title></dc:title>
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label=\"Layer 1\"
     inkscape:groupmode=\"layer\"
     id=\"layer1\"
     transform=\"translate(0,0)\" />
</svg>
"
  "Blank document for inkscape. You cannot create a file at the
  command line, so we put this template in and open it. This one works for Inkscape 0.92.2")


(defun scimax-inkscape-open (path)
  "Open the PATH in inkscape.
Make a new file if needed."
  (interactive)
  (unless (f-ext-p path "svg") (error "Must be an svg file."))
  (unless (file-exists-p path)
    (with-temp-file path
      (insert scimax-inkscape-template-svg)))
  (let ((display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window . ())))))
    (shell-command (format "inkscape %s &" path))))


(defun scimax-inkscape-thumbnail (start end path bracketp)
  "Put a thumbnail on an inkscape link."
  (let (img ov)
    (when (and
	   ;; got a path
	   path
	   ;; it is an image
	   (org-string-match-p (image-file-name-regexp) path)
	   ;; and it exists
	   (f-exists? path)
	   ;; and there is no overlay here.
	   (not (ov-at start)))
      (setq img (create-image
		 (expand-file-name path)
		 'imagemagick nil :width scimax-inkscape-thumbnail-width
		 :background "lightgray"))
      (setq ov (make-overlay start end))
      (overlay-put ov 'display img)
      (overlay-put ov 'face 'default)
      ;; (overlay-put ov 'before-string "inkscape:")
      (overlay-put ov 'org-image-overlay t)
      (overlay-put ov 'modification-hooks
		   (list
		    `(lambda (&rest args)
		       (org-display-inline-remove-overlay ,ov t ,start ,end))))
      (push ov org-inline-image-overlays))))


(defun scimax-inkscape-redraw-thumbnails (&rest args)
  "Use font-lock to redraw the links."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-restart-font-lock)))

;; This gets the thumbnails to be redrawn with inline image toggling.
(advice-add 'org-display-inline-images :after 'scimax-inkscape-redraw-thumbnails)


(defun scimax-inkscape-preprocess (backend)
  "Preprocessing function to run in `org-export-before-processing-hook'.
Here are two examples:
 (browse-url (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
  (org-html-export-to-html)))
 (org-open-file (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
  (org-latex-export-to-pdf)))"
  (let ((links (reverse (org-element-map (org-element-parse-buffer) 'link
			  (lambda (link)
			    (when (string= (org-element-property :type link) "inkscape")
			      link))))))
    (cl-loop for link in links
	     do
	     (goto-char (org-element-property :begin link))
	     (re-search-forward "inkscape:" (org-element-property :end link))
	     (replace-match "file:"))))


(org-link-set-parameters
 "inkscape"
 :follow 'scimax-inkscape-open
 :help-echo "Click to open in inkscape."
 :activate-func 'scimax-inkscape-thumbnail
 :export (lambda (path desc backend)
	   ;;  You need to use the `scimax-inkscape-preprocess' function in a hook for
	   ;; more advanced export options like captions.
	   (cond
	    ((eq 'latex backend)
	     (format "\\includesvg{%s}" path))
	    ((eq 'html backend)
	     (format "<img src=\"%s\"" path)))))


(defun scimax-inkscape-insert-drawing (path)
  "Convenience function to insert a drawing with filename PATH."
  (interactive "sFilename: ")
  (insert (format "inkscape:%s" path)))
;; -----------------------------------------------------------
;; -------------------- EVALUATE WITH SAGE -------------------
(defun org-special-replace-between (left right text)
  "Replace content between LEFT and RIGHT with TEXT.
Cursor should be between BEGIN and END."

  (let ((begin (search-forward right))
        (end (search-backward left)))

    (kill-region begin end)
    (insert text)))


(defun org-special-get-text-between (left right)
  "Return content of surrounding cursor LEFT and RIGHT."

  (save-excursion
    (let ((begin (search-forward right))
          (end (search-backward left)))

      (buffer-substring (- begin
                           (length left))
                        (+ end
                           (length right))))))


(defun org-special-get-sage-output ()
  "Get sage output from *sage-output* buffer."

  (with-current-buffer "*sage-output*"
    (goto-char (point-min))
    (forward-line 4)
    (buffer-substring (+ (point) 40)
                      (line-end-position))))


(defun org-special-evaluate-with-sage (left right surround)
  "Evaluate content of surrounding LEFT RIGHT with sage and replace with result.
Result will be in LaTeX math notation and surrounded according to SURROUND.

When SURROUND is 'inline or 'display the result will be surrounded with
inline and display math notation accordingly.

Otherwise the result will not be surrounded with anything."

  (interactive)

  (if (get-buffer "*sage-output*")
      (with-current-buffer "*sage-output*"
        (erase-buffer)))

  (set-process-sentinel
   (start-process "sage-evaluate-expression"
                  "*sage-output*" "bash" "-c"
                  (format "sage <<< 'show(%s)'"
                          (org-special-get-text-between left right)))

   (lambda (&rest _)
     (org-special-replace-between left right
      (format (cond ((eq surround 'inline) "\\(%s\\)")
                    ((eq surround 'display) "\\[%s\\]")
                    (t "%s"))

              (org-special-get-sage-output))))))


(defun org-special-evaluate-with-sage-inline ()
  "See org-special-evaluate-with-sage. It the same thing.
But with default arguments \"{{\" \"}}\" 'inline."

  (interactive)
  (org-special-evaluate-with-sage "{{" "}}" 'inline))


(defun org-special-evaluate-with-sage-display ()
  "See org-special-evaluate-with-sage. It the same thing.
But with default arguments \"{{\" \"}}\" 'display."

  (interactive)
  (org-special-evaluate-with-sage "{{" "}}" 'display))


(defun org-special-evaluate-with-sage-no-surround ()
  "See org-special-evaluate-with-sage. It the same thing.
But with default arguments \"{{\" \"}}\" nil."

  (interactive)
  (org-special-evaluate-with-sage "{{" "}}" nil))


(with-eval-after-load 'org
  (defvar org-mode-map)

  (bind-key "C-c e i" #'org-special-evaluate-with-sage-inline
            org-mode-map)

  (bind-key "C-c e d" #'org-special-evaluate-with-sage-display
            org-mode-map)

  (bind-key "C-c e n" #'org-special-evaluate-with-sage-no-surround
            org-mode-map))

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("extarticle"
                 "\\documentclass{extarticle}"

                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


(setq org-preview-latex-default-process 'dvisvgm)

(require 'org-ref)

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(defcustom projects-directory "~/projects/"
  "Directory with projects.")


(defun create-new-project ()
  "Create new project in default location specified by `projects-directory'."

  (interactive)

  (let* ((name (read-string "Enter name for new project: "))
         (directory (concat projects-directory name)))
    (mkdir directory)
    (cd directory)
    (magit-init directory)
    (counsel-find-file)))
;; -------------------- --------------------------------------

;; ---> Ukranian VNO test <---
(defun eval-file (file)
  "Execute FILE and return the result of the last expression."
  (eval
   (ignore-errors
     (read-from-whole-string
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))))

(eval-file "~/.doom.d/ukranian-vno.el")

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
