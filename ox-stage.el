;;; ox-stage.el --- Export Org drama scripts to stage.cls  -*- lexical-binding: t; -*-
;; Copyright (C) 2025  Joseph Huang

;; Author: Joseph Huang <josephtesfaye022@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; Keywords: outlines, tex, wp
;; URL: https://josephtesfaye.github.com/ox-stage

;;; Commentary:
;;
;; This library defines an Org-mode export back-end – `stage' – that turns
;; specially-shaped Org files into LaTeX documents using the `stage.cls'
;; class (see `texdoc stage'). The mapping rules are hard-wired so that authors
;; only write plain Org syntax; all theater-specific macros are generated
;; automatically.
;;
;; See the README for the Org examples.

;;; Code:
(require 'ox)
(require 'ox-latex)

;; Install a default set-up
(when-let (((null (assoc "stage" org-latex-classes)))
           (headers (concat "\\documentclass{stage}")))
  (add-to-list 'org-latex-classes (list "stage" headers nil)))

;; Entry points
;;;###autoload
(defun org-stage-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a stage script (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'stage file
      async subtreep visible-only body-only ext-plist)))

(defun org-stage-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a stage script (PDF).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'stage file
      async subtreep visible-only body-only ext-plist #'org-latex-compile)))

;; Define Backend
(org-export-define-derived-backend 'stage 'latex
  ;; Overwrite existing transcoders defined in the `latex' backend.
  :translate-alist '((headline  . org-stage--headline)
                     (paragraph . org-stage--paragraph)
                     (plain-list . org-stage--plain-list)
                     (template . org-stage--template))

  ;; Add or redefine options. See `org-export-options-alist'.
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "stage" t)
    ;; Choose a supported script style (CJK, ...)
    (:stage-script "STAGE_SCRIPT" "script" nil)
    ;; Disable date, toc, speical strings, etc. by default, same as
    ;; `#+OPTIONS: date:nil toc:nil -:nil' in an Org file.
    (:with-date nil "date" nil)
    (:with-toc nil "toc" nil)
    (:with-special-strings nil "-" nil)
    (:latex-hyperref-template nil nil nil t))

  :menu-entry
  '(?l 1
       ((?s "As LaTeX file (stage)" org-stage-export-to-latex)
        (?S "As PDF file (stage)" org-stage-export-to-pdf))))

;; Transcode Functions
(defun org-stage--template (contents info)
  "Return complete document string after conversion. CONTENTS is the transcoded
contents string. INFO is a plist holding export options."

  (let ((headers (plist-get info :latex-header)))
    ;; Combine predefined headers with user defined headers. See
    ;; `org-latex-make-preamble'.
    (pcase (plist-get info :stage-script)
      ;; Compile with `xeCJK' and support Japanese (main), Chinese (simplified),
      ;; symbols, emojis.
      ("xeJC" (setq info (plist-put info :latex-header (concat "% ==== Mixed Scripts ====
% ---- Chinese, Japanese ----
\\usepackage{xeCJK}
\\xeCJKsetup{AutoFallBack=true}
\\setCJKmainfont{BIZ UDMincho}
\\setCJKfallbackfamilyfont{\\CJKrmdefault}{Sarasa Fixed SC}

% ---- Furigana ----
\\usepackage{ruby}
% Tune look & spacing
\\renewcommand\\rubysep{0.0ex}    % vertical gap (default −0.5 ex)
\\renewcommand\\rubysize{0.5}     % ruby 50% of main size
\\setlength{\\lineskiplimit}{2pt} % prevents lines from touching when ruby is tall

% ---- Symbols ----
\\usepackage{unicode-math}
\\setmathfont{STIX Two Math}

\\usepackage{newunicodechar}
% For symbols found in the unicode-math font
\\newunicodechar{→}{\\(→\\)}
\\newunicodechar{←}{\\(←\\)}
\\newunicodechar{↓}{\\(↓\\)}
\\newunicodechar{↑}{\\(↑\\)}
\\newunicodechar{⟶}{\\(⟶\\)}
\\newunicodechar{⟵}{\\(⟵\\)}
\\newunicodechar{⇄}{\\(⇄\\)}
\\newunicodechar{⇆}{\\(⇆\\)}
\\newunicodechar{⟷}{\\(⟷\\)}
\\newunicodechar{⟺}{\\(⟺\\)}
\\newunicodechar{○}{\\(○\\)}
\\newunicodechar{✓}{\\(✓\\)}

% For symbols not found in the unicode-math font
\\newfontfamily\\symbolsfont{Sarasa Mono CL}[Scale=1.0]
\\newunicodechar{⮂}{{\\symbolsfont ⮂}}
\\newunicodechar{⮀}{{\\symbolsfont ⮀}}

% ---- Emojis ----
\\usepackage{bxcoloremoji}

% ---- Translations ----
% Mark any inline translation in a smaller, lighter style
\\usepackage{xcolor}             % provides gray
\\newcommand{\\trans}[1]{\\begingroup\\small\\color{gray}#1\\endgroup}
\\NewDocumentEnvironment{translation}{}
  {\\par\\small\\color{gray}\\ignorespaces}
  {\\par\\ignorespacesafterend}

% ---- Customize title ----
% Make title bigger
% titling hooks before \\maketitle, so it overrides the size even though the
% class redefines the command internally.
\\usepackage{titling}
\\pretitle{\\begin{center}\\Huge\\bfseries\\textsc}
\\posttitle{\\end{center}}
\\preauthor{\\begin{center}\\Large}
\\postauthor{\\end{center}}

" headers)))))

    (setq contents (org-stage--convert-furigana contents))
    (org-latex-template contents info)))

(defun org-stage--headline (headline contents info)
  "Translate HEADLINE to LaTeX according to stage rules."
  (cond
   ((org-stage--regex-headline-p headline "\\`[Cc]ast")
    (format "\\begin{castpage}\n%s\\end{castpage}\n" contents))
   ((org-stage--regex-headline-p headline "\\`[Aa]ct")
    (concat "\\act\n" contents))
   ((org-stage--regex-headline-p headline "\\`[Ss]cene")
    (concat "\\scene\n" contents))
   (t (org-latex-headline headline contents info))))

(defun org-stage--paragraph (paragraph contents info)
  "Maybe wrap paragraph CONTENTS in `\opensd{}' or `\stage{}' depending on
context."

  (cl-block nil
    ;; Exclude paragraphs from a list item
    (when (equal 'item (org-element-type (org-export-get-parent paragraph)))
      (cl-return contents))

    (let* ((prev (org-export-get-previous-element paragraph info))
           (next (org-export-get-next-element paragraph info))
           (headline (org-export-get-parent-headline paragraph))
           (scene-first? (and headline
                              (org-stage--regex-headline-p headline "\\`[Ss]cene")
                              (null prev)))
           (between-lists? (and (equal 'plain-list (org-element-type prev))
                                (equal 'plain-list (org-element-type next))))
           (raw-contents (string-trim contents)))
      (cond
       (scene-first? (format "\\opensd{%s}\n" raw-contents))
       (between-lists? (format "\\stage{%s}\n" raw-contents))
       (t contents)))))

(defun org-stage--plain-list (list contents info)
  "Emit list CONTENTS verbatim—no itemize/enumerate wrapper."

  ;; (concat contents "\n")

  (let* ((headline (org-export-get-parent-headline list))
         (in-cast? (and headline (org-stage--regex-headline-p headline "\\`[Cc]ast")))
         (items (org-list-get-items-in-region (org-element-property :begin list))))
    (if in-cast?
        (mapconcat
         (lambda (item)
           (pcase-let ((`(,name . ,body) (org-stage--item-body item)))
             (format "  \\addcharacter{%s}{%s}" name body)))
         items "\n")

      (mapconcat
       (lambda (item)
         (pcase-let ((`(,name . ,body) (org-stage--item-body item)))
           (format "\\dialog{%s}{%s}" name body)))
       items "\n"))))

(defun org-stage--regex-headline-p (headline re)
  "Does HEADLINE's raw value match RE (case-insensitive)?"
  (let ((title (org-element-property :raw-value headline)))
    (and title (string-match-p re title))))

(defun org-stage--convert-furigana (contents)
  "Convert the Org Mode furigana format `(word:furigana)' to TeX format
`\ruby{word}{furigana}'."

  (let ((regexp "[(（]\\(.*?\\)[:：]\\(.*?\\)[)）]"))
    (replace-regexp-in-string
     regexp
     (lambda (match)
       (when (string-match regexp match)
         (format "\\\\ruby{%s}{%s}"
                 (match-string 1 match)
                 (match-string 2 match))))
     contents)))

(defun org-stage--item-body (point)
  "Return the value of a key-value pair from the item at POINT."

  ;; Parse translation, if any
  (pcase-let* ((`(,name . ,body) (org-stage--list-key-item point))
               (parts (org-stage--string-split-to-nth body "\n" 1))
               (trans (cadr parts))
               (blanks (when (and trans (string-match "[^ ]" trans))
                         (substring trans 0 (match-beginning 0)))))
    (when trans
      (setq body (concat (car parts) "\\\\\n" blanks "\\trans{"
                         (string-trim trans) "}")))
    (cons name body)))

(defun org-stage--list-key-item (&optional point)
  "A \"key item\" is an item specially formatted as \"- <key>: <value>\", where
the key and value can be any string separated by a colon. Return the item at
POINT as a key-value pair in a cons cell. If POINT is `nil' the current point is
used. If POINT is not on a key item return `nil'."

  (when-let ((content (org-list-item-get-content point))
             (strs (org-stage--string-split-to-nth content "[:：]" 1))
             ((length> strs 1)))
    (cons (car strs) (string-trim (cadr strs)))))

(defun org-stage--string-split-to-nth (string separator n)
  "Split STRING by SEPARATOR, but only to the N-th occurrence of the SEPARATOR."

  (let ((start 0)
        (num 0)
        (list '())
        (substr nil))
    (while (and (< num n) (string-match separator string start))
      (setq substr (substring string start (match-beginning 0)))
      (when (length> substr 0)          ; Remove empty strings
        (push substr list))
      (setq num (1+ num))
      (setq start (match-end 0)))
    (push (substring string (match-end 0)) list)
    (nreverse list)))

(provide 'ox-stage)
