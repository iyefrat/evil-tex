;;; evil-tex.el --- Useful features for editing TeX in evil-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yoav Marco, Itai Y. Efrat
;;
;; Author: Yoav Marco <http://github/yoavm448>, Itai Y. Efrat <http://github/itai33>
;; Maintainers: Yoav Marco <yoavm448@gmail.com>, Itai Y. Efrat <itai3397@gmail.com>
;; Created: February 01, 2020
;; Modified: February 01, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/itai33/evil-tex
;; Package-Requires: ((evil "1.0") (auctex "11.88") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Useful features for editing TeX in evil-mode
;;
;;; Code:


(require 'cl-lib)
(require 'latex)
(require 'evil)


;;; helper functions for text objects

(defun evil-tex-max-key (seq fn &optional compare-fn)
  "Return the element of SEQ for which FN gives the biggest result.

Comparison is done with COMPARE-FN if defined, and with `>' if not.
\(evil-tex-max-key '(1 2 -4) (lambda (x) (* x x))) => -4"
  (let* ((res (car seq))
         (res-val (funcall fn res))
         (compare-fn (or compare-fn #'>)))
    (dolist (cur (cdr seq))
      (let ((cur-val (funcall fn cur)))
        (when (funcall compare-fn cur-val res-val)
          (setq res-val cur-val
                res cur))))
    res))

(defun evil-tex--delim-compare (a b)
  "Receive two cons' A B of structure (LR IA BEG END ...).
where LR is t for e.g. \\left( and nil for e.g. (,
 IA is t for -an- text objects and nil for -inner-,
BEG and END are the coordinates for the begining and end of the potential delim,
and the _'s are unimportant.
Compare between the delimiters to find which one has the largest BEG, while
making sure to choose [[\\left(]] over the \\left[[(]] delimiter evil-tex--delim finds"

  (let ((a-lr (nth 0 a))
        (b-lr (nth 0 b))
        (ia (nth 1 a))
        (a-beg (nth 2 a))
        (b-beg (nth 2 b)))
    (cond
     ((not a)                       nil)
     ((not b)                       t)
     ((and ia (not (or a-lr b-lr))) (> a-beg b-beg))
     ((and ia (and a-lr b-lr))       (> a-beg b-beg))
     ((and ia a-lr)                 (if (= (+ a-beg 5) b-beg) t   (> (+ a-beg 5) b-beg)))
     ((and ia b-lr)                 (if (= a-beg (+ b-beg 5)) nil (> a-beg (+ b-beg 5))))
     ((not (or a-lr b-lr))          (> a-beg b-beg))
     ((and a-lr b-lr)                (> a-beg b-beg))
     (a-lr                          (if (= a-beg b-beg) t (> a-beg b-beg)))
     (b-lr                          (if (= a-beg b-beg) nil (> a-beg b-beg)))
     (t                             nil))))

(defun evil-tex--delim-finder (lr deliml delimr args)
  "Return delimiter location (and more) for evil-tex--select-delim.
LR is t for e.g. \\left( and nil for e.g. (.
DELIML and DELIMR is a string containing the non \\left part of the delimiter.
ARGS is the information about the text object needed for the functions to work,
such as whether the delimiter is an \\left( type or a ( type,
and if the text object is an -an- or an -inner-"

  (let ((delim-pair-lr (ignore-errors
                         (apply #'evil-select-paren
                                (regexp-quote (concat "\\left" deliml))
                                (regexp-quote (concat "\\right" delimr)) args)))
        (delim-pair-not-lr (ignore-errors
                             (apply #'evil-select-paren
                                    (regexp-quote deliml)
                                    (regexp-quote delimr) args))))

    (if lr ; checks if there is a delimiter of the searched type. if so returns the needed information, if not returns nil.
        (when delim-pair-lr
          (cons t (cons (car (last args))
                        delim-pair-lr)))
      (when delim-pair-not-lr
        (cons nil (cons (car (last args))
                        delim-pair-not-lr))))))

(defun evil-tex--select-delim (&rest args)
  "Return (start . end) of closes delimiter match.

ARGS passed to evil-select-paren, within evil-tex--delim-finder."
  (cddr (evil-tex-max-key
         (list (evil-tex--delim-finder nil "(" ")" args)
               (evil-tex--delim-finder t "(" ")" args) ; when t finds \left(foo\right) instead
               (evil-tex--delim-finder nil "[" "]" args)
               (evil-tex--delim-finder t "[" "]" args)
               (evil-tex--delim-finder nil "\\{" "\\}" args)
               (evil-tex--delim-finder t "\\{" "\\}" args)
               (evil-tex--delim-finder nil "\\langle" "\\rangle" args)
               (evil-tex--delim-finder t "\\langle" "\\rangle" args))
         (lambda (arg) (when (consp arg) ; check if selection succeeded
                         arg))
         #'evil-tex--delim-compare)))

(defvar evil-tex--last-command-empty nil
  "Global to hold if the last command text object used was empty.

For example, \\epsilon is empty, \\dv{x} is not.")

(defun evil-tex--select-command ()
  "Return command (macro) text object boundries.
Inner commmand defined to be what is inside {}'s and []'s,
or empty if none exist.

Return in format (list beg-an end-an beg-inner end-inner)"
  (let ((beg-an (TeX-find-macro-start))
        (end-an (TeX-find-macro-end))
        beg-inner end-inner)
    (save-excursion
      (goto-char beg-an)
      (if (re-search-forward "{\\|\\[" end-an t)
          (setq evil-tex--last-command-empty nil)
        (setq evil-tex--last-command-empty t)))
    (unless beg-an
      (user-error "No surrounding command found"))
    (save-excursion
      (goto-char beg-an)
      (if (re-search-forward "{\\|\\[" end-an t)
          (setq beg-inner (point))
        (setq beg-inner end-an))) ;goto opeing brace if exists.
    (save-excursion
      (goto-char end-an)
      (when (looking-back "}\\|\\]" (- (point) 2))
        (backward-char))
      (setq end-inner (point)) ; set end of inner to be {|} only in command is not empty
      (list beg-an end-an beg-inner end-inner))))

(defvar evil-tex-include-newlines-in-envs t
  "Whether include newlines with env insersion.

When non-nil, env insersions would force separate lines for
\\begin, inner text, and \\end.")

(defvar evil-tex-select-newlines-with-envs t
  "Whether to select newlines with env commands.

When non-nil, the newline proceeding \\begin{...} and preceding
\\end{...} is selected as part of the delimiter. This way, when
doing 'cie' you're placed on a separate line.")

(defun evil-tex--select-env ()
  "Return (list outer-beg outer-end inner-beg inner-end) for enviornment object.
with the -an- text object beign the first and last ^'s,
 and the -inner- being the inner ones.
Moves the inner ^'s to before/after the newline if `evil-tex-select-newlines-with-envs' is t.
\\begin{foo}{bar}[baz]
^                    ^
qux
\\end{foo}
^        ^"
  (let (outer-beg outer-end inner-beg inner-end)
    (save-excursion
      (unless (looking-at (regexp-quote "\\begin{"))
        (LaTeX-find-matching-begin))
      ;; We are at backslash
      (setq outer-beg (point))
      (goto-char (TeX-find-macro-end))
      (when (and evil-tex-select-newlines-with-envs
                 (looking-at "\n"))
        (forward-char))
      (setq inner-beg (point))
      (goto-char (1+ outer-beg))
      (LaTeX-find-matching-end)        ; we are at closing brace
      (setq outer-end (point))
      (search-backward "\\end")        ; goto backslash
      (when (and evil-tex-select-newlines-with-envs
                 (looking-back "\n" (1- (point))))
        (backward-char))
      (setq inner-end (point))
      (list outer-beg outer-end inner-beg inner-end))))

(defvar evil-tex--section-regexp "\\\\\\(part\\|chapter\\|subsubsection\\|subsection\\|section\\|subparagraph\\|paragraph\\)\\*?"
  "Regexp that matches for LaTeX section commands.")

(defun evil-tex--section-regexp-higher (str)
  "For section type STR, return regex that only match higher sections."
  (cond
   ((string-match "\\\\part\\*?" str)  "\\\\part\\*?")
   ((string-match "\\\\chapter\\*?" str)   "\\\\\\(part\\|chapter\\)\\*?")
   ((string-match "\\\\section\\*?" str)   "\\\\\\(part\\|chapter\\|section\\)\\*?")
   ((string-match "\\\\subsection\\*?" str)   "\\\\\\(part\\|chapter\\|subsection\\|section\\)\\*?")
   ((string-match "\\\\subsubsection\\*?" str)   "\\\\\\(part\\|chapter\\|subsubsection\\|subsection\\|section\\)\\*?")
   ((string-match "\\\\paragraph\\*?" str)   "\\\\\\(part\\|chapter\\|subsubsection\\|subsection\\|section\\|paragraph\\)\\*?")
   ((string-match "\\\\subparagraph\\*?" str)   "\\\\\\(part\\|chapter\\|subsubsection\\|subsection\\|section\\|subparagraph\\|paragraph\\)\\*?")))

(defun evil-tex--select-section ()
  "Return (outer-beg outer-end inner-beg inner-end type) for section object.

The outer -an- variant is defined from the first character of the
\\section{} command, to the line above the next \\section{}
command of equal or higher rank, e.g. \\chapter{}. The inner
variant starts after the end of the command, and respects a
following newline if exists.

type is the type of the heading, e.g subsection, chapter*.

\\section{} and \\section*{} are treated the same."
  (let (outer-beg outer-end inner-beg inner-end what-section)
    (save-excursion
      ;; back searching won't work if we are on the \section itself
      (search-backward "\\" (line-beginning-position) t)
      (if (looking-at evil-tex--section-regexp)
          (setq what-section (match-string 0))
        (re-search-backward evil-tex--section-regexp)
        (setq what-section (match-string 0)))
      ;; We are at backslash
      (setq outer-beg (point))
      (skip-chars-forward "^{")        ; goto opening brace
      (forward-sexp)                   ; goto closing brace
      (when (and evil-tex-select-newlines-with-envs
                 (looking-at "\n"))
        (forward-line 1))
      (setq inner-beg (point))
      (re-search-forward (concat (evil-tex--section-regexp-higher what-section) "\\|\\\\end{document}"))
      (move-beginning-of-line 1)
      (setq inner-end (point))
      (setq outer-end (point))
      (list outer-beg outer-end inner-beg inner-end what-section))))

(defun evil-tex--goto-script-prefix (subsup)
  "Return goto end of the found SUBSUP prefix.
{(ab)}_c => {(ab)}_c
    ^              ^"
  (let ((orig-point (point))
        subsup-end)
    (or
     ;; subsup after point
     (when (search-forward subsup (line-end-position 2) t) ; 2 lines down
       (let (beg end)
         (setq subsup-end (match-end 0))
         (goto-char (match-beginning 0))
         (setq end (point))
         (when
             (cond
              ;; {}^
              ((eq (char-before) ?})
               (backward-sexp)
               (setq beg (point)))
              ;; \command^
              ((and (search-backward "\\" (line-beginning-position) t)
                    (looking-at "\\\\[A-Za-z@*]+")
                    (eq end (match-end 0)))
               (setq beg (match-beginning 0)))
              ;; a^
              (t
               (setq beg (1- (point)))))
           ;; require point to be inside the base bounds
           (<= beg orig-point end))))
     ;; subsup before point
     (when (search-backward subsup (line-beginning-position 0))
       (setq subsup-end (match-end 0)))
     (user-error "No surrounding %s found" subsup))
    (goto-char subsup-end)))

(defun evil-tex-script-beginning-begend (subsup)
  "Return (start . end) of the sub/superscript that point is in.
SUBSUP should be either \"^\" or \"_\"

a_{n+1}
 ^^"
  (let (start)
    (save-excursion
      (evil-tex--goto-script-prefix subsup)
      (setq start (1- (point)))
      (when (looking-at "{") ; select brace if present
        (forward-char 1))
      (cons start (point)))))

(defun evil-tex-script-end-begend (subsup)
  "Return (start . end) of the sub/superscript that point is in.
SUBSUP should be either \"^\" or \"_\"

a_{n+1}
      ^"
  (save-excursion
    (evil-tex--goto-script-prefix subsup)
    (cond
     ;; a_{something}
     ((looking-at "{")
      (forward-sexp)
      (cons (1- (point)) (point)))
     ;; a_\something
     ((looking-at "\\\\[a-zA-Z@*]+")
      (goto-char (match-end 0))
      ;; skip command arguments
      (while (looking-at "{\\|\\[")
        (forward-sexp))
      (cons (point) (point)))
     (t ;; a_1 a_n
      (forward-char)
      (cons (point) (point))))))

(defun evil-tex--select-table-cell ()
  "Return (list outer-beg outer-end inner-beg inner-end) for table cell."
  (let (
        outer-beg outer-end
        inner-beg inner-end
        env-beg env-end
        (found-beg nil) (found-end nil))
    (save-excursion
      (setq env-beg (nth 2 (evil-tex--select-env)))
      (setq env-end (nth 3 (evil-tex--select-env)))
      (when (or (> env-beg (point)) (< env-end (point))) ;; for when you are on \begin or \end of a sub-env.
        (save-excursion
          (goto-char (1- (nth 0 (evil-tex--select-env))))
          (setq env-beg (nth 2 (evil-tex--select-env)))
          (setq env-end (nth 3 (evil-tex--select-env)))))
      (while (not found-beg)
        (cond ;; incase we are already at &
         ((looking-at "&")          (setq outer-beg (point)
                                          inner-beg (1+ (point))
                                          found-beg t)))
        (unless found-beg
          (if (re-search-backward "&\\|\\\\\\\\\\|\\\\end" env-beg t)
              (cond
               ((looking-at "&")          (setq outer-beg (point)
                                                inner-beg (1+ (point))
                                                found-beg t))
               ((looking-at "\\\\\\\\\n") (setq outer-beg (+ 3 (point))
                                                inner-beg (+ 4 (point))
                                                found-beg t))
               ((looking-at "\\\\\\\\")   (setq outer-beg (+ 2 (point))
                                                inner-beg (+ 3 (point))
                                                found-beg t))
               ((looking-at "\\\\end")    (LaTeX-find-matching-begin)))
            (setq outer-beg env-beg
                  inner-beg env-beg
                  found-beg t))))
      (goto-char inner-beg)
      (while (not found-end)
        (if (re-search-forward "&\\|\\\\\\\\\\|\\\\begin" env-end t)
            (progn
              (backward-char)
              (cond
               ((looking-at "&")      (setq outer-end (1+ (point))
                                            inner-end (point)
                                            found-end t))
               ((looking-at "\\\\")   (setq outer-end (1+ (point))
                                            inner-end (1- (point))
                                            found-end t))
               ((looking-at "n")      (LaTeX-find-matching-end))))
          (setq outer-end env-end
                inner-end env-end
                found-end t)))
      (list outer-beg outer-end inner-beg inner-end))))


;;; Toggles

(defun evil-tex--regexp-overlay-replace (deliml delimr an-over in-over)
  "Replace surround area with new delimiters.
Take the surround area defined by overlays AN-OVER and IN-OVER,
delete the parts of AN-OVER that don't overlap with IN-OVER, and surround
the remaining IN-OVER with new delimiters DELIML and DELIMR.
Should be used inside of a 'save-excursion'."
  (progn (delete-region (overlay-start an-over) (overlay-start in-over))
         (goto-char (overlay-start an-over))
         (insert deliml)
         (delete-region (overlay-end in-over) (overlay-end an-over))
         (goto-char (overlay-end in-over))
         (insert delimr)))

(defun evil-tex-toggle-delim ()
  "Toggle surrounding delimiters between e.g. (foo) and \\left(foo\\right) ."
  (let ((an-over (make-overlay (car (evil-tex-a-delim)) (cadr (evil-tex-a-delim))))
        (in-over (make-overlay (car (evil-tex-inner-delim)) (cadr (evil-tex-inner-delim)))))
    (save-excursion
      (goto-char (overlay-start an-over))
      (cl-destructuring-bind (l . r)
          (cond
           ((looking-at (regexp-quote "("))
            '("\\left(" . "\\right)"))
           ((looking-at (regexp-quote "\\left("))
            '("(" . ")"))
           ((looking-at (regexp-quote "["))
            '("\\left[" . "\\right]"))
           ((looking-at (regexp-quote "\\left["))
            '("[" . "]"))
           ((looking-at (regexp-quote "\\{"))
            '("\\left\\{" . "\\right\\}"))
           ((looking-at (regexp-quote "\\left\\{"))
            '("\\{" . "\\}"))
           ((looking-at (regexp-quote "\\langle"))
            '("\\left\\langle" . "\\right\\rangle"))
           ((looking-at (regexp-quote "\\left\\langle"))
            '("\\langle" . "\\rangle"))
           (t
            (user-error "No surrounding delimiter found")))
        (evil-tex--regexp-overlay-replace l r an-over in-over)))
    (delete-overlay an-over) (delete-overlay in-over)))

(defun evil-tex-toggle-env ()
  "Toggle surrounding enviornments between e.g. \\begin{equation} and \\begin{equation*}."
  (let ((an-over (make-overlay (car (evil-tex-an-env)) (cadr (evil-tex-an-env))))
        (in-over (make-overlay (car (evil-tex-inner-env)) (cadr (evil-tex-inner-env)))))
    (save-excursion
      (goto-char (overlay-start an-over))
      (skip-chars-forward "^}")
      (backward-char 1)
      (if (eq ?* (char-after)) (delete-char 1) (progn (forward-char 1) (insert-char ?*)))
      (goto-char (overlay-end in-over))
      (skip-chars-forward "^}")
      (backward-char 1)
      (if (eq ?* (char-after)) (delete-char 1) (progn (forward-char 1) (insert-char ?*))))
    (delete-overlay an-over) (delete-overlay in-over)))

(defun evil-tex-toggle-math ()
  "Toggle surrounding math between \\(foo\\) and \\[foo\\]."
  (let ((an-over (make-overlay (car (evil-tex-a-math)) (cadr (evil-tex-a-math))))
        (in-over (make-overlay (car (evil-tex-inner-math)) (cadr (evil-tex-inner-math)))))
    (save-excursion
      (goto-char (overlay-start an-over))
      (cond
       ((looking-at (regexp-quote "\\("))
        (evil-tex--regexp-overlay-replace "\\[" "\\]" an-over in-over))
       ((looking-at (regexp-quote "\\["))
        (evil-tex--regexp-overlay-replace "\\(" "\\)" an-over in-over))))
    (delete-overlay an-over) (delete-overlay in-over)))

(defun evil-tex-toggle-command ()
  "Toggle command between \\foo and \\foo*."
  (let ((an-over (make-overlay (car (evil-tex-a-command)) (cadr (evil-tex-a-command))))
        (in-over (make-overlay (car (evil-tex-inner-command)) (cadr (evil-tex-inner-command)))))
    (save-excursion
      (goto-char (overlay-start in-over))
      ;;(skip-chars-forward "^{")
      (backward-char 1)
      (when (eq ?{ (char-after)) (backward-char 1) )
      (if (eq ?* (char-after)) (delete-char 1) (progn (forward-char 1) (insert-char ?*))))
    (delete-overlay an-over) (delete-overlay in-over)))

(defvar evil-tex-section-name-history nil
  "History used for changing section names with `evil-tex-toggle-section'.")

(defun evil-tex-toggle-section ()
  "Enter new name for surrounding section. Meta-n for original name."
  (let ((section-info (evil-tex--select-section)))
    (save-excursion
      (goto-char (nth 0 section-info))
      (skip-chars-forward "^{")
      (when-let ((curly (evil-inner-curly))
                 (orig-name (buffer-substring-no-properties
                             (nth 0 curly) (nth 1 curly)))
                 (new-name
                  (minibuffer-with-setup-hook
                      (lambda () (add-hook 'pre-command-hook #'evil-ex-remove-default))
                    (read-string (concat
                                  (substring-no-properties (nth 4 section-info) 1)
                                  ": ")
                                 (propertize orig-name 'face 'shadow)
                                 'evil-tex-section-name-history
                                 orig-name t))))
        (goto-char (nth 0 curly))
        (delete-region (nth 0 curly) (nth 1 curly))
        (insert new-name)))))


;;; Some movement commands

(defun evil-tex-go-back-section (&optional arg)
  "Go back to the closest part/section/subsection etc.
If given, go ARG sections up."
  (interactive)
  (re-search-backward evil-tex--section-regexp nil t arg))

(defun evil-tex-go-forward-section (&optional arg)
  "Go forward to the closest part/section/subsection etc.
If given, go ARG sections down."
  (interactive)
  (when (looking-at evil-tex--section-regexp)
    (goto-char (match-end 0)))
  (when (re-search-forward evil-tex--section-regexp nil arg)
    (goto-char (match-beginning 0))))

(defun evil-tex-brace-movement ()
  "Brace movement similar to TAB in cdlatex.

Example: (| symbolizes point)
\bar{h|} => \bar{h}|
\frac{a|}{} => \frac{a}{|}
\frac{a|}{b} => \frac{a}{b|}
\frac{a}{b|} => \frac{a}{b}|"
  (interactive)
  ;; go to the closing } of the current scope
  (search-backward "{" (line-beginning-position))
  (forward-sexp)
  ;; encountered a {? go to just before its terminating }
  (when (looking-at "{")
    (forward-sexp)
    (backward-char)))


;;; Text object definitions
;; some of which stolen from  https://github.com/hpdeifel/evil-latex-textobjects

(evil-define-text-object evil-tex-a-dollar (count &optional beg end type)
  "Select a dollar."
  :extend-selection t
  (evil-select-paren "QQ" "QQ" beg end type count t))

(evil-define-text-object evil-tex-inner-math (count &optional beg end type)
  "Select inner \\[ \\] or \\( \\)."
  :extend-selection nil
  (evil-select-paren (rx (or "\\(" "\\[" "$"))
                     (rx (or "\\)" "\\]" "$"))
                     beg end type count nil))

(evil-define-text-object evil-tex-a-math (count &optional beg end type)
  "Select a \\[ \\] or \\( \\)."
  :extend-selection nil
  (evil-select-paren (rx (or "\\(" "\\[" "$"))
                     (rx (or "\\)" "\\]" "$"))
                     beg end type count t))

(evil-define-text-object evil-tex-a-delim (count &optional beg end type)
  "Select a delimiter, e.g. (foo) or \\left[bar\\right]."
  :extend-selection nil
  (evil-tex--select-delim beg end type count t))

(evil-define-text-object evil-tex-inner-delim (count &optional beg end type)
  "Select inner delimiter, e.g. (foo) or \\left[bar\\right]."
  :extend-selection nil
  (evil-tex--select-delim beg end type count nil))

(evil-define-text-object evil-tex-a-command (count &optional beg end type)
  "Select a LaTeX command (macro)."
  (list (nth 0 (evil-tex--select-command))
        (nth 1 (evil-tex--select-command))))

(evil-define-text-object evil-tex-inner-command (count &optional beg end type)
  "Select inner LaTeX command (macro)."
  (list (nth 2 (evil-tex--select-command))
        (nth 3 (evil-tex--select-command))))

(evil-define-text-object evil-tex-an-env (count &optional beg end type)
  "Select a LaTeX environment."
  (list (nth 0 (evil-tex--select-env))
        (nth 1 (evil-tex--select-env))))

(evil-define-text-object evil-tex-inner-env (count &optional beg end type)
  "Select inner LaTeX environment."
  (list (nth 2 (evil-tex--select-env))
        (nth 3 (evil-tex--select-env))))

(evil-define-text-object evil-tex-a-section (count &optional beg end type)
  "Select a LaTeX section."
  (list (nth 0 (evil-tex--select-section))
        (nth 1 (evil-tex--select-section))))

(evil-define-text-object evil-tex-inner-section (count &optional beg end type)
  "Select inner LaTeX section."
  (list (nth 2 (evil-tex--select-section))
        (nth 3 (evil-tex--select-section))))

(evil-define-text-object evil-tex-a-subscript (count &optional beg end type)
  "Select a LaTeX subscript."
  (list (car (evil-tex-script-beginning-begend "_"))
        (cdr (evil-tex-script-end-begend "_"))))

(evil-define-text-object evil-tex-inner-subscript (count &optional beg end type)
  "Select inner LaTeX subscript."
  :extend-selection nil
  (list (cdr (evil-tex-script-beginning-begend "_"))
        (car (evil-tex-script-end-begend "_"))))

(evil-define-text-object evil-tex-a-superscript (count &optional beg end type)
  "Select a LaTeX superscript."
  (list (car (evil-tex-script-beginning-begend "^"))
        (cdr (evil-tex-script-end-begend "^"))))

(evil-define-text-object evil-tex-inner-superscript (count &optional beg end type)
  "Select inner LaTeX superscript."
  :extend-selection nil
  (list (cdr (evil-tex-script-beginning-begend "^"))
        (car (evil-tex-script-end-begend "^"))))

(evil-define-text-object evil-tex-a-table-cell (count &optional beg end type)
  "Select a LaTeX table cell."
  (list (nth 0 (evil-tex--select-table-cell))
        (nth 1 (evil-tex--select-table-cell))))

(evil-define-text-object evil-tex-inner-table-cell (count &optional beg end type)
  "Select inner LaTeX table cell."
  (list (nth 2 (evil-tex--select-table-cell))
        (nth 3 (evil-tex--select-table-cell))))


;;; evil-surround setup

(defun evil-tex--populate-surround-keymap (keymap generator-alist prefix
                                                  single-strings-fn &optional cons-fn)
  "Populate KEYMAP with keys and callbacks from GENERATOR-ALIST.
see `evil-tex-env-map-generator-alist' the the alist fromat.
PREFIX is the prefix to give the generated functions created
by (lambda () (interactive) (SINGLE-STRINGS-FN env)) if the input is a string,
and by (lambda () (interactive) (CONS-FN env)) if it's a cons
Return KEYMAP."

  (dolist (pair generator-alist)
    (let* ((key (car pair))
           (env (cdr pair))
           name)
      (cond
       ((stringp env)
        (setq name (intern (concat prefix env)))
        (fset name (lambda () (interactive) (funcall single-strings-fn env)))
        (define-key keymap key name))
       ((consp env)
        (setq name (intern (concat prefix (car env))))
        (if cons-fn
            (fset name (lambda () (interactive) (funcall cons-fn env)))
          (fset name (lambda () (interactive) env)))
        (define-key keymap key name))
       ((or (functionp env) (not env))
        (define-key keymap key env)))))
  keymap)

(defun evil-tex-read-with-keymap (keymap)
  "Prompt the user to press a key from KEYMAP.

Return the result of the called function, or error if the key
pressed isn't found."
  (let (key map-result)
    (when (require 'which-key nil t)
      (run-with-idle-timer
       which-key-idle-delay nil
       (lambda () (unless key
                    (which-key--show-keymap nil keymap nil nil t)))))
    (setq key (string (read-char)))
    (when (functionp 'which-key--hide-popup)
      (which-key--hide-popup))
    (setq map-result (lookup-key keymap key))
    (cond
     ((or (not map-result) (numberp map-result))
      (user-error "%s not found in keymap" key))
     ((functionp map-result)
      (funcall map-result))
     ((keymapp map-result)
      (evil-tex-read-with-keymap map-result)))))

;; working code courtesy of @hlissner
(defmacro evil-tex-dispatch-single-key (catch-key callback &optional fallbacks)
  "Define a an evil command to execute CALLBACK when given CATCH-KEY.

Otherwise try to call any of the functions in FALLBACKS (a
symbol) until any of them succeeds (returns non-nil.)"
  `(evil-define-command
     ,(intern (concat "evil-tex-dispath-" (string catch-key))) (count key)
     (interactive "<c><C>")
     (if (eq key ,catch-key)
         (funcall ,callback)
       (run-hook-with-args-until-success ,fallbacks
                                         count key))))

(defun evil-tex-format-env-for-surrounding (env-name)
  "Format ENV-NAME for surrounding: return a cons of \\begin{ENV-NAME} . \end{ENV-NAME}."
  (cons (format "\\begin{%s}%s"
                env-name
                (if evil-tex-include-newlines-in-envs "\n" ""))
        (format "%s\\end{%s}"
                (if evil-tex-include-newlines-in-envs "\n" "")
                env-name)))

(defun evil-tex-format-env-cons-for-surrounding (env-cons)
  "Format ENV-CONS for surrounding.
Add newlines if `evil-tex-include-newlines-in-envs' is t"
  (if evil-tex-include-newlines-in-envs
      (cons (concat (car env-cons) "\n")  (concat "\n" (cdr env-cons)))
    (env-cons)))

(defun evil-tex-format-cdlatex-accent-for-surrounding (accent)
  "Format ACCENT for surrounding: return a cons of \\ACCENT{ . }."
  (cons (concat "\\" accent "{") "}"))

(defun evil-tex-format-command-for-surrounding (command)
  "Format COMMAND for surrounding: return a cons of \\COMMAND{ . }."
  (if evil-tex--last-command-empty
      (cons (concat "\\" command "") "")
    (cons (concat "\\" command "{") "}")))

(defun evil-tex-prompt-for-env ()
  "Prom{pt} the user for an env to insert."
  (evil-tex-format-env-for-surrounding
   (read-from-minibuffer "env: " nil minibuffer-local-ns-map)))

(defvar evil-tex--env-function-prefix "evil-tex-envs:"
  "Prefix used when generating env functions from `evil-tex-env-map-generator-alist'.")

(defvar evil-tex--cdlatex-accents-function-prefix "evil-tex-cdlatex-accents:"
  "Prefix used when generating accent functions from `evil-tex-cdlatex-accent-map-generator-alist'.")

(defvar evil-tex--delim-function-prefix "evil-tex-delims:"
  "Prefix used when generating delimiter functions from `evil-tex-delim-map-generator-alist'.")

(defvar evil-tex-env-map-generator-alist
  `(("x" . ,#'evil-tex-prompt-for-env)
    ("e" . "equation")
    ("E" . "equation*")
    ("f" . "figure")
    ("i" . "itemize")
    ("I" . "enumerate")
    ("b" . "frame")
    ("a" . "align")
    ("A" . "align*")
    ("n" . "alignat")
    ("N" . "alignat*")
    ("r" . "eqnarray")
    ("l" . "flalign")
    ("L" . "flalign*")
    ("g" . "gather")
    ("G" . "gather*")
    ("m" . "multline")
    ("M" . "multline*")
    ("c" . "cases")
    ("z" . "tikzpicture")
    ;; prefix t - theorems
    ("ta" . "axiom")
    ("tc" . "corollary")
    ("td" . "definition")
    ("te" . "examples")
    ("ts" . "exercise")
    ("tl" . "lemma")
    ("tp" . "proof")
    ("tq" . "question")
    ("tr" . "remark")
    ("tt" . "theorem"))
  "Initial alist used to generate `evil-tex-env-map'.

Don't modify this directly; use `evil-tex-user-env-map-generator-alist'")

(defvar evil-tex-user-env-map-generator-alist nil
  "Your alist for modifications of `evil-tex-env-map'.

See `evil-tex-cdlatex-accents-map-generator-alist' for what it
should look like.

Each item is a cons. The car is the key (a string) to the
keymap. The cdr can be:

A string: then the inserted env would be an env
with that name

A cons: then the text would be wrapped between the car and the
cdr. For example, you can make a cons of
'(\\begin{figure}[!ht] . \\end{figure})
to have default placements for the figure.
Note that these definition will respect `evil-tex-include-newlines-in-envs', so
there is no need to manually include \\n's.

A function: then the function would be called, and the result is
assumed to be a cons. The text is wrapped in the resulted cons.")

(defvar evil-tex-cdlatex-accents-map-generator-alist
  `(("." . "dot")
    (":" . "ddot")
    ("~" . "tilde")
    ("N" . "widetilde")
    ("^" . "hat")
    ("H" . "widehat")
    ("-" . "bar")
    ("T" . "overline")
    ("_" . "underline")
    ("{" . "overbrace")
    ("}" . "underbrace")
    (">" . "vec")
    ("/" . "grave")
    ("\"". "acute")
    ("v" . "check")
    ("u" . "breve")
    ("m" . "mbox")
    ("c" . "mathcal")
    ("r" . ,#'evil-tex-cdlatex-accents:rm)
    ("i" . ,#'evil-tex-cdlatex-accents:it)
    ("l" . ,#'evil-tex-cdlatex-accents:sl)
    ("b" . ,#'evil-tex-cdlatex-accents:bold)
    ("e" . ,#'evil-tex-cdlatex-accents:emph)
    ("y" . ,#'evil-tex-cdlatex-accents:tt)
    ("f" . ,#'evil-tex-cdlatex-accents:sf)
    ("0"   "{\\textstyle " . "}")
    ("1"   "{\\displaystyle " . "}")
    ("2"   "{\\scriptstyle " . "}")
    ("3"   "{\\scriptscriptstyle " . "}"))
  "Initial alist used to generate `evil-tex-cdlatex-accents-map'.

Don't modify this directly; use `evil-tex-user-cdlatex-accents-map-generator-alist'")

(defvar evil-tex-user-cdlatex-accents-map-generator-alist nil
  "Your alist for modifications of `evil-tex-cdlatex-accents-map'.
See `evil-tex-user-env-map-generator-alist' for format specification.")

(defun evil-tex-cdlatex-accents:rm ()
  "Return the (start . end) that would make text rm style if wrapped between start and end."
  (cons (if (texmathp) "\\mathrm{" "\\textrm{")) "}")
(defun evil-tex-cdlatex-accents:it ()
  "Return the (start . end) that would make text it style if wrapped between start and end."
  (cons (if (texmathp) "\\mathit{" "\\textit{")) "}")
(defun evil-tex-cdlatex-accents:sl ()
  "Return the (start . end) that would make text sl style if wrapped between start and end."
  (unless (texmathp) '("\\textsl{" . "}")))
(defun evil-tex-cdlatex-accents:bold ()
  "Return the (start . end) that would make text bold style if wrapped between start and end."
  (cons (if (texmathp) "\\mathbf{" "\\textbf{") "}"))
(defun evil-tex-cdlatex-accents:emph ()
  "Return the (start . end) that would make text emph style if wrapped between start and end."
  (cons (if (texmathp) "\\mathem{" "\\emph{") "}"))
(defun evil-tex-cdlatex-accents:tt ()
  "Return the (start . end) that would make text tt style if wrapped between start and end."
  (cons (if (texmathp) "\\mathtt{" "\\texttt{") "}"))
(defun evil-tex-cdlatex-accents:sf ()
  "Return the (start . end) that would make text sf style if wrapped between start and end."
  (cons (if (texmathp) "\\mathsf{" "\\textsf{") "}"))

(defvar evil-tex-delim-map-generator-alist
  `(("p"  "(" . ")")
    ("P"  "\\left(" . "\\right)")
    ("s"  "[" . "]")
    ("S"  "\\left[" . "\\right]")
    ("c"  "\\{" . "\\}")
    ("C"  "\\left\\{" . "\\right\\}")
    ("r"  "\\langle" . "\\rangle")
    ("R"  "\\left\\langle" . "\\right\\rangle"))
  "Initial alist used to generate `evil-tex-delim-map'.

Don't modify this directly; use `evil-tex-user-delim-map-generator-alist'")

(defvar evil-tex-user-delim-map-generator-alist nil
  "Your alist for modifications of `evil-tex-delim-map'.
See `evil-tex-user-env-map-generator-alist' for format specification.")

(defvar evil-tex-mode-map
  (let ((keymap (make-sparse-keymap)))
    (evil-define-key* '(motion normal) keymap
      "[[" #'evil-tex-go-back-section
      "]]" #'evil-tex-go-forward-section)
    (define-key keymap (kbd "M-n") #'evil-tex-brace-movement)
    keymap)
  "Basic keymap for `evil-tex-mode'.")

(defvar evil-tex-env-map
  (evil-tex--populate-surround-keymap
   (make-sparse-keymap)
   (append evil-tex-env-map-generator-alist
           evil-tex-user-env-map-generator-alist)
   evil-tex--env-function-prefix #'evil-tex-format-env-for-surrounding #'evil-tex-format-env-cons-for-surrounding)
  "Keymap for surrounding with environments, usually through `evil-tex-surround-env-prompt'.")

(defvar evil-tex-cdlatex-accents-map
  (evil-tex--populate-surround-keymap
   (make-sparse-keymap)
   (append evil-tex-cdlatex-accents-map-generator-alist
           evil-tex-user-cdlatex-accents-map-generator-alist)
   evil-tex--cdlatex-accents-function-prefix
   #'evil-tex-format-cdlatex-accent-for-surrounding)
  "Keymap for surrounding with environments, usually through `evil-tex-surround-cdlatex-accents-prompt'.")

(defvar evil-tex-delim-map
  (evil-tex--populate-surround-keymap
   (make-sparse-keymap)
   (append evil-tex-delim-map-generator-alist
           evil-tex-user-delim-map-generator-alist)
   evil-tex--delim-function-prefix
   #'identity)
  "Keymap for surrounding with environments, usually through `evil-tex-delim-map'.")

(defun evil-tex-surround-env-prompt ()
  "Prompt user for an env to surround with using `evil-tex-env-map'."
  (evil-tex-read-with-keymap evil-tex-env-map))

(defun evil-tex-surround-cdlatex-accents-prompt ()
  "Prompt user for an accent to surround with using `evil-tex-cdlatex-accents-map'."
  (evil-tex-read-with-keymap evil-tex-cdlatex-accents-map))

(defun evil-tex-surround-delim-prompt ()
  "Prompt user for an delimiter to surround with using `evil-tex-delim-map'."
  (evil-tex-read-with-keymap evil-tex-delim-map))

(defun evil-tex-surround-command-prompt ()
  "Ask the user for the command they'd like to surround with."
  (evil-tex-format-command-for-surrounding
   (read-from-minibuffer "command: \\" nil minibuffer-local-ns-map)))


;;; Text object keybindings and surround declirations.

;; Shorten which-key descriptions in auto-generated keymaps
(with-eval-after-load 'which-key
  (push
   '(("\\`." . "evil-tex-.*:\\(.*\\)") . (nil . "\\1"))
   which-key-replacement-alist))

(defvar evil-tex-inner-text-objects-map (make-sparse-keymap)
  "Inner text object keymap for `evil-tex-mode'.")

(defvar evil-tex-outer-text-objects-map (make-sparse-keymap)
  "Outer text object keymap for `evil-tex-mode'.")
(if (boundp  'evil-surround-local-inner-text-object-map-list)
    (progn ;; deifines everything on local keymaps if evil-surround is new enough
      (define-key evil-tex-inner-text-objects-map "e" #'evil-tex-inner-env)
      (define-key evil-tex-inner-text-objects-map "c" #'evil-tex-inner-command)
      (define-key evil-tex-inner-text-objects-map "m" #'evil-tex-inner-math)
      (define-key evil-tex-inner-text-objects-map "d" #'evil-tex-inner-delim)
      (define-key evil-tex-inner-text-objects-map "S" #'evil-tex-inner-section)
      (define-key evil-tex-inner-text-objects-map "^" #'evil-tex-inner-superscript)
      (define-key evil-tex-inner-text-objects-map "_" #'evil-tex-inner-subscript)
      (define-key evil-tex-inner-text-objects-map "T" #'evil-tex-inner-table-cell)

      (define-key evil-tex-outer-text-objects-map "e" #'evil-tex-an-env)
      (define-key evil-tex-outer-text-objects-map "c" #'evil-tex-a-command)
      (define-key evil-tex-outer-text-objects-map "m" #'evil-tex-a-math)
      (define-key evil-tex-outer-text-objects-map "d" #'evil-tex-a-delim)
      (define-key evil-tex-outer-text-objects-map "S" #'evil-tex-a-section)
      (define-key evil-tex-outer-text-objects-map "^" #'evil-tex-a-superscript)
      (define-key evil-tex-outer-text-objects-map "_" #'evil-tex-a-subscript)
      (define-key evil-tex-outer-text-objects-map "T" #'evil-tex-a-table-cell)

      (evil-define-key '(visual operator) 'evil-tex-mode
        "ie" #'evil-tex-inner-env
        "ae" #'evil-tex-an-env
        "ic" #'evil-tex-inner-command
        "ac" #'evil-tex-a-command
        "im" #'evil-tex-inner-math
        "am" #'evil-tex-a-math
        "id" #'evil-tex-inner-delim
        "ad" #'evil-tex-a-delim
        "iS" #'evil-tex-inner-section
        "aS" #'evil-tex-a-section
        "i^" #'evil-tex-inner-superscript
        "a^" #'evil-tex-a-superscript
        "i_" #'evil-tex-inner-subscript
        "a_" #'evil-tex-a-subscript
        "iT" #'evil-tex-inner-table-cell
        "aT" #'evil-tex-a-table-cell)

      (setq evil-surround-local-inner-text-object-map-list (list evil-tex-inner-text-objects-map ))
      (setq evil-surround-local-outer-text-object-map-list (list evil-tex-outer-text-objects-map )))
  ;; pollutes the global namespace if evil-surround is too old
  (define-key evil-inner-text-objects-map "e" 'evil-tex-inner-env)
  (define-key evil-inner-text-objects-map "c" 'evil-tex-inner-command)
  (define-key evil-inner-text-objects-map "m" 'evil-tex-inner-math)
  (define-key evil-inner-text-objects-map "d" 'evil-tex-inner-delim)
  (define-key evil-inner-text-objects-map "S" 'evil-tex-inner-section)
  (define-key evil-inner-text-objects-map "^" 'evil-tex-inner-superscript)
  (define-key evil-inner-text-objects-map "_" 'evil-tex-inner-subscript)
  (define-key evil-inner-text-objects-map "T" 'evil-tex-inner-table-cell)

  (define-key evil-outer-text-objects-map "e" 'evil-tex-an-env)
  (define-key evil-outer-text-objects-map "c" 'evil-tex-a-command)
  (define-key evil-outer-text-objects-map "m" 'evil-tex-a-math)
  (define-key evil-outer-text-objects-map "d" 'evil-tex-a-delim)
  (define-key evil-outer-text-objects-map "S" 'evil-tex-a-section)
  (define-key evil-outer-text-objects-map "^" 'evil-tex-a-superscript)
  (define-key evil-outer-text-objects-map "_" 'evil-tex-a-subscript)
  (define-key evil-outer-text-objects-map "T" 'evil-tex-a-table-cell))


(defvar evil-tex-surround-delimiters
  `((?m "\\(" . "\\)")
    (?M "\\[" . "\\]")
    (?c . ,#'evil-tex-surround-command-prompt)
    (?e . ,#'evil-tex-surround-env-prompt)
    (?d . ,#'evil-tex-surround-delim-prompt)
    (?\; . ,#'evil-tex-surround-cdlatex-accents-prompt)
    (?^ "^{" . "}")
    (?_ "_{" . "}")
    (?T "&" . "&"))
  "Mappings to be used in evil-surround as an interface to evil-tex.

See `evil-surround-pairs-alist' for the format.")

(defun evil-tex-set-up-surround ()
  "Configure evil-surround so things like 'csm' work."
  (setq-local evil-surround-pairs-alist
              (append evil-tex-surround-delimiters evil-surround-pairs-alist)))
(defun evil-tex-set-up-embrace ()
  "Configure evil-embrace not to steal our evil-surround keybinds."
  (setq-local evil-embrace-evil-surround-keys
              (append
               ;; embrace only needs the key chars, not the whole delimiters
               (mapcar #'car evil-tex-surround-delimiters)
               evil-embrace-evil-surround-keys)))


;;; Set up text object toggling.

(defvar evil-tex-toggle-override-t nil
  "Set to t to bind evil-tex toggles to 'ts*' keybindings.
Overrides normal 't' functionality for `s' only.
'ts*' now executes toggles,
see `evil-tex-toggle-delimiter-map' for more invormation
Needs to be defined before loading evil-tex.")

(defvar evil-tex-toggle-override-m t
  "Set to t to bind evil-tex toggles to 'mt*' keybindings.
Overrides normal `m' functionality for 't' only.
'mt*' now executes toggles,
see `evil-tex-toggle-delimiter-map' for more invormation
Needs to be defined before loading evil-tex.")

(defvar evil-tex-t-functions
  (list (defun evil-tex-try-evil-snipe (count key)
          (when (bound-and-true-p evil-snipe-mode)
            (setq evil-snipe--last-direction t)
            (evil-snipe-t count (list key))
            t)
          #'evil-find-char-to))
  "List of functions that should run on 't' key by default.

The functions are called one by one, with arguments (count key),
until one of them returns non-nil.")

(defvar evil-tex-m-functions
  (list (lambda (_count key)
          (evil-set-marker key)
          t))
  "List of functions that should run on 'm' key by default.

The functions are called one by one, with arguments (count key),
until one of them returns non-nil.")

(defvar evil-tex-toggle-delimiter-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "d" #'evil-tex-toggle-delim)
    (define-key keymap "e" #'evil-tex-toggle-env)
    (define-key keymap "m" #'evil-tex-toggle-math)
    (define-key keymap "c" #'evil-tex-toggle-command)
    (define-key keymap "S" #'evil-tex-toggle-section)
    keymap)
  "Keymap for delimiter surrounding.")

(defun evil-tex-read-and-execute-toggle ()
  "Prompt user with `evil-tex-toggle-delimiter-map' to toggle something."
  (save-excursion
    (evil-tex-read-with-keymap evil-tex-toggle-delimiter-map)))

(when evil-tex-toggle-override-t
  (evil-define-key 'normal evil-tex-mode-map "t"
    (evil-tex-dispatch-single-key ?s #'evil-tex-read-and-execute-toggle
                                  'evil-tex-t-functions)))

(when evil-tex-toggle-override-m
  (evil-define-key 'normal evil-tex-mode-map "m"
    (evil-tex-dispatch-single-key ?t #'evil-tex-read-and-execute-toggle
                                  'evil-tex-m-functions)))

;;;###autoload
(define-minor-mode evil-tex-mode
  "Minor mode for latex-specific text objects in evil.
Also provides evil-surround integration and toggles.

An overview of the included text objects:
| Key | Abbreviation    | Text Object Target                                       | Surround behavior                          |
|-----+-----------------+----------------------------------------------------------+--------------------------------------------|
| c | command         | TeX marcos: \\foo{...}                                     | Prompts you for a macro                    |
| e | environment     | \\begin{...} \\end{...} blocks                             | Prompts you with the env keymap            |
| m | math            | Both inline \\( \\) and display \\[ \\].                   | Surrounds with \\( \\)                     |
| M | Display math    | N/A                                                        | Surrounds with \\[ \\]                     |
| $ | dollar          | Old TeX inline maths.                                      |                                            |
| d | delimiters      | Math delimiters, (foo), \\left(foo\\right),                |                                            |
|   |                 | [foo], \\left[foo\\right] etc.                             | Prompts you with the delim keymap          |
| S | sections        | LaTeX parts, chapters, ((sub)sections, and (sub)paragraphs |                                            |
| ; | CDLaTeX accents | N/A                                                        | Prompts you with the cdlatex accent keymap |
| ^ | superscript     | x^a x^\\alpha x^{...}                                      | Surrounds with ^{ }                        |
| _ | subscript       | x_a x_\\alpha x_{...}                                      | Surroudns with _{ }                        |

The text object definitions can be found under evil-tex-a-$NAME and evil-tex-inner-$NAME.

the default provided doggle functionality is as follows:

| Key | Abbreviation | Behaviour                                                                 |
|-----+--------------+---------------------------------------------------------------------------|
| mtc | command      | toggle asterisk on command, e.g. \\foo ==> \\foo*                         |
| mtd | delimiter    | toggle between delimiter autosizing, e.g.  (foo) ==> \\Left(foo\\right)   |
| mte | environment  | toggle enviornment asterisk e.g. \\begin{equation} ==> \\begin{equation*} |
| mtm | math         | toggle between inline and display math, i.e. \\(foo\\) ==> \\[foo\\]      |
| mtS | section      | toggle section text object asterisk, e.g. \\chapter{} ==> \\chapter*{}    |

"
  :init-value nil
  :keymap evil-tex-mode-map
  (when evil-tex-mode
    (evil-normalize-keymaps)
    ;; (set-keymap-parent evil-tex-outer-map evil-outer-text-objects-map)
    ;; (set-keymap-parent evil-tex-inner-map evil-inner-text-objects-map)
    (eval-after-load 'evil-surround
      #'evil-tex-set-up-surround)
    (eval-after-load 'evil-embrace
      #'evil-tex-set-up-embrace)))

(provide 'evil-tex)
;;; evil-tex ends here
