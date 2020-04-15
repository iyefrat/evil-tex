;;; evil-tex-util.el -- Functions to be used by evil-texi -*- lexical-binding: t; -*-
;;; Commentary:
;; This file is part of evil-tex, which provides license
;; information.
;;; Code:

(require 'latex)
(require 'evil-common)


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

(defun evil-tex--select-math (&rest args)
  "Return (beg . end) of best math match.

ARGS passed to evil-select-(paren|quote)."
  (evil-tex-max-key
   (list
    (ignore-errors (apply #'evil-select-paren
                          (regexp-quote "\\(") (regexp-quote "\\)") args))
    (ignore-errors (apply #'evil-select-paren
                          (regexp-quote "\\[") (regexp-quote "\\]") args))
    (ignore-errors (apply #'evil-select-quote ?$ args)))
   (lambda (arg) (if (and (consp arg) ; selection succeeded
                          ;; Selection is close enough to point.
                          ;; evil-select-quote can select things further down in
                          ;; the buffer.
                          (<= (- (car arg) 2) (point))
                          (>= (+ (cadr arg) 3) (point)))
                     (car arg)
                   most-negative-fixnum))))

(defun evil-tex--delim-compare (a b)
  "Recieve two cons' A B of structure (LR IA BEG END ...).
where LR is t for e.g. \\left( and nil for e.g. (,
 IA is t for -an- text objects and nil for -inner-,
BEG and END are the coordinates for the begining and end of the potential delim,
and the _'s are unimportant.
Compares between the delimiters to find which one has the largest BEG, while
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
  "Return delimiter location (and more) for evil-tex--delim-finder.
LR is t for e.g. \\left( and nil for e.g. (.
DELIML and DELIMR is a string containing the non \\left part of the delimiter.
ARGS is the information about the text object needed for the functions to work,
such as wether the delimiter is an \\left( type or a ( type,
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
  "Return (beg . end) of best math match.

ARGS passed to evil-select-(paren|quote)."
  (cddr (evil-tex-max-key
         (list
          (evil-tex--delim-finder nil "(" ")" args)
          (evil-tex--delim-finder t "(" ")" args)
          (evil-tex--delim-finder nil "[" "]" args)
          (evil-tex--delim-finder t "[" "]" args)
          (evil-tex--delim-finder nil "\\{" "\\}" args)
          (evil-tex--delim-finder t "\\{" "\\}" args)
          (evil-tex--delim-finder nil "\\langle" "\\rangle" args)
          (evil-tex--delim-finder t "\\langle" "\\rangle" args)
          )
         (lambda (arg) (if (consp arg) ; selection succeeded
                           arg
                         nil )) 'evil-tex--delim-compare )))

(defvar evil-tex-include-newlines-in-envs t
  "Whether to select the newlines when selecting begin/end blocks, and add newlines when surrounding with envs.")

(defun evil-tex-format-env-for-surrounding (env-name)
  "Format ENV-NAME for surrounding: return a cons of \\begin{ENV-NAME} . \end{ENV-NAME}."
  (cons (format "\\begin{%s}%s"
                env-name
                (when evil-tex-include-newlines-in-envs "\n"))
        (format "%s\\end{%s}"
                (when evil-tex-include-newlines-in-envs "\n")
                env-name)))

(defun evil-tex-format-cdlatex-accent-for-surrounding (accent)
  "Format ACCENT for surrounding: return a cons of \\ACCENT{ . }."
  (cons (concat "\\" accent "{") "}"))

(defun evil-tex-prompt-for-env ()
  "Prompt the user for an env to insert."
  (evil-tex-format-env-for-surrounding
   (read-from-minibuffer "env: " nil minibuffer-local-ns-map)))

(defvar evil-tex--env-function-prefix "evil-tex-envs:"
  "Prefix used when generating env functions from `evil-tex-env-map-generator-alist'.")

(defvar evil-tex--cdlatex-accents-function-prefix "evil-tex-cdlatex-accents:"
  "Prefix used when generating accent functions from `evil-tex-cdlatex-accent-map-generator-alist'.")

(defvar evil-tex--delim-function-prefix "evil-tex-delims:"
  "Prefix used when generating delimiter functions from `evil-tex-delim-map-generator-alist'.")

(defun evil-tex--populate-surround-kemap (keymap generator-alist prefix
                                                 single-strings-fn)
  "Populate KEYMAP with keys and callbacks from GENERATOR-ALIST.
see `evil-tex-env-map-generator-alist' the the alist fromat.
PREFIX is the prefix to give the generated functions created
by (lambda () (interactive) (SINGLE-STRINGS-FN env)).
Return KEYMAP."

  (dolist (pair generator-alist)
    (message "pair: %s" pair)
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
        (fset name (lambda () (interactive) env))
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
    (message "read it! %s" key)
    (setq map-result (lookup-key keymap key))
    (cond
     ((or (not map-result) (numberp map-result))
      (user-error "%s not found in keymap" key))
     ((functionp map-result)
      (funcall map-result))
     ((keymapp map-result)
      (evil-tex-read-with-keymap map-result)))))


;; Stuff from evil-latex-textobjects

(defun evil-tex-macro-beginning-begend ()
  "Return (start . end) of the macro-beginning to the left of point.

If no enclosing macro is found, return nil.
For example for \\macro{foo|bar} it returns the start and end of \"\\macro{\""
  (let ((beg (TeX-find-macro-start)))
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-char)                  ; backslash
        (skip-chars-forward "A-Za-z@*") ; macro-name
        (when (looking-at "{\\|\\[")
          (forward-char))               ; opening brace
        (cons beg (point))))))

(defun evil-tex-macro-end-begend ()
  "Return (start . end) of the end of the enclosing macro.

If no such macro can be found, return nil"
  (let ((end (TeX-find-macro-end)))
    (when end
      (save-excursion
        (goto-char end)
        (when (looking-back "}\\|\\]" (- (point) 2))
          (backward-char))              ; closing brace
        (cons (point) end)))))

;; TODO Support visual selection
;; TODO Support count

;; Now ours

(defun evil-tex--re-search-backwards-unless-already (str)
  "Search backward for STR unless point is already on it."
  (unless (looking-at str)
    (re-search-backward str)))

(defun evil-tex--get-macro-braces-begend ()
  "Return (beg . end) of the macro braces currently active.
\foo{bar}
    ^   ^"
  (let (beg end)
    (save-excursion
      (evil-tex--re-search-backwards-unless-already "\\\\")
      (search-forward "{")
      (setq beg (point))
      (forward-sexp)
      (setq end (point)))
    (cons beg end)))

(defvar evil-tex-select-newlines-with-envs t
  "Whether to select and insert newlines with env commands.

By default, the newline proceeding \\begin{...} and preceding
\\end{...} is selected as part of the delimiter. This way, when
doing =cie= you're placed on a separate line, and surrounding
with envs would force separate lines for \\begin, inner text, and
\\end.")

(defun evil-tex-env-beginning-begend ()
  "Return (start . end) of the \\begin{foo} of current env.

\\begin{equation}
^               ^"
  (let (beg)
    (save-excursion
      ;; LaTeX-find-matching-begin doesn't work if on the \begin itself
      (search-backward "\\" (line-beginning-position) t)
      (unless (looking-at (regexp-quote "\\begin{"))
        (LaTeX-find-matching-begin))
      ;; We are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")        ; goto opening brace
      (forward-sexp)                   ; goto closing brace
      (when (and evil-tex-select-newlines-with-envs
                 (looking-at "\n"))
        (forward-line 1))
      (cons beg (point)))))

(defun evil-tex-env-end-begend ()
  "Return (start . end) of the \\end{foo} of current env.

\\end{equation}
^             ^"
  (let (end)
    (save-excursion
      ;; LaTeX-find-matching-end doesn't work if on the \begin itself
      (search-backward "\\" (line-beginning-position) t)
      (when (looking-at (regexp-quote "\\begin{"))
        (skip-chars-forward "^{")      ; goto opening brace
        (forward-sexp))                ; goto closing brace
      ;; Now definitely inside the env
      (LaTeX-find-matching-end)        ; we are at closing brace
      (setq end (point))
      (backward-sexp)                  ; goto opening brace
      (search-backward "\\")           ; goto backslash
      (when (and evil-tex-select-newlines-with-envs
                 (looking-back "\n" (1- (point))))
        (backward-char))
      (cons (point) end))))

(defun evil-tex-section-beginning-begend ()
  "Return (start . end) of the \\(sub)*section{foo} of current section.

\\section{foo}
^               ^"
  (let (beg)
    (save-excursion
      ;; LaTeX-find-matching-begin doesn't work if on the \begin itself
      (search-backward "\\" (line-beginning-position) t)
      (unless (looking-at "\\\\\\(sub\\)*section")
        (re-search-backward "\\\\\\(sub\\)*section"))
      ;; We are at backslash
      (setq beg (point))
      (skip-chars-forward "^{")        ; goto opening brace
      (forward-sexp)                   ; goto closing brace
      (when (and evil-tex-select-newlines-with-envs
                 (looking-at "\n"))
        (forward-line 1))
      (cons beg (point)))))

(defun evil-tex-section-end-begend ()
  "Return (start . end) of the end of the current section.
defined to be TODO.

\\end{equation}
^             ^"
    (save-excursion
      ;; LaTeX-find-matching-end doesn't work if on the \begin itself
      (re-search-forward "\\\\\\(sub\\)*section\\|\\\\end{document}")
      (move-beginning-of-line 1)
      (cons (point) (point))))

(defun evil-tex--select-section ()
  "TODO A better section selecting function."

  )

(defun evil-tex--goto-script-prefix (subsup)
  "Return goto end of the found SUBSUP prefix.
{(ab|)}_c => {(ab)}_|c"
  (let ((orig-point (point)))
    (unless (when (search-forward subsup (line-end-position 2) t) ; 2 lines down
              (let (beg end)
                (backward-char) ; before the ^_
                (setq end (point))
                (if (eq (char-before) ?}) ; {}^b
                    (backward-sexp)
                  (backward-char)) ; a^b
                (setq beg (point))
                ;; require point to be inside the base bounds
                (<= beg orig-point end)))
      (search-backward subsup (line-beginning-position 0))))
  (goto-char (match-end 0)))

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
  (let ((qsubsup (regexp-quote subsup)))
    (save-excursion
      (evil-tex--goto-script-prefix subsup)
      (cond
       ;; a_{something}
       ((looking-at "{")
        (forward-sexp)
        (cons (1- (point)) (point)))
       ;; a_\something
       ((looking-at "\\\\[a-zA-Z@*]+")
        (message "macro")
        (goto-char (match-end 0))
        ;; skip macro arguments
        (while (looking-at "{\\|\\[")
          (forward-sexp))
        (cons (point) (point)))
       (t ;; a_1 a_n
        (forward-char)
        (cons (point) (point)))))))

(provide 'evil-tex-util)
;;; evil-tex-util.el ends here
