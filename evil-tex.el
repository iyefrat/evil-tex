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
(require 'evil)
(require 'evil-tex-util)

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


;; stolen code from https://github.com/hpdeifel/evil-latex-textobjects
(evil-define-text-object evil-tex-inner-dollar (count &optional beg end type)
  "Select inner dollar."
  :extend-selection nil
  (evil-select-quote ?$ beg end type count nil))

(evil-define-text-object evil-tex-a-dollar (count &optional beg end type)
  "Select a dollar."
  :extend-selection t
  (evil-select-quote ?$ beg end type count t))

(evil-define-text-object evil-tex-inner-math (count &optional beg end type)
  "Select innter \\[ \\] or \\( \\)."
  :extend-selection nil
  (evil-select-paren (rx (or "\\(" "\\["))
                     (rx (or "\\)" "\\]"))
                     beg end type count nil))

(evil-define-text-object evil-tex-a-math (count &optional beg end type)
  "Select a \\[ \\] or \\( \\)."
  :extend-selection nil
  (evil-select-paren (rx (or "\\(" "\\["))
                     (rx (or "\\)" "\\]"))
                     beg end type count t))

(evil-define-text-object evil-tex-a-delim (count &optional beg end type)
  "Select a delimiter, e.g. (foo) or \\left[bar\\right]."
  :extend-selection nil
  (evil-tex--select-delim beg end type count t))

(evil-define-text-object evil-tex-inner-delim (count &optional beg end type)
  "Select inner delimiter, e.g. (foo) or \\left[bar\\right]."
  :extend-selection nil
  (evil-tex--select-delim beg end type count nil))

(evil-define-text-object evil-tex-a-macro (count &optional beg end type)
  "Select a TeX macro."
  :extend-selection nil
  (let ((beg (evil-tex-macro-beginning-begend))
        (end (evil-tex-macro-end-begend)))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-tex-inner-macro (count &optional beg end type)
  "Select inner TeX macro, i.e the argument to the macro."
  :extend-selection nil
  (let ((beg (evil-tex-macro-beginning-begend))
        (end (evil-tex-macro-end-begend)))
    (cond
     ;; TODO is this needed/could be simplified
     ((= (cdr beg) (car end))          ; macro has no content
      (list (1+ (car beg))             ; return macro boundaries excluding \
            (cdr beg)))
     (t (list (cdr beg) (car end))))))

(evil-define-text-object evil-tex-an-env (count &optional beg end type)
  "Select a LaTeX environment."
  (let ((beg (evil-tex-env-beginning-begend))
        (end (evil-tex-env-end-begend)))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-tex-inner-env (count &optional beg end type)
  "Select a LaTeX environment."
  :extend-selection nil
  (let ((beg (evil-tex-env-beginning-begend))
        (end (evil-tex-env-end-begend)))
    (list (cdr beg) (car end))))

(evil-define-text-object evil-tex-a-section (count &optional beg end type)
  "Select a LaTeX section."
  (let ((beg (nth 0 (evil-tex--select-section)))
        (end (nth 1 (evil-tex--select-section))))
    (list beg end)))

(evil-define-text-object evil-tex-inner-section (count &optional beg end type)
  "Select a LaTeX section."
  (let ((beg (nth 2 (evil-tex--select-section)))
        (end (nth 3 (evil-tex--select-section))))
    (list beg end)))

(evil-define-text-object evil-tex-a-subscript (count &optional beg end type)
  "Select a LaTeX subscript."
  (let ((beg (evil-tex-script-beginning-begend "_"))
        (end (evil-tex-script-end-begend "_")))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-tex-inner-subscript (count &optional beg end type)
  "Select a LaTeX subscript."
  :extend-selection nil
  (let ((beg (evil-tex-script-beginning-begend "_"))
        (end (evil-tex-script-end-begend "_")))
    (list (cdr beg) (car end))))

(evil-define-text-object evil-tex-a-superscript (count &optional beg end type)
  "Select a LaTeX superscript."
  (let ((beg (evil-tex-script-beginning-begend "^"))
        (end (evil-tex-script-end-begend "^")))
    (list (car beg) (cdr end))))

(evil-define-text-object evil-tex-inner-superscript (count &optional beg end type)
  "Select a LaTeX superscript."
  :extend-selection nil
  (let ((beg (evil-tex-script-beginning-begend "^"))
        (end (evil-tex-script-end-begend "^")))
    (list (cdr beg) (car end))))


;; (defvar evil-tex-outer-map (make-sparse-keymap))
;; (defvar evil-tex-inner-map (make-sparse-keymap))

(defvar evil-tex-env-map-generator-alist
  `(("x"  . ,#'evil-tex-prompt-for-env)
    ("e"  . "equation")
    ("E"  . "equation*")
    ("f"  . "figure")
    ("i"  . "itemize")
    ("I"  . "enumerate")
    ("b"  . "frame")
    ("a"  . "align")
    ("A"  . "align*")
    ("n"  . "alignat")
    ("N"  . "alignat*")
    ("r"  . "eqnarray")
    ("l"  . "flalign")
    ("L"  . "flalign*")
    ("g"  . "gather")
    ("G"  . "gather*")
    ("m"  . "multline")
    ("M"  . "multline*")
    ("c"  . "cases")
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

(defun evil-tex-cdlatex-accents:rm ()  "Return the (beg . end) that would make text rm style if wrapped between the car and cdr."
       (cons (if (texmathp) "\\mathrm{" "\\textrm{")) "}")
(defun evil-tex-cdlatex-accents:it () "Return the (beg . end) that would make text it style if wrapped between the car and cdr."
       (cons (if (texmathp) "\\mathit{" "\\textit{")) "}")
(defun evil-tex-cdlatex-accents:sl () "Return the (beg . end) that would make text sl style if wrapped between the car and cdr."
       (unless (texmathp) '("\\textsl{" . "}")))
(defun evil-tex-cdlatex-accents:bold () "Return the (beg . end) that would make text bold style if wrapped between the car and cdr."
       (cons (if (texmathp) "\\mathbf{" "\\textbf{") "}"))
(defun evil-tex-cdlatex-accents:emph () "Return the (beg . end) that would make text emph style if wrapped between the car and cdr."
       (cons (if (texmathp) "\\mathem{" "\\emph{") "}"))
(defun evil-tex-cdlatex-accents:tt () "Return the (beg . end) that would make text tt style if wrapped between the car and cdr."
       (cons (if (texmathp) "\\mathtt{" "\\texttt{") "}"))
(defun evil-tex-cdlatex-accents:sf () "Return the (beg . end) that would make text sf style if wrapped between the car and cdr."
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
    keymap)
  "Keymap for `evil-tex-mode'.")

(defvar evil-tex-env-map
  (evil-tex--populate-surround-kemap
   (make-sparse-keymap)
   (append evil-tex-env-map-generator-alist
           evil-tex-user-env-map-generator-alist)
   evil-tex--env-function-prefix #'evil-tex-format-env-for-surrounding)
  "Keymap for surrounding with environments.")

(defvar evil-tex-cdlatex-accents-map
  (evil-tex--populate-surround-kemap
   (make-sparse-keymap)
   (append evil-tex-cdlatex-accents-map-generator-alist
           evil-tex-user-cdlatex-accents-map-generator-alist)
   evil-tex--cdlatex-accents-function-prefix
   #'evil-tex-format-cdlatex-accent-for-surrounding)
  "Keymap for surrounding with cdlatex accents.")

(defvar evil-tex-delim-map
  (evil-tex--populate-surround-kemap
   (make-sparse-keymap)
   (append evil-tex-delim-map-generator-alist
           evil-tex-user-delim-map-generator-alist)
   evil-tex--delim-function-prefix
   #'identity)
  "Keymap for surrounding with delimiters.")

(defun evil-tex-surround-env-prompt ()
  "Prompt user for an env to surround with using `evil-tex-env-map'."
  (evil-tex-read-with-keymap evil-tex-env-map))

(defun evil-tex-surround-cdlatex-accents-prompt ()
  "Prompt user for an accent to surround with using `evil-tex-cdlatex-accents-map'."
  (evil-tex-read-with-keymap evil-tex-cdlatex-accents-map))

(defun evil-tex-surround-delim-prompt ()
  "Prompt user for an delimiter to surround with using `evil-tex-delim-map'."
  (evil-tex-read-with-keymap evil-tex-delim-map))

;; Shorten which-key descriptions in auto-generated keymaps
(with-eval-after-load 'which-key
  (push
   '(("\\`." . "evil-tex-.*:\\(.*\\)") . (nil . "\\1"))
   which-key-replacement-alist))

(define-key evil-inner-text-objects-map "e" 'evil-tex-inner-env)
(define-key evil-inner-text-objects-map "$" 'evil-tex-inner-dollar)
(define-key evil-inner-text-objects-map "c" 'evil-tex-inner-macro)
(define-key evil-inner-text-objects-map "m" 'evil-tex-inner-math)
(define-key evil-inner-text-objects-map "d" 'evil-tex-inner-delim)
(define-key evil-inner-text-objects-map "S" 'evil-tex-inner-section)
(define-key evil-inner-text-objects-map "^" 'evil-tex-inner-superscript)
(define-key evil-inner-text-objects-map "_" 'evil-tex-inner-subscript)

(define-key evil-outer-text-objects-map "e" 'evil-tex-an-env)
(define-key evil-outer-text-objects-map "$" 'evil-tex-a-dollar)
(define-key evil-outer-text-objects-map "c" 'evil-tex-a-macro)
(define-key evil-outer-text-objects-map "m" 'evil-tex-a-math)
(define-key evil-outer-text-objects-map "d" 'evil-tex-a-delim)
(define-key evil-outer-text-objects-map "S" 'evil-tex-a-section)
(define-key evil-outer-text-objects-map "^" 'evil-tex-a-superscript)
(define-key evil-outer-text-objects-map "_" 'evil-tex-a-subscript)


;; (evil-define-key 'operator evil-tex-mode-map
;;   "a" evil-tex-outer-map
;;   "i" evil-tex-inner-map)

;; (evil-define-key 'visual evil-tex-mode-map
;;   "a" evil-tex-outer-map
;;   "i" evil-tex-inner-map)

(defun evil-tex-surround-command-prompt ()
  "Ask the user for the macro they'd like to surround with."
  (evil-tex-format-cdlatex-accent-for-surrounding
   (read-from-minibuffer "macro: \\" nil minibuffer-local-ns-map)))

(defvar evil-tex-surround-delimiters
  `((?m "\\(" . "\\)")
    (?M "\\[" . "\\]")
    (?$ "$" . "$")
    (?c . ,#'evil-tex-surround-command-prompt)
    (?e . ,#'evil-tex-surround-env-prompt)
    (?d . ,#'evil-tex-surround-delim-prompt)
    (?\; . ,#'evil-tex-surround-cdlatex-accents-prompt)
    (?^ "^{" . "}")
    (?_ "_{" . "}"))
  "Mappings to be used in evil-surround as an interface to evil-tex.

See `evil-surround-pairs-alist' for the format.")

(defun evil-tex-set-up-surround ()
  "Configure evil-surround so things like 'csm' would work."
  (setq-local evil-surround-pairs-alist
              (append evil-tex-surround-delimiters evil-surround-pairs-alist)))
(defun evil-tex-set-up-embrace ()
  "Configure evil-embrace not to steal our evil-surround keybinds."
  (setq-local evil-embrace-evil-surround-keys
              (append
               ;; embrace only needs the key chars, not the whole delimiters
               (mapcar #'car evil-tex-surround-delimiters)
               evil-embrace-evil-surround-keys)))

(defvar evil-tex-toggle-override-t nil
  "Set to t to bind evil-tex toggles to 'ts*' keybindings.
overrides normal 't' functionality for `s' only.
Needs to be defined before loading evil-tex.")

(defvar evil-tex-toggle-override-q t
  "Set to t to bind evil-tex toggles to 'qt*' keybindings.
overrides normal `q' functionality for 't' only.
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

(defvar evil-tex-q-functions
  (list (lambda (_count key)
          (evil-record-macro key)
          t))
  "List of functions that should run on 'q' key by default.

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

(when evil-tex-toggle-override-q
  (evil-define-key 'normal evil-tex-mode-map "q"
    (evil-tex-dispatch-single-key ?t #'evil-tex-read-and-execute-toggle
                                  'evil-tex-q-functions)))

;;;###autoload
(define-minor-mode evil-tex-mode
  "Minor mode for latex-specific text objects in evil.

Installs the following additional text objects:

  \\[evil-tex-a-dollar] TeX math: $ .. $
  \\[evil-tex-a-macro] TeX command/macro: \\foo{..}
  \\[evil-tex-an-env] LaTeX environment \\begin{foo}..\\end{foo}
TODO F1X TH1S"
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
