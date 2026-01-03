;;; tmpl-mode.el --- Minor mode for "tmpl" template files  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Anders Lindgren

;; Author: Anders Lindgren
;; Version: 1.0.0
;; Created: 2025-01-11
;; URL: https://github.com/Lindydancer/tmpl-mode
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a minor mode for Go's `text/template' template system.
;;
;; This package highlights the `{{ ... }}` constructs used by this
;; template system.  As this is a minor mode, it can be used in
;; addition to a suitable major mode.
;;
;; The following is syntax colored:
;;
;; * Tmpl keywords like `if' `eq' and `index'.
;;
;; * The entire `{{ ... }}' is highlighted, to make it stand out
;;   against the rest of the buffer.
;;
;; * The `{{-' or `-}}' constructs makes the template engine "eat"
;;   whitespace before or after the template, respectively.  To
;;   visualize the effect of this, the whitespace that will be removed
;;   is also highlighted as part of the template.

;; Usage:
;;
;; Install this with an Emacs package manager.
;;
;; Add this to your init file:
;;
;;     (tmpl-global-mode)

;; Example:
;;
;; This is an example of a Chezmoi init file using tmpl templates.
;;
;;     [data]
;;         {{- if eq .chezmoi.os "windows" }}
;;         my_emacs_bin_dir 	   = "c:/Program Files/emacs/bin"
;;         {{- else if eq .chezmoi.os "darwin" }}
;;         my_emacs_bin_dir 	   = "/Applications/Emacs.app/Contents/MacOS"
;;         {{- else }}
;;         my_emacs_bin_dir 	   = "/usr/bin"
;;         {{- end }}

;;; Code:


(defgroup tmpl nil
  "Minor mode used to highlight \"tmpl\" templates."
  :group 'faces)


;; Don't warn for using these dynamically bound variables, see
;; `font-lock-extend-region-functions'.
(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))


(defvar tmpl-keywords
  '(;; Actions
    "block"
    "break"
    "continue"
    "else"
    "end"
    "if"
    "include"
    "promptString"
    "range"
    "template"
    "with"
    ;; Functions
    "and"
    "call"
    "html"
    "index"
    "slice"
    "js"
    "len"
    "not"
    "or"
    "print"
    "printf"
    "urlquery"
    ;; Comparisons
    "eq"
    "ne"
    "le"
    "lt"
    "ge"
    "gt"
    "hasKey")
  "List of tmpl keywords.")


;; Note: The Emacs style guide recommends that a face should either
;; define properties or inherited from another face. Since this only
;; adds the :extend property, that recommendation is not followed, so
;; that the user or themes doesn't need to customize yet another face.
(defface tmpl-highlight-face
  '((t :inherit highlight :extend t))
  "Face for highlighting a template."
  :group 'tmpl)


;;;###autoload
(define-minor-mode tmpl-mode
  "Minor mode for \".tmpl\" templates."
  :group 'tmpl
  (if tmpl-mode
      (tmpl-add-keywords)
    (tmpl-remove-keywords))
  (font-lock-flush))


(defun tmpl-maybe-include-whitespace-backward (beg)
  "The beginning of the template, maybe with extra whitespace.

When the template starts with `{{-' whitespace before is
\"eaten\" emitted by the template engine.

BEG is the buffer positions of the start marker.

Return a buffer position at the start of the eaten whitespace area, or
BEG."
  (save-excursion
    (goto-char beg)
    (if (eq (char-after (+ (point) 2)) ?-)
        (skip-chars-backward " \t\n"))
    (point)))


(defun tmpl-maybe-include-whitespace-forward (end)
  "The end of the template, maybe with extra whitespace.

When the template ends with `-}}' whitespace after is \"eaten\"
by the template engine.

END is the buffer positions after the end marker.

Return a buffer position at the end of the eaten whitespace area, or
END."
  (save-excursion
    (goto-char end)
    (if (eq (char-before (- (point) 2)) ?-)
        (skip-chars-forward " \t\n"))
    (point)))


(defun tmpl-extend-font-lock-region ()
  "Extend the font-lock region to include whitespace around a template."
  (save-excursion
    ;; Note: The region is always extended even if the "-" character
    ;; isn't present.  The user might have just deleted it and the
    ;; surrounding highlighting needs to be updated.
    (let ((beg (save-excursion
                 (goto-char font-lock-beg)
                 (skip-chars-forward " \t\n")
                 (when (looking-at "{{")
                   (skip-chars-backward " \t\n"))
                 (point)))
          (end (save-excursion
                 (goto-char font-lock-end)
                 (skip-chars-backward " \t\n")
                 (when (save-excursion
                         (backward-char 2)
                         (looking-at "}}"))
                   (skip-chars-forward " \t\n"))
                 (point))))
      ;; Important:
      ;;
      ;; * Never shrink the region, in either direction.
      ;;
      ;; * Only return non-nil when the region is extended.
      ;;
      ;; If the above is not followed, font-lock could loop forever.
;;    (message "%s %s / %s %s" font-lock-beg beg font-lock-end end)
      (let ((res nil))
        (when (< beg font-lock-beg)
          (setq font-lock-beg beg)
          (setq res t))
        (when (> end font-lock-end)
          (setq font-lock-end end)
          (setq res t))
        res))))


(defun tmpl-match-template (limit)
  "Match template, return non-nil if a template is found.

LIMIT is the buffer position that bounds the search.

Content of match data:

 * 0 -- The template, maybe extended with whitespace
 * 1 -- The start marker `{{'
 * 2 -- The content between the markers
 * 3 -- The end marker `}}'"
  (and (re-search-forward "\\({{\\)\\(.*?\\)\\(}}\\)" limit t)
       (let ((md (match-data))
             (beg (tmpl-maybe-include-whitespace-backward (match-beginning 0)))
             (end (tmpl-maybe-include-whitespace-forward (match-end 0))))
         ;; Modify the match data, so that the highlighting can be
         ;; done with a simple `(0 'face ...).
         (set-match-data `(,beg
                           ,end
                           ,@(cdr (cdr md))))
         t)))


(defvar tmpl-font-lock-keywords
  `((tmpl-match-template
     ;; Comments inside the template.
     ("/\\*.*?\\*/"
      ;; PRE-MATCH-FORM:
      (progn
        (goto-char (match-beginning 2))
        (match-end 2))
      ;; POST-MATCH-FORM:
      (goto-char (match-end 0))
      (0 'font-lock-comment-face))
     ;; The "{{" and "}}" delimiters.
     (1 'font-lock-constant-face prepend)
     (3 'font-lock-constant-face prepend)
     ;; Template keywords.
     (,(concat "\\_<" (regexp-opt tmpl-keywords) "\\_>")
      ;; PRE-MATCH-FORM:
      (progn
        (goto-char (match-beginning 2))
        (match-end 2))
      ;; POST-MATCH-FORM:
      (goto-char (match-end 0))
      (0 'font-lock-keyword-face))
     ;; Highlight the entire template (maybe with extra whitespace).
     (0 'tmpl-highlight-face prepend)))
  "Font-lock keywords for Tmpl mode.

See `font-lock-keywords' for details on the format.")


(defun tmpl-add-keywords ()
  "Add font-lock keywords to highlight \".tmpl\" templates."
  (add-to-list 'font-lock-extend-region-functions
               #'tmpl-extend-font-lock-region)
  (setq font-lock-multiline t)
  ;; Note: `font-lock-multiline' is not restored. It may have gotten
  ;; set by some other minor mode. Besides, it doesn't hurt keeping it
  ;; set to t.
  (font-lock-add-keywords
   nil
   tmpl-font-lock-keywords
   'append))


(defun tmpl-remove-keywords ()
  "Remove tmpl font-lock keywords."
  (setq font-lock-extend-region-functions
        (delq #'tmpl-extend-font-lock-region
              font-lock-extend-region-functions))
  (font-lock-remove-keywords nil tmpl-font-lock-keywords))


;; ----------------------------------------------------------------------
;; Global tmpl mode
;;
;; This is used to activate `tmpl-mode' for suitable files.

(defcustom tmpl-trigger-files
  '("\\.tmpl\\'"
    "/.chezmoiignore\\'")
  "List of filename patterns to match for Tmpl mode to activate itself."
  :type '(repeat regexp)
  :group 'tmpl)


(defconst tmpl-auto-mode-alist-entry
  '("\\.tmpl\\'" nil tmpl-mode)
  "Entry in the `auto-mode-alist' when Tmpl global mode is active.

Note that the last element in the list only needs to be
non-nil.  The value `tmpl-mode' was selected to make this entry
more unique.")


(defun tmpl-turn-on-if-desired ()
  "Maybe enable Tmpl mode.

The mode is enabled if the buffer file name matches one of the regexps
in `tmpl-trigger-files'."
  (when (and buffer-file-name
             (assoc-default buffer-file-name tmpl-trigger-files
                            #'string-match-p t))
    (tmpl-mode 1)))


;; NOTE: `tmpl-global-mode' is implemented in two layers.
;;
;; The outer layer, `tmpl-global-mode', manages `auto-mode-alist' and
;; enables/disables the inner layer `tmpl-global-mode-internal'.
;;
;; This implementation is used since `define-globalized-minor-mode' is
;; on one hand very useful for activating the underlying `tmpl-mode'
;; in relevant buffers.  On the other hand, it doesn't support adding
;; extra code, which is needed to manage `auto-mode-alist'.

;;;###autoload
(define-globalized-minor-mode tmpl-global-mode-internal tmpl-mode
  tmpl-turn-on-if-desired
  :group 'tmpl
  "Automatically enable Tmpl mode when file name matches `tmpl-trigger-files'.

Users are intended to use Tmpl global mode, for example by
placing `(tmpl-global-mode) in their init file.")


;;;###autoload
(define-minor-mode tmpl-global-mode
  "Automatically enable Tmpl mode when file name matches `tmpl-trigger-files'.

Also, ignore the .tmpl suffix when selecting major mode."
  :global t
  :group 'tmpl
  (if tmpl-global-mode
      (progn
        (tmpl-global-mode-internal 1)
        (add-to-list 'auto-mode-alist tmpl-auto-mode-alist-entry))
    (tmpl-global-mode-internal -1)
    (setq auto-mode-alist
          (delete tmpl-auto-mode-alist-entry auto-mode-alist))))


;; ----------------------------------------------------------------------
;; The end
;;

(provide 'tmpl-mode)
;;; tmpl-mode.el ends here
