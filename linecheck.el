;;; linecheck.el --- Quickly add marks to the beginnings of lines

;; Copyright (C) 2014 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.0
;; Url:
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Put this file in ~/.emacs.d/
;;; and add
;; (require 'linecheck)
;;; to ~/.emacs

;;; Code:

(defvar linecheck-mode-map (make-sparse-keymap)
  "Keymap for linecheck minor mode.")

(defvar linecheck-markkeys
  '((?i . "+") (?d . "-") (?c . "#") (?v . "|") (?q . "?") (?e . "=") (?s . "/"))
  "Assoc list containing pairs of keybinding (as single char) and
a mark to add to the beginning of line on typing the char at the
beginning of a line.")
(put 'linecheck-markkeys 'safe-local-variable 'listp)

(defun linecheck-looking-at-markkey ()
  (looking-at (regexp-opt (mapcar 'cdr linecheck-markkeys))))

(defvar linecheck-item-regex "[[:alpha:]][[:alpha:] .-]*[[:alpha:].]"
  "Used for searching for the first \"item\" – the thing you're
  checking on this line.")

;;;###autoload
(define-minor-mode linecheck-mode
  "Toggle linecheck-mode.
With arg, turn on linecheck-mode if and only if arg is positive.

linecheck-mode is a minor mode for quickly adding \"marks\" to
beginnings of lines using single-key keybindings, see
`linecheck-mode-markkeys'. Marks are only added if point is at
the beginning of a line.

                             KEY BINDINGS
                             ------------
\\{linecheck-mode-map}

Entering linecheck-mode calls the hook linecheck-mode-hook.
------------------------------------------------------------------------------"
  :init-value nil
  :lighter    " lc"
  :keymap     linecheck-mode-map
  ;; body:
  (linecheck-bind-markkeys)
  (toggle-truncate-lines 1)
  (auto-fill-mode 0)
  (when (and linecheck-mode (eq major-mode 'fundamental-mode))
    (diff-mode)		      ; TODO: define some syntax highlighting?
    (linecheck-mode)))


(defmacro linecheck-when-bolp (&rest body)
  "Only execute `body' if `bolp', otherwise call
`self-insert-command'."
  (declare (indent 0) (debug t))
  `(if (bolp)
       (progn ,@body)
     (call-interactively 'self-insert-command)))

(defmacro linecheck-defbolp (name arglist docstring &rest body)
  "Define two interactive versions of function – second one only
has an effect at the beginning of line."
  (declare (indent defun) (debug t))
  `(progn
     (defun ,(intern (concat (symbol-name name) "-when-bolp")) ,arglist
       ,docstring
       (interactive)
       (linecheck-when-bolp ,@body))
     (defun ,name ,arglist
       ,docstring
       (interactive)
       ,@body)))

(linecheck-defbolp linecheck-toggle-line-mark ()
  "Toggle the line mark on this line. See
`linecheck-markkeys' for the list of keybindings and
corresponding marks."
  (let ((char (cdr (assoc last-command-event linecheck-markkeys))))
    (if (looking-at char)
	(delete-char 1)
      (when (linecheck-looking-at-markkey)
	(delete-char 1))
      (insert char))
    (beginning-of-line)))

(defun linecheck-line-is-markedp ()
  (save-excursion
    (beginning-of-line)
    (linecheck-looking-at-markkey)))

(defun linecheck-initial-line-mark (char)
  (beginning-of-line)
  (unless (linecheck-line-is-markedp)
    (insert char)
    (beginning-of-line)))

(linecheck-defbolp linecheck-next-line-checked ()
  "Go to next line and check it with the first markkey (see
`linecheck-markkeys')."
  (forward-line)
  (recenter)
  (linecheck-initial-line-mark (cdar linecheck-markkeys)))

(linecheck-defbolp linecheck-previous-line ()
  "Go to previous line"
  (forward-line -1)
  (beginning-of-line))

(linecheck-defbolp linecheck-goto-next-unchecked-line ()
  "Go to the next line that needs checking."
  (beginning-of-line)
  (let ((goback (linecheck-looking-at-markkey)))
    (while (linecheck-looking-at-markkey)
      (forward-line 1))
    (when goback
      (forward-line -1))))




(linecheck-defbolp linecheck-edit ()
  ""
  (when (re-search-forward linecheck-item-regex (line-end-position) 'noerror)
    (goto-char (match-beginning 0))))


;;; Search/lookup functions:
(defun linecheck-item ()
  (save-excursion
    (and
     (re-search-forward linecheck-item-regex (line-end-position) 'noerror)
     (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(defvar linecheck-favourite-search-fns '(linecheck-search-ddg-abstract
					 linecheck-search-wiki-abstract
					 linecheck-search-ddg-browser)
  "Used by linecheck-search-favourites")

(linecheck-defbolp linecheck-search-favourites ()
  "Stop at the first hit."
  (let ((res "")
	(fns linecheck-favourite-search-fns))
    (while (and fns (equal res ""))
      (setq res (funcall (car fns))
	    fns (cdr fns)))))

(linecheck-defbolp linecheck-search-wiki-abstract ()
  "Search wikipedia, requires dimwiki.el."
  (let ((res (dimwiki-search
	      (format "%s" (linecheck-item)))))
    (message "%s" (substring res 0 (min (length res)
					250)))))

(linecheck-defbolp linecheck-search-ddg-abstract ()
  "Search DuckDuckGo for an abstract-result, requires ddg.el"
  (message "%s" (cdr (assoc 'Abstract
			    (ddg-search
			     (format "%s" (linecheck-item)))))))

(linecheck-defbolp linecheck-search-ddg-browser ()
  "Search DuckDuckGo with your browser."
  (browse-url (format "http://ddg.gg/?q=\"%s\"" (linecheck-item))))

(linecheck-defbolp linecheck-search-lexin ()
  "Search lexin"
  (let ((word (linecheck-item)))
    (when word
      (browse-url
       (concat "http://decentius.hit.uib.no/lexin.html?"
	       "&dict=nbo-nny-maxi"
	       "&checked-languages=E"
	       "&checked-languages=N"
	       "&checked-languages=NNY"
	       "&search=" word)))))

(linecheck-defbolp linecheck-search-nwn ()
  "Requires moz-nwn-lookup.el"
  (let ((word (linecheck-item)))
    (moz-nwn-lookup-thing-at-point word)))




;;; Keybindings:
(defun linecheck-bind-markkeys ()
  "Set the keybindings from `linecheck-markkeys'."
  (mapc
   (lambda (pair)
     (define-key
       linecheck-mode-map
       (string (car pair))
       'linecheck-toggle-line-mark))
   linecheck-markkeys))

(define-key linecheck-mode-map (kbd "j") 'linecheck-next-line-checked-when-bolp)
(define-key linecheck-mode-map (kbd "SPC") 'linecheck-next-line-checked-when-bolp)
(define-key linecheck-mode-map (kbd "k")  'linecheck-previous-line-when-bolp)
(define-key linecheck-mode-map (kbd "n") 'linecheck-goto-next-unchecked-line-when-bolp)

(define-key linecheck-mode-map (kbd "e") 'linecheck-edit-when-bolp)

(define-key linecheck-mode-map (kbd "S") 'linecheck-search-ddg-browser-when-bolp)
(define-key linecheck-mode-map (kbd "s") 'linecheck-search-ddg-abstract-when-bolp)
(define-key linecheck-mode-map (kbd "f") 'linecheck-search-favourites-when-bolp)
(define-key linecheck-mode-map (kbd "w") 'linecheck-search-wiki-abstract-when-bolp)
(define-key linecheck-mode-map (kbd "L") 'linecheck-search-lexin-when-bolp)

(provide 'linecheck)

;;; linecheck.el ends here
