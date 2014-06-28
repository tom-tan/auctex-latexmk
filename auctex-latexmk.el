;;; auctex-latexmk.el --- Add LatexMk support to AUCTeX

;; Copyright (C) 2013, 2014 by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; URL: https://github.com/tom-tan/auctex-latexmk/
;; Package-Requires: ((auctex "11.87"))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library adds LatexMk support to AUCTeX.
;;
;; Requirements:
;;   * AUCTeX
;;   * LatexMk
;;   * TeXLive (2011 or later if you write TeX source in Japanese)
;;
;; To use this package, add the following line to your .emacs file:
;;     (require 'auctex-latexmk)
;;     (auctex-latexmk-setup)
;; And add the following line to your .latexmkrc file:
;;     # .latexmkrc starts
;;     $pdf_mode = 1;
;;     # .latexmkrc ends
;; After that, by using M-x TeX-command-master (or C-c C-c), you can use
;; LatexMk command to compile TeX source.
;;
;; For Japanese users:
;;
;; LatexMk command automatically stores the encoding of a source file
;; and passes it to latexmk via an environment variable named "LATEXENC".
;; Here is the example of .latexmkrc to use "LATEXENC":
;;     # .latexmkrc starts
;;     $kanji    = "-kanji=$ENV{\"LATEXENC\"}" if defined $ENV{"LATEXENC"};
;;     $latex    = "platex -interaction=nonstopmode $kanji";
;;     $bibtex   = 'pbibtex $kanji';
;;     $dvipdf   = 'perl -e "exec(\'dvipdfmx\', \$ARGV[0])"';
;;     $pdf_mode = 3;
;;     # .latexmkrc ends

;;; Code:

(require 'tex-buf)
(require 'latex)

(defgroup auctex-latexmk nil
  "Add LatexMk support to AUCTeX."
  :group 'AUCTeX
  :prefix "auctex-latexmk")

(defcustom auctex-latexmk-encoding-alist
  '((japanese-iso-8bit      . "euc")
    (japanese-iso-8bit-unix . "euc")
    (euc-jp                 . "euc")
    (euc-jp-unix            . "euc")
    (utf-8                  . "utf8")
    (utf-8-unix             . "utf8")
    (japanese-shift-jis     . "sjis")
    (japanese-shift-jis-dos . "sjis"))
  "Encoding mapping for platex."
  :group 'auctex-latexmk)

;; -neal-
;; Add an option for adding the -pvc flag to the latexmk call.
;; I am sure this is not the right way to add this!
(setq neal-latexmk-flags " -pvc ") ;; neal

;; -neal-
;; When run in -pvc mode, latexmk doesn't terminate when t is done 
;; with the LaTeX processing.  Instead it outputs the following string:
;;
;;   === Watching for updated files. Use ctrl/C to stop ...
;;
;; and then watches the files that the process depends on.  When one of 
;; them changes, it starts another round, and repeats.
;; 
;; As I understand it, the auctex system runs the latexmk process and 
;; monitors it in two ways.  First, the process has an elisp *process
;; filter* that is called whenever latexmk produces output.  Second,
;; the process has a *sentinel* that is called when the latexmk process
;; terminates.  Currently the process filter mostly just collects the
;; output in the compilation buffer, and when the process ends the sentinel
;; is called to report on the results to the user.
;;
;; But in -pvc mode the latexmk process doesn't terminate.  We replace the
;; process filter with a function that first calls the regular process filter
;; then checks the buffer for the "=== Watching for .." string at the end.
;; If it finds that string, then it narrows the region to just the output
;; since the last run, then calls the sentinel directly on that portion.
;;
;; One issue is that the latexmk output is slightly different in -pvc mode,
;; so some of the strings that the sentinel looked for to parse the latexmk
;; output will not be there.  I tried to patch the sentinel function below,
;; but I doubt I did it the right way.
;;

;; we cache the last filter used in this global variable. this may fail if 
;; there are mulitple latexmk processes and for some reason they are using
;; different filters:
(setq neal-cached-filter 'TeX-format-filter) 

;; the string that we expect latexmk -pvc to output if it is done processing:
(setq neal-waiting-string "=== Watching for updated files. Use ctrl/C to stop ...")

;; the filter function as described above:
(defun neal-filter-wrapper (process string)
  "in ~/.emacs.d/auctex-latexmk.el"
  (apply neal-cached-filter process string ())  ;; call the auctex process filter

  ;; check the buffer for neal-waiting-string
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (- (point-max) (+ 1 (length neal-waiting-string))))
      (if (search-forward neal-waiting-string nil t)
          ;; found it
          (progn
            ;; go to the previous occurrence, or start of file if none
            (goto-char (- (point-max) (+ 1 (length neal-waiting-string))))
            (if (not (search-backward neal-waiting-string nil t))
                (goto-char (point-min)))
            ;; focus just on output from last processing round of latexmk:
            (narrow-to-region (point) (point-max))
            (goto-char (point-max))
            ;; call the latexmk sentinel process defined below
            (Latexmk-sentinel process "LatexMk")
            )))))

(defun TeX-run-latexmk (name command file)
  (let ((TeX-sentinel-default-function 'Latexmk-sentinel)
        (pair (assq buffer-file-coding-system auctex-latexmk-encoding-alist)))
    (unless (null pair)
      (setenv "LATEXENC" (cdr pair)))

    ;; (TeX-run-TeX name command file) ;; neal ->

    ;; (1) add pvc flag to latexmk command (if set).
    ;;  This is probably not the right way to support this option.
    ;;  But I don't know auctex well enough to know the "right" way.
    ;;  Maybe there should be two separate auctex TeX commands,
    ;;  one for latexmk, one for latexmk -pvc.
    (let* ((command (concat command neal-latexmk-flags))
           (process (TeX-run-TeX name command file))
           (filter (process-filter process)))
      ;; (2) wrap neal-filter-wrapper around TeX buffer's filter 
      (setq neal-cached-filter filter)
      (set-process-filter process 'neal-filter-wrapper))

    (setenv "LATEXENC" nil)))

;;;###autoload
(defun auctex-latexmk-setup ()
  "Add LatexMk command to TeX-command-list."
  (setq-default TeX-command-list
                (cons
                 '("LatexMk" "latexmk %t" TeX-run-latexmk nil
                   (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk")
                 TeX-command-list)
                LaTeX-clean-intermediate-suffixes
                (append LaTeX-clean-intermediate-suffixes
                        '("\\.fdb_latexmk" "\\.aux.bak" "\\.fls"))))

;; -neal-  Tried to modify the sentinel to do something reasonable
;;  with the output of latexmk -pvc when called by the filter.
;;  My changes seems to more or less work, but I probably didn't get 
;;  it really right, because I'm not sure of what this function is
;;  trying to do (e.g. in case of errors, bibtex, etc.)

(defun Latexmk-sentinel (process name)
  (save-excursion
    (let ((something-to-do nil)) ;; added -neal
      (goto-char (point-max))
      (cond
       ;; added "or" with first condition -neal
       ((or
         (search-backward neal-waiting-string nil t)
         (re-search-backward (format "^%s finished at" name) nil t))

        (if (re-search-backward "^Run number [0-9]+ of rule '\\(pdf\\|lua\\|xe\\)?latex'" nil t)
            (progn
              (setq something-to-do t) ;; added -neal
              (forward-line 5)
              (let ((beg (point)))
                (when (string= (current-word) "Latexmk")
                  ;; Special treatment for MiKTeX
                  (forward-line))
                (re-search-forward "^Latexmk:" nil t)
                (beginning-of-line)
                (save-restriction
                  (narrow-to-region beg (point))
                  (goto-char (point-min))
                  (TeX-LaTeX-sentinel process name)))))))
      ;; moved "nothing to do message" from above to below -neal

      ;; made the condition block below independent of one above -neal
      (cond
       ((re-search-backward (format "^%s exited abnormally with code" name) nil t)
        (re-search-backward "^Collected error summary (may duplicate other messages):" nil t)
        (re-search-forward "^  \\([^:]+\\):" nil t)
        (let ((com (TeX-match-buffer 1)))
          (cond
           ((string-match "^\\(pdf\\|lua\\|xe\\)?latex" com)
            (setq something-to-do t)  ;; -neal
            (goto-char (point-min))
            (TeX-LaTeX-sentinel process name)
            (when (string= TeX-command-next TeX-command-BibTeX)
              (setq TeX-command-default)))
           ((string-match "^bibtex " com)
            (setq something-to-do t)  ;; -neal
            (forward-line -1)
            (re-search-backward com nil t)
            (forward-line 5)
            (let ((beg (point)))
              (re-search-forward "^Rule" nil t)
              (beginning-of-line)
              (save-restriction
                (narrow-to-region beg (point))
                (TeX-BibTeX-sentinel process name))))))))
      ;; added: -neal
      (if (not something-to-do) 
          (message "LatexMk: Nothing to do")))))

(defadvice TeX-recenter-output-buffer (around recenter-for-latexmk activate)
  (setq ad-return-value
        (let ((buffer (TeX-active-buffer)))
          (if buffer
              (if (with-current-buffer buffer
                    (goto-char (point-max))
                    (re-search-backward "^latexmk" nil t))
                  (let ((old-buffer (current-buffer)))
                    (TeX-pop-to-buffer buffer t t)
                    (bury-buffer buffer)
                    (goto-char (point-max))
                    (re-search-backward "^Run number [0-9]+ of rule 'bibtex .+'"
                                        nil t)
                    (re-search-forward "^Rule" nil t)
                    (forward-line -1)
                    (recenter (if line
                                  (prefix-numeric-value line)
                                  (/ (window-height) 2)))
                    (TeX-pop-to-buffer old-buffer nil t))
                  ad-do-it)
              (message "No process for this document.")))))

(provide 'auctex-latexmk)
;;; auctex-latexmk.el ends here
