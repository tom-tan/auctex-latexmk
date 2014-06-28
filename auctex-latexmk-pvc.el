;;; auctex-latexmk-pvc.el --- Add LatexMk support (with -pvc option) to AUCTeX

(require 'auctex-latexmk)

;;  -neal-
;;
;; Attempt to add support for -pvc mode to auctex-latexmk.
;;
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

;;;###autoload
(defun auctex-latexmk-pvc-setup ()
  "Add LatexMk and LatexMkPvc command to TeX-command-list."

  (auctex-latexmk-setup)

  (setq-default TeX-command-list
                (cons
                 '("LatexMkPvc" "latexmk -pvc %t" TeX-run-latexmk-pvc nil
                   (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk in -pvc mode")
                 TeX-command-list)
                ))

(defun TeX-run-latexmk-pvc (name command file)
  (let ((TeX-sentinel-default-function 'Latexmk-pvc-sentinel)
        (pair (assq buffer-file-coding-system auctex-latexmk-encoding-alist)))
    (unless (null pair)
      (setenv "LATEXENC" (cdr pair)))

    (let* ((process (TeX-run-TeX name command file))
           (original-filter (process-filter process)))
      (Latexmk-pvc-wrap-filter process original-filter nil))

    (setenv "LATEXENC" nil)))

;; the string that we expect latexmk -pvc to output if it is done processing:
(setq-default Latexmk-pvc-waiting-string
              "=== Watching for updated files. Use ctrl/C to stop ...")

(defun Latexmk-pvc-wrap-filter (process original-filter announce)
  "Wrap the process filter for PROCESS, currying ORIGINAL-FILTER and ANNOUNCE."
  (let ((wrapper (apply-partially 'Latexmk-pvc-filter original-filter announce)))
    (set-process-filter process wrapper)))

;; the filter function as described above:
(defun Latexmk-pvc-filter (original-filter announce process string)
  (funcall original-filter process string)  ;; call the auctex process filter

  (if announce
      (progn
        (message "LatexMkPvc: processing")  ; announce start of each cycle
        (Latexmk-pvc-set-filter process original-filter nil)))

  ;; check the buffer for Latexmk-pvc-waiting-string
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (- (point-max) (+ 1 (length Latexmk-pvc-waiting-string))))
      (if (search-forward Latexmk-pvc-waiting-string nil t)
          ;; found it
          (progn
            ;; go to the previous occurrence, or start of file if none
            (goto-char (- (point-max) (+ 1 (length Latexmk-pvc-waiting-string))))
            (if (not (search-backward Latexmk-pvc-waiting-string nil t))
                (goto-char (point-min)))

            ;; focus just on output from last processing round of latexmk:
            (narrow-to-region (point) (point-max))

            ;; call the latexmk fake sentinel process defined below
            (goto-char (point-max))
            (Latexmk-pvc-fake-sentinel process "LatexMkPvc")

            ;; reset filter to announce at start of next cycle
            (Latexmk-pvc-set-filter process original-filter t)))
      )))


;; this one is called if the Latexmk process changes state (exits?)
(defun Latexmk-pvc-sentinel (process name)
  (Latexmk-sentinel process name)
  (message (format "%s: exited" name)))

;; Tried to modify Latexmk-sentinel to do something reasonable
;; with the output of latexmk -pvc when called by the filter.
;; My changes seems to more or less work, but I probably didn't get 
;; it really right, because I'm not sure of what this function is
;; trying to do (e.g. in case of errors, bibtex, etc.)

;; This one is called by the (wrapped) process filter 
;; at the end of each processing cycle.
(defun Latexmk-pvc-fake-sentinel (process name)
  (save-excursion
    (let ((something-to-do nil)) ;; added -neal
      (goto-char (point-max))
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
                (TeX-LaTeX-sentinel process name)))))
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
      (if (not something-to-do) 
          (message (format "%s: Nothing to do" name))))))


(provide 'auctex-latexmk-pvc)
;;; auctex-latexmk-pvc.el ends here
