# auctex-latexmk.el

This library adds LatexMk support to AUCTeX.

Requirements:
* AUCTeX
* LatexMk
* TeXLive (2011 or later if you write TeX source in Japanese)

You can install it by using `package-install` via [MELPA](https://melpa.org/).

To use this package, add the following line to your `.emacs` file:
```elisp
    (require 'auctex-latexmk)
    (auctex-latexmk-setup)
```
After that, by using `M-x TeX-command-master` (or C-c C-c), you can use
LatexMk command to compile TeX source.
    
LatexMk will inherit many AUCTeX settings, including:
* Run with `-interaction-nonestopmode` if `TeX-interactive-mode` minor mode is
  active
* Run with `-synctex` if `TeX-source-correlate-mode` is active
  
If you would like LatexMk to aso pass the `-pdf` flag when `TeX-PDF-mode` is
active add
```elisp
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
```
to your `.emacs` file.  Setting
```elisp
    (setq auctex-latexmk-run-previewer-continuously t)
```
will run latexmk with the `-pvc` flag, which continuously updates the viewer
as the source file is changed.

Additional configuration of `latexmk` is possible by creating a `~/.latexmkrc` file. For
example, to always compile to pdf add the following line to your `.latexmkrc`
file:
```perl
# .latexmkrc starts
$pdf_mode = 1;
# .latexmkrc ends
```
Additional documention describing all the available options is available on
[CTAN](http://ctan.org/pkg/latexmk).

For Japanese users:

LatexMk command automatically stores the encoding of a source file
and passes it to latexmk via an environment variable named `LATEXENC`.
Here is the example of `.latexmkrc` to use `LATEXENC`:
```perl
    # .latexmkrc starts
    $kanji    = "-kanji=$ENV{\"LATEXENC\"}" if defined $ENV{"LATEXENC"};
    $latex    = "platex -interaction=nonstopmode $kanji";
    $bibtex   = 'pbibtex $kanji';
    $dvipdf   = 'perl -e "exec(\'dvipdfmx\', \$ARGV[0])"';
    $pdf_mode = 3;
    # .latexmkrc ends
```
