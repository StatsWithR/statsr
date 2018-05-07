
## Test environments

* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0 and R-devel
* win-builder (devel and release)

## R CMD check results with --as-cran and --run-donttest
There were no ERRORs or WARNINGs.
2 NOTES:  (1) new package submission;  (2) Possibly mis-spelled words in DESCRIPTION:   Bartlett's, Lindley's   
  
Bartlett and Lindley are proper nouns

Examples in \donttest take more than 5 secs

## Reverse Dependencies

None

## Comments

Resubmission of new package - as requested we:

* updated Description to avoid starting with "R package"

* wrote package names and software names in single quotes (e.g. 'shiny') in the Description.

* added more small executable examples in Rd-file for  `inference` and more details in others.

* replaced `\dontrun{}` by `\donttest{}` in Rd-files as suggested. The examples in  `bayes_inference`  with `\donttest` take 13 seconds elapsed time to run now when checking the package with --run-donttest.  Examples for shiny apps now test for an interactive environment.

* checked spelling of DESCRIPTION and Rd files



