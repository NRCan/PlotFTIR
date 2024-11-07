## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Previously resubmitted with corrections made as requested by Uwe Ligges:
   * package names in description surrounded by a single quote,
   * with CODE_OF_CONDUCT.md references removed from readme,
   * file LICENSE removed and 'cph' specified in Authors@R
   
* This is a resubmission with corrections made as requested by Konstanze Lauseker:
   * Examples with \dontrun are unwrapped and (if required) wrapped by requireNamespace() checks.
   * At this point no external references to these methods exist, so no change to DESCRIPTION has been made. 
