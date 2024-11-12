## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Previously resubmitted with corrections made as requested by Uwe Ligges:
   * package names in description surrounded by a single quote,
   * with CODE_OF_CONDUCT.md references removed from readme,
   * file LICENSE removed and 'cph' specified in Authors@R
   
* Previously resubmitted with corrections made as requested by Konstanze Lauseker:
   * Examples with \dontrun are unwrapped and (if required) wrapped by requireNamespace() checks.
   * At this point no external references to these methods exist, so no change to DESCRIPTION has been made. 
   
* This is a resubmission with changes made as requested by Beni Altmann:
   * Changes to working directory were unavoidable by the interface to ChemoSpec (that package doesn't allow
   for specifying directory when importing files). As such, in-code changes to working directory are protected
   with a call to `on.exit()`. 
