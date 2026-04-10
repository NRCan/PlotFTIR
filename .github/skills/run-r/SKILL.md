---
name: run-r
description: Use when running R code in terminal, testing R functions, documenting R, or any R processing. Ensures use of full R binary path on Windows.
---

When executing R code chunks or scripts through a terminal or PowerShell, always use the full path to the R binary: C:\Program Files\R\R-4.5.1\bin\x64\R.exe

If more appropriate, use Rscript.exe from the same directory: C:\Program Files\R\R-4.5.1\bin\x64\Rscript.exe

This ensures compatibility and avoids path issues.

If there are newer versions of R installed at the same C:\Program Files\R\ directory, adjust the path accordingly to point to the correct version. Always verify the path before executing to ensure it points to the intended R version.
