# PlotFTIR (development version)

* Added Raman spectral plotting with `plot_raman()`, `read_raman()`, and utilities for baseline correction, smoothing, and peak finding (#34).
* `smooth_spectra()`, `baseline_correct()`, `normalize_raman()`, and `find_peak_maxima()` accept `sample_ids = NULL` to select every sample (#34).
* `smooth_spectra()` and `baseline_correct()` now report a clear error when given FTIR absorbance or transmittance data instead of Raman spectra (#34).
* `normalize_raman()` now rejects spectra with a non-positive maximum and reports `NA` intensity values rather than silently propagating them (#34).
* `read_raman()` captures single-`#` header metadata, reports true file line numbers for invalid data, and warns about malformed rows (#34).

# PlotFTIR 1.3.1

* Updated package data sets to properly print after v1.3.0.
* Added bilingual error codes and bilingual pretty printing (#40)
* `read_ftir_jdx()` now auto-converts .jdx files with wavelength (micrometer) x-axis units to wavenumber (#43)

# PlotFTIR 1.3.0

* `print()` now provides pretty printing for PlotFTIR data structures, showing spectral range, resolution, intensity type, number of samples, and sample IDs. ([#27](https://github.com/NRCan/PlotFTIR/issues/27))
* Updated to use `ggplot2::coord_transform()` instead of `coord_trans`, which is now deprecated. ([#25](https://github.com/NRCan/PlotFTIR/issues/25))
* Resolved issue with data exchange with `ir` package. ([#35](https://github.com/NRCan/PlotFTIR/issues/35))

# PlotFTIR 1.2.1

* Updates (mostly to testing) to comply with pending ggplot2 4.0.0 release (#23)

# PlotFTIR 1.2.0

* Patch update to expose `check_ftir_data` for downstream packages. (related to #19)
* Add import capability for .jdx filetype using `readJDX`. (#20)

# PlotFTIR 1.1.0

* Added ability to set default language in `options()`. (#10) 
* Added ability to highlight one or more samples in spectra. (#15)
* Added ability to add a background shaded band (like a wide marker) 
to indicate a given range. (#16)
* Added attributes to imported data to simplify internal calculation 
of thingslike y axis units. (#17)

# PlotFTIR 1.0.0

* Initial Release & CRAN Submission.
