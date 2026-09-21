# Changelog

## PlotFTIR (development version)

- [`add_wavenumber_marker()`](https://nrcan.github.io/PlotFTIR/reference/add_wavenumber_marker.md)
  and
  [`add_band()`](https://nrcan.github.io/PlotFTIR/reference/add_band.md)
  now respect `label_aesthetics = list(vjust = ...)` so peak labels can
  be vertically offset to reduce overlap
  ([@pbulsink](https://github.com/pbulsink),
  [\#38](https://github.com/NRCan/PlotFTIR/issues/38)).

- [`fit_peaks()`](https://nrcan.github.io/PlotFTIR/reference/fit_peaks.md)
  now documents explicit fitting controls and clarifies that automatic
  peak discovery is a heuristic starting point for fitting, not a
  validated deconvolution standard
  ([\#33](https://github.com/NRCan/PlotFTIR/issues/33)).

- Peak fitting functionality added with
  [`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md),
  [`fit_peaks()`](https://nrcan.github.io/PlotFTIR/reference/fit_peaks.md),
  and related functions.
  ([\#33](https://github.com/NRCan/PlotFTIR/issues/33))

- Added Raman spectral plotting with
  [`plot_raman()`](https://nrcan.github.io/PlotFTIR/reference/plot_raman.md),
  [`read_raman()`](https://nrcan.github.io/PlotFTIR/reference/read_raman.md),
  and utilities for baseline correction, smoothing, and peak finding
  ([\#34](https://github.com/NRCan/PlotFTIR/issues/34)).

- [`smooth_spectra()`](https://nrcan.github.io/PlotFTIR/reference/smooth_spectra.md),
  [`baseline_correct()`](https://nrcan.github.io/PlotFTIR/reference/baseline_correct.md),
  [`normalize_raman()`](https://nrcan.github.io/PlotFTIR/reference/normalize_raman.md),
  and
  [`find_peak_maxima()`](https://nrcan.github.io/PlotFTIR/reference/find_peak_maxima.md)
  accept `sample_ids = NULL` to select every sample
  ([\#34](https://github.com/NRCan/PlotFTIR/issues/34)).

- [`smooth_spectra()`](https://nrcan.github.io/PlotFTIR/reference/smooth_spectra.md)
  and
  [`baseline_correct()`](https://nrcan.github.io/PlotFTIR/reference/baseline_correct.md)
  now report a clear error when given FTIR absorbance or transmittance
  data instead of Raman spectra
  ([\#34](https://github.com/NRCan/PlotFTIR/issues/34)).

- [`normalize_raman()`](https://nrcan.github.io/PlotFTIR/reference/normalize_raman.md)
  now rejects spectra with a non-positive maximum and reports `NA`
  intensity values rather than silently propagating them
  ([\#34](https://github.com/NRCan/PlotFTIR/issues/34)).

- [`read_raman()`](https://nrcan.github.io/PlotFTIR/reference/read_raman.md)
  captures single-`#` header metadata, reports true file line numbers
  for invalid data, and warns about malformed rows
  ([\#34](https://github.com/NRCan/PlotFTIR/issues/34)).

## PlotFTIR 1.3.1

CRAN release: 2026-09-17

- Updated package data sets to properly print after v1.3.0.
- Added bilingual error codes and bilingual pretty printing
  ([\#40](https://github.com/NRCan/PlotFTIR/issues/40))
- `read_ftir_jdx()` now auto-converts .jdx files with wavelength
  (micrometer) x-axis units to wavenumber
  ([\#43](https://github.com/NRCan/PlotFTIR/issues/43))

## PlotFTIR 1.3.0

CRAN release: 2026-07-21

- [`print()`](https://rdrr.io/r/base/print.html) now provides pretty
  printing for PlotFTIR data structures, showing spectral range,
  resolution, intensity type, number of samples, and sample IDs.
  ([\#27](https://github.com/NRCan/PlotFTIR/issues/27))
- Updated to use
  [`ggplot2::coord_transform()`](https://ggplot2.tidyverse.org/reference/coord_transform.html)
  instead of `coord_trans`, which is now deprecated.
  ([\#25](https://github.com/NRCan/PlotFTIR/issues/25))
- Resolved issue with data exchange with `ir` package.
  ([\#35](https://github.com/NRCan/PlotFTIR/issues/35))

## PlotFTIR 1.2.1

CRAN release: 2025-08-25

- Updates (mostly to testing) to comply with pending ggplot2 4.0.0
  release ([\#23](https://github.com/NRCan/PlotFTIR/issues/23))

## PlotFTIR 1.2.0

CRAN release: 2025-03-31

- Patch update to expose `check_ftir_data` for downstream packages.
  (related to [\#19](https://github.com/NRCan/PlotFTIR/issues/19))
- Add import capability for .jdx filetype using `readJDX`.
  ([\#20](https://github.com/NRCan/PlotFTIR/issues/20))

## PlotFTIR 1.1.0

CRAN release: 2025-02-05

- Added ability to set default language in
  [`options()`](https://rdrr.io/r/base/options.html).
  ([\#10](https://github.com/NRCan/PlotFTIR/issues/10))
- Added ability to highlight one or more samples in spectra.
  ([\#15](https://github.com/NRCan/PlotFTIR/issues/15))
- Added ability to add a background shaded band (like a wide marker) to
  indicate a given range.
  ([\#16](https://github.com/NRCan/PlotFTIR/issues/16))
- Added attributes to imported data to simplify internal calculation of
  thingslike y axis units.
  ([\#17](https://github.com/NRCan/PlotFTIR/issues/17))

## PlotFTIR 1.0.0

CRAN release: 2024-11-13

- Initial Release & CRAN Submission.
