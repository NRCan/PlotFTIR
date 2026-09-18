# Package index

## Data Loading and Exchange

Functions for loading data or converting to and from other spectral R
packages

- [`read_ftir()`](https://nrcan.github.io/PlotFTIR/reference/read_ftir.md)
  : Read FTIR file
- [`read_ftir_directory()`](https://nrcan.github.io/PlotFTIR/reference/read_ftir_directory.md)
  : Read FTIR file
- [`read_raman()`](https://nrcan.github.io/PlotFTIR/reference/read_raman.md)
  : Read Raman file
- [`chemospec_to_plotftir()`](https://nrcan.github.io/PlotFTIR/reference/chemospec_to_plotftir.md)
  : \`ChemoSec\` to \`PlotFTIR\` conversions
- [`plotftir_to_chemospec()`](https://nrcan.github.io/PlotFTIR/reference/plotftir_to_chemospec.md)
  : Convert \`PlotFTIR\` data to \`ChemoSpec\` format
- [`plotftir_to_ir()`](https://nrcan.github.io/PlotFTIR/reference/plotftir_to_ir.md)
  : Convert \`PlotFTIR\` data to \`ir\`
- [`ir_to_plotftir()`](https://nrcan.github.io/PlotFTIR/reference/ir_to_plotftir.md)
  : Convert \`ir\` to \`PlotFTIR\` data format
- [`check_ftir_data()`](https://nrcan.github.io/PlotFTIR/reference/check_ftir_data.md)
  : Check FTIR Data
- [`print(`*`<PlotFTIR_data>`*`)`](https://nrcan.github.io/PlotFTIR/reference/print.PlotFTIR_data.md)
  : Print PlotFTIR Data
- [`biodiesel`](https://nrcan.github.io/PlotFTIR/reference/biodiesel.md)
  : FTIR Bio-diesel data
- [`sample_spectra`](https://nrcan.github.io/PlotFTIR/reference/sample_spectra.md)
  : FTIR example data

## Spectral Mathematical Changes

Functions for changing or modifying spectra numerically

- [`add_scalar_value()`](https://nrcan.github.io/PlotFTIR/reference/add_subtract_scalar.md)
  [`subtract_scalar_value()`](https://nrcan.github.io/PlotFTIR/reference/add_subtract_scalar.md)
  : Add or Subtract Scalar Value
- [`average_spectra()`](https://nrcan.github.io/PlotFTIR/reference/average_spectra.md)
  : Average FTIR Spectra
- [`absorbance_to_transmittance()`](https://nrcan.github.io/PlotFTIR/reference/conversion.md)
  [`transmittance_to_absorbance()`](https://nrcan.github.io/PlotFTIR/reference/conversion.md)
  : Convert Between Absorbance and Transmittance
- [`normalize_spectra()`](https://nrcan.github.io/PlotFTIR/reference/normalize_spectra.md)
  : Normalize FTIR spectra
- [`recalculate_baseline()`](https://nrcan.github.io/PlotFTIR/reference/recalculate_baseline.md)
  : Recalculate Baseline
- [`normalize_raman()`](https://nrcan.github.io/PlotFTIR/reference/normalize_raman.md)
  : Normalize Raman Intensity Spectra
- [`smooth_spectra()`](https://nrcan.github.io/PlotFTIR/reference/smooth_spectra.md)
  : Smooth Spectra Using Savitzky-Golay Filter
- [`baseline_correct()`](https://nrcan.github.io/PlotFTIR/reference/baseline_correct.md)
  : Correct Spectrum Baseline
- [`find_peak_maxima()`](https://nrcan.github.io/PlotFTIR/reference/find_peak_maxima.md)
  : Find Peak Maxima in Raman Spectra

## Plotting and Graphical Changes

Functions for plotting specta and annotating or manipulating plots

- [`plot_ftir()`](https://nrcan.github.io/PlotFTIR/reference/plot_ftir.md)
  : Plot FTIR Spectra Overlaid
- [`plot_ftir_stacked()`](https://nrcan.github.io/PlotFTIR/reference/plot_ftir_stacked.md)
  : Plot FTIR in stacked format
- [`plot_raman()`](https://nrcan.github.io/PlotFTIR/reference/plot_raman.md)
  : Plot Raman Spectra Overlaid
- [`plot_raman_stacked()`](https://nrcan.github.io/PlotFTIR/reference/plot_raman_stacked.md)
  : Plot Raman in stacked format
- [`add_band()`](https://nrcan.github.io/PlotFTIR/reference/add_band.md)
  : Add Band
- [`add_wavenumber_marker()`](https://nrcan.github.io/PlotFTIR/reference/add_wavenumber_marker.md)
  : Add a Marker at a Wavenumber
- [`compress_low_energy()`](https://nrcan.github.io/PlotFTIR/reference/compress_low_energy.md)
  : Compress Low-Energy Region
- [`highlight_sample()`](https://nrcan.github.io/PlotFTIR/reference/highlight_sample.md)
  : Highlight Sample Spectra
- [`move_plot_legend()`](https://nrcan.github.io/PlotFTIR/reference/move_plot_legend.md)
  : Move Plot Legend
- [`get_plot_sample_ids()`](https://nrcan.github.io/PlotFTIR/reference/get_plot_sample_ids.md)
  : Get Plot Sample IDs
- [`rename_plot_sample_ids()`](https://nrcan.github.io/PlotFTIR/reference/rename_plot_sample_ids.md)
  : Rename Sample IDs in Plot
- [`save_plot()`](https://nrcan.github.io/PlotFTIR/reference/save_plot.md)
  : Save FTIR Plot
- [`zoom_in_on_range()`](https://nrcan.github.io/PlotFTIR/reference/zoom_in_on_range.md)
  : Zoom in on a spectral range

## Peak Fitting

Functions for finding peaks and performing deconvolution, and plotting
the results

- [`find_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/find_ftir_peaks.md)
  : Find FTIR Peaks
- [`fit_peak_df()`](https://nrcan.github.io/PlotFTIR/reference/fit_peak_df.md)
  : Fitted Peaks Data.Frame
- [`fit_peaks()`](https://nrcan.github.io/PlotFTIR/reference/fit_peaks.md)
  : Fit Peaks
- [`plot_components()`](https://nrcan.github.io/PlotFTIR/reference/plot_components.md)
  : Plot Components
- [`plot_fit_ftir_peaks()`](https://nrcan.github.io/PlotFTIR/reference/plot_fit_ftir_peaks.md)
  : Plot Fitted Peaks
- [`plot_fit_residuals()`](https://nrcan.github.io/PlotFTIR/reference/plot_fit_residuals.md)
  : Plot Residuals
