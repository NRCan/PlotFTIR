---
title: 'Gala: A Python package for galactic dynamics'
tags:
  - R
  - Spectroscopy
  - Visualization
authors:
  - name: Philip Bulsink
    orcid: 0000-0001-9668-2429
    affiliation: 1
  - name: Ulrich Makanda
    affiliation: 1
affiliations:
 - name: CanmetENERGY-Ottawa, Natural Resources Canada
   index: 1
date: `r format(Sys.Date(), "%e %B %Y")`
bibliography: paper.bib
---

# Summary

Chemicals can be identified by the characteristics of their internal (inter-atomic) bonds. Determining the properties 
of these bonds is commonly performed by infra-red analysis, where the bonds interact with light of specific energies. 
When infra-red (lower energy) light is shone on a chemical sample, the different bonds absorb more or less light depending
on the frequency (wavelength) of the light. Bonds between different atoms, and in different materials have characteristic
absorbance values and can be used to confirm the identity of a chemical or the concentration of a component in a mixture.

These infra-red absorbance measurements are commonly performed on Fourier Transform Infra-red (FTIR) instruments. Scientists
and researchers may compare different FTIR spectra to observe changes in concentration of components, or changes in the
bond properties in a sample after a manipulation in the laboratory. These comparisons may be simple graphical comparisons, 
or advanced statistical comparisons such as PCA/PCR or chemometrics to deduce properties. Regardless of the analysis, 
scientists and researchers often need to publish FTIR spectra in journal articles. 

This package provides a convenient and reproducible workflow option for the production of graphics with minor spectra
manipulation capabilities. This package works on raw data from 

# Statement of need

`PlotFTIR` is a package that takes input data and produces high-quality, journal-article ready graphics, *reproducibly*\autoref{fig:example}. 
Scientists often rely on manual processes in software (such as Excel) to produce graphics for journals, but these can be 
cumbersome and slow, requiring manual changes as new data is produced, or difficult manipulations if the researcher needs 
to perform even simple manipulations (such as shifting spectra or averaging results).

![Example FTIR spectra produced by `PlotFTIR`.\label{fig:example}](man/figures/paper-biodiesel.png)

`PlotFTIR` is designed with a repeatable, object-oriented workflow in mind, enabling reproducible workflows from loading 
data through saving graphical files. Users can easily run a short script on their data set to produce graphics, or use 
a few functions to visualize important elements during exploratory data analysis. 

`PlotFTIR` provides simple manipulation functions, such as averaging of spectra, addition/subtraction of scalar values,
multiple linear baselining functions, and conversion between absorbance and transmittance units (both commonly used in 
FTIR studies, but each preferred by different scientific applications).

`PlotFTIR` supports annotation of graphics with markers and labels, simple title and legend changes, sample renaming
functions, and output options. It also determines logical default graphical parameters, including titles, legends, 
axis labels, color palettes, and the inversion of the x-axis. It does this in contrast to other `R` packages for FTIR 
which focus on spectra statistics or processing, but ignore the graphical requirements of publishing journal articles 
or reports (i.e. normal x-axis, default ggplot2 colour palettes and labels, no support for annotation beyond ggplot 
functions, etc.) `[@teickner2022;hanson2024]`. 
Since `PlotFTIR` produces graphics using `ggplot2`, it enables novice users to produce graphics easily without preventing 
advanced users from performing customization using ggplot2 or other add-on packages `[@wickham:2016]`. 

`PlotFTIR`

# Acknowledgements

The authors declare no competing financial interest.

# References

