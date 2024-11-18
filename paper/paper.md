---
title: 'PlotFTIR: Plotting FTIR Spectra'
tags:
  - R
  - spectroscopy
  - visualization
  - chemometrics
authors:
  - name: Philip Bulsink
    orcid: 0000-0001-9668-2429
    affiliation: 1
affiliations:
 - name: CanmetENERGY-Ottawa, Natural Resources Canada
   index: 1
date: 22 October 2024
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
or use advanced statistical tools such as PCA/PCR or chemometrics to deduce properties. Regardless of the analysis, 
scientists and researchers often need to publish FTIR spectra in journal articles. 

This package provides a convenient and reproducible workflow option for the production of graphics with minor spectra
manipulation capabilities.

# Statement of need

`PlotFTIR` is a package that takes input data and produces high-quality, journal-article ready graphics, *reproducibly* and
with high-quality defaults (\autoref{fig:example}). Scientists and researchers often rely on manual processes in software 
(such as Excel) to produce graphics for journals, but these can be cumbersome and slow, requiring manual changes as new data 
is produced, or difficult changes if the researcher needs to perform even simple data manipulation (such as shifting spectra 
or averaging results). Key components of producing high-quality figures are outlined by Rougier et. al. [@rougier:2014] and 
the core principle of `PlotFTIR` is to follow these 'simple rules'.

![Example FTIR spectra produced by `PlotFTIR`.\label{fig:example}](./paper-biodiesel.png)

`PlotFTIR` is designed with a repeatable, data- and plot-oriented workflow in mind, enabling reproducible workflows from 
loading data through saving graphical files. Users can easily run a short script on their data set to produce graphics, 
or use a few functions to visualize important elements during exploratory data analysis.

`PlotFTIR` provides simple manipulation functions, such as averaging of spectra, addition/subtraction of scalar values,
multiple (linear) baselining functions, and conversion between absorbance and transmittance units (both commonly used in 
FTIR studies, but each preferred by different scientific applications). More advanced spectra manipulation or chemometrics 
may be performed by other packages such as `ir` [@teickner:2022] or  `ChemoSpec` [@hanson:2024]. To facilitate collaboration 
between packages, `PlotFTIR` contains functions which can convert  data to or from the format specified by these other 
packages. This permits complex baseline, smoothing, or derivative calculations in `ir`, or detailed statistics such as PCA, 
clustering, or ANOVA in `ChemoSpec`, and finishing with pretty plotting in `PlotFTIR`, all in one workflow. 

`PlotFTIR` supports annotation of graphics with markers and labels, simple title and legend changes, sample renaming
functions, and output options. It also provides logical default graphical parameters, including titles, legends, 
axis labels, color palettes, and the inversion of the x-axis typical of IR spectral images. It does this in contrast to 
the other `R` packages for FTIR  which focus on spectra statistics or processing, but ignore the graphical requirements 
of publishing journal articles or reports (i.e. producing plots with normal x-axis orientation, default ggplot2 colour 
palettes and labels, no support for annotation beyond ggplot functions, etc.) [@teickner:2022; @hanson:2024]. 

While `PlotFTIR` produces graphics using `ggplot2` [@wickham:2016], it enables novice users to produce graphics easily 
without preventing advanced users from performing customization. Users need not know `ggplot2` functions to produce 
`PlotFTIR` graphics, instead, the graphical functions in `PlotFTIR` are designed to be used in a traditional step-wise 
or piped fashion where multiple graphical manipulations can be performed sequentially. However, should users more 
comfortable with `R` and `ggplot2` feel the need, the entire plot can be modified to their purposes with `ggplot2` 
manipulations.

The documentation for `PlotFTIR` is available in English and French (including having default French labels and titles
available for plotting by setting `lang = 'fr'` in the initial arguments for the plot). 


# Acknowledgements

The author would like to thank Ulrich Makanda for translation of documentation and feedback on the manuscript. 

The author declares no competing financial interest.

# References
