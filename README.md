# Spectra MsBackend for the New RawFileReader from Thermo Fisher Scientific 

## [FGCZ project p3181](https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-project.html?id=3181) application

The [Bioconductor project (bioc)](https://doi.org/10.1038/nmeth.3252)
has been extremely successful in creating data
analysis frameworks for high-throughput genomic analysis.
Yet, similar
frameworks for proteomics data do not exist. The R for Mass Spectrometry
Initiative https://www.rformassspectrometry.org/ now aims to provide
efficient, thoroughly documented, tested
and flexible R software for the analysis and interpretation of high throughput
mass spectrometry assays, including proteomics and metabolomics experiments.
The resulting software will be committed to Bioconductor, once a satisfactory
level of maturity has been reached.
We believe that such a community-driven framework is crucial for the field of
proteomics and will hopefully foster the integration of genomics and
proteomics data in the near future. The success of this framework will be
closely linked to providing direct support for proprietary vendor formats like
the Thermo RAW format. Given our experience gained by developing the R package
[rawDiag](https://doi.org/10.1021/acs.jproteome.8b00173),
we would like to contribute an alternative MSBackend to
the R for Mass Spectrometry Initiative that allows direct access to the content
of Thermo RAW files. In addition, we plan to apply the framework in our package
rawDiag to make it fully compatible with bioc standards.


