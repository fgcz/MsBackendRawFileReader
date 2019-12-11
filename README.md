# Spectra MsBackend for the New RawFileReader from Thermo Fisher Scientific 

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



## Install

a Windows/Linux/MacOSX x64 platform 


###  .NET Framework and R

- https://www.mono-project.com/ (>4.0.22) for (Linux and MacOSX)
- .NET Framework 4.5.1 or higher (Windows)
- R (>= 3.6.0)
- install https://CRAN.R-project.org/package=devtools
- if you want support for [Open File Standards](http://www.psidev.info/) install the [mzR](http://bioconductor.org/packages/mzR/) package. 
- U.S. language setting on windows/linux/apple [see issue #33](https://github.com/fgcz/rawDiag/issues/33)

### R packages


```{r}
BiocManager::install('cpanse/MsBackendRawFileReader')
```
or 

```{r}
install.packages('rDotNet')

library(devtools)
devtools::install_github("rformassspectrometry/Spectra")

# git clone git@github.com:cpanse/MsBackendRawFileReader.git && R CMD build MsBackendRawFileReader --no-build-vignettes && R CMD INSTALL MsBackendRawFileReader_0.0.1.tar.gz 
devtools::install_github("cpanse/MsBackendRawFileReader",
  args="--no-build-vignettes")
```

## `sample.(raw|mzXML)`

### raw file summary
```
The RAW file has data from 1 instruments
General File Information:
   RAW file: sample.raw
   RAW file version: 66
   Creation date: 2/13/2018 11:09:03 AM
   Operator: ExactiveUser
   Number of instruments: 1
   Description: 
   Instrument model: Q Exactive HF-X Orbitrap
   Instrument name: Q Exactive HF-X Orbitrap
   Serial number: Exactive Series slot #6114
   Software version: 2.9-290033/2.9.0.2926
   Firmware version: rev. 1
   Units: None
   Mass resolution: 0.500 
   Number of scans: 574
   Number of ms2 scans: 546
   Scan range: 1 - 574
   Time range: 0.00 - 0.78
   Mass range: 140.0000 - 1805.0000

Sample Information:
   Sample name: 
   Sample id: 
   Sample type: Unknown
   Sample comment: 
   Sample vial: 
   Sample volume: 0
   Sample injection volume: 0
   Sample row number: 1
   Sample dilution factor: 0

Filter Information:
   Scan filter (first scan): FTMS + c NSI Full ms [350.0000-1800.0000]
   Scan filter (last scan): FTMS + c NSI Full ms2 487.2567@hcd28.00 [140.0000-1015.0000]
   Total number of filters: 21
```

## MD5 checksums
```
8965671c678e02bfa6c997055cb58fe2  sample.mzXML
fe67058456c79af7442316c474d20e96  sample.raw

8f03d21ea9f98b6986bab2bc2af00cb2  ThermoFisher.CommonCore.BackgroundSubtraction.dll
ee4a4904ecd833fdd58bfc0a4a7b5a4d  ThermoFisher.CommonCore.Data.dll
e31c4d00c19a487f26f96ac63f67862c  ThermoFisher.CommonCore.MassPrecisionEstimator.dll
89750f35acb1b8ea111bf2633c8abf1a  ThermoFisher.CommonCore.RawFileReader.dll
```
