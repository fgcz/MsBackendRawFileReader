# MsBackendRawFileReader

Provides an alternative `MsBackend` to [Spectra](https://bioconductor.org/packages/Spectra) through the [rawrr](https://bioconductor.org/packages/rawrr/) package. Ultimately this backend will allow direct access to spectral data logged in ThermoFischer Scientific .raw files using accessor functions defined in the `Spectra` package (on-disk backend). 

Details of the backend concept are outlined in https://github.com/rformassspectrometry/Spectra/blob/master/man/MsBackend.Rd

Formally, our MsBackendRawFileReader will extend extend the base \code{MsBackend} class and
implement the following methods:

...

## Goal

Integration of [rawDiag](https://github.com/fgcz/rawDiag) and [rawrr](https://bioconductor.org/packages/rawrr/) into the RforMassSpectrometry [Spectra](https://bioconductor.org/packages/Spectra) ecosystem.

<img src="https://github.com/fgcz/abrf/blob/main/MS-BioC-arch.jpg" alt="R4MS arch" width="400"/>

see also https://github.com/Bioconductor/Contributions/issues/1886


## Install
tbd
