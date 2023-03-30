[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![JPR](https://img.shields.io/badge/JPR-10.1021%2Facs.jproteome.0c00866-brightgreen)](http://dx.doi.org/10.1021/acs.jproteome.0c00866)

# MsBackendRawFileReader

Provides an alternative `MsBackend` to [Spectra](https://bioconductor.org/packages/Spectra) through the [rawrr](https://bioconductor.org/packages/rawrr/) package. Ultimately this backend will allow direct access to spectral data logged in ThermoFischer Scientific .raw files using accessor functions defined in the `Spectra` package (on-disk backend). 

Details of the backend concept are outlined in https://github.com/rformassspectrometry/Spectra/blob/master/man/MsBackend.Rd


## Install

Please follow the install instructions provided through 

https://bioconductor.org/packages/MsBackendRawFileReader/

## Goal

Integration of [rawDiag](https://github.com/fgcz/rawDiag) and [rawrr](https://bioconductor.org/packages/rawrr/) into the RforMassSpectrometry [Spectra](https://bioconductor.org/packages/Spectra) ecosystem.

<img src="https://github.com/fgcz/abrf/blob/main/MS-BioC-arch.jpg" alt="R4MS arch" width="400"/>

see also https://github.com/Bioconductor/Contributions/issues/1886

