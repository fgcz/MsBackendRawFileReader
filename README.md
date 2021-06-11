# MsBackendRawFileReader

Provides an alternative `MsBackend` to [Spectra](https://bioconductor.org/packages/Spectra) through the [rawrr](https://bioconductor.org/packages/rawrr/) package. Ultimately this backend will allow direct access to spectral data logged in ThermoFischer Scientific .raw files using accessor functions defined in the `Spectra` package (on-disk backend). 

Details of the backend concept are outlined in https://github.com/rformassspectrometry/Spectra/blob/master/man/MsBackend.Rd

Formally, our MsBackendRawFileReader will extend extend the base `MsBackend` class and
implement the following methods:

```
[: subset the backend. Only subsetting by element (row/i) is allowed

$, $<-: access or set/add a single spectrum variable (column) in the backend.

[[, [[<-: access or set/add a single spectrum variable (column) in the backend. The default implementation uses $, thus these methods don't have to be implemented for new classes extending MsBackend.

acquisitionNum: returns the acquisition number of each spectrum. Returns an integer of length equal to the number of spectra (with NA_integer_ if not available).

peaksData returns a list with the spectras' peak data. The length of the list is equal to the number of spectra in object. Each element of the list is a matrix with columns "mz" and "intensity". For an empty spectrum, a matrix with 0 rows and two columns (named mz and intensity) is returned.

backendInitialize: initialises the backend. This method is supposed to be called rights after creating an instance of the backend class and should prepare the backend (e.g. set the data for the memory backend or read the spectra header data for the MsBackendMzR backend). This method has to ensure to set the spectra variable dataStorage correctly.

backendMerge: merges (combines) MsBackend objects into a single instance. All objects to be merged have to be of the same type (e.g. MsBackendDataFrame()).

dataOrigin: gets a character of length equal to the number of spectra in object with the data origin of each spectrum. This could e.g. be the mzML file from which the data was read.

dataStorage: gets a character of length equal to the number of spectra in object with the data storage of each spectrum. Note that a dataStorage of NA_character_ is not supported.

dropNaSpectraVariables: removes spectra variables (i.e. columns in the object's spectraData that contain only missing values (NA). Note that while columns with only NAs are removed, a spectraData call after dropNaSpectraVariables might still show columns containing NA values for core spectra variables.

centroided, centroided<-: gets or sets the centroiding information of the spectra. centroided returns a logical vector of length equal to the number of spectra with TRUE if a spectrum is centroided, FALSE if it is in profile mode and NA if it is undefined. See also isCentroided for estimating from the spectrum data whether the spectrum is centroided. value for centroided<- is either a single logical or a logical of length equal to the number of spectra in object.

collisionEnergy, collisionEnergy<-: gets or sets the collision energy for all spectra in object. collisionEnergy returns a numeric with length equal to the number of spectra (NA_real_ if not present/defined), collisionEnergy<- takes a numeric of length equal to the number of spectra in object.

export: exports data from a Spectra class to a file. This method is called by the export,Spectra method that passes itself as a second argument to the function. The export,MsBackend implementation is thus expected to take a Spectra class as second argument from which all data is exported. Taking data from a Spectra class ensures that also all eventual data manipulations (cached in the Spectra's lazy evaluation queue) are applied prior to export - this would not be possible with only a MsBackend class. An example implementation is the export method for the MsBackendMzR backend that supports export of the data in mzML or mzXML format. See the documentation for the MsBackendMzR class below for more information.

filterAcquisitionNum: filters the object keeping only spectra matching the provided acquisition numbers (argument n). If dataOrigin or dataStorage is also provided, object is subsetted to the spectra with an acquisition number equal to n in spectra with matching dataOrigin or dataStorage values retaining all other spectra.

filterDataOrigin: filters the object retaining spectra matching the provided dataOrigin. Parameter dataOrigin has to be of type character and needs to match exactly the data origin value of the spectra to subset. filterDataOrigin should return the data ordered by the provided dataOrigin parameter, i.e. if dataOrigin = c("2", "1") was provided, the spectra in the resulting object should be ordered accordingly (first spectra from data origin "2" and then from "1"). Implementation of this method is optional since a default implementation for MsBackend is available.

filterDataStorage: filters the object retaining spectra matching the provided dataStorage. Parameter dataStorage has to be of type character and needs to match exactly the data storage value of the spectra to subset. filterDataStorage should return the data ordered by the provided dataStorage parameter, i.e. if dataStorage = c("2", "1") was provided, the spectra in the resulting object should be ordered accordingly (first spectra from data storage "2" and then from "1"). Implementation of this method is optional since a default implementation for MsBackend is available.

filterEmptySpectra: removes empty spectra (i.e. spectra without peaks). Implementation of this method is optional since a default implementation for MsBackend is available.

filterFile: retains data of files matching the file index or file name provided with parameter file.

filterIsolationWindow: retains spectra that contain mz in their isolation window m/z range (i.e. with an isolationWindowLowerMz <= mz and isolationWindowUpperMz >= mz. Implementation of this method is optional since a default implementation for MsBackend is available.

filterMsLevel: retains spectra of MS level msLevel. Implementation of this method is optional since a default implementation for MsBackend is available.

filterPolarity: retains spectra of polarity polarity. Implementation of this method is optional since a default implementation for MsBackend is available.

filterPrecursorMz: retains spectra with a precursor m/z within the provided m/z range. Implementation of this method is optional since a default implementation for MsBackend is available.

filterPrecursorCharge: retains spectra with the defined precursor charge(s). Implementation of this method is optional since a default implementation for MsBackend is available.

filterPrecursorScan: retains parent (e.g. MS1) and children scans (e.g. MS2) of acquisition number acquisitionNum. Implementation of this method is optional since a default implementation for MsBackend is available.

filterRt: retains spectra of MS level msLevel with retention times within (>=) rt[1] and (<=) rt[2]. Implementation of this method is optional since a default implementation for MsBackend is available.

intensity: gets the intensity values from the spectra. Returns a NumericList() of numeric vectors (intensity values for each spectrum). The length of the list is equal to the number of spectra in object.

intensity<-: replaces the intensity values. value has to be a list (or NumericList()) of length equal to the number of spectra and the number of values within each list element identical to the number of peaks in each spectrum (i.e. the lengths(x)). Note that just writeable backends support this method.

ionCount: returns a numeric with the sum of intensities for each spectrum. If the spectrum is empty (see isEmpty), NA_real_ is returned.

isCentroided: a heuristic approach assessing if the spectra in object are in profile or centroided mode. The function takes the qtl th quantile top peaks, then calculates the difference between adjacent m/z value and returns TRUE if the first quartile is greater than k. (See Spectra:::.isCentroided for the code.)

isEmpty: checks whether a spectrum in object is empty (i.e. does not contain any peaks). Returns a logical vector of length equal number of spectra.

isolationWindowLowerMz, isolationWindowLowerMz<-: gets or sets the lower m/z boundary of the isolation window.

isolationWindowTargetMz, isolationWindowTargetMz<-: gets or sets the target m/z of the isolation window.

isolationWindowUpperMz, isolationWindowUpperMz<-: gets or sets the upper m/z boundary of the isolation window.

isReadOnly: returns a logical(1) whether the backend is read only or does allow also to write/update data.

length: returns the number of spectra in the object.

lengths: gets the number of peaks (m/z-intensity values) per spectrum. Returns an integer vector (length equal to the number of spectra). For empty spectra, 0 is returned.

msLevel: gets the spectra's MS level. Returns an integer vector (of length equal to the number of spectra) with the MS level for each spectrum (or NA_integer_ if not available).

mz: gets the mass-to-charge ratios (m/z) from the spectra. Returns a NumericList() or length equal to the number of spectra, each element a numeric vector with the m/z values of one spectrum.

mz<-: replaces the m/z values. value has to be a list of length equal to the number of spectra and the number of values within each list element identical to the number of peaks in each spectrum (i.e. the lengths(x)). Note that just writeable backends support this method.

polarity, polarity<-: gets or sets the polarity for each spectrum. polarity returns an integer vector (length equal to the number of spectra), with 0 and 1 representing negative and positive polarities, respectively. polarity<- expects an integer vector of length 1 or equal to the number of spectra.

precursorCharge, precursorIntensity, precursorMz, precScanNum, precAcquisitionNum: get the charge (integer), intensity (numeric), m/z (numeric), scan index (integer) and acquisition number (interger) of the precursor for MS level 2 and above spectra from the object. Returns a vector of length equal to the number of spectra in object. NA are reported for MS1 spectra of if no precursor information is available.

peaksData<- replaces the peak data (m/z and intensity values) of the backend. This method expects a list of matrix objects with columns "mz" and "intensity" that has the same length as the number of spectra in the backend. Note that just writeable backends support this method.

reset a backend (if supported). This method will be called on the backend by the reset,Spectra method that is supposed to restore the data to its original state (see reset,Spectra for more details). The function returns the reset backend. The default implementation for MsBackend returns the backend as-is.

rtime, rtime<-: gets or sets the retention times for each spectrum (in seconds). rtime returns a numeric vector (length equal to the number of spectra) with the retention time for each spectrum. rtime<- expects a numeric vector with length equal to the number of spectra.

scanIndex: returns an integer vector with the scan index for each spectrum. This represents the relative index of the spectrum within each file. Note that this can be different to the acquisitionNum of the spectrum which is the index of the spectrum as reported in the mzML file.

selectSpectraVariables: reduces the information within the backend to the selected spectra variables.

smoothed,smoothed<-: gets or sets whether a spectrum is smoothed. smoothed returns a logical vector of length equal to the number of spectra.  smoothed<- takes a logical vector of length 1 or equal to the number of spectra in object.

spectraData, spectraData<-: gets or sets general spectrum metadata (annotation, also called header). spectraData returns a DataFrame,  spectraData<- expects a DataFrame with the same number of rows as there are spectra in object. Note that spectraData has to return the full data, i.e. also the m/z and intensity values (as a list or SimpleList in columns "mz" and "intensity".

spectraNames: returns a character vector with the names of the spectra in object.

spectraVariables: returns a character vector with the available spectra variables (columns, fields or attributes) available in object. This should return all spectra variables which are present in object, also "mz" and "intensity" (which are by default not returned by the  spectraVariables,Spectra method).

split: splits the backend into a list of backends (depending on parameter f). The default method for MsBackend uses split.default(), thus backends extending MsBackend don't necessarily need to implement this method.

tic: gets the total ion current/count (sum of signal of a spectrum) for all spectra in object. By default, the value reported in the original raw data file is returned. For an empty spectrum, NA_real_ is returned.
```

## Goal

Integration of [rawDiag](https://github.com/fgcz/rawDiag) and [rawrr](https://bioconductor.org/packages/rawrr/) into the RforMassSpectrometry [Spectra](https://bioconductor.org/packages/Spectra) ecosystem.

<img src="https://github.com/fgcz/abrf/blob/main/MS-BioC-arch.jpg" alt="R4MS arch" width="400"/>

see also https://github.com/Bioconductor/Contributions/issues/1886


## Install
tbd
