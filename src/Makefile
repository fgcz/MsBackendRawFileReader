CSC=mcs
CSCFLAGS=-lib:../exec /r:ThermoFisher.CommonCore.Data.dll /r:ThermoFisher.CommonCore.MassPrecisionEstimator.dll /r:ThermoFisher.CommonCore.RawFileReader.dll /optimize /platform:anycpu

dll:
	$(CSC) /target:library MsBackendRawFileReader.cs  /out:../exec/MsBackendRawFileReader.dll  $(CSCFLAGS)
