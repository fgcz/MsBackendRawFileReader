CSC=mcs
CSCFLAGS=-lib:../exec /r:ThermoFisher.CommonCore.Data.dll /r:ThermoFisher.CommonCore.MassPrecisionEstimator.dll /r:ThermoFisher.CommonCore.RawFileReader.dll /optimize /platform:anycpu

dll:
	$(CSC) /target:library MsBackendRawFileReader.cs  /out:../exec/MsBackendRawFileReader.dll  $(CSCFLAGS)
run: dll
	/usr/bin/mono --nollvm /home/cp/R/x86_64-pc-linux-gnu-library/3.6/rDotNet/server/bin/Debug/CLRServer.exe -url svc://localhost:56789/ -dll $(PWD)/../exec/MsBackendRawFileReader.dll

	 #/Library/Frameworks/Mono.framework/Commands/mono64 --llvm /Library/Frameworks/R.framework/Versions/3.6/Resources/library/rDotNet/server/bin/Debug/CLRServer.exe -url svc://localhost:56789/ -dll  ../exec/MsBackendRawFileReader.dll 
