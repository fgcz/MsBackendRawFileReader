#R


#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
	if(interactive()){
		version <- packageVersion('MsBackendRawFileReader')
		packageStartupMessage("Package 'MsBackendRawFileReader' version ", version)
	
		.cinit(dll=file.path(path.package(package = "MsBackendRawFileReader"),
		                     "exec", "MsBackendRawFileReader.dll"))
	  invisible()
	}
}
