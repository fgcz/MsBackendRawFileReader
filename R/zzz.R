#R


#' @importFrom utils packageVersion
.onAttach <- function(lib, pkg){
	if(interactive()){
		version <- packageVersion('MsBackendRawFileReader')
		packageStartupMessage("Package 'MsBackendRawFileReader' version ", version)
	  invisible()
	}
}
