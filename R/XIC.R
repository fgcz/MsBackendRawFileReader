#R

.vec2XIC <- function(x){
    df <- data.frame(times=x[seq(1, length(x), by=2)], intensities=x[seq(2, length(x), by=2)])
    df[df$intensities > 0,]
}

#' Extracts XICs of a given mass vector
#'
#' @param rawfile the file name 
#' @param masses a vector of masses 
#' @param tol tolerance in ppm
#'
#' @return list of XIC objects
#' @export readXICs 
#' @examples
#' # Example 1: extract iRT peptides
#' iRTpeptide <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
#' "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
#' "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
#' "LFLQFGAQGSPFLK")
#' 
#' library(protViz)
#' # 2Hplus 
#' (mZ <- (parentIonMass(iRTpeptide) + 1.008) / 2)
#' 
#' \dontrun{
#' rawfile <- "/home/cp/Downloads/20180220_14_autoQC01.raw"
#'  X <-readXICs(rawfile, masses=mZ)
#' }
#' 
#' 
#' (rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw'))
#' RAW <- read.raw(rawfile)
#' 
#' # not meaning full but proof-of-concept
#' X <-readXICs(rawfile, masses=unique(RAW$PrecursorMass), tol=1000)
#' plot(X)
#' 
readXICs <- function(rawfile,
                     masses = c(428.2738, 424.2667, 420.7581, 417.7512, 414.7443),
                     tol = 10, filter="ms"){
    
    x <- .cnew ("Rawfile", rawfile)
	
    if (! x$IsValidFilter(filter)){
	    message(paste(filter, "is not a valid filter"))
	    return (NULL)
    }

    
    S <- x$GetXIC(masses, tol, filter)
    n <- length(masses)

    S <- split(S, gl(n, length(S) / n))
    
    S <- lapply(S, .vec2XIC)
    
    S <- lapply(S,
                 function(x){
                     class(x) <- c(class(x), 'XIC');
                     x$filename <- basename(rawfile);
                     x})
    
    class(S) <- c(class(S), 'XICs')
    return(S)
}


