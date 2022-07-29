#' load package with trying package install when it is not found.
#' @description Try load package by require. If the package does not exist, install.package or install_github is tried.
#' @param pck character of package name
#' @param location if location is "github.com/xxx", the package is installed from github.com
#' @return True: found and loaded, False: fail to load
#' @importFrom utils install.packages
#' @export
strong_require = function(pck, location=NULL){
	#try package load
	if(require(pck,character.only = TRUE))return(TRUE)

	#try install.packages
	if(is.null(location)){
		install.packages(pck)
	}else if(grep("^github.com/[^/]+$",location)){
		strong_require("devtools")
		pos = gsub("^github.com/([^/]+)$","\\1",location)
		devtools::install_github(paste0(pos,"/",pck))
	}

	return(require(pck,character.only = TRUE))
}

#' print log data with thread pid, time.
#' @description print log with thread pid, time.
#' @param str print message
#' @export
print_log = function(str){
	cat(sprintf("[%d] %s: %s\n",Sys.getpid(),as.POSIXlt(Sys.time(),"UTC"),str))
}
