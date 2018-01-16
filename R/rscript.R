#' Run as Rscript with dragged files
#' @description Run given function as Rscript with dragged files.
#' @param fn Funtion whose first argument is a dragged file
#' @param ... Arguments are passed to function f.
#' @export
run_as_rscript = function(fn,...){
	for(file in commandArgs(trailingOnly=TRUE)){
		fn(file, ...)
	}
}

#' Read package after install if it is required
#' @decription Read package from CRAN like "library" function, but if it is not found in local try to install it.
#' @param Name Name of the package
#' @export
try_library = function(Names){
	for(Name in Names){
		if(!require(Name)){
			install.packages(Name)
			if(!require(Name)){
				stop(paste("fail to install package\"",Name,"\""))
			}
		}
	}
}
