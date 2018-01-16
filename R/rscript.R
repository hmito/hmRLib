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

