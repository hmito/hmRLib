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

#' Check if the program run on the Linux-base or windows from path system.
#' @description This function return TRUE when the program run on the linux environment.
#' @return Ligcal values: TRUE on Linux or Mac, FALSE on Windows.
#' @export
is_on_linux = function(){
	return(substr(getwd(),1,1)=="/")
}

#' Return the raw file path based on the os path system.
#' @param path Path for path translation
#' @return path with replace the path separater if it is required.
#' @export
os_path = function(path){
	path = ifelse(is_on_linux()&is.character(path),path,gsub("/","\\\\",path))
	return(path)
}

#' Execute given exe file with arguments.
#' @description Execute given exe file with arguments.
#' @param exe Path of exe file.
#' @param ... optional arguments for exe file.
#' @export
execute = function(exe, ...){
	cmd = os_path(exe)
	argc = length(list(...))

	if(argc!=0){
		for(i in 1:argc){
			cmd = paste(cmd, os_path(list(...)[[i]]))
		}
	}
	system(cmd)
}
