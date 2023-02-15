#' Run as Rscript with dragged files
#' @description Run given function as Rscript with drag&drop files.
#' @param fn Function whose first argument is a drag&drop file
#' @param ... Arguments are passed to function f.
#' @export
run_as_filedrop_script = function(fn,...){
	for(file in commandArgs(trailingOnly=TRUE)){
		fn(file, ...)
	}
}

#' Get OS name.
#' @description Get OS name as sysname,
#' @return System name; Windows=Windows, Linux=Linux, Mac=Darwin
#' @export
get_os_name = function(){
	return(Sys.info()[["sysname"]])
}

#' Check if the program run on the Linux-base or windows from path system.
#' @description This function return TRUE when the program run on the linux environment.
#' @return Logical values: TRUE on Linux or Mac, FALSE on Windows.
#' @export
is_on_linux = function(){
	return(get_os_name() != "Windows")
#	return(substr(getwd(),1,1)=="/")
}

#' Return the raw file path based on the OS path system.
#' @param path Path for path translation
#' @return path with replace the path separator if it is required.
#' @export
as_os_style_path = function(path){
	path = ifelse(is_on_linux()&is.character(path),path,gsub("/","\\\\",path))
	return(path)
}

#' Return home path.
#' @return home path.
#' @export
home_path = function(){
	if(is_on_linux()) return("~")
	else return(paste0(Sys.getenv("homedrive"),Sys.getenv("homepath")))
}

#' Execute given exe file with arguments.
#' @description Execute given exe file with arguments.
#' @param exe Path of exe file.
#' @param ... optional arguments for exe file.
#' @export
execute = function(exe, ...){
	cmd = as_os_style_path(exe)
	argc = length(list(...))

	if(argc!=0){
		for(i in 1:argc){
			cmd = paste(cmd, as_os_style_path(list(...)[[i]]))
		}
	}

	system(cmd)
}

#' Execute given Rscript file with arguments.
#' @description Execute given Rscript file with arguments.
#' @param rfile Path of Rscript file
#' @param ... optional arguments for rscript file.
#' @export
run_rscript = function(rfile, ...){
	cmd = as_os_style_path(rfile)
	cmd = paste("rscript",cmd)
	argc = length(list(...))

	if(argc!=0){
		for(i in 1:argc){
			cmd = paste(cmd, as_os_style_path(list(...)[[i]]))
		}
	}

	return(system(cmd))
}


#' Execute given Rscript file in parallel with arguments.
#' @description Execute given Rscript file in parallel with arguments.
#' @param rfile Path of Rscript file
#' @param ... optional arguments for Rscript file.
#' @export
start_rscript = function(rfile, ...){
	cmd = as_os_style_path(rfile)
	cmd = paste("rscript",cmd)
	argc = length(list(...))

	if(argc!=0){
		for(i in 1:argc){
			cmd = paste(cmd, as_os_style_path(list(...)[[i]]))
		}
	}

	system(cmd, wait=FALSE, invisible=FALSE)
	return(0)
}

#' Execute ownself in parallel with arguments.
#' @description Run ownself as Rscript in parallel with arguments. In parallel started files, this function does not start new process.
#' @param ownpath Own file path
#' @param ... optional arguments for Rscript file.
#' @export
start_ownself = function(ownpath, ...){
	if(!any(commandArgs(trailingOnly=TRUE)=="#hmRLib::start_ownself::slave")){
		start_rscript(ownpath, ..., "#hmRLib::start_ownself::slave")
		return(TRUE)
	}
	return(FALSE)
}


