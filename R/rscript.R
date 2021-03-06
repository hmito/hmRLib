#' Run as Rscript with dragged files
#' @description Run given function as Rscript with drag&drop files.
#' @param fn Funtion whose first argument is a drag&drop file
#' @param ... Arguments are passed to function f.
#' @export
run_as_filedrop_script = function(fn,...){
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


#' Execute given Rscript file with arguments.
#' @description Execute given Rscript file with arguments.
#' @param rfile Path of Rscript file
#' @param ... optional arguments for rscript file.
#' @export
run_rscript = function(rfile, ...){
	cmd = os_path(rfile)
	cmd = paste("rscript",cmd)
	argc = length(list(...))

	if(argc!=0){
		for(i in 1:argc){
			cmd = paste(cmd, os_path(list(...)[[i]]))
		}
	}

	return(system(cmd))
}


#' Execute given Rscript file in parallele with arguments.
#' @description Execute given Rscript file in parallele with arguments.
#' @param rfile Path of Rscript file
#' @param ... optional arguments for rscript file.
#' @export
start_rscript = function(rfile, ...){
	cmd = os_path(rfile)
	cmd = paste("rscript",cmd)
	argc = length(list(...))

	if(argc!=0){
		for(i in 1:argc){
			cmd = paste(cmd, os_path(list(...)[[i]]))
		}
	}

	system(cmd, wait=FALSE, invisible=FALSE)
	return(0)
}

#' Execute ownself in parallele with arguments.
#' @description Run ownself as Rscript in parallele with arguments. In parallele started files, this function does not start new process.
#' @param ownpath Own file path
#' @param ... optional arguments for rscript file.
#' @export
start_ownself = function(ownpath, ...){
	if(!any(commandArgs(trailingOnly=TRUE)=="#parallelrun_slave")){
		start_rscript(ownpath, ..., "#parallelrun_slave")
		return(TRUE)
	}
	return(FALSE)
}


