#' load package with trying package install when it is not found.
#' @description Try load package by require. If the package does not exist, install.package or install_github is tried.
#' @param pck character of package name
#' @param location if location is "github.com/xxx", the package is installed from github.com
#' @return True: found and loaded, False: fail to load
#' @importFrom utils install.packages
#' @importFrom devtools install_github
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

#' create output function list.
#' @description crate list of functions including print, warning and error, each of which accept format style like sprintf.
#' @param outpath Optional; output file path which log all of output.
#' @param errpath Optional; error file path which log only warning and error output.
#' @param head_format header format of each row. Default is \%Y-\%m-\%d \%H:\%M:\%S.
#' @param use_utc Logical; if TRUE, the head format is filled by UTC and head end with 'Z'.
#' @param use_pid Logical; if TRUE, the pid is added as '@@xxxxx'.
#' @return print, warning and error functions.
#' @export
formatted_output = function(outpath=NULL,errpath=NULL,head_format="%Y-%m-%d %H:%M:%S",use_utc=FALSE, use_pid=FALSE){
	list(
		print = function(str, ..., silent = FALSE){
			if(use_utc){
				head = format(as.POSIXlt(Sys.time(),"UTC"),paste0(head_format,"Z"))
			}else{
				head = format(Sys.time(),head_format)
			}
			if(use_pid){
				head = sprintf("@%05d %s",Sys.getpid(),head)
			}
			if(length(list(...))==0){
				msg = paste0(paste0(head," ",str),collapse="\n")
			}else{
				msg = paste0(paste0(head," ",sprintf(str,...)),collapse="\n")
			}
			if(!is.null(outpath)){
				out.file <- file(outpath, open = "a")
				writeLines(msg, out.file)
				close(out.file)
			}
			if(!silent){
				cat(paste0(msg,"\n"))
			}
		},
		warning = function(str, ..., silent = FALSE){
			if(use_utc){
				head = format(as.POSIXlt(Sys.time(),"UTC"),paste0(head_format,"Z"))
			}else{
				head = format(Sys.time(),head_format)
			}
			if(use_pid){
				head = sprintf("@%05d %s",Sys.getpid(),head)
			}
			if(length(list(...))==0){
				msg = paste0(paste0(head,"<W> ",str),collapse="\n")
			}else{
				msg = paste0(paste0(head,"<W> ",sprintf(str,...)),collapse="\n")
			}
			if(!is.null(outpath)){
				out.file <- file(outpath, open = "a")
				writeLines(msg, out.file)
				close(out.file)
			}
			if(!is.null(errpath)){
				out.file <- file(errpath, open = "a")
				writeLines(msg, out.file)
				close(out.file)
			}
			if(!silent){
				base::warning(msg,call.=FALSE,immediate. = TRUE)
			}
		},
		error = function(str,...){
			if(use_utc){
				head = format(as.POSIXlt(Sys.time(),"UTC"),paste0(head_format,"Z"))
			}else{
				head = format(Sys.time(),head_format)
			}
			if(use_pid){
				head = sprintf("@%05d %s",Sys.getpid(),head)
			}
			if(length(list(...))==0){
				msg = paste0(paste0(head,"<E> ",str),collapse="\n")
			}else{
				msg = paste0(paste0(head,"<E> ",sprintf(str,...)),collapse="\n")
			}
			if(!is.null(outpath)){
				out.file <- file(outpath, open = "a")
				writeLines(msg, out.file)
				close(out.file)
			}
			if(!is.null(errpath)){
				out.file <- file(errpath, open = "a")
				writeLines(msg, out.file)
				close(out.file)
			}
			hmRLib::throw(msg, IgnoreThrower=TRUE)
		}
	)
}
