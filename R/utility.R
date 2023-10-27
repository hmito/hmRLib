#' generate new environment with arglist.
#' @description generate new environment from parent with arglist which are assigned with those names.
#' @param env parent environment
#' @param arglist argument list; argument without name is ignored.
#' @return environment
#' @export
new_env = function(env=.GlobalEnv, arglist = NULL){
	this.env = new.env(parent = env)

	if(length(arglist)>0){
		namelist = names(arglist)
		for(i in 1:length(arglist)){
			if(namelist[i]=="")next

			assign(namelist[i],arglist[[i]],this.env)
		}
	}

	return(this.env)
}

#' read R code from a file, with changing directory.
#' @description Same with default source except: 1. local is TRUE in default, 2. chdir is TRUE in default.
#' @param file file name
#' @param local local environment; default is current environment
#' @param ... argument for source
#' @param chdir change dir
#' @export
source_r = function(file, local = TRUE, ..., chdir=TRUE){
	if(is.logical(local)){
		if(local){
			local = parent.frame()
		}else{
			local = .GlobalEnv
		}
	}else if(!is.environment(local)){
		stop("'local' must be TRUE, FALSE or an environment")
	}
	source(file,local=local,...,chdir=chdir)
}

#' read and run R script code from a file, with changing directory.
#' @description read and run R script code from a file, with changing directory.
#' @param file file name
#' @param args set Rscript arguments
#' @param local local environment; default is current environment
#' @param ... argument for source
#' @param chdir change dir
#' @return invisible evaluated environment
#' @export
source_rscript = function(file, args = NULL, local = TRUE, ..., chdir=TRUE){
	if(is.logical(local)){
		if(local){
			local = parent.frame()
		}else{
			local = .GlobalEnv
		}
	}else if(!is.environment(local)){
		stop("'local' must be TRUE, FALSE or an environment")
	}
	argenv = new_env(local,list(
		commandArgs = function(trailingOnly=FALSE){
			if(trailingOnly)return(args)
			else return(c(utils::head(base::commandArgs(FALSE),length(base::commandArgs(FALSE))-length(base::commandArgs(TRUE))),args))
		}
	))
	source(file,local=argenv,chdir=chdir)
	return(invisible(argenv))
}

#' reload all packages which have been loaded in any environment.
#' @description reload all packages which have been loaded in any environment.
#' @param env target environment
#' @export
reload_package = function(env = parent.env()){
	if(identical(env,.GlobalEnv))return()
	if(env|>environmentName() %in% search())return()

	while(!identical(env,emptyenv())){
		parent = parent.env(env)
		if(parent|>environmentName() %in% search()){
			if(identical(parent,.GlobalEnv))return()
			parent.env(env) = parent.env(.GlobalEnv)
			return()
		}
		env = parent
	}
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
				hmRLib::try_({
					out.file <- file(outpath, open = "a")
					writeLines(msg, out.file)
					close(out.file)
				}) |> hmRLib::catch(function(excp){
					hmRLib::throw(excp)
				})
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
				hmRLib::try_({
					out.file <- file(outpath, open = "a")
					writeLines(msg, out.file)
					close(out.file)
				}) |> hmRLib::catch(function(excp){
					hmRLib::throw(excp)
				})
			}
			if(!is.null(errpath)){
				hmRLib::try_({
					out.file <- file(errpath, open = "a")
					writeLines(msg, out.file)
					close(out.file)
				}) |> hmRLib::catch(function(excp){
					hmRLib::throw(excp)
				})
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
				hmRLib::try_({
					out.file <- file(outpath, open = "a")
					writeLines(msg, out.file)
					close(out.file)
				}) |> hmRLib::catch(function(excp){
					hmRLib::throw(excp)
				})
			}
			if(!is.null(errpath)){
				hmRLib::try_({
					out.file <- file(errpath, open = "a")
					writeLines(msg, out.file)
					close(out.file)
				}) |> hmRLib::catch(function(excp){
					hmRLib::throw(excp)
				})
			}
			hmRLib::throw(msg, IgnoreThrower=TRUE)
		}
	)
}
