#' Get file name without extension
#' @description Get file name without extension from file path.
#' @param filepath file path (character)
#' @return file name of given file path.
#' @importFrom stringr str_remove
#' @export
file.name = function(filepath){
	str_remove(file.remove_dir(filepath),"\\.[^\\.]+$")
}

#' Get file extension
#' @description Get file extension from file path.
#' @param filepath file path (character)
#' @return extension of given file path.
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @export
file.ext = function(filepath){
	ifelse(stringr::str_detect(filepath,".+\\.([^/\\\\\\.]+)$"),
			 stringr::str_replace(filepath,".+\\.([^/\\\\\\.]+)$","\\1"),"")
}

#' Get dir and name without extension
#' @description Remove ext part from the path
#' @param filepath file path (character)
#' @return extension of given file path.
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @export
file.remove_ext = function(filepath){
	stringr::str_remove(filepath,"\\.([^/\\\\\\.]+)$")
}

#' Replace without extension
#' @description Remove ext part from the path
#' @param filepath file path (character)
#' @param ext new extension
#' @return extension of given file path.
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @export
file.replace_ext = function(filepath,ext){
	stringr::str_c(file.remove_ext(filepath),".",ext)
}

#' Get directory of file
#' @description Get directory of file from file path.
#' @param filepath file path (character)
#' @return directory of file of given file path.
#' @importFrom stringr str_remove
#' @export
file.dir = function(filepath){
	ifelse(stringr::str_detect(filepath,"[/\\\\]"),
			 str_remove(filepath,"[/\\\\]+[^/\\\\]+$"),".")
}

#' Get file name with extension
#' @description Get file name with extension from file path.
#' @param filepath file path (character)
#' @return file name with extension of given file path.
#' @importFrom stringr str_remove
#' @export
file.remove_dir= function(filepath){
	str_remove(filepath,"^.+[/\\\\]+")
}

#' Check whether the given path is absolute path or not.
#' @description Return logical value which represents whether the given path is absolute path or not.
#' @param filepath file path (character)
#' @return logical: true if it is absolute path.
#' @export
file.is_abspath = function(filepath){
		return(stringr::str_detect(filepath,"(:|^)/{1,2}"))
}

file.path_depth = function(path){

}
#' Return standardized path
#' @description Return standardized path from rootpath. head/tail spaces, current directory
#' @param path target file/dir path (character)
#' @param rootpath root file/dir path of the target path. if path is absolute path, rootpath is ignored
#' @param strict logical: TRUE ~ remove unnecessary reference "..", e.g., "a/b/../c".
#' @param stdroot logical: TRUE ~ skip rootpath standarization
#' @return standardized path
#' @export
file.stdpath = function(path,rootpath=NA_character_,strict=TRUE,stdroot=FALSE){
	if(!stdroot){
		rootpath = file.stdpath(rootpath,strict=strict,stdroot=TRUE)
		#path = paste0(file.stdpath(rootpath),"/",path)
	}
	rootpath = rootpath |>
		stringr::str_replace("([^/])$","\\1/")

	path = path |>
		stringr::str_remove("^\\s+") |> # remove head spaces
		stringr::str_remove("\\s+$") |> # remove tail spaces
		stringr::str_replace_all("\\\\","/") |> # replace backslash separation
		stringr::str_replace_all("(^|/)\\.(/|$)","\\1") # replace current directory

	len = max(length(path),length(rootpath))
	path = rep(path,length=len)
	rootpath = rep(rootpath,length=len)

	rootdepth = rootpath |>
		stringr::str_remove("(^.+:/+|^/+)") |>
		stringr::str_replace_all("(^|/)\\.\\.($|/)","\\1//\\2") |>
		stringr::str_extract("(([^/]+/)+)$") |>
		stringr::str_count("/")|>
		tidyr::replace_na(0)
	remvdepth = path |>
		stringr::str_extract("^(\\.\\./)+") |>
		stringr::str_count("\\.\\.") |>
		tidyr::replace_na(0)
	depth = ifelse(rootdepth>remvdepth,remvdepth,rootdepth)

	path = ifelse(
		file.is_abspath(path) | is.na(rootpath),
		path,
		stringr::str_c(
			rootpath |>
				stringr::str_remove(sprintf("([^/]+/){%d}$",depth)),
			path |>
				stringr::str_remove(sprintf("^(\\.\\./){%d}",depth))))

	if(strict){
		while(any(stringr::str_detect(path,"(?!(/|^)\\.\\./)(/|^)[^/]+/\\.\\.(/|$)"),na.rm = TRUE)){
			path = stringr::str_replace(path,"(?!(/|^)\\.\\./)(/|^)[^/]+/\\.\\.(/|$)","\\2")
		}
		#path = path |> stringr::str_remove("/$") # remove tail slash
	}

	path = ifelse(path=="","./",path)

	return(path)
}

#' Return absolute path ==BUG EXIST REPLACE BY file.stdpath==
#' @description Return logical value which represents whether the given path is absolute path or not.
#' @param filepath file path (character)
#' @param filedir file directory
#' @param strict logical: TRUE ~ remove unnecessary reference, e.g., "a/b/../c".
#' @return absolute file path
#' @export
file.abspath = function(filepath,filedir,strict=TRUE){
	.Deprecated("file.stdpath")
	filepath = stringr::str_replace_all(filepath,"\\\\","/")
	filedir = stringr::str_replace_all(filedir,"\\\\","/")

	filepath = stringr::str_replace_all(filepath,"(^|[^\\.])(\\./)+","\\1")
	abspath = dplyr::if_else(
		file.is_abspath(filepath),
		filepath,
		stringr::str_c(
			stringr::str_remove(
				filedir,sprintf("(/[^/]+){%d}$",tidyr::replace_na(stringr::str_count(stringr::str_extract(filepath,"^(\\.\\./)+"),"\\.\\."),0))
			),
			"/",
			stringr::str_remove(filepath,"^(\\.\\./)+")
		)
	)
	if(strict){
		while(any(stringr::str_detect(abspath,"./\\.\\."))){
			abspath = stringr::str_remove(abspath,"(?!/\\.\\./)/[^/]+/\\.\\.")
		}
		abspath[stringr::str_detect(abspath,"^/\\.\\.($|/)")]=NA_character_
	}
	return(abspath)
}

#' Return function which generate standardized path of the focal directory
#' @description Returned function generates standardized path of the given directory, base_dir.
#' @param filedir file directory
#' @return function of standardized path generator
#' @export
file.at = function(filedir){
	filedir = file.stdpath(filedir)
	function(...){
		if(length(list(...))==0)return(filedir)
		return(file.stdpath(sprintf(...),filedir))
	}
}

#' Backup given file
#' @description Make backup of the given file in backup directory
#' @param filepath file path (character)
#' @param backup backup directory name; ignored if backup.dir is defined.
#' @param backup.dir backup directory path; default is the same directory of given file.
#' @param timeformat set backup head name.
#' @return filepath
#' @export
file.backup = function(filepath, backup = "bak", backup.dir = NULL,timeformat = "%y%m%d_%H%M%S_"){
	if(!file.exists(filepath))return(filepath)

	filedir = file.dir(filepath)
	filename_ext = file.remove_dir(filepath)
	if(is.null(backup.dir)){
		backup.dir = paste0(filedir,"/",backup)
	}
	dir.create(backup.dir,recursive = TRUE,showWarnings = FALSE)
	if(is.na(timeformat)||length(timeformat)==0||timeformat==""){
		file.copy(filepath, sprintf("%s/%s", backup.dir,filename_ext))
	}else{
		file.copy(filepath, sprintf("%s/%s%s", backup.dir,format(Sys.time(), timeformat),filename_ext))
	}
	return(filepath)
}

#' generate file output
#' @description Generate file output
#' @param filepath file path (character)
#' @param open open mode
#' @param encoding encoding
#' @return fout function
#' @export
file.fout = function(filepath, open = "w", encoding = getOption("encoding")){
	if(encoding == "UTF-8-BOM"){
		outfile = file(filepath, open = open, encoding = "UTF-8")
		if(open %in% c("rb","wb","ab","r+b","w+b","a+b")){
			suppressWarnings(writeBin(as.raw(c(0xef, 0xbb, 0xbf)), outfile))
		}else{
			suppressWarnings(writeChar("\ufeff", outfile, eos = NULL))
		}
	}else{
		outfile = file(filepath, open = open,encoding = encoding)
	}
	function(str, ..., close = FALSE){
		if(close){
			close(outfile)
		}else{
			if(length(list(...))==0){
				writeLines(str,outfile)
			}else{
				writeLines(sprintf(str,...),outfile)
			}
		}
	}
}

#' Cache expression result
#' @description expression result is saved as rds with time stamp. If cached file already exist, it try to use it.
#' @param expr expression for caching
#' @param path path for cache file
#' @param expire diftim until the cache is expired, e.g. as.difftime function with unit argument.
#' @param required_ver integer for controlling file format version; smaller version file is always ignored.
#' @return loaded file
#' @export
cache = function(expr,path,expire=NULL,required_ver=NULL){
	now = Sys.time()

	if(length(path)>0){
		if(file.exists(path) && (length(expire)==0 || expire>=0)){
			rds = readRDS(path)
			if((length(required_ver)==0 || (length(rds$ver)!=0 && rds$ver >= required_ver)) &&
				(length(expire)==0 || rds$time + expire > now)){
				return(rds$dat)
			}
		}
	}

	rds = list(dat = expr, time=now, ver=required_ver)
	if(length(path)>0)saveRDS(rds,path,compress = FALSE)
	return(rds$dat)
}
