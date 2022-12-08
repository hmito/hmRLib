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


#' Get file name with extension
#' @description Get file name with extension from file path.
#' @param filepath file path (character)
#' @return file name with extension of given file path.
#' @importFrom stringr str_remove
#' @export
file.name_ext = function(filepath){
	str_remove(filepath,"^.+[/\\\\]+")
}

#' Get file name without extension
#' @description Get file name without extension from file path.
#' @param filepath file path (character)
#' @return file name of given file path.
#' @importFrom stringr str_remove
#' @export
file.name = function(filepath){
	str_remove(file.name_ext(filepath),"\\.[^\\.]+$")
}

#' Get directory of file
#' @description Get directory of file from file path.
#' @param filepath file path (character)
#' @return directory of file of given file path.
#' @importFrom stringr str_remove
#' @export
file.dir = function(filepath){
	str_remove(filepath,"[/\\\\]+[^/\\\\]+$")
}

#' Backup given file
#' @description Make backup of the given file in backup directory
#' @param filepath file path (character)
#' @param backup backup directory name; ignored if backup.dir is defined.
#' @param backup.dir backup directory path; default is the same directory of given file.
#' @return logical: TRUE if fail to find the file path.
#' @export
file.backup = function(filepath,backup = "bak", backup.dir = NULL){
	if(file.exists(filepath))return(TRUE)

	filedir = file.dir(filepath)
	filename_ext = file.name_ext(filepath)
	if(is.null(backup.dir)){
		backup.dir = paste0(filedir,"/",backup)
	}
	dir.create(backup.dir,recursive = TRUE,showWarnings = FALSE)
	file.copy(filepath, sprintf("%s/%s_%s", backup.dir,format(Sys.time(), "%y%m%d_%H%M%S"),filename_ext))
	return(FALSE)
}
