#' source C++ code from a file with include/library path.
#' @description call Rcpp::sourceCpp with temporally defining include/library path
#' @param file A character string giving the path name of a file
#' @param I A character (or vector of character) giving the path name of include directory
#' @param L A character (or vector of character) giving the path name of library directory
#' @param l_opt A character (or vector of character) giving the additional library option (e.g. LibOptions = c("-lm"," -lmpc"))
#' @param ... other arguments for sourceCpp
#' @return a vector which containing root values
#' @importFrom Rcpp sourceCpp
#' @export
source_cpp = function(file, I=NULL,L=NULL,l_opt=NULL,...){
	PKG_CXXFLAGS = Sys.getenv("PKG_CXXFLAGS")
	PKG_LIBS =  Sys.getenv("PKG_LIBS")
	on.exit(Sys.setenv("PKG_CXXFLAGS" = PKG_CXXFLAGS),add=TRUE)
	on.exit(Sys.setenv("PKG_LIBS" = PKG_LIBS),add=TRUE)

	PKG_CXXFLAGS_tmp = paste(PKG_CXXFLAGS,sprintf("-I%s",I),collapse=" ")
	PKG_LIBS_tmp = paste(PKG_LIBS,sprintf("-L%s",L),l_opt,collapse=" ")
	Sys.setenv("PKG_CXXFLAGS" = PKG_CXXFLAGS_tmp)
	Sys.setenv("PKG_LIBS" = PKG_LIBS_tmp)
	sourceCpp(file,...)
}
