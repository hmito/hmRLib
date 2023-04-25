#' Create matrix from function
#' @description This function create the matrix from a defined function and two sequences. The used function should support the sequence-style return from sequence-style arguments. Elements of the created matrix are obtained from the function with two arguments, which are possible combinations of elements from two sequences.
#' @param x First argument vectors, and length of x is the row of created matrix.
#' @param y Second argument vectors, and length of y is the col of created matrix.
#' @param f Function for creating matrix. This function should support the sequence-style return from sequence-style arguments.
#' @param ... Arguments are passed to function f.
#' @return matrix whose elements are obtained from f with possible all combination of x and y
#' @examples
#' x=0:3
#' y=0:2
#' matrix_from_func(x,y,function(x,y){return(x+y)})
#' #((0,1,2,3),(1,2,3,4),(2,3,4,5))
#' @export
matrix_from_func=function(x,y,f,...){
	return(matrix(f(rep(x,times=length(y)),rep(y,each=length(x)),...),length(x),length(y)))
}

#' Create matrix from function without vector argument support.
#' @description This function create the matrix from a defined function and two sequences. The used function don't have to support the sequence-style return from sequence-style arguments. Elements of the created matrix are obtained from the function with two arguments, which are possible combinations of elements from two sequences.
#' @param x First argument vectors, and length of x is the row of created matrix.
#' @param y Second argument vectors, and length of y is the col of created matrix.
#' @param f Function for creating matrix.
#' @param ... Arguments are passed to function f.
#' @return matrix whose elements are obtained from f with possible all combination of x and y
#' #' @examples
#' x=0:3
#' y=0:2
#' matrix_from_func.nonseqarg(x,y,function(x,y){return(x+y)})
#' #((0,1,2,3),(1,2,3,4),(2,3,4,5)
#' @export
matrix_from_func.nonseqarg=function(x,y,f,...){
	return(matrix(mapply(f,rep(x,times=length(y)),rep(y,each=length(x)),...),length(x),length(y)))
}

#' Remove na elements from sequences.
#' @param seq Terget sequence.
#' @return Sequence including only non.na elements.
#' @examples
#' x=c(0,NA,2,3,NA,5,NA)
#' remove.na(x)
#' #(0,2,3,5)
#' @export
remove.na=function(seq){seq[!is.na(seq)]}

#' Create sequence with NA
#' @param num Length of sequence.
#' @return Sequence whose all elements are NA
#' @examples
#' seq_na(4)
#' #(NA,NA,NA,NA)
#' @export
seq_na=function(num){
	ans=numeric(num)
	is.na(ans)=TRUE
	return(ans)
}

#' Create matrix with NA
#' @param nx Length of row.
#' @param ny Length of col.
#' @return Matrix whose all elements are NA
#' @examples
#' matrix_na(3,2)
#' #((NA,NA),(NA,NA),(NA,NA))
#' @export
matrix_na=function(nx,ny){
	ans=matrix(0,nx,ny)
	is.na(ans)=TRUE
	return(ans)
}
