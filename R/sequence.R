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
#' matrix_from_function(function(x,y){return(x+y)},x,y)
#' #((0,1,2,3),(1,2,3,4),(2,3,4,5)
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
#' matrix_from_function(function(x,y){return(x+y)},x,y)
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
#' seq.na(4)
#' #(NA,NA,NA,NA)
#' @export
seq.na=function(num){
	ans=numeric(num)
	is.na(ans)=TRUE
	return(ans)
}

matrix_of_subseq=function(mx,my,x,y,z){
	select_functor=function(dx,dy){
		return(z[x==dx & y==dy])
	}
	return(matrix(mapply(select_functor,rep(mx,times=length(my)),rep(my,each=length(mx))),nrow=length(mx),ncol=length(my)))
}

matrix.na=function(nx,ny){
	ans=matrix(0,nx,ny)
	is.na(ans)=TRUE
	return(ans)
}


seq.hist=function(Vec,SepSeq){
	Ans=numeric(length(SepSeq-1))
	for(i in 2:length(SepSeq)){
		if(i==length(SepSeq))Ans[i-1]=sum(Vec>=SepSeq[i-1] & Vec<=SepSeq[i])
		else Ans[i-1]=sum(Vec>=SepSeq[i-1] & Vec<SepSeq[i])
	}
	return(Ans)
}

seq_of_subseq=function(mx,x,y){
	select_functor=function(dx){
		return(y[x==dx])
	}
	return(as.vector(apply(as.matrix(mx,1,length(mx)),c(1),select_functor)))
}

count_equal_point=function(x,y,xylist){sum(remove.na((xylist[[1]]==x)&(xylist[[2]]==y)))}

search_equal_point=function(xseq1,yseq1,xseq2,yseq2){
	xseq1.cmp=rep(xseq1,each=length(xseq2))
	yseq1.cmp=rep(yseq1,each=length(yseq2))
	xseq2.cmp=rep(xseq2,times=length(xseq1))
	yseq2.cmp=rep(yseq2,times=length(yseq1))
	acs=((xseq1.cmp==xseq2.cmp)&(yseq1.cmp==yseq2.cmp))
	return(list(xseq1.cmp[acs],yseq1.cmp[acs]))
}
