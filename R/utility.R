##############################
#utility_v1_02
#	count_equal_point追加
#	search_equal_point追加
#utility_v1_01
#	matrix_from_virtual_function追加
#	matrix_from_functionの引数の順序を変更
#utility_v1_00
#	matrix_from_function
#	matrix_of_subseq
#	seq.na, matrix.na
#	seq.hist

##############################

#' matrix_from_function
#' @description Create matrix from function
#' @param f using function with two arguments
#' @param x first argument vectors
#' @param y second argument vectors
#' @return matrix whose elements are obtained from f with possible all combination of x and y
#' @examples
#' x=0:3
#' y=0:2
#' matrix_from_function(function(x,y){return(x+y)},x,y)
#' #((0,1,2,3),(1,2,3,4),(2,3,4,5)
matrix_from_function=function(f,x,y,...){
	return(matrix(f(rep(x,times=length(y)),rep(y,each=length(x)),...),length(x),length(y)))
}

#' matrix_from_virtual_function
#' @description Create matrix from function without vector argument support.
matrix_from_virtual_function=function(f,x,y,...){
	ans=matrix(0,length(x),length(y))
	for(dx in 1:length(x)){
		for(dy in 1:length(y)){
			ans[dx,dy]=f(x[dx],y[dy],...)
		}
	}
	return(ans)
}
#2変数(x,y)のx平面、mxにおけるyの配列を作成する
seq_of_subseq=function(mx,x,y){
	select_functor=function(dx){
		return(y[x==dx])
	}
	return(as.vector(apply(as.matrix(mx,1,length(mx)),c(1),select_functor)))
}
#3変数(x,y,z)のx-y平面、mx-myにおけるzの行列を作成する
matrix_of_subseq=function(mx,my,x,y,z){
	select_functor=function(dx,dy){
		return(z[x==dx & y==dy])
	}
	return(matrix(mapply(select_functor,rep(mx,times=length(my)),rep(my,each=length(mx))),nrow=length(mx),ncol=length(my)))
}
#初期値がNA、要素数numの配列を取得
seq.na=function(num){
	ans=numeric(num)
	is.na(ans)=TRUE
	return(ans)
}
#初期値がNA、要素数nx*nyの行列を取得
matrix.na=function(nx,ny){
	ans=matrix(0,nx,ny)
	is.na(ans)=TRUE
	return(ans)
}
#histgram配列を取り出す
seq.hist=function(Vec,SepSeq){
	Ans=numeric(length(SepSeq-1))
	for(i in 2:length(SepSeq)){
		if(i==length(SepSeq))Ans[i-1]=sum(Vec>=SepSeq[i-1] & Vec<=SepSeq[i])
		else Ans[i-1]=sum(Vec>=SepSeq[i-1] & Vec<SepSeq[i])
	}
	return(Ans)
}
#naを取り除く
remove.na=function(seq){seq[!is.na(seq)]}
#(xylist[1],xylist[2])となるような点配列の中に、(x,y)と同位置となるような点の数
count_equal_point=function(x,y,xylist){sum(remove.na((xylist[[1]]==x)&(xylist[[2]]==y)))}
#(seq[1],seq[2])となるような点配列の中に、(xseq,yseq)と同位置となるような点を返す
search_equal_point=function(xseq1,yseq1,xseq2,yseq2){
	xseq1.cmp=rep(xseq1,each=length(xseq2))
	yseq1.cmp=rep(yseq1,each=length(yseq2))
	xseq2.cmp=rep(xseq2,times=length(xseq1))
	yseq2.cmp=rep(yseq2,times=length(yseq1))
	acs=((xseq1.cmp==xseq2.cmp)&(yseq1.cmp==yseq2.cmp))
	return(list(xseq1.cmp[acs],yseq1.cmp[acs]))
}
