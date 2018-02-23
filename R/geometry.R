#' Check if the segment seg1 is cross with seg2
#' @description Check if two segments has thier cross point or not.
#' @param seg1.a_x x value of point A on segment 1
#' @param seg1.a_y y value of point A on segment 1
#' @param seg1.b_x x value of point B on segment 1
#' @param seg1.b_y y value of point B on segment 1
#' @param seg2.a_x x value of point A on segment 2
#' @param seg2.a_y y value of point A on segment 2
#' @param seg2.b_x x value of point B on segment 2
#' @param seg2.b_y y value of point B on segment 2
#' @return logical value: TRUE when segment1 and segment2 has a cross point.
#' @export
has_cross_point=function(seg1.a_x,seg1.a_y,seg1.b_x,seg1.b_y,seg2.a_x,seg2.a_y,seg2.b_x,seg2.b_y){
	#x座標によるチェック
	XCheck=rep(TRUE,length=length(seg1.a_x))
	XCheck=!(((seg1.a_x >= seg1.b_x) & ((seg1.a_x < seg2.a_x & seg1.a_x < seg2.b_x) | (seg1.b_x > seg2.a_x & seg1.b_x > seg2.b_x)))|((seg1.a_x < seg1.b_x) & ((seg1.b_x < seg2.a_x & seg1.b_x < seg2.b_x) | (seg1.a_x > seg2.a_x & seg1.a_x > seg2.b_x))))

	#y座標によるチェック
	YCheck=rep(TRUE,length=length(seg1.a_y))
	YCheck=!(((seg1.a_y >= seg1.b_y) & ((seg1.a_y < seg2.a_y & seg1.a_y < seg2.b_y) | (seg1.b_y > seg2.a_y & seg1.b_y > seg2.b_y)))|((seg1.a_y < seg1.b_y) & ((seg1.b_y < seg2.a_y & seg1.b_y < seg2.b_y) | (seg1.a_y > seg2.a_y & seg1.a_y > seg2.b_y))))

	#交差条件チェック
	Cross1Check=rep(TRUE,length=length(seg1.a_x))
	Cross1Check=!(((seg1.a_x - seg1.b_x) * (seg2.a_y - seg1.a_y) + (seg1.a_y - seg1.b_y) * (seg1.a_x - seg2.a_x)) *((seg1.a_x - seg1.b_x) * (seg2.b_y - seg1.a_y) + (seg1.a_y - seg1.b_y) * (seg1.a_x - seg2.b_x)) > 0)

	Cross2Check=rep(TRUE,length=length(seg1.a_x))
	Cross2Check=!(((seg2.a_x - seg2.b_x) * (seg1.a_y - seg2.a_y) + (seg2.a_y - seg2.b_y) * (seg2.a_x - seg1.a_x)) *((seg2.a_x - seg2.b_x) * (seg1.b_y - seg2.a_y) + (seg2.a_y - seg2.b_y) * (seg2.a_x - seg1.b_x)) > 0)

	return(XCheck&YCheck&Cross1Check&Cross2Check)
}


#' Get cross point between two segments
#' @description Check if two segments has thier cross point or not.
#' @param seg1.a_x x value of point A on segment 1
#' @param seg1.a_y y value of point A on segment 1
#' @param seg1.b_x x value of point B on segment 1
#' @param seg1.b_y y value of point B on segment 1
#' @param seg2.a_x x value of point A on segment 2
#' @param seg2.a_y y value of point A on segment 2
#' @param seg2.b_x x value of point B on segment 2
#' @param seg2.b_y y value of point B on segment 2
#' @return vector whose first and second elements are x and y.
#' @note This function return incorrect values when given segments has no cross point.
#' @export
get_cross_point=function(seg1.a_x,seg1.a_y,seg1.b_x,seg1.b_y,seg2.a_x,seg2.a_y,seg2.b_x,seg2.b_y){
	r=((seg2.b_y-seg2.a_y)*(seg2.a_x-seg1.a_x)-(seg2.b_x-seg2.a_x)*(seg2.a_y-seg1.a_y))/((seg1.b_x-seg1.a_x)*(seg2.b_y-seg2.a_y)-(seg1.b_y-seg1.a_y)*(seg2.b_x-seg2.a_x))
	return(c((1-r)*seg1.a_x+r*seg1.b_x,(1-r)*seg1.a_y+r*seg1.b_y))
}


#' Find equal points from point groups.
#' @description Return equal points from two point groups, seq1 and seq2.
#' @param xseq1 x-values of first point group
#' @param yseq1 y-values of first point group
#' @param xseq2 x-values of second point group
#' @param yseq2 y-values of second point group
#' @return list(x,y). x and y are sequence of points.
#' @export
search_equal_point=function(xseq1,yseq1,xseq2,yseq2){
	xseq1.cmp=rep(xseq1,each=length(xseq2))
	yseq1.cmp=rep(yseq1,each=length(yseq2))
	xseq2.cmp=rep(xseq2,times=length(xseq1))
	yseq2.cmp=rep(yseq2,times=length(yseq1))
	acs=((xseq1.cmp==xseq2.cmp)&(yseq1.cmp==yseq2.cmp))
	return(list(x=xseq1.cmp[acs],y=yseq1.cmp[acs]))
}


#' Find equal points from point groups.
#' @description Return equal points from two point groups, seq1 and seq2.
#' @param xseq1 x-values of first point group
#' @param yseq1 y-values of first point group
#' @param xseq2 x-values of second point group
#' @param yseq2 y-values of second point group
#' @return list(x,y). x and y are sequence of points.
#' @export
search_cross_point=function(xseq1,yseq1,xseq2,yseq2){
	pos1.x=xseq1[-length(xseq1)]
	pos1.y=yseq1[-length(yseq1)]
	pos2.x=xseq1[-1]
	pos2.y=yseq1[-1]
	pos3.x=xseq2[-length(xseq2)]
	pos3.y=yseq2[-length(yseq2)]
	pos4.x=xseq2[-1]
	pos4.y=yseq2[-1]

	pos1.x.cmp=rep(pos1.x,each=length(pos3.x))
	pos1.y.cmp=rep(pos1.y,each=length(pos3.y))
	pos2.x.cmp=rep(pos2.x,each=length(pos3.x))
	pos2.y.cmp=rep(pos2.y,each=length(pos3.y))
	pos3.x.cmp=rep(pos3.x,times=length(pos1.x))
	pos3.y.cmp=rep(pos3.y,times=length(pos1.y))
	pos4.x.cmp=rep(pos4.x,times=length(pos1.x))
	pos4.y.cmp=rep(pos4.y,times=length(pos1.y))

	#  IsCross=mapply(is.cross_segments,pos1.x.cmp,pos1.y.cmp,pos2.x.cmp,pos2.y.cmp,pos3.x.cmp,pos3.y.cmp,pos4.x.cmp,pos4.y.cmp)
	IsCross=has_cross_point(pos1.x.cmp,pos1.y.cmp,pos2.x.cmp,pos2.y.cmp,pos3.x.cmp,pos3.y.cmp,pos4.x.cmp,pos4.y.cmp)
	IsCross[is.na(IsCross)]=FALSE

	ans.x=numeric(0)
	ans.y=numeric(0)

	for(i in (1:length(pos1.x.cmp))[IsCross]){
		pair=get_cross_point(pos1.x.cmp[i],pos1.y.cmp[i],pos2.x.cmp[i],pos2.y.cmp[i],pos3.x.cmp[i],pos3.y.cmp[i],pos4.x.cmp[i],pos4.y.cmp[i])
		ans.x=c(ans.x,pair[[1]])
		ans.y=c(ans.y,pair[[2]])
	}

	return(list(x=ans.x,y=ans.y))
}
