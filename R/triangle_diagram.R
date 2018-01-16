#' Plot for triangle diagram.
#' @param xlab label of x axis.
#' @param ylab label of y axis.
#' @param zlab label of z axis.
#' @export
triangle_plot = function(xlab="", ylab="", zlab=""){
#	par.old <- par(no.readonly=T)

	ex = c(1,0)
	ey = c(cos(acos(-1)*120/180),sin(acos(-1)*120/180))
	ez = c(cos(acos(-1)*240/180),sin(acos(-1)*240/180))

	par(mai=c(0,0,0,0))
	plot(0,0,xlim=c(-0.1,1.1),ylim=c(-0.2,1.0),type = "n", axes=FALSE, xlab="", ylab="")
	triangle_lines(c(0,1,0,0),c(0,0,1,0))

	triangle_lines(c(0.0,0.0),c(0,-0.03))
	triangle_lines(c(0.5,0.5),c(0,-0.03))
	triangle_lines(c(1.0,1.0),c(0,-0.03))
	triangle_text(0.0,-0.05, "  0%", adj=0.0, srt = -120)
	triangle_text(0.5,-0.05, " 50%", adj=0.0, srt = -120)
	triangle_text(1.0,-0.05, "100%", adj=0.0, srt = -120)

	triangle_lines(c(1,1+0.03),c(0.0,0.0))
	triangle_lines(c(0.5,0.5+0.03),c(0.5,0.5))
	triangle_lines(c(0,0+0.03),c(1.0,1.0))
	triangle_text(1.05,0.00, "  0%", adj=0.0, srt = 0)
	triangle_text(0.55,0.50, " 50%", adj=0.0, srt = 0)
	triangle_text(0.05,1.00, "100%", adj=0.0, srt = 0)

	triangle_lines(c(0,0-0.03),c(1.0,1.0+0.03))
	triangle_lines(c(0,0-0.03),c(0.5,0.5+0.03))
	triangle_lines(c(0,0-0.03),c(0.0,0.0+0.03))
	triangle_text(-0.05,1.05, "  0%", adj=0.0, srt = 120)
	triangle_text(-0.05,0.55, " 50%", adj=0.0, srt = 120)
	triangle_text(-0.05,0.05, "100%", adj=0.0, srt = 120)

	triangle_text(0.58,-0.16,xlab)
	triangle_text(0.58,0.58,ylab)
	triangle_text(-0.16,0.58,zlab)
#	par(par.old)
}

#' Plot triangle vector map.
#' @param x position of first element.
#' @param y position of second element. Note that the third element is not required because z is always 1-x-y.
#' @param vx vector/allow of first element.
#' @param vy vector/allow of second element. Note that the third element is not required because z is always 1-x-y.
#' @param xlab label of x-axis
#' @param ylab label of y-axis
#' @param zlab label of z-axis
#' @param vlim maximum length of vector
#' @param arrow.len length of the arrow
#' @param arrow.headlen head length of the arrow
#' @param col color of the vector
#' @export
triangle_vectormap = function(x, y, vx, vy, xlab="", ylab="", zlab="", vlim = NA, arrow.len=0.05, arrow.headlen=0.10, col = rev(gray.colors(20))){
	triangle_plot(xlab, ylab, zlab)

	pos1 = x*ex[1] + y*(ex[1]+ey[1])
	pos2 = x*ex[2] + y*(ex[2]+ey[2])
	vec1 = vx*ex[1]+vy*(ex[1]+ey[1])
	vec2 = vx*ex[2]+vy*(ex[2]+ey[2])
	vlen = sqrt(vec1**2+vec2**2)
	if(is.na(vlim)){
		vlim = c(0,max(vlen))
	}
	alen =arrow.len

	vcolno = trunc((vlen-vlim[1])/vlim[2]*length(col))+1
	vcolno[vcolno<1]=1
	vcolno[vcolno>length(col)]=length(col)
	arrows(pos1-vec1/vlen*alen/2,pos2-vec2/vlen*alen/2,pos1+vec1/vlen*alen/2,pos2+vec2/vlen*alen/2,length=arrow.headlen,col=col[vcolno])
}

#' Change axis for triangle diagram.
#' @param x position of first element.
#' @param y position of second element. Note that the third element is not required because z is always 1-x-y.
#' @export
to_triangle=function(x,y){
	ex = c(1,0)
	ey = c(cos(acos(-1)*120/180),sin(acos(-1)*120/180))
	ez = c(cos(acos(-1)*240/180),sin(acos(-1)*240/180))
	pos1 = x*ex[1] + y*(ex[1]+ey[1])
	pos2 = x*ex[2] + y*(ex[2]+ey[2])

	return(list(tx=pos1,ty=pos2))
}

#' Plot points on triangle diagram.
#' @param x position of first element.
#' @param y position of second element. Note that the third element is not required because z is always 1-x-y.
#' @param ... Other parameters for points function.
#' @export
triangle_points = function(x,y,...){
	t = to_triangle(x,y)
	points(t$tx,t$ty,...)
}

#' Plot lines on triangle diagram.
#' @param x position of first element.
#' @param y position of second element. Note that the third element is not required because z is always 1-x-y.
#' @param ... Other parameters for lines function.
#' @export
triangle_lines = function(x,y,...){
	t = to_triangle(x,y)
	lines(t$tx,t$ty,...)
}
#' Plot text on triangle diagram.
#' @param x position of first element.
#' @param y position of second element. Note that the third element is not required because z is always 1-x-y.
#' @param ... Other parameters for text function.
#' @export
triangle_text = function(x,y,...){
	t = to_triangle(x,y)
	text(t$tx,t$ty,...)
}
