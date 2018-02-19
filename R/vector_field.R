#' Plot vector field for 2D. Length is the strength of the vector.
#' @param dxf function for the gradient for x-axix
#' @param dyf function for the gradient for y-axix
#' @param arrow.length Basic length of the arrow.
#' @param arrow.sublength Basic sub length of the arrow.
#' @param xlim range of x-axis
#' @param ylim range of y-axis
#' @param num Density of the arrows.
#' @param col color of the vector
#' @param limit limit function with two argument x,y. If limit(x,y) return true, the arrow is not drown.
#' @param add boolian: whether add existign plot space or not.
#' @param ... same with the plot function.
#' @importFrom graphics arrows
#' @export
vector_field=function(dxf,dyf,arrow.length,arrow.sublength,xlim=c(0,1),ylim=c(0,1),num=20,col="black",limit=NULL,add=FALSE,...){
	x_seq=seq(xlim[1],xlim[2],length=num+1)
	y_seq=seq(ylim[1],ylim[2],length=num+1)
	dx=matrix(mapply(dxf,rep(x_seq,times=length(y_seq)),rep(y_seq,each=length(x_seq))),length(x_seq),length(y_seq))
	dy=matrix(mapply(dyf,rep(x_seq,times=length(y_seq)),rep(y_seq,each=length(x_seq))),length(x_seq),length(y_seq))
	if(add==FALSE){
		plot.null(xlim=xlim,ylim=ylim,...)
	}
	for(nx in 1:length(x_seq)){
		for(ny in 1:length(y_seq)){
			if(!is.null(limit) && limit(x_seq[nx],y_seq[ny])==FALSE)next;
			arrows(
				x_seq[nx]-dx[nx,ny]/2*arrow.length,
				y_seq[ny]-dy[nx,ny]/2*arrow.length,
				x_seq[nx]+dx[nx,ny]/2*arrow.length,
				y_seq[ny]+dy[nx,ny]/2*arrow.length,
				col=col,
				length=arrow.sublength,
				...
			)
		}
	}
}

#' Plot vector field for 2D. Color is the strength of the vector.
#' @param dxf function for the gradient for x-axix
#' @param dyf function for the gradient for y-axix
#' @param colrate influence of the strength on the color.
#' @param arrow.length Basic length of the arrow.
#' @param arrow.sublength Basic sub length of the arrow.
#' @param xyrate relative length of x and y values.
#' @param xlim range of x-axis
#' @param ylim range of y-axis
#' @param num Density of the arrows.
#' @param col color of the vector
#' @param limit limit function with two argument x,y. If limit(x,y) return true, the arrow is not drown.
#' @param add boolian: whether add existign plot space or not.
#' @param ... same with the plot function.
#' @importFrom graphics arrows
#' @importFrom grDevices heat.colors
#' @export
vector_field.color=function(dxf,dyf,colrate,arrow.length=NA,arrow.sublength=0.25,xyrate=1.0,xlim=c(0,1),ylim=c(0,1),num=20,col=rev(heat.colors(100)),limit=NULL,add=FALSE,...){
	if(is.na(arrow.length))arrow.length=0.6

	yweight=(xlim[2]-xlim[1])/(ylim[2]-ylim[1])
	xlength=(xlim[2]-xlim[1])/(num-1)*arrow.length/2
	ylength=(ylim[2]-ylim[1])/(num-1)*arrow.length/2*yweight*xyrate

	x_seq=seq(xlim[1],xlim[2],length=num+1)
	y_seq=seq(ylim[1],ylim[2],length=num+1)
	dx=matrix(mapply(dxf,rep(x_seq,times=length(y_seq)),rep(y_seq,each=length(x_seq))),length(x_seq),length(y_seq))
	dy=matrix(mapply(dyf,rep(x_seq,times=length(y_seq)),rep(y_seq,each=length(x_seq))),length(x_seq),length(y_seq))
	if(add==FALSE){
		plot.null(xlim=xlim,ylim=ylim,...)
	}
	for(nx in 1:length(x_seq)){
		for(ny in 1:length(y_seq)){
			if(!is.null(limit) && limit(x_seq[nx],y_seq[ny])==FALSE)next;
			d=sqrt(dx[nx,ny]^2+yweight^2 * dy[nx,ny]^2)
			arrows(
				x_seq[nx]-dx[nx,ny]/d*xlength,
				y_seq[ny]-dy[nx,ny]/d*ylength,
				x_seq[nx]+dx[nx,ny]/d*xlength,
				y_seq[ny]+dy[nx,ny]/d*ylength,
				col=col[min(ceiling(d*colrate),length(col))],
				length=arrow.sublength,
				...
			)
		}
	}
}
