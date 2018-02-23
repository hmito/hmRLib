#' Find root of a function using small_step method
#' @description This function returns roots of a focal function by checking the sign change in each step.
#' @param func the function for which the root is sought.
#' @param step a vector containing the step values
#' @param ... optional arguments to be passed to "func"
#' @return data.frame with two col, "lower" and "upper" which are the range of the solutions. root
#' @export
#' @examples
#' f = function(x,y){return(x*x-y*x)}
#' ans = small_step_root(f, seq(-2,2,length=41),y=1.333)
#' # ans == data.frame("lower"=c(0.0,1.3),"upper"=c(0.0,1.4))
small_step_root=function(func,step,...){
	fval=func(step,...)
	pos = (fval[-1]*fval[-length(step)]<0)
	zpos = (fval==0)
	return(data.frame("lower"=c(step[-length(step)][pos],step[zpos]),"upper"=c(step[-1][pos],step[zpos])))
}


#' Find root of a function using combination of small_step method and newton method.
#' @description Returns roots of a focal function by checking the sign change in each step.
#' @param func the function for which the root is sought.
#' @param step a vector containing the step values
#' @param ... optional arguments to be passed to "func"
#' @return a vector which containing root values
#' @importFrom stats uniroot
#' @export
#' @examples
#' f = function(x,y){return(x*x-y*x)}
#' ans = newton_root(f, seq(-2,2,length=1001),y=1.333)
#' #ans == c(0.000,1.333)
newton_root=function(func,step,...){
	ans=numeric(0)

	pair=small_step_root(func,step,...)
	if(nrow(pair)==0){
		return(ans)
	}

	for(i in 1:nrow(pair)){
		if(pair$lower[i]==pair$upper[i]){
			ans=c(ans,pair$lower[i])
		}else{
			ans=c(ans,uniroot(func,c(pair$lower[i],pair$upper[i]),...)[["root"]])
		}
	}
	return(ans)
}


#' Find root points on x-y plane from logical matrix and x,y axes.
#' @description Return vectors which are sequence of root points on the given grid (x,y).
#' @param x sequence of x-axis
#' @param y sequence oy y-axis
#' @param z logical matrix of function values whose root are searched
#' @return list(x,y). x and y are sequence of points.
#' @note nrow(z) and ncol(z) should be equal to length(x) and length(y), respectively.
#' @export
small_step_planeroot_from_matrix=function(x,y,z){
	if(is.matrix(z)==FALSE){
		return(NA)
	}

	xmax=nrow(z)
	ymax=ncol(z)

	pos=matrix(0,xmax-1,ymax-1)
	pos=1*z[-xmax,-ymax]+2*z[-1,-ymax]+4*z[-1,-1]+8*z[-xmax,-1]
	pos[is.na(pos)]=0
	pos[pos>7]=15-pos[pos>7]

	search=function(dx,dy,ans){
		pre_dx=0
		pre_dy=0

		stock=FALSE
		stock_dx=0
		stock_dy=0
		stock_pre_dx=0
		stock_pre_dy=0
		stock_mode=0

		while(TRUE){
			if(dx<1 || dy<1 || dx>=xmax || dy>=ymax || ans[["pos"]][dx,dy]==0){
				if(stock && (dx!=stock_dx || dy!=stock_dy)){
					dx=stock_dx
					dy=stock_dy
					pre_dx=stock_pre_dx
					pre_dy=stock_pre_dy
					ans[["pos"]][stock_dx,stock_dy]=stock_mode
					stock=FALSE

					ans[["x"]]=rev(ans[["x"]])
					ans[["y"]]=rev(ans[["y"]])
				}else{
					return(ans)
				}
			}else if(ans[["pos"]][dx,dy]==1){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=1
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==2){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=2
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==4){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=4
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==7){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=7
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==3){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}else{
					if(pre_dx!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=3
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==6){
				if(pre_dy==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=0
						stock_pre_dy=-1
						stock_dx=dx
						stock_dy=dy
						stock_mode=6
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==5){
				if(pre_dx!=0){
					if(pre_dx==-1){
						ans[["x"]]=c(ans[["x"]],x[dx+1])
						ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
						pre_dx=-1
						pre_dy=0
					}else{
						if(pre_dx!=1){
							stock_pre_dx=-1
							stock_pre_dy=0
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],x[dx])
						ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
						pre_dx=1
						pre_dy=0
					}
					ans[["pos"]][dx,dy]=6;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}else{
					if(pre_dy==-1){
						ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
						ans[["y"]]=c(ans[["y"]],y[dy+1])
						pre_dx=0
						pre_dy=-1
					}else{
						if(pre_dy!=1){
							stock_pre_dx=0
							stock_pre_dy=-1
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
						ans[["y"]]=c(ans[["y"]],y[dy])
						pre_dx=0
						pre_dy=1
					}
					ans[["pos"]][dx,dy]=3;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}
			}
		}
	}

	ansx=numeric(0)
	ansy=numeric(0)

	while(TRUE){
		tmp=which(pos!=0, arr.ind=TRUE)
		if(nrow(tmp)==0)break
		ans=search(tmp[1,1],tmp[1,2],list(pos=pos,x=numeric(0),y=numeric(0)))
		ansx=c(ansx,ans[["x"]],NA)
		ansy=c(ansy,ans[["y"]],NA)
		pos=ans[["pos"]]
	}

	return(list(x=ansx,y=ansy))
}


#' Find root points on x-y plane by using small step method
#' @description Return vectors which are sequence of root points on the given grid (x,y).
#' @param func Function  whose root points are searched
#' @param xlim min and max value of x-axis.
#' @param ylim min and max value of y-axis.
#' @param n step num for small_step search
#' @param ... optional arguments to be passed to "func"
#' @return list(x,y). x and y are sequence of points.
#' @export
small_step_planeroot=function(func,xlim=c(0,1),ylim=c(0,1),n=1001,...){
	x=seq(xlim[1],xlim[2],length=n)
	y=seq(ylim[1],ylim[2],length=n)
	z=outer(x,y,func,...)
	z=(z>0)
	return(small_step_planeroot_from_matrix(x,y,z))
}


#' Find root points on x-y plane by using newton method
#' @description Return vectors which are sequence of root points on the given grid (x,y).
#' @param func Function  whose root points are searched
#' @param xlim min and max value of x-axis.
#' @param ylim min and max value of y-axis.
#' @param n step num for small_step search
#' @param ... optional arguments to be passed to "func"
#' @return list(x,y). x and y are sequence of points.
#' @importFrom stats uniroot
#' @export
newton_planeroot=function(func,xlim=c(0,1),ylim=c(0,1),n=1001,...){
	x=seq(xlim[1],xlim[2],length=n)
	y=seq(ylim[1],ylim[2],length=n)
	z=outer(x,y,func,...)
	z=(z>0)
	pos=matrix(0,nrow(z)-1,ncol(z)-1)

	xfunc=function(x1,x2,y){
		f = function(x){func(x,y)}
		if(f(x1)*f(x2)>0){
			if(f(x1)>0)return(x1)
			else return (x2)
		}
		return(uniroot(f,c(x1,x2),...)[["root"]])
	}
	yfunc=function(x,y1,y2){
		f = function(y){func(x,y)}
		if(f(y1)*f(y2)>0){
			if(f(y1)>0)return(y1)
			else return (y2)
		}
		return(uniroot(f,c(y1,y2),...)[["root"]])
	}

	pos=1*z[-nrow(z),-ncol(z)]+2*z[-1,-ncol(z)]+4*z[-1,-1]+8*z[-nrow(z),-1]
	pos[is.na(pos)]=0
	pos[pos>7]=15-pos[pos>7]
	search=function(dx,dy,ans){
		#ans=list(pos,x,y)

		pre_dx=0
		pre_dy=0

		stock=FALSE
		stock_dx=0
		stock_dy=0
		stock_pre_dx=0
		stock_pre_dy=0
		stock_mode=0
		while(TRUE){
			if(dx<1 || dy<1 || dx>=length(x) || dy>=length(y) || ans[["pos"]][dx,dy]==0){
				if(stock && (dx!=stock_dx || dy!=stock_dy)){
					dx=stock_dx
					dy=stock_dy
					pre_dx=stock_pre_dx
					pre_dy=stock_pre_dy
					ans[["pos"]][stock_dx,stock_dy]=stock_mode
					stock=FALSE

					ans[["x"]]=rev(ans[["x"]])
					ans[["y"]]=rev(ans[["y"]])
				}else{
					return(ans)
				}
			}else if(ans[["pos"]][dx,dy]==1){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy]))
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=1
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],yfunc(x[dx],y[dy],y[dy+1]))
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==2){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy]))
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=2
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],yfunc(x[dx+1],y[dy],y[dy+1]))
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==4){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy+1]))
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=4
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],yfunc(x[dx+1],y[dy],y[dy+1]))
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==7){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy+1]))
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=7
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],yfunc(x[dx],y[dy],y[dy+1]))
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==3){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],yfunc(x[dx+1],y[dy],y[dy+1]))
					pre_dx=-1
					pre_dy=0
				}else{
					if(pre_dx!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=3
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],yfunc(x[dx],y[dy],y[dy+1]))
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==6){
				if(pre_dy==-1){
					ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy+1]))
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=0
						stock_pre_dy=-1
						stock_dx=dx
						stock_dy=dy
						stock_mode=6
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy]))
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==5){
				if(pre_dx!=0){
					if(pre_dx==-1){
						ans[["x"]]=c(ans[["x"]],x[dx+1])
						ans[["y"]]=c(ans[["y"]],yfunc(x[dx+1],y[dy],y[dy+1]))
						pre_dx=-1
						pre_dy=0
					}else{
						if(pre_dx!=1){
							stock_pre_dx=-1
							stock_pre_dy=0
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],x[dx])
						ans[["y"]]=c(ans[["y"]],yfunc(x[dx],y[dy],y[dy+1]))
						pre_dx=1
						pre_dy=0
					}
					ans[["pos"]][dx,dy]=6;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}else{
					if(pre_dy==-1){
						ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy+1]))
						ans[["y"]]=c(ans[["y"]],y[dy+1])
						pre_dx=0
						pre_dy=-1
					}else{
						if(pre_dy!=1){
							stock_pre_dx=0
							stock_pre_dy=-1
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],xfunc(x[dx],x[dx+1],y[dy]))
						ans[["y"]]=c(ans[["y"]],y[dy])
						pre_dx=0
						pre_dy=1
					}
					ans[["pos"]][dx,dy]=3;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}
			}
		}
	}

	ansx=numeric(0)
	ansy=numeric(0)

	while(TRUE){
		tmp=which(pos!=0, arr.ind=TRUE)
		if(nrow(tmp)==0)break
		ans=search(tmp[1,1],tmp[1,2],list(pos=pos,x=numeric(0),y=numeric(0)))
		ansx=c(ansx,ans[["x"]],NA)
		ansy=c(ansy,ans[["y"]],NA)
		pos=ans[["pos"]]
	}

	return(list(ansx,ansy))
}

