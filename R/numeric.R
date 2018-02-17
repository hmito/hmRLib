#' Find root of a function using small_step method
#' @description This function returns roots of a focal function by checking the sign change in each step.
#' @param func the function for which the root is sought.
#' @param step a vector containing the step values
#' @param ... Additional argument for "func"
#' @return data.frame with two col, "lower" and "upper" which are the range of the solutions. root
#' @export
#' @examples
#' f = function(x,y){return(x*x-y*x)}
#' ans = solve_small_step(f, seq(-2,2,length=41),y=1.333)
#' # ans == data.frame("lower"=c(0.0,1.3),"upper"=c(0.0,1.4))
small_step_root=function(func,step,...){
	fval=func(step,...)
	pos = (fval[-1]*fval[-length(step)]<0)
	zpos = (fval==0)
	return(data.frame("lower"=c(step[-length(step)][pos],step[zpos]),"upper"=c(step[-1][pos],step[zpos])))
}


#' Find root of a function using combination of small_step method and newton method.
#' @description This function returns roots of a focal function by checking the sign change in each step.
#' @param func the function for which the root is sought.
#' @param step a vector containing the step values
#' @return a vector which containing root values
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


#' Find border of the
border.small_step=function(x,y,z){
	#??????????????????????????????
	if(is.matrix(z)==FALSE){
		return(NA)
	}

	#????????????????????????????????????
	xmax=nrow(z)
	ymax=ncol(z)

	#?????????????????????
	#	xy????????????????????????????????????????????????
	#		(-,-)?????????1,(+,-)?????????2,(+,+)?????????4,(-,+)?????????8
	#		????????????????????????????????????????????????0???15??????????????????????????????????????????????????????
	#	?????????/??????0???15????????????????????????????????????4????????????????????????????????????????????????????????????????????????
	pos=matrix(0,xmax-1,ymax-1)
	pos=1*z[-xmax,-ymax]+2*z[-1,-ymax]+4*z[-1,-1]+8*z[-xmax,-1]
	pos[is.na(pos)]=0
	pos[pos>7]=15-pos[pos>7]

	#??????????????????
	search=function(dx,dy,ans){
		#????????????????????????
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
				#?????????(-,-)?????????????????????????????????
			}else if(ans[["pos"]][dx,dy]==1){
				#??????
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

	return(list(ansx,ansy))
}

#????????????????????????????????????????????????
border.uniroot=function(func,xlim=c(0,1),ylim=c(0,1),n=1001){
	#xy????????????????????????????????????????????????
	#(-,-)?????????1,(+,-)?????????2,(+,+)?????????4,(-,-)?????????8
	#????????????????????????????????????????????????0???15??????????????????????????????????????????????????????
	#?????????/??????0???15????????????????????????????????????4????????????????????????????????????????????????????????????????????????

	x=seq(xlim[1],xlim[2],length=n)
	y=seq(ylim[1],ylim[2],length=n)
	z=outer(x,y,func)
	z=(z>0)
	pos=matrix(0,nrow(z)-1,ncol(z)-1)

	xfunc=function(x1,x2,y){
		f = function(x){func(x,y)}
		if(f(x1)*f(x2)>0){
			if(f(x1)>0)return(x1)
			else return (x2)
		}
		return(uniroot(f,c(x1,x2))[["root"]])
	}
	yfunc=function(x,y1,y2){
		f = function(y){func(x,y)}
		if(f(y1)*f(y2)>0){
			if(f(y1)>0)return(y1)
			else return (y2)
		}
		return(uniroot(f,c(y1,y2))[["root"]])
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

#線分p1-p2とp3-p4が交差しているかの判定
is.cross_segments=function(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y,p4.x,p4.y){
	#x座標によるチェック
	XCheck=rep(TRUE,length=length(p1.x))
	XCheck=!(((p1.x >= p2.x) & ((p1.x < p3.x & p1.x < p4.x) | (p2.x > p3.x & p2.x > p4.x)))|((p1.x < p2.x) & ((p2.x < p3.x & p2.x < p4.x) | (p1.x > p3.x & p1.x > p4.x))))

	#y座標によるチェック
	YCheck=rep(TRUE,length=length(p1.y))
	YCheck=!(((p1.y >= p2.y) & ((p1.y < p3.y & p1.y < p4.y) | (p2.y > p3.y & p2.y > p4.y)))|((p1.y < p2.y) & ((p2.y < p3.y & p2.y < p4.y) | (p1.y > p3.y & p1.y > p4.y))))

	#交差条件チェック
	Cross1Check=rep(TRUE,length=length(p1.x))
	Cross1Check=!(((p1.x - p2.x) * (p3.y - p1.y) + (p1.y - p2.y) * (p1.x - p3.x)) *((p1.x - p2.x) * (p4.y - p1.y) + (p1.y - p2.y) * (p1.x - p4.x)) > 0)

	Cross2Check=rep(TRUE,length=length(p1.x))
	Cross2Check=!(((p3.x - p4.x) * (p1.y - p3.y) + (p3.y - p4.y) * (p3.x - p1.x)) *((p3.x - p4.x) * (p2.y - p3.y) + (p3.y - p4.y) * (p3.x - p2.x)) > 0)

	return(XCheck&YCheck&Cross1Check&Cross2Check)
}

#直線p1-p2とp3-p4の交点
get.cross_segments_point=function(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y,p4.x,p4.y){
	r=((p4.y-p3.y)*(p3.x-p1.x)-(p4.x-p3.x)*(p3.y-p1.y))/((p2.x-p1.x)*(p4.y-p3.y)-(p2.y-p1.y)*(p4.x-p3.x))
	return(c((1-r)*p1.x+r*p2.x,(1-r)*p1.y+r*p2.y))
}
#(xylist[1],xylist[2])となるような点配列の中に、(x,y)と同位置となるような点の数
count.equal_point=function(x,y,xylist){sum(remove.na((xylist[[1]]==x)&(xylist[[2]]==y)))}
#(seq[1],seq[2])となるような点配列の中に、(xseq,yseq)と同位置となるような点を返す
search.equal_point=function(xseq1,yseq1,xseq2,yseq2){
	xseq1.cmp=rep(xseq1,each=length(xseq2))
	yseq1.cmp=rep(yseq1,each=length(yseq2))
	xseq2.cmp=rep(xseq2,times=length(xseq1))
	yseq2.cmp=rep(yseq2,times=length(yseq1))
	acs=((xseq1.cmp==xseq2.cmp)&(yseq1.cmp==yseq2.cmp))
	return(list(xseq1.cmp[acs],yseq1.cmp[acs]))
}

#(seq[1],seq[2])となるような点配列の中に、(xseq,yseq)と同位置となるような点を返す
search.cross_point=function(xseq1,yseq1,xseq2,yseq2){
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
	IsCross=is.cross_segments(pos1.x.cmp,pos1.y.cmp,pos2.x.cmp,pos2.y.cmp,pos3.x.cmp,pos3.y.cmp,pos4.x.cmp,pos4.y.cmp)
	IsCross[is.na(IsCross)]=FALSE

	ans.x=numeric(0)
	ans.y=numeric(0)

	for(i in (1:length(pos1.x.cmp))[IsCross]){
		pair=get.cross_segments_point(pos1.x.cmp[i],pos1.y.cmp[i],pos2.x.cmp[i],pos2.y.cmp[i],pos3.x.cmp[i],pos3.y.cmp[i],pos4.x.cmp[i],pos4.y.cmp[i])
		ans.x=c(ans.x,pair[[1]])
		ans.y=c(ans.y,pair[[2]])
	}

	return(list(ans.x,ans.y))
}
