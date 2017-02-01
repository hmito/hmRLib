##############################
#graphics.R v1_00
#v1_00
#	下記関数を追加
#		plot.null
#		virtual_curve
#		fill.interval
#		limited_image, limited_lines
#		multi_graph, multi_graph.double
#		vectormap, vectormap.color
##############################

#画面だけ表示するためのplot
plot.null=function(...){plot(0,0,type="n",...)}

#陰関数を疑似的に描画する
virtual_curve=function (expr, from = NULL, to = NULL, n = 101, add = FALSE, 
    type = "l", ylab = NULL, log = NULL, xlim = NULL, ...){
    sexpr <- substitute(expr)
    fcall <- paste(sexpr, "(x)")
    if (is.null(ylab)){
        ylab <- fcall
    }
    if (is.null(xlim)) 
        delayedAssign("lims", {
            pu <- par("usr")[1L:2L]
            if (par("xaxs") == "r") 
                pu <- extendrange(pu, f = -1/27)
            if (par("xlog")) 
                10^pu
            else pu
        })
    else lims <- xlim
    if (is.null(from)) 
        from <- lims[1L]
    if (is.null(to)) 
        to <- lims[2L]
    lg <- if (length(log)) 
        log
    else paste(if (add && par("xlog")) 
        "x", if (add && par("ylog")) 
        "y", sep = "")
    if (length(lg) == 0) 
        lg <- ""
    x <- if (lg != "" && "x" %in% strsplit(lg, NULL)[[1L]]) {
        if (any(c(from, to) <= 0)) 
            stop("'from' and 'to' must be > 0 with log=\"x\"")
        exp(seq.int(log(from), log(to), length.out = n))
    }
    else seq.int(from, to, length.out = n)
#    y=numeric(length(x))
#    for(i in 1:length(x))y[i] <- eval(expr, envir = list(x = x[i]), enclos = parent.frame())
    y=sapply(x,expr)
    if (add) 
        lines(x, y, type = type, ...)
    else plot(x, y, type = type, ylab = ylab, xlim = xlim, log = lg, 
        ...)
    invisible(list(x = x, y = y))
}
#expr1とexpr2に挟まれた領域を塗りつぶす
fill.interval=function (expr1, expr2, from = NULL, to = NULL, n = 101, add = FALSE, 
    type = "l", ylab = NULL, log = NULL, xlim = NULL, ...){
    sexpr1 <- substitute(expr1)
    if (is.name(sexpr1)) {
        fcall1 <- paste(sexpr1, "(x)")
        expr1 <- parse(text = fcall1)
        ylab1 <- fcall1
    }else {
        if (!((is.call(sexpr1) || is.expression(sexpr1)) && match("x", 
            all.vars(sexpr1), nomatch = 0L))) 
            stop("'expr1' must be a function, call or an expression containing 'x'")
        expr1 <- sexpr1
        ylab1 <- deparse(sexpr1)
    }
    sexpr2 <- substitute(expr2)
    if (is.name(sexpr2)) {
        fcall2 <- paste(sexpr2, "(x)")
        expr2 <- parse(text = fcall2)
        ylab2 <- fcall2
    }else {
        if (!((is.call(sexpr2) || is.expression(sexpr2)) && match("x", 
            all.vars(sexpr2), nomatch = 0L))) 
            stop("'expr2' must be a function, call or an expression containing 'x'")
        expr2 <- sexpr2
        ylab2 <- deparse(sexpr2)
    }
    if (is.null(ylab)) 
        ylab <- paste(ylab1,ylab2,sep="_")
    if (is.null(xlim)) 
        delayedAssign("lims", {
            pu <- par("usr")[1L:2L]
            if (par("xaxs") == "r") 
                pu <- extendrange(pu, f = -1/27)
            if (par("xlog")) 
                10^pu
            else pu
        })
    else lims <- xlim
    if (is.null(from)) 
        from <- lims[1L]
    if (is.null(to)) 
        to <- lims[2L]
    lg <- if (length(log)) 
        log
    else paste(if (add && par("xlog")) 
        "x", if (add && par("ylog")) 
        "y", sep = "")
    if (length(lg) == 0) 
        lg <- ""
    x <- if (lg != "" && "x" %in% strsplit(lg, NULL)[[1L]]) {
        if (any(c(from, to) <= 0)) 
            stop("'from' and 'to' must be > 0 with log=\"x\"")
        exp(seq.int(log(from), log(to), length.out = n))
    }
    else seq.int(from, to, length.out = n)
    y1=numeric(length(x))
    for(i in 1:length(x))y1[i] <- eval(expr1, envir = list(x = x[i]), enclos = parent.frame())
    y2=numeric(length(x))
    for(i in 1:length(x))y2[i] <- eval(expr2, envir = list(x = x[i]), enclos = parent.frame())
    if (!add)plot.null(ylab = ylab, xlim = xlim, log = lg, ...) 

    sep=c(((is.na(y1)|is.na(y2))|(y1<y2)),TRUE)
    sep[is.na(sep)]=TRUE

    if(sep[1]){
        Last=NA
    }else{
        Last=1
    }
    for(i in 1:(length(sep)-1)){
        if(sep[i+1]){            
            if(!is.na(Last)){
                polygon(c(x[Last:i],x[i:Last]),c(y1[Last:i],y2[i:Last]), ...)
                Last=NA
            }
        }else{
            if(is.na(Last))Last=i
        }
    }
    invisible(list(x = c(x,rev(x)),y = c(y1,rev(y2))))
}
#条件に合う領域のみ描画
limited_image=function(f,limit=NULL,num=400,xlim=c(0,1),ylim=c(0,1),col=rgb(0,0,0,1),add=FALSE,...){
	#基本となる探索x,yシーケンス作成
	x=seq(xlim[1],xlim[2],length=num)
	y=seq(ylim[1],ylim[2],length=num)

	#侵入可能エリアz1,共存可能エリアz2を計算
	z=matrix(mapply(f,rep(x,times=num),rep(y,each=num)),num,num)
	
	#制限されていない領域のみ描画
	if(!is.null(limit)){
		limit_area=matrix(mapply(limit,rep(x,times=num),rep(y,each=num)),num,num)
		z=z&limit_area
	}
	
	#共存域について描画
	image(x,y,z,col=c(rgb(1.,1.,1.,0.),col),add=add,...)
}
#条件に合う線のみ描画
limited_lines=function(x,y,limit=NULL,...){
	#制限されていない領域のみ描画
	if(!is.null(limit)){
		tmpx=x
		tmpy=y
		limit_area=mapply(limit,tmpx,tmpy)
		x=numeric(0)
		y=numeric(0)
		for(i in 1:length(limit_area)){
			if(is.na(limit_area[i])){
				x=c(x,NA)
				y=c(y,NA)
			}else if(limit_area[i]==FALSE){
				if(i>1 && (!is.na(limit_area[i-1])) && limit_area[i-1]==TRUE){
					x=c(x,NA)
					y=c(y,NA)
				}
			}else{
				x=c(x,tmpx[i])
				y=c(y,tmpy[i])
			}
		}
	}
	
	#描画
	lines(x,y,...)
  
  return(length(x[!is.na(x)]))
}
#パラメータを受け取って多重グラフを生成する
#x方向にx_seq,y方向にy_seqのようにパラメータ変化させたグラフを描画
#prmfuncで引数となるパラメータリスト生成 prmは常にprmfuncに引数として配置される
multi_graph=function(func,prmfunc,x_seq,y_seq,prm=NULL){
	par(mfrow=c(length(y_seq),length(x_seq)))
	for( dy in rev(y_seq)){
		for( dx in x_seq){
			if(is.null(prm)){
				func(prmfunc(dx,dy))
			}else{
				func(prmfunc(dx,dy,prm))
			}
		}
	}
}
#パラメータを受け取って多重グラフを生成する
#x方向にx_seq,y方向にy_seqのようにパラメータ変化させたグラフを描画
#prmfuncで引数となるパラメータリスト生成 prmは常にprmfuncに引数として配置される
multi_graph.double=function(func1,prmfunc1,func2,prmfunc2,x_seq,y_seq,prm1,prm2){
	par(mfrow=c(length(y_seq),length(x_seq)*2))
	for( dy in y_seq){
		for( dx in x_seq){
			func1(prmfunc1(dx,dy,prm1))
			func2(prmfunc2(dx,dy,prm2))
		}
	}
}
#ベクトルマップを描く
vectormap=function(dxf,dyf,length,sublength,xlim=c(0,1),ylim=c(0,1),num=20,col="black",limit=NULL,add=FALSE,...){
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
				x_seq[nx]-dx[nx,ny]/2*length,
				y_seq[ny]-dy[nx,ny]/2*length,
				x_seq[nx]+dx[nx,ny]/2*length,
				y_seq[ny]+dy[nx,ny]/2*length,
				col=col,
				length=sublength,
				...
			)
		}
	}
}
#ベクトルマップを描く
vectormap.color=function(dxf,dyf,colrate,length=NA,sublength=0.25,xyrate=1.0,xlim=c(0,1),ylim=c(0,1),num=20,col=rev(heat.colors(100)),limit=NULL,add=FALSE,...){
	if(is.na(length))length=0.6
	
	yweight=(xlim[2]-xlim[1])/(ylim[2]-ylim[1])
	xlength=(xlim[2]-xlim[1])/(num-1)*length/2
	ylength=(ylim[2]-ylim[1])/(num-1)*length/2*yweight*xyrate
	
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
				length=sublength,
				...
			)
		}
	}
}
#PIPを描く（分岐後解析付）
imagePIP=function(f,col=c(rgb(0.6,0.85,0.6,1),rgb(0.1,0.65,0.3,1)),...){
	#侵入可能性
	can_invade=function(y,x){
		f(y,x)>f(y,y)
	}
	#共存可能性
	can_coexist=function(y,x){
		can_invade(x,y) && can_invade(y,x)
	}
	plot.null(...)
	limited_image(can_invade,col=col[1],add=TRUE)
	limited_image(can_coexist,col=col[2],add=TRUE)
}
#赤青の正負表示用色を返す
rb.colors=function(n,low=0.25,high=0.90,interval=FALSE){
	if(interval){
		if(n%%2==0)n=n+1
		return(c(rgb(1,seq(1-high,1-low,length=n/2),seq(1-high,1-low,length=n/2),1),"white",rgb(seq(1-low,1-high,length=n/2),seq(1-low,1-high,length=n/2),1,1)))
	}else{
		if(n%%2==1)n=n+1
		return(c(rgb(1,seq(1-high,1-low,length=n/2),seq(1-high,1-low,length=n/2),1),rgb(seq(1-low,1-high,length=n/2),seq(1-low,1-high,length=n/2),1,1)))
	}
}
#寒色系のheat.colors
cold.colors=function (n, val.white=0.2,rate.white=0.4) {
	hsv(seq(3/6,4/6,length=n),c(seq(val.white,1.0,length=round(n*rate.white)),rep(1.0,length=n-round(n*rate.white))),1)
}
cold.colors.max=function(){rgb(0,0,1,1)}

green.colors=function (n, val.white=0.2,rate.white=0.5,rate.dark=0.5) {
	hsv(seq(1/6,2/6,length=n),c(seq(val.white,1.0,length=round(n*rate.white)),rep(1.0,length=n-round(n*rate.white))),seq(1.0,rate.dark,length=n))
}
green.colors.max=function(rate.dark=0.5){hsv(2/6,1,rate.dark)}

