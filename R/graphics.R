#' Draw plot without any data
#' @description This function just call plot function with parameter type = "n".
#' @param ... Arguments are passed to function plot.
#' @importFrom graphics plot
#' @export
plot.null=function(...){plot(0,0,type="n",...)}


#陰関数を疑似的に描画する
#' @importFrom grDevices extendrange
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
#' @importFrom grDevices extendrange
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
