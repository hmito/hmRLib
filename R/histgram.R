#' Translate axis of histgram sequence into another axis
#' @description This function translate the histgram sequence with a certain axis into another histgram sequence with a different axis.
#' @param src.hist Source histgram sequence of float values.
#' @param src.ax Source histgram axis. The length is length(src.hist)+1.
#' @param trg.ax Target histgram axis.
#' @return Target histgram sequence of float values.
#' @examples
#' x = c(1,2,3)
#' x.ax = c(0,1,2,3)
#' y.ax = c(0,0.5,1,1.5,2,2.5)
#' hist.axis_translate(x,x.ax,y.ax)
#' # c(0.5,0.5,1,1,1.5)
#' @export
hist.axis_translate=function(src.hist,src.ax,trg.ax){
	src.ln = length(src.ax)-1
	trg.ln = length(trg.ax)-1

	trg1 = matrix(rep(trg.ax[-length(trg.ax)],times=src.ln),trg.ln,src.ln)
	trg2 = matrix(rep(trg.ax[-1],times=src.ln),trg.ln,src.ln)

	src1 = matrix(rep(src.ax[-length(src.ax)],each=trg.ln),trg.ln,src.ln)
	src2 = matrix(rep(src.ax[-1],each=trg.ln),trg.ln,src.ln)

	dif = ifelse(trg2<src2,trg2,src2) - ifelse(trg1>src1,trg1,src1)

	dif[dif<0]=0
	dif=dif/(src2-src1)

	return(dif%*%src.hist)
}
