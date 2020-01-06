#' Return index numbers of the target sequence which is matching to the obj,
#' @description Find index number at which trg is equal to obj. Return 0 if it is not found in trg, and return NA if it has been multitmes found.
#' @param from Value, vector or matrix
#' @param trg Target sequence
#' @return Found once: Index numbers, Found multiple times: NA, Didn't find: 0
#' @export
#' @examples
#' x = c(115,103,99,120)
#' trg = c(1:50 + 100,120)
#' match.order(x,trg)
#' # c(15,3,0,NA)
match.order=function(from,trg){
	err = (from!=from)
	ans = 0*err
	for(i in 1:length(trg)){
		lis = (trg[i]==from)
		err = err | (ans>0 & lis)
		ans[lis] = i
	}
	ans[err]=NA
	return(ans)
}

#' Return reduced value by the given target sequence.
#' @description Find values at which trg is equal to from.
#' @param from Value, vector or matrix
#' @param fromval Value, vector or matrix
#' @param trg Finding target sequence
#' @param ini initial values of trg
#' @param func function for reduce (default=sum)
#' @export
match.order.reduce = function(from,fromval,trg,ini=0,func=sum){
	fromno = match.order(from,trg)
	trgval = rep(ini,length(trg))
	for(i in 1:length(trg)){
		trgval[i] = func(fromval[fromno == i])
	}
	return(trgval)
}
