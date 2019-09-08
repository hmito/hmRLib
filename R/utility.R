#' Return index numbers of the target sequence which is matching to the obj,
#' @description Find index number at which trg is equal to obj. Return 0 if it is not found in trg, and return NA if it has been multitmes found.
#' @param obj Value, vector or matrix
#' @param trg Target sequence
#' @return Found once: Index numbers, Found multiple times: NA, Didn't find: 0
#' @export
#' @examples
#' x = c(115,103,99,120)
#' trg = c(1:50 + 100,120)
#' match.order(x,trg)
#' # c(15,3,0,NA)
match.order=function(obj,trg){
	err = (obj!=obj)
	ans = 0*err
	for(i in 1:length(trg)){
		lis = (trg[i]==obj)
		err = err | (ans>0 & lis)
		ans[lis] = i
	}
	ans[err]=NA
	return(ans)
}
