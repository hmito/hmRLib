#' Return vector of the mid points of the argument
#' @description Create the sequence of the mid points of vector, i.e., (x[-1]+x[-length(x)])/2
#' @param x terget vector
#' @return vector of mid points
#' @export
#' @examples
#' x = c(0.0,0.2,0.6,1.2)
#' ans = mids(x)
#' # ans == c(0.1,0.4,0.9)
mids = function(x){
	(x[-1]+x[-length(x)])/2
}

#' Return mean value of the probability distribution
#' @description Calculate the mean value of the probability distribution or histgram data.
#' @param x axis value
#' @param pd probability distribution or histgram data
#' @return mean value of the given pd
#' @export
pd.mean = function(x,pd){
	sum(x*pd)/sum(pd)
}

#' Return variance of the probability distribution
#' @description Calculate the variance of the probability distribution or histgram data.
#' @param x axis value
#' @param pd probability distribution or histgram data
#' @return variance of the given pd
#' @export
pd.var = function(x,pd){
	(sum(x*x*pd)/sum(pd)) - (sum(x*pd)/sum(pd))^2
}

#' Return skewness of the probability distribution
#' @description Calculate the skewness of the probability distribution or histgram data.
#' @param x axis value
#' @param pd probability distribution or histgram data
#' @return skewness of the given pd
#' @export
pd.skewness = function(x,pd){
	mean = pd.mean(x,pd)
	var = pd.var(x,pd)
	return((sum(x*x*x*pd)/sum(pd) - 3*mean*sum(x*x*pd)/sum(pd) + 2*mean^3)/var^1.5)
}

#' Return median of the probability distribution
#' @description Calculate the median of the probability distribution or histgram data.
#' @param x axis value
#' @param pd probability distribution or histgram data
#' @return median of the given pd
#' @export
pd.median = function(x,pd){
	cpd = cumsum(pd)/sum(pd)
	ui = (1:length(x))[cpd>0.5][1]
	li = max(1,ui - 1)
	return((x[li]*(cpd[ui]-0.5)+x[ui]*(0.5-cpd[li]))/(cpd[ui]-cpd[li]))
}
