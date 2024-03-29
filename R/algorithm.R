#' Return y if x satisfies given condition, otherwise return x
#' @description Return y if x satisfies given condition, otherwise return x
#' @param x target value
#' @param y alternatively returned value
#' @param condition a function return logical values for each x
#' @return x or y, whose length is euqal to length_x
#' @export
either = function(x,y,condition = is.na){
	ifelse(condition(x),y,x)
}
#' Return y if x does not exist, otherwise return x
#' @description Return y if x does not exist, otherwise return x
#' @param x target value
#' @param y alternatively returned value
#' @return x or y
#' @export
ensure = function(x,y){
	x  = substitute(x)
	if(is.null(eval(x)))return(y)
	return(eval(x))
}
#' Return list of index of from which is matched to the argument
#' @description Return list of index of from which is matched to the argument
#' @param what value of sequence for finding
#' @param from index of this argument is returned
#' @param condition condition for finding. Default is "=="
#' @return List of index which is found in from at each what
#' @export
#' @examples
#' find_all(c(1,3,5), c(0,1,2,3,3))
#' # list(2,c(4,5),integer(0))
find_all = function(what,from,condition=`==`){
	purrr::map(what,function(y){which(condition(y,from))})
}
#' Return index of from which is firstly matched to the argument
#' @description Find first index number in from at which is equal to what.
#' @param what value of sequence for finding
#' @param from index of this argument is returned
#' @param condition condition for finding. Default is "=="
#' @return List of index which is found in from. NA if it is not found.
#' @export
#' @examples
#' find_first(c(1,3,5), c(0,1,2,3,3))
#' # c(2,4,NA)
find_first = function(what,from,condition=`==`){
	purrr::map_int(what,function(y){z = which(condition(y,from)); ifelse(length(z),z[1],NA)})
}
#' Return index of from which is uniquely matched to the argument
#' @description Find unique index number in from at which is equal to what.
#' @param what value of sequence for finding
#' @param from index of this argument is returned
#' @param condition condition for finding. Default is "=="
#' @return List of index which is found in from. NA if it is not found, or multiple index are found.
#' @export
#' @examples
#' find_unique(c(1,3,5), c(0,1,2,3,3))
#' # c(2,NA,NA)
find_unique = function(what,from,condition=`==`){
	purrr::map_int(what,function(y){z = which(condition(y,from)); ifelse(length(z)==1,z,NA)})
}
#' Return counted number which is matched to the argument
#' @description Count what in from
#' @param what value of sequence for finding
#' @param from index of this argument is returned
#' @param condition condition for finding. Default is "=="
#' @return List of count number.
#' @export
#' @examples
#' count(c(1,3,5), c(0,1,2,3,3))
#' # c(1,2,0)
count = function(what,from,condition=`==`){
	purrr::map_int(what,function(y){sum(condition(y,from))})
}
#' Return list of row in fromMD in any of which is matched to what.
#' @description Return list of row in fromMD in any of which is matched to what.
#' @param what value of sequence for finding
#' @param fromMD multi-dimensional target.
#' @param condition condition for finding. Default is "=="
#' @return List of row which is found in from at each what
#' @export
#' @examples
#' find_all_row(c(1,3,5), matrix(c(0,0,1,2,0,0,3,0,0,3,4,4),ncol=2,byrow=TRUE))
#' # list(2,c(4,5),integer(0))
find_all_row = function(what,fromMD,condition=`==`){
	purrr::map(what,function(y){which(apply(condition(y,fromMD),1,any))})
}
#' Return list of first row in fromMD in any of which is matched to what.
#' @description Return list of row in fromMD in any of which is matched to what.
#' @param what value of sequence for finding
#' @param fromMD multi-dimensional target.
#' @param condition condition for finding. Default is "=="
#' @return List of first row which is found in from at each what. NA if it is not found.
#' @export
#' @examples
#' find_first_row(c(1,3,5), matrix(c(0,0,1,2,0,0,3,0,0,3,4,4),ncol=2,byrow=TRUE))
#' #' # c(2,4,NA)
find_first_row = function(what,fromMD,condition=`==`){
	purrr::map_int(what,function(y){z = which(apply(condition(y,fromMD),1,any)); ifelse(length(z),z[1],NA)})
}
#' Return list of unique row in fromMD in any of which is matched to what.
#' @description Return list of row in fromMD in any of which is matched to what.
#' @param what value of sequence for finding
#' @param fromMD multi-dimensional target.
#' @param condition condition for finding. Default is "=="
#' @return List of unique row which is found in from at each what. NA if it is not found, or multiple index are found.
#' @export
#' @examples
#' find_unique_row(c(1,3,5), matrix(c(0,0,1,2,0,0,3,0,0,3,4,4),ncol=2,byrow=TRUE))
#' #' # c(2,NA,NA)
find_unique_row = function(what,fromMD,condition=`==`){
	purrr::map_int(what,function(y){z = which(apply(condition(y,fromMD),1,any)); ifelse(length(z)==1,z,NA)})
}
#' Return counted number which is matched to the any argument in each row.
#' @description Count what in from
#' @param what value of sequence for finding
#' @param fromMD index of this argument is returned
#' @param condition condition for finding. Default is "=="
#' @return List of count number.
#' @export
#' @examples
#' count_row(c(1,3,5), matrix(c(0,0,1,2,0,0,3,0,0,3,4,4),ncol=2,byrow=TRUE))
#' # c(1,2,0)
count_row = function(what,fromMD,condition=`==`){
	purrr::map_int(what,function(y){sum(apply(condition(y,fromMD),1,any))})
}

#' <Deprecated> Return index numbers of the target sequence which is matching to the obj,
#' @description Find index number at which trg is equal to obj. Return 0 if it is not found in trg, and return NA if it has been multitmes found.
#' @param from Value, vector or matrix
#' @param trg Target sequence
#' @return Found once: Index numbers, Found multiple times: NA, Didn't find: 0
#' @export
match.order=function(from,trg){
	.Deprecated("find_unique")
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
#' <Deprecated> Return reduced value by the given target sequence.
#' @description Find values at which trg is equal to from.
#' @param from Value, vector or matrix
#' @param fromval Value, vector or matrix
#' @param trg Finding target sequence
#' @param ini initial values of trg
#' @param func function for reduce (default=sum)
#' @export
match.order.reduce = function(from,fromval,trg,ini=0,func=sum){
	.Deprecated("find_unique and map_int")
	fromno = match.order(from,trg)
	trgval = rep(ini,length(trg))
	for(i in 1:length(trg)){
		trgval[i] = func(fromval[fromno == i])
	}
	return(trgval)
}

#' Replace elements by other values
#' @param target target vector, matrix or other types
#' @param from the element values which should be replaced
#' @param to the values which should be set for the replaced elements
#' @param other if it is not NULL value, the target elements which is not hit to any of from values are replaced by other.
#' @return replaced results
#' @export
replace_by = function(target, from, to, other=NULL){
	if(length(to)!=1 & length(to)!=length(from))	warning('in replace_by: "from" and "to" have different length.')
	to = rep(to,length=length(from))
	ok = target == target
	if(!is.null(other)){
		if(is.vector(target)){
			ans = rep(other,length=length(target))
		}else if(is.matrix(target)){
			ans = matrix(other, nrow=nrow(target),ncol=ncol(target))
		}else{
			stop('in replace_by: target should be vector or matrix if other is defined.')
		}
	}else{
		ans = target
	}
	for(i in 1:length(from)){
		pos = target==from[i]
		ans[pos & ok]=to[i]
		ok = ok & !pos
	}

	return(ans)
}
#' Select elements depending on thresholds
#' @param target target vector, matrix or other types
#' @param thresholds the threshold values (should be sorted)
#' @param to the values which should be set depending on the thresholds
#' @return replaced results
#' @export
replace_by_threshold = function(target, thresholds, to){
	if(length(to)!=length(thresholds)+1)	warning('in replace_by_threshold: the length of "to" should be equal to the length of "threshold" + 1.')
	to = rep(to,length=length(thresholds)+1)
	if(is.vector(target)){
		ans = rep(to[1],length=length(target))
	}else if(is.matrix(target)){
		ans = matrix(to[1], nrow=nrow(target),ncol=ncol(target))
	}else{
		stop('in replace_by_threshold: target should be vector or matrix if other is defined.')
	}
	for(i in 1:length(thresholds)){
		ans[target >= thresholds[i]] = to[i+1]
	}
	return(ans)
}
