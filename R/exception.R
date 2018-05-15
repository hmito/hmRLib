#' Try excute function which might throw exception.
#' @description Try excute function which might throw exception.
#' @param expr Function which should be excuted.
#' @param fail_ans return value when expr throw exception.
#' @param silent Logical value: if TRUE, the error message is not shown.
#' @param catch Function which will be called when expr throw exception with error message as argument.
#' @examples
#' f = function(i){
#'    if(i<0)stop("negative value")
#'    return(i)
#' }
#' x = try_either(f(3))
#' #x == 3
#' a = try_either(f(-1))
#' #a == NULL
#' b = try_either(f(-1),0)
#' #b == 0
#' @export
try_either = function(expr,fail_ans=NULL,silent=FALSE,catch=NULL){
	ans = try(eval(expr),silent=silent)
	if(class(ans)=="try-error"){
		if(!is.null(catch)){
			catch(ans)
		}
		ans=fail_ans
	}
	return(ans)
}
