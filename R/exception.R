#' Create callstack data at last error
#' @description Create callstack data from .traceback function, which return the call stack at the last error
#' @param IgnoreLevel Number of callstack data level which should be ignored from return value
#' @return callstack dataframe with name, line and filename.
#' @export
callstack_trace = function(IgnoreLevel=0){
	calls = .traceback()
	names = as.character(calls)
	lines = sapply(calls, function(v) if (!is.null(srcref <- attr(v, "srcref"))) {srcref[1L]}else"")
	files = sapply(calls, function(v) if (!is.null(srcref <- attr(v, "srcref"))) {
		srcfile <- attr(srcref, "srcfile")
		return(srcfile$filename)
	}else "")

	if(length(names)!=length(lines)){
		lines = rep(-1,length(names))
	}
	if(length(names)!=length(files)){
		files = rep("",length(names))
	}

	if(length(names)<1+IgnoreLevel){
		last = NULL
	}else{
		last = length(names):(1+IgnoreLevel)
	}
	return(data.frame(name=names[last],line=lines[last],file=files[last]))
}

#' Create callstack data at the point this function is called
#' @description Create callstack data from sys.calls function, which return the current call stack
#' @param IgnoreLevel Number of callstack data level which should be ignored from return value
#' @return callstack dataframe with name, line and filename.
#' @export
callstack_info = function(IgnoreLevel=0){
	calls = sys.calls()
	names = as.character(calls)
	lines = sapply(calls, function(v) if (!is.null(srcref <- attr(v, "srcref"))) {srcref[1L]}else"")
	files = sapply(calls, function(v) if (!is.null(srcref <- attr(v, "srcref"))) {
		srcfile <- attr(srcref, "srcfile")
		return(srcfile$filename)
	}else "")

	last = (length(calls)-IgnoreLevel):length(calls)
	return(data.frame(name=names[-last],line=lines[-last],file=files[-last]))
}

#' Create callstack message from callstack data
#' @description Create callstack message which include the information of callstack data.
#' @param message Header mesasge
#' @param stack callstack data
#' @return message with callstack data
#' @export
callstack_message = function(message,stack){
	if(is.null(stack)){
		return(message)
	}
	if(!is.data.frame(stack)){
		return(message)
	}
	if(nrow(stack)<1){
		return(message)
	}
	for(i in nrow(stack):1){
		name = gsub("\n","",as.character(stack[i,1]))
		if(nchar(name)>50){
			name = paste0(substr(name,1,47),"...")
		}
		if(stack[i,3]!=""){
			message = paste0(message,"\n\tin ",stack[i,3], "[",stack[i,2],"]: ",name)
		}else{
			message = paste0(message,"\n\tin ",name)
		}
		if(i != 1){
			if(stack[i,3]=="" && stack[i,2]!="" ){
				message = paste0(message," in function ",gsub("\\(.*", "", stack[i-1,1]),"#",stack[i,2])
			}else{
				message = paste0(message," in function ",gsub("\\(.*", "", stack[i-1,1]))
			}
		}
	}
	return(paste0(message,"\n"))
}

#' Create warning message from warnings data
#' @description Create warning message which include the information of warnings data.
#' @param message Header mesasge
#' @return message with warnings data
#' @export
warnings_message = function(message){
	return(paste0(
		paste0(
			c(
				message,
				paste0("[",warnings(),"] ",names(warnings()))
			),
			collapse = "\n\t"
		),
		"\n"
	))
}

#' Show callstack info at last error
#' @description print callstack info when the last error occured.
#' @export
show_callstack_trace=function(){
	stack = callstack_trace()
	cat(callstack_message(">>> call stack",stack))
}

#' Check whether the argument is exception
#' @description Check whether the argument is exception
#' @param arg target argument.
#' @return Logical; TRUE:exception, FALSE: otherwise.
#' @export
is_exception=function(arg){
	inherits(arg,"try-error")
}

#' Throw exception
#' @description Throw exception with message.
#' @param message message of exception
#' @param IgnoreThrower Logical: if true, the callstack data don't include the function it call throw.
#' @export
throw=function(message,IgnoreThrower=FALSE){
	stack = callstack_info(IgnoreThrower*1)
	message = callstack_message(message,stack)
	stop(message,call.=FALSE)
}

#' Assert
#' @description Assert: throw exception if the condition is not satisfied.
#' @param condition Condition for assert.
#' @param message Exception message if the condition is not satisfied.
#' @export
assert=function(condition,message="assert error"){
	if(condition==FALSE){
		throw(message,TRUE)
	}
}

#' Try execute function which might throw exception.
#' @description Try execute function which might throw exception.
#' @param expr Function which should be executed.
#' @param silent Logical value: if TRUE, the error message is not shown.
#' @return return value if expr don't throw, or exception if it throw.
#' @export
try_ = function(expr, silent=TRUE){
	ans = base::try(eval(expr),silent=silent)
	if(inherits(ans,"try-error")){
		stack = callstack_trace()
		ans = paste0(ans[1],callstack_message(">>> call stack",stack),warnings_message(">>> warnings"))
		class(ans)="try-error"
	}
	return(ans)
}

#' Try excute function which might throw exception.
#' @description Try excute function which might throw exception.
#' @param expr Function which should be excuted.
#' @param catchfn Function which will be called when expr throw exception with error message as argument.
#' @param silent Logical value: if TRUE, the error message is not shown.
#' @return return value if expr don't throw, or exception if it throw.
#' @export
try_catch = function(expr,catchfn, silent=TRUE){
	ans = base::try(eval(expr),silent=silent)
	if(inherits(ans,"try-error")){
		stack = callstack_trace()
		ans = paste0(ans[1],callstack_message(">>> call stack",stack),warnings_message(">>> warnings"))
		class(ans)="try-error"
		catchfn(ans)
	}
	return(ans)
}

#' Try and catch_either.
#' @description Try and catch_either.
#' @param expr Function which should be executed.
#' @param alt_value return value when expr throw exception.
#' @param catchfn Function which will be called when expr throw exception with error message as argument.
#' @param silent Logical value: if TRUE, the error message is not shown.
#' @examples
#' f = function(i){
#'    if(i<0)stop("negative value")
#'    return(i)
#' }
#' x = try_either(f(3),NULL)
#' #x == 3
#' a = try_either(f(-1),NULL)
#' #a == NULL
#' b = try_either(f(-1),0)
#' #b == 0
#' @export
try_either = function(expr,alt_value,catchfn=NULL,silent=TRUE){
	ans = base::try(eval(expr),silent=silent)
	if(inherits(ans,"try-error")){
		if(!is.null(catchfn)){
			stack = callstack_trace()
			message = paste0(ans[1],callstack_message(">>> call stack",stack),warnings_message(">>> warnings"))
			class(message)="try-error"
			catchfn(message)
		}
		ans=alt_value
	}
	return(ans)
}


#' Catch and execute function.
#' @description Try execute function which might throw exception.
#' @param value Value which might be exception.
#' @param catchfn Function which will be called when expr throw exception with error message as argument.
#' @return return value if expr don't throw, or exception if it throw.
#' @export
catch = function(value, catchfn){
	if(inherits(value,"try-error")){
		catchfn(value)
	}
	return(invisible(value))
}

#' Try excute function which might throw exception.
#' @description Try execute function which might throw exception.
#' @param value Value which might be exception.
#' @param alt_value return value when expr throw exception.
#' @param catchfn Function which will be called when expr throw exception with error message as argument.
#' @return return value if expr don't throw, or exception if it throw.
#' @export
catch_either = function(value, alt_value, catchfn=NULL){
	if(inherits(value,"try-error")){
		if(!is.null(catchfn)){
			stack = callstack_trace()
			message = paste0(ans[1],callstack_message(">>> call stack",stack),warnings_message(">>> warnings"))
			class(message)="try-error"
			catchfn(message)
		}
		value=alt_value
	}
	return(invisible(value))
}


