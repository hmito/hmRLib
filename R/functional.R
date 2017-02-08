#operator functions 
operator_plus=function(f1,f2){
	return(
		function(...){f1(...)+f2(...)}
	)
}
operator_minus=function(f1,f2){
	return(
		function(...){f1(...)-f2(...)}
	)
}
operator_multiplies=function(f1,f2){
	return(
		function(...){f1(...)*f2(...)}
	)
}
operator_divides=function(f1,f2){
	return(
		function(...){f1(...)/f2(...)}
	)
}
operator_negate=function(f){
	return(
		function(...){-f(...)}
	)
}
operator_not=function(f){
	return(
		function(...){return(!f(...))}
	)
}
operator_and=function(f1,f2){
	return(
		function(...){return(f1(...)&f2(...))}
	)
}
operator_or=function(f1,f2){
	return(
		function(...){return(f1(...)|f2(...))}
	)
}

#logical functions
is_equal=function(f1,f2){
	return(
		function(...){f1(...)==f2(...)}
	)
}
is_not_equal=function(f1,f2){
	return(
		function(...){f1(...)!=f2(...)}
	)
}
is_greater_or_equal=function(f1,f2){
	return(
		function(...){f1(...)>=f2(...)}
	)
}
is_greater=function(f1,f2){
	return(
		function(...){f1(...)>f2(...)}
	)
}
is_less=function(f1,f2){
	return(
		function(...){f1(...)<f2(...)}
	)
}
is_less_or_equal=function(f){
	return(
		function(...){f1(...)<=f2(...)}
	)
}
is_positive=function(f){
	return(
		function(...){return(f(...)>0)}
	)
}
is_positive_or_zero=function(f){
	return(
		function(...){return(f(...)>=0)}
	)
}
is_negative=function(f){
	return(
		function(...){return(f(...)<0)}
	)
}
is_negative_or_zero=function(f){
	return(
		function(...){return(f(...)<=0)}
	)
}

#argbind functions
argbind_front=function(f,val){return(function(...){return(f(val,...))})}
argbind_back=function(f,val){return(function(...){return(f(...,val))})}
argbind_1st=function(f,val){return(function(...){return(f(val,...))})}
argbind_2nd=function(f,val){return(function(x,...){return(f(x,val,...))})}
argbind_3rd=function(f,val){return(function(x,y,...){return(f(x,y,val,...))})}
argbind_4th=function(f,val){return(function(x,y,z,...){return(f(x,y,z,val,...))})}
argbind = argbind_1st

#arglinkage functions
arglinkage_1st2nd=function(f){return(function(x,...){return(f(x,x,...))})}
arglinkage_1st3rd=function(f){return(function(x,y,...){return(f(x,y,x,...))})}
arglinkage_1st4th=function(f){return(function(x,y,z,...){return(f(x,y,z,x,...))})}
arglinkage_2nd3rd=function(f){return(function(x,y,...){return(f(x,y,y,...))})}
arglinkage_2nd4th=function(f){return(function(x,y,z,...){return(f(x,y,z,y,...))})}
arglinkage_3rd4th=function(f){return(function(x,y,z,...){return(f(x,y,z,z,...))})}
arglinkage = arglinkage_1st2nd

#argswap functions
argswap_1st2nd=function(f){return(function(x,y,...){return(f(y,x,...))})}
argswap_1st3rd=function(f){return(function(x,y,z,...){return(f(z,y,x,...))})}
argswap_1st4th=function(f){return(function(x,y,z,w,...){return(f(w,y,z,x,...))})}
argswap_2nd3rd=function(f){return(function(x,y,z,...){return(f(x,z,y,...))})}
argswap_2nd4th=function(f){return(function(x,y,z,w,...){return(f(x,w,z,y,...))})}
argswap_3rd4th=function(f){return(function(x,y,z,w,...){return(f(x,y,w,z,...))})}
argswap = argswap_1st2nd
