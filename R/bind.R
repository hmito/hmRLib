##############################
#bind_v1_02.R
#	bind_1st-4thのバグを修正
#bind_v1_01.R
#	bind_front,bind_backを追加
#	bind_1st,bind_2nd,bind_3rdを追加
#	linkage4シリーズ追加
#bind_v1_00.R
#	bindシリーズ追加
#	linkageシリーズ追加
#	swapシリーズ追加
#	rotateシリーズ追加
##############################

#bind関数群
bind_front=function(f,val){return(function(...){return(f(val,...))})}
bind_back=function(f,val){return(function(...){return(f(...,val))})}
bind_1st=function(f,val){return(function(...){return(f(val,...))})}
bind_2nd=function(f,val){return(function(x,...){return(f(x,val,...))})}
bind_3rd=function(f,val){return(function(x,y,...){return(f(x,y,val,...))})}
bind_4th=function(f,val){return(function(x,y,z,...){return(f(x,y,z,val,...))})}
bind2_1st=function(f,val){return(function(x){return(f(val,x))})}
bind2_2nd=function(f,val){return(function(x){return(f(x,val))})}
bind3_1st=function(f,val){return(function(x,y){return(f(val,x,y))})}
bind3_2nd=function(f,val){return(function(x,y){return(f(x,val,y))})}
bind3_3rd=function(f,val){return(function(x,y){return(f(x,y,val))})}
bind3_1st2nd=function(f,val){return(function(x){return(f(val,val,x))})}
bind3_1st3rd=function(f,val){return(function(x){return(f(val,x,val))})}
bind3_2nd3rd=function(f,val){return(function(x){return(f(x,val,val))})}
bind3_1st_2nd=function(f,val1,val2){return(function(x){return(f(val1,val2,x))})}
bind3_1st_3rd=function(f,val1,val2){return(function(x){return(f(val1,x,val2))})}
bind3_2nd_3rd=function(f,val1,val2){return(function(x){return(f(x,val1,val2))})}
bind4_1st=function(f,val){return(function(x,y,z){return(f(val,x,y,z))})}
bind4_2nd=function(f,val){return(function(x,y,z){return(f(x,val,y,z))})}
bind4_3rd=function(f,val){return(function(x,y,z){return(f(x,y,val,z))})}
bind4_4th=function(f,val){return(function(x,y,z){return(f(x,y,z,val))})}
#linkage関数群
linkage_1st2nd=function(f){return(function(x,...){return(f(x,x,...))})}
linkage_1st3rd=function(f){return(function(x,y,...){return(f(x,y,x,...))})}
linkage_2nd3rd=function(f){return(function(x,y,...){return(f(x,y,y,...))})}
linkage2_1st2nd=function(f){return(function(x){return(f(x,x))})}
linkage3_1st2nd_1st=function(f){return(function(x,y){return(f(x,x,y))})}
linkage3_2nd3rd_1st=function(f){return(function(x,y){return(f(y,x,x))})}
linkage3_1st3rd_1st=function(f){return(function(x,y){return(f(x,y,x))})}
linkage3_1st2nd_2nd=function(f){return(function(x,y){return(f(y,y,x))})}
linkage3_2nd3rd_2nd=function(f){return(function(x,y){return(f(x,y,y))})}
linkage3_1st3rd_2nd=function(f){return(function(x,y){return(f(y,x,y))})}
linkage3_1st2nd3rd=function(f){return(function(x){return(f(x,x,x))})}
linkage4_1st2nd_1st=function(f){return(function(x,y,z){return(f(x,x,y,z))})}
linkage4_1st3rd_1st=function(f){return(function(x,y,z){return(f(x,y,x,z))})}
linkage4_1st4th_1st=function(f){return(function(x,y,z){return(f(x,y,z,x))})}
linkage4_2nd3rd_1st=function(f){return(function(x,y,z){return(f(y,x,x,z))})}
linkage4_2nd4th_1st=function(f){return(function(x,y,z){return(f(y,x,z,x))})}
linkage4_3rd4th_1st=function(f){return(function(x,y,z){return(f(y,z,x,x))})}
linkage4_1st2nd_2nd=function(f){return(function(x,y,z){return(f(y,y,x,z))})}
linkage4_1st3rd_2nd=function(f){return(function(x,y,z){return(f(y,x,y,z))})}
linkage4_1st4th_2nd=function(f){return(function(x,y,z){return(f(y,x,z,y))})}
linkage4_2nd3rd_2nd=function(f){return(function(x,y,z){return(f(x,y,y,z))})}
linkage4_2nd4th_2nd=function(f){return(function(x,y,z){return(f(x,y,z,y))})}
linkage4_3rd4th_2nd=function(f){return(function(x,y,z){return(f(x,z,y,y))})}
linkage4_1st2nd_3rd=function(f){return(function(x,y,z){return(f(z,z,x,y))})}
linkage4_1st3rd_3rd=function(f){return(function(x,y,z){return(f(z,x,z,y))})}
linkage4_1st4th_3rd=function(f){return(function(x,y,z){return(f(z,x,y,z))})}
linkage4_2nd3rd_3rd=function(f){return(function(x,y,z){return(f(x,z,z,y))})}
linkage4_2nd4th_3rd=function(f){return(function(x,y,z){return(f(x,z,y,z))})}
linkage4_3rd4th_3rd=function(f){return(function(x,y,z){return(f(x,y,z,z))})}
linkage4_1st2nd3rd_1st=function(f){return(function(x,y){return(f(x,x,x,y))})}
linkage4_1st2nd4th_1st=function(f){return(function(x,y){return(f(x,x,y,x))})}
linkage4_1st3rd4th_1st=function(f){return(function(x,y){return(f(x,y,x,x))})}
linkage4_2nd3rd4th_1st=function(f){return(function(x,y){return(f(y,x,x,x))})}
linkage4_1st2nd3rd_2nd=function(f){return(function(x,y){return(f(y,y,y,x))})}
linkage4_1st2nd4th_2nd=function(f){return(function(x,y){return(f(y,y,x,y))})}
linkage4_1st3rd4th_2nd=function(f){return(function(x,y){return(f(y,x,y,y))})}
linkage4_2nd3rd4th_2nd=function(f){return(function(x,y){return(f(x,y,y,y))})}
linkage4_1st2nd3rd4th=function(f){return(function(x){return(f(x,x,x,x))})}
#swap関数群
swap2_1st2nd=function(f){return(function(x,y){return(f(y,x))})}
swap3_1st2nd=function(f){return(function(x,y,z){return(f(y,x,z))})}
swap3_2nd3rd=function(f){return(function(x,y,z){return(f(x,z,y))})}
swap3_1st3rd=function(f){return(function(x,y,z){return(f(z,y,x))})}
#rotate関数群
rotate3_2nd3st1rd=function(f){return(function(x,y,z){return(f(y,z,x))})}
rotate3_3rd1st2nd=function(f){return(function(x,y,z){return(f(z,x,y))})}
