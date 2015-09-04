##########################################
#===adaptive_dynamics===
#AD関連の便利関数群
#
#adaptive_dynamisc:v1_00/130612 hmIto
# 関数群を移動する形で完成
#
##########################################
source("bind.R")
source("functional.R")
source("virtual_differential.R")

function.canInvade=function(W){function(y,x){W(y,x)-W(x,x)>0}}
function.canCoexist=function(W){function(y,x){(W(y,x)-W(x,x)>0)&(W(x,y)-W(y,y)>0)}}
function.dWdt=function(W){
  linkage_1st2nd( virtual_differential_1st(W) )
}
function.isCS=function(W){
  function(x,...){virtual_differential_1st(linkage_1st2nd(virtual_differential_1st(W)))(x,...)<0}
}
function.isES=function(W){
  function(x,...){linkage_1st2nd(virtual_2nd_differential_1st(W))(x,...)<0}
}
function.isNSS=function(W){
  function(x,...){!function.isCS(W)(x,...)}
}
function.isCSS=function(W){
  function(x,...){function.isCS(W)(x,...)&function.isES(W)(x,...)}
}
function.isBSS=function(W){
  function(x,...){function.isCS(W)(x,...)&!function.isES(W)(x,...)}
}
