##############################
#===numeric.R===
#数学的演算を担当する
#
#numeric.R v1_02
# is.cross_segments追加
#   ２つの線分の交差の有無を判定
# get.cross_segments_point追加
#   ２つの線分の交点を計算
# is.cross_segments追加
#   ２つの線分の交差の有無を判定
# get.cross_segments_point追加
#   ２つの線分の交点を計算
# search.equal_point追加
#   二つのxy配列から同値となる解を抜き出す
# search.cross_point追加
#   二つのxy配列曲線から交点を抜き出す
#numeric.R v1_01
# border
#		NAの値を返してきた場合にも、0で上書きするように修正
#numeric.R v1_00
#	solve.small_step, solve.combination
#	border
#	projecting_split
##############################
source("utility.R",TRUE)
#small_step法による高速＆低精度解確定関数
solve.small_step=function(func,range,sepnum=10001){
	xseq=seq(range[1],range[2],length=sepnum)
	yseq=func(xseq)
	postyseq=c(yseq[-1],yseq[length(yseq)])
	ans=xseq[yseq*postyseq<0 | yseq==0]
	return(ans[!is.na(ans)])
}
#hm.search_cross0で見つけた変極点をもとに、解を出す
solve.combination=function(func,range,sepnum=10001){
	Seq=solve.small_step(func,range,sepnum)
	Step=(range[2]-range[1])/sepnum
	Ans=numeric(0)
	if(length(Seq)==0)return(Ans)
	for(i in 1:length(Seq)){
		Ans=c(Ans,uniroot(func,c(Seq[i]-Step,Seq[i]))[["root"]])
	}
	return(Ans)
}
#探索行列生成関数
create_dirmatrix=function(z){
	ans=1*z[-nrow(z),-ncol(z)]+2*z[-1,--ncol(z)]+4*z[-1,-1]+8*z[-nrow(z),-1]
	ans[is.na(ans)]=0
	ans[ans>7]=15-ans[ans>7]
	return(ans)
}
#行列探索関数
search_on_dirmatr=function(x,y,dirmatrix){
	#xy方向の数
	xmax=nrow(dirmatrix)
	ymax=ncol(dirmatrix)
	
	#前回どっちにいたか
	px=x
	py=y

	#最初の地点だけ覚えておく
	stock=TRUE
	stock.x=x
	stock.y=y
	stock.px=x
	stock.py=y
	
	#返答用データ
	ans.x=x
	ans.y=y

	#もし、(-,-)方向のみが違うとき
	if(dirmatrix[x,y]==1){
		y=y-1
		stock.x=stock.x-1
		dirmatrix[x,y]=0
	#もし、(+,-)方向のみ違うとき
	}else if(dirmatrix[x,y]==2){
		y=y-1
		stock.x=stock.x+1
		dirmatrix[x,y]=0
	#もし、(+,+)方向のみ違うとき
	}else if(dirmatrix[x,y]==4){
		y=y+1
		stock.x=stock.x+1
		dirmatrix[x,y]=0
	#もし、(-,+)方向のみ違うとき
	}else if(dirmatrix[x,y]==7){
		y=y+1
		stock.x=stock.x-1
		dirmatrix[x,y]=0
	#もし、-y方向と+y方向で違うとき
	}else if(dirmatrix[x,y]==3){
		x=x+1
		stock.x=stock.x-1
		dirmatrix[x,y]=0
	#もし、-x方向と+x方向で違うとき
	}else if(dirmatrix[x,y]==6){
		y=y+1
		stock.y=stock.y-1
		dirmatrix[x,y]=0
	#もし、クロスポイントの時
	}else if(dirmatrix[x,y]==5){
		x=x-1
		stock.y=stock.y-1
		dirmatrix[x,y]=4
	}else{
		return("fail in error case")
	}
	
	while(TRUE){
		#端まで到達したとき
		if(x<1 || y<1 || x>xmax || y>ymax || dirmatrix[x,y]==0){
			#スタート時のもう一つのコースが残っている場合
			if(stock==TRUE){
				#記録を反転
				ans.x=rev(ans.x)
				ans.y=rev(ans.y)

				#もう一つのコース呼び出し
				x=stock.x
				y=stock.y
				px=stock.px
				py=stock.py
				stock=FALSE

				next
			}
			#唯一の出口
			return(list(dirmatrix=dirmatrix,x=ans.x,y=ans.y))
		}
		
		#現状を記録
		ans.x=c(ans.x,x)
		ans.y=c(ans.y,y)
		px=x
		py=y
		
		#もし、(-,-)方向のみが違うとき
		if(dirmatrix[x,y]==1){
			#前回-x方向にいたなら、-y方向へ
			if(px<x & py==y){
				y=y-1
			#前回-y方向にいたなら、-x方向へ
			}else if(py<y & px==x){
				x=x-1
			}else{
				return("fail in case 1")
			}
			dirmatrix[x,y]=0
		#もし、(+,-)方向のみ違うとき
		}else if(dirmatrix[x,y]==2){
			#前回+x方向にいたなら、-y方向へ
			if(px>x & py==y){
				y=y-1
			#前回-y方向にいたなら、+x方向へ
			}else if(py<y & px==x){
				x=x+1
			}else{
				return("fail in case 2")
			}
			dirmatrix[x,y]=0
		#もし、(+,+)方向のみ違うとき
		}else if(dirmatrix[x,y]==4){
			#前回+x方向にいたなら、+y方向へ
			if(px>x & py==y){
				y=y+1
			#前回+y方向にいたなら、+x方向へ
			}else if(py>y & px==x){
				x=x+1
			}else{
				return("fail in case 4")
			}
			dirmatrix[x,y]=0
		#もし、(-,+)方向のみ違うとき
		}else if(dirmatrix[x,y]==7){
			#前回-x方向にいたなら、+y方向へ
			if(px<x & py==y){
				y=y+1
			#前回+y方向にいたなら、-x方向へ
			}else if(py>y & px==x){
				x=x-1
			}else{
				return("fail in case 7")
			}
			dirmatrix[x,y]=0
		#もし、-y方向と+y方向で違うとき
		}else if(dirmatrix[x,y]==3){
			#前回-x方向にいたなら、+x方向へ
			if(px<x & py==y){
				x=x+1
			#前回+x方向にいたなら、-x方向へ
			}else if(px>x & py==y){
				x=x-1
			}else{
				return("fail in case 3")
			}
			dirmatrix[x,y]=0
		#もし、-x方向と+x方向で違うとき
		}else if(dirmatrix[x,y]==6){
			#前回-y方向にいたなら、+y方向へ
			if(py<y & px==x){
				y=y+1
			#前回+y方向にいたなら、-y方向へ
			}else if(py>y & px==x){
				y=y-1
			}else{
				return("fail in case 6")
			}
			dirmatrix[x,y]=0
		#もし、クロスポイントの時
		}else if(dirmatrix[x,y]==5){
			#前回-y方向にいたなら、-x方向へ
			if(py<y & px==x){
				x=x-1
				dirmatrix[dx,dy]=4;
			#前回-x方向にいたなら、-y方向へ
			}else if(px<x & py==y){
				y=y-1
				dirmatrix[dx,dy]=4;
			#前回+y方向にいたなら、+x方向へ
			}else if(py>y & px==x){
				x=x+1
				dirmatrix[x,y]=1
			#前回+x方向にいたなら、+y方向へ
			}else if(px>x & py==y){
				y=y+1
				dirmatrix[x,y]=1
			}else{
				return("fail in case 5")
			}
		}else{
			return("fail in error case")
		}
	}
}
#与えられた配列のTRUE/FALSEの境界を疑似的に求める
#	zは、TRUE/FALSE行列
border=function(z,n=1001){
	#行列じゃなければ無視
	if(is.matrix(z)==FALSE){
		return(NA)
	}

	#行列の長さを取得しておく
	xmax=nrow(z)
	ymax=ncol(z)

	#コアとなる行列
	#	xy方向に対して以下のように重み付け
	#		(-,-)方向を1,(+,-)方向を2,(+,+)方向を4,(-,+)方向を8
	#		正の場合のみ各領域の値を足すと、0～15で隣り合う４タイルの正負を表現できる
	#	全て正/負の0と15を除外して、残った部分に4タイルの接する面の中点を結ぶような線を割り当てる
	pos=matrix(0,xmax-1,ymax-1)	
	pos=1*z[-xmax,-ymax]+2*z[-1,-ymax]+4*z[-1,-1]+8*z[-xmax,-1]
	pos[is.na(pos)]=0
	pos[pos>7]=15-pos[pos>7]
	
	#行列探索関数
	search=function(dx,dy,ans){
		#前回どこにいたか
		pre_dx=0
		pre_dy=0

		stock=FALSE
		stock_dx=0
		stock_dy=0
		stock_pre_dx=0
		stock_pre_dy=0
		stock_mode=0

		while(TRUE){
			if(dx<1 || dy<1 || dx>=xmax || dy>=ymax || ans[["pos"]][dx,dy]==0){
				if(stock && (dx!=stock_dx || dy!=stock_dy)){
					dx=stock_dx
					dy=stock_dy
					pre_dx=stock_pre_dx
					pre_dy=stock_pre_dy
					ans[["pos"]][stock_dx,stock_dy]=stock_mode
					stock=FALSE

					ans[["x"]]=rev(ans[["x"]])
					ans[["y"]]=rev(ans[["y"]])
				}else{
					return(ans)
				}
			#もし、(-,-)方向のみが違っている時
			}else if(ans[["pos"]][dx,dy]==1){
				#前回
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=1
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==2){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=2
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==4){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=4
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==7){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=7
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==3){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}else{
					if(pre_dx!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=3
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==6){
				if(pre_dy==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=0
						stock_pre_dy=-1
						stock_dx=dx
						stock_dy=dy
						stock_mode=6
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==5){
				if(pre_dx!=0){
					if(pre_dx==-1){
						ans[["x"]]=c(ans[["x"]],x[dx+1])
						ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
						pre_dx=-1
						pre_dy=0
					}else{
						if(pre_dx!=1){
							stock_pre_dx=-1
							stock_pre_dy=0
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],x[dx])
						ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
						pre_dx=1
						pre_dy=0
					}
					ans[["pos"]][dx,dy]=6;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}else{
					if(pre_dy==-1){
						ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
						ans[["y"]]=c(ans[["y"]],y[dy+1])
						pre_dx=0
						pre_dy=-1
					}else{
						if(pre_dy!=1){
							stock_pre_dx=0
							stock_pre_dy=-1
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
						ans[["y"]]=c(ans[["y"]],y[dy])
						pre_dx=0
						pre_dy=1
					}
					ans[["pos"]][dx,dy]=3;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}
			}
		}
	}

	ansx=numeric(0)
	ansy=numeric(0)

	while(TRUE){
		tmp=which(pos!=0, arr.ind=TRUE)
		if(nrow(tmp)==0)break
		ans=search(tmp[1,1],tmp[1,2],list(pos=pos,x=numeric(0),y=numeric(0)))
		ansx=c(ansx,ans[["x"]],NA)
		ansy=c(ansy,ans[["y"]],NA)
		pos=ans[["pos"]]	
	}
	
	return(list(ansx,ansy))
}
#関数の正負の境界を疑似的に求める
border=function(f,xlim=c(0,1),ylim=c(0,1),n=1001){
	#xy方向に対して以下のように重み付け
	#(-,-)方向を1,(+,-)方向を2,(+,+)方向を4,(-,-)方向を8
	#正の場合のみ各領域の値を足すと、0～15で隣り合う４タイルの正負を表現できる
	#全て正/負の0と15を除外して、残った部分に4タイルの接する面の中点を結ぶような線を割り当てる

	x=seq(xlim[1],xlim[2],length=n)
	y=seq(ylim[1],ylim[2],length=n)
	z=matrix_from_function(f,x,y)
	z=(z>0)
	pos=matrix(0,nrow(z)-1,ncol(z)-1)	

	pos=1*z[-nrow(z),-ncol(z)]+2*z[-1,-ncol(z)]+4*z[-1,-1]+8*z[-nrow(z),-1]
	pos[is.na(pos)]=0
	pos[pos>7]=15-pos[pos>7]
	search=function(dx,dy,ans){
		#ans=list(pos,x,y)

		pre_dx=0
		pre_dy=0

		stock=FALSE
		stock_dx=0
		stock_dy=0
		stock_pre_dx=0
		stock_pre_dy=0
		stock_mode=0
		while(TRUE){
			if(dx<1 || dy<1 || dx>=length(x) || dy>=length(y) || ans[["pos"]][dx,dy]==0){
				if(stock && (dx!=stock_dx || dy!=stock_dy)){
					dx=stock_dx
					dy=stock_dy
					pre_dx=stock_pre_dx
					pre_dy=stock_pre_dy
					ans[["pos"]][stock_dx,stock_dy]=stock_mode
					stock=FALSE

					ans[["x"]]=rev(ans[["x"]])
					ans[["y"]]=rev(ans[["y"]])
				}else{
					return(ans)
				}
			}else if(ans[["pos"]][dx,dy]==1){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=1
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==2){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}else{
					if(pre_dy!=-1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=2
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==4){
				if(pre_dx==1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=4
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==7){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=7
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==3){
				if(pre_dx==-1){
					ans[["x"]]=c(ans[["x"]],x[dx+1])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=-1
					pre_dy=0
				}else{
					if(pre_dx!=1){
						stock_pre_dx=-1
						stock_pre_dy=0
						stock_dx=dx
						stock_dy=dy
						stock_mode=3
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],x[dx])
					ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
					pre_dx=1
					pre_dy=0
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==6){
				if(pre_dy==-1){
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy+1])
					pre_dx=0
					pre_dy=-1
				}else{
					if(pre_dy!=1){
						stock_pre_dx=0
						stock_pre_dy=-1
						stock_dx=dx
						stock_dy=dy
						stock_mode=6
						stock=TRUE
					}
					ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
					ans[["y"]]=c(ans[["y"]],y[dy])
					pre_dx=0
					pre_dy=1
				}
				ans[["pos"]][dx,dy]=0;
				dx=dx-pre_dx
				dy=dy-pre_dy
			}else if(ans[["pos"]][dx,dy]==5){
				if(pre_dx!=0){
					if(pre_dx==-1){
						ans[["x"]]=c(ans[["x"]],x[dx+1])
						ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
						pre_dx=-1
						pre_dy=0
					}else{
						if(pre_dx!=1){
							stock_pre_dx=-1
							stock_pre_dy=0
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],x[dx])
						ans[["y"]]=c(ans[["y"]],(y[dy]+y[dy+1])/2)
						pre_dx=1
						pre_dy=0
					}
					ans[["pos"]][dx,dy]=6;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}else{
					if(pre_dy==-1){
						ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
						ans[["y"]]=c(ans[["y"]],y[dy+1])
						pre_dx=0
						pre_dy=-1
					}else{
						if(pre_dy!=1){
							stock_pre_dx=0
							stock_pre_dy=-1
							stock_dx=dx
							stock_dy=dy
							stock_mode=5
							stock=TRUE
						}
						ans[["x"]]=c(ans[["x"]],(x[dx]+x[dx+1])/2)
						ans[["y"]]=c(ans[["y"]],y[dy])
						pre_dx=0
						pre_dy=1
					}
					ans[["pos"]][dx,dy]=3;
					dx=dx-pre_dx
					dy=dy-pre_dy
				}
			}
		}
	}

	ansx=numeric(0)
	ansy=numeric(0)

	while(TRUE){
		tmp=which(pos!=0, arr.ind=TRUE)
		if(nrow(tmp)==0)break
		ans=search(tmp[1,1],tmp[1,2],list(pos=pos,x=numeric(0),y=numeric(0)))
		ansx=c(ansx,ans[["x"]],NA)
		ansy=c(ansy,ans[["y"]],NA)
		pos=ans[["pos"]]	
	}
	
	return(list(ansx,ansy))
}
#二つの配列xseq,yseqから、xseqに投影した曲線を返す
projecting_split=function(xseq,yseq){
	front=NA#前進中か後退中かのフラグ
	prev=NA#直前のxの値
	xpos=0#prevを保存した位置
	xans=numeric(0)
	yans=numeric(0)
	
	#念のため、終端記号を付け直し
	xseq=c(xseq,NA)
	yseq=c(yseq,NA)
	
	for( i in 1:length(xseq)){
		if(is.na(prev)){#前回位置を記録していないとき
			prev=xseq[i]
			xpos=i
			front=NA
		}else if(is.na(front)){#位置は記録したが、方向を記録していないとき
			if(is.na(xseq[i]) || is.na(yseq[i])){#終端処理記号だった場合
				prev=NA
				front=NA
			}else if(prev!=xseq[i]){#通常データで、前回位置と違うとき
				xans=c(xans,xseq[i-1])
				yans=c(yans,mean(yseq[xpos:(i-1)]))
				front=(xseq[i]>prev)
				prev=xseq[i]
				xpos=i
			}
		}else if(is.na(xseq[i]) || is.na(yseq[i])){#終端処理記号だった場合
			xans=c(xans,xseq[i-1],NA)
			yans=c(yans,mean(yseq[xpos:(i-1)]),NA)
			prev=NA
			front=NA
		}else if(prev != xseq[i]){#記録対象の時
			if(front != (xseq[i]>prev)){#進行方向に食い違いが存在しているとき
				xans=c(xans,xseq[i-1],NA,xseq[i-1])
				yans=c(yans,mean(yseq[xpos:(i-1)]),NA,mean(yseq[xpos:(i-1)]))
				front=(xseq[i]>prev)
				prev=xseq[i]
				xpos=i
			}else{#通常データの場合
				xans=c(xans,xseq[i-1])
				yans=c(yans,mean(yseq[xpos:(i-1)]))
				prev=xseq[i]
				xpos=i
			}
		}
	}
	return(list(xans,yans))
}
#線分p1-p2とp3-p4が交差しているかの判定
is.cross_segments_v1=function(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y,p4.x,p4.y){
  #値チェック
  if(sum(is.na(c(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y,p4.x,p4.y)))>0){
    return(FALSE)
  }
  
  #x座標によるチェック
  if(p1.x >= p2.x){
    if((p1.x < p3.x && p1.x < p4.x) || (p2.x > p3.x && p2.x > p4.x)){
      return(FALSE)
    }
  }else{
    if((p2.x < p3.x && p2.x < p4.x) || (p1.x > p3.x && p1.x > p4.x)){
      return(FALSE)
    }
  }
  
  #y座標によるチェック
  if(p1.y >= p2.y){
    if((p1.y < p3.y && p1.y < p4.y) || (p2.y > p3.y && p2.y > p4.y)){
      return(FALSE)
    }
  }else{
    if((p2.y < p3.y && p2.y < p4.y) || (p1.y > p3.y && p1.y > p4.y)){
      return(FALSE)
    }
  }

  #交差条件チェック
  if(
    ((p1.x - p2.x) * (p3.y - p1.y) + (p1.y - p2.y) * (p1.x - p3.x)) *((p1.x - p2.x) * (p4.y - p1.y) + (p1.y - p2.y) * (p1.x - p4.x)) > 0
  ){
    return(FALSE)
  }
  if(
    ((p3.x - p4.x) * (p1.y - p3.y) + (p3.y - p4.y) * (p3.x - p1.x)) *((p3.x - p4.x) * (p2.y - p3.y) + (p3.y - p4.y) * (p3.x - p2.x)) > 0
  ){
    return(FALSE)
  }
  return(TRUE)
}
#線分p1-p2とp3-p4が交差しているかの判定
is.cross_segments=function(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y,p4.x,p4.y){
  #x座標によるチェック
  XCheck=rep(TRUE,length=length(p1.x))
  XCheck=!(((p1.x >= p2.x) & ((p1.x < p3.x & p1.x < p4.x) | (p2.x > p3.x & p2.x > p4.x)))|((p1.x < p2.x) & ((p2.x < p3.x & p2.x < p4.x) | (p1.x > p3.x & p1.x > p4.x))))
  
  #y座標によるチェック
  YCheck=rep(TRUE,length=length(p1.y))
  YCheck=!(((p1.y >= p2.y) & ((p1.y < p3.y & p1.y < p4.y) | (p2.y > p3.y & p2.y > p4.y)))|((p1.y < p2.y) & ((p2.y < p3.y & p2.y < p4.y) | (p1.y > p3.y & p1.y > p4.y))))
  
  #交差条件チェック
  Cross1Check=rep(TRUE,length=length(p1.x))
  Cross1Check=!(((p1.x - p2.x) * (p3.y - p1.y) + (p1.y - p2.y) * (p1.x - p3.x)) *((p1.x - p2.x) * (p4.y - p1.y) + (p1.y - p2.y) * (p1.x - p4.x)) > 0)

  Cross2Check=rep(TRUE,length=length(p1.x))
  Cross2Check=!(((p3.x - p4.x) * (p1.y - p3.y) + (p3.y - p4.y) * (p3.x - p1.x)) *((p3.x - p4.x) * (p2.y - p3.y) + (p3.y - p4.y) * (p3.x - p2.x)) > 0)

  return(XCheck&YCheck&Cross1Check&Cross2Check)
}
#直線p1-p2とp3-p4の交点
get.cross_segments_point=function(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y,p4.x,p4.y){
  r=((p4.y-p3.y)*(p3.x-p1.x)-(p4.x-p3.x)*(p3.y-p1.y))/((p2.x-p1.x)*(p4.y-p3.y)-(p2.y-p1.y)*(p4.x-p3.x))
  return(c((1-r)*p1.x+r*p2.x,(1-r)*p1.y+r*p2.y))
}
#(xylist[1],xylist[2])となるような点配列の中に、(x,y)と同位置となるような点の数
count.equal_point=function(x,y,xylist){sum(remove.na((xylist[[1]]==x)&(xylist[[2]]==y)))}
#(seq[1],seq[2])となるような点配列の中に、(xseq,yseq)と同位置となるような点を返す
search.equal_point=function(xseq1,yseq1,xseq2,yseq2){
	xseq1.cmp=rep(xseq1,each=length(xseq2))
	yseq1.cmp=rep(yseq1,each=length(yseq2))
	xseq2.cmp=rep(xseq2,times=length(xseq1))
	yseq2.cmp=rep(yseq2,times=length(yseq1))
	acs=((xseq1.cmp==xseq2.cmp)&(yseq1.cmp==yseq2.cmp))
	return(list(xseq1.cmp[acs],yseq1.cmp[acs]))
}
#(seq[1],seq[2])となるような点配列の中に、(xseq,yseq)と同位置となるような点を返す
search.cross_point=function(xseq1,yseq1,xseq2,yseq2){
	pos1.x=xseq1[-length(xseq1)]
	pos1.y=yseq1[-length(yseq1)]
	pos2.x=xseq1[-1]
	pos2.y=yseq1[-1]
	pos3.x=xseq2[-length(xseq2)]
	pos3.y=yseq2[-length(yseq2)]
	pos4.x=xseq2[-1]
	pos4.y=yseq2[-1]
	
	pos1.x.cmp=rep(pos1.x,each=length(pos3.x))
	pos1.y.cmp=rep(pos1.y,each=length(pos3.y))
	pos2.x.cmp=rep(pos2.x,each=length(pos3.x))
	pos2.y.cmp=rep(pos2.y,each=length(pos3.y))
	pos3.x.cmp=rep(pos3.x,times=length(pos1.x))
	pos3.y.cmp=rep(pos3.y,times=length(pos1.y))
	pos4.x.cmp=rep(pos4.x,times=length(pos1.x))
	pos4.y.cmp=rep(pos4.y,times=length(pos1.y))
	
#  IsCross=mapply(is.cross_segments,pos1.x.cmp,pos1.y.cmp,pos2.x.cmp,pos2.y.cmp,pos3.x.cmp,pos3.y.cmp,pos4.x.cmp,pos4.y.cmp)
	IsCross=is.cross_segments(pos1.x.cmp,pos1.y.cmp,pos2.x.cmp,pos2.y.cmp,pos3.x.cmp,pos3.y.cmp,pos4.x.cmp,pos4.y.cmp)
	IsCross[is.na(IsCross)]=FALSE
  
  ans.x=numeric(0)
  ans.y=numeric(0)
  
  for(i in (1:length(pos1.x.cmp))[IsCross]){
    pair=get.cross_segments_point(pos1.x.cmp[i],pos1.y.cmp[i],pos2.x.cmp[i],pos2.y.cmp[i],pos3.x.cmp[i],pos3.y.cmp[i],pos4.x.cmp[i],pos4.y.cmp[i])
    ans.x=c(ans.x,pair[[1]])
    ans.y=c(ans.y,pair[[2]])
  }
  
	return(list(ans.x,ans.y))
}
#ニュートン法を用いたステップ移動用関数
newton_forwarder_1st=function(df,step){
  function(x,...){x+step*df(x,...)}
}
#4次のルンゲクッタ法を用いたステップ移動用関数
runge_kutta_forwarder_1st=function(df,step){
  function(x,...){
    k1=df(x,...)
    k2=df(x+k1*step/2,...)
    k3=df(x+k2*step/2,...)
    k4=df(x+k3*step,...)
    return(x+step*(k1+2*k2+2*k3+k4)/6)
  }
}
#4次のルンゲクッタ法を用いたステップ移動用関数
runge_kutta_forwarder_2nd=function(df,step){
  function(y1,x,...){
    k1=df(y1,x,...)
    k2=df(y1,x+k1*step/2,...)
    k3=df(y1,x+k2*step/2,...)
    k4=df(y1,x+k3*step,...)
    return(x+step*(k1+2*k2+2*k3+k4)/6)
  }
}
#4次のルンゲクッタ法を用いたステップ移動用関数
runge_kutta_forwarder_3rd=function(df,step){
  function(y1,y2,x,...){
    k1=df(y1,y2,x,...)
    k2=df(y1,y2,x+k1*step/2,...)
    k3=df(y1,y2,x+k2*step/2,...)
    k4=df(y1,y2,x+k3*step,...)
    return(x+step*(k1+2*k2+2*k3+k4)/6)
  }
}
#4次のルンゲクッタ法を用いたステップ移動用関数
runge_kutta_forwarder_4th=function(df,step){
  function(y1,y2,y3,x,...){
    k1=df(y1,y2,y3,x,...)
    k2=df(y1,y2,y3,x+k1*step/2,...)
    k3=df(y1,y2,y3,x+k2*step/2,...)
    k4=df(y1,y2,y3,x+k3*step,...)
    return(x+step*(k1+2*k2+2*k3+k4)/6)
  }
}