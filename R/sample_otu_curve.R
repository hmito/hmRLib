#サンプル-OTU曲線を、ランダムセレクトの結果から出力
try.sample_otu_curve=function(SpArray){
	arr=rep(1:length(SpArray),SpArray)
	garr=numeric(length(SpArray))
	ord=order(runif(length(arr)))
	ans=numeric(0)
	for(i in ord){
		garr[arr[i]]=1
		ans=c(ans,sum(garr))
	}
	return(ans)
}
#サンプル-OTU曲線を、ランダムセレクトの結果から出力
try.subset_sample_otu_curve=function(SpArray,Filter){
	arr=rep(1:length(SpArray),SpArray)
	garr=numeric(length(SpArray))
	ord=order(runif(length(arr)))
	ans=numeric(0)
	for(i in ord){
		garr[arr[i]]=1
		ans=c(ans,sum(garr&Filter))
	}
	return(ans)
}
#サンプル-OTU曲線を、複数回繰り返した平均から求める関数
get.sample_otu_curve=function(SpArray,num=100){
	ans=numeric(sum(SpArray))
	for(i in 1:num){
		ans=ans+try.sample_otu_curve(SpArray)
	}
	return(ans/i)
}
#サンプル-OTU曲線を、複数回繰り返した平均から求める関数
get.subset_sample_otu_curve=function(SpArray,Filter,num=100){
	ans=numeric(sum(SpArray))
	for(i in 1:num){
		ans=ans+try.subset_sample_otu_curve(SpArray,Filter)
	}
	return(ans/i)
}
#サンプル-OTU曲線の全体のOTU数に対する増加率が一定値以下となるような点を返す
#	Saple_OTU_curve:サンプル-OTU曲線
#	lim_rate:閾値とする増加率
#	range:増加率を計算する際に参照する幅　100なら、前後計100サンプル増加の間のOTU増加数を判定に使用する
#	戻り値:閾値を最初に下回るsample数を戻す　なければ、NAを返す
get.otu_satulation_point=function(Sample_OTU_curve,lim_rate,range=100){
	if(length(Sample_OTU_curve) <= range)return(NA)
	
	for(i in 1:(length(Sample_OTU_curve)-range)){
		if((Sample_OTU_curve[i+range]-Sample_OTU_curve[i])/Sample_OTU_curve[as.integer(i+range/2)] < lim_rate*range){
			return(as.integer(i+range/2))
		}
	}
	return(NA)
}


#------------使い方-------------------

#各種のサンプル数（疑似データ）
#	OTU1=400サンプル,OTU2=300サンプル・・・という形で配列にしておく
Array=c(400,300,200,100,100,50,20,20,20,10,10,10,5,2,2,2,2,1,1,1,1,1)
#サブセットを求めたい場合は、そのためのtrue/false配列
#	サブセットを構成するOTU番号のところだけ、T,それ以外はFとする配列にしておく
Filter=rep(c(F,F,T),length=length(Array))

#サンプル-OTU曲線を、1000回平均で計算
AllCurve=get.sample_otu_curve(Array,1000)
#サブセットと、サブセット以外のグループのサンプル-OTU曲線を、1000回平均で計算
SubsetCurve=get.subset_sample_otu_curve(Array,Filter,1000)
OSubsetCurve=get.subset_sample_otu_curve(Array,!Filter,1000)

#曲線をひとまず描画
plot(AllCurve,type="n")
lines(1:length(AllCurve),AllCurve)
lines(1:length(SubsetCurve),SubsetCurve,col="red")
lines(1:length(OSubsetCurve),OSubsetCurve,col="blue")

#サンプルあたりのOTU増加率が閾値を下回るsample数を取得
#	0.05%を下回る場所を閾値として、計算
AllPos=get.otu_satulation_point(AllCurve,0.0005)
SubsetPos=get.otu_satulation_point(SubsetCurve,0.0005)
OSubsetPos=get.otu_satulation_point(OSubsetCurve,0.0005)

#閾値sample数とその時のOTU数の点を追加でプロット
points(AllPos,AllCurve[AllPos])
points(SubsetPos,SubsetCurve[SubsetPos],col="red")
points(OSubsetPos,OSubsetCurve[OSubsetPos],col="blue")