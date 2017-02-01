##############################
#biology.R v1_00
#v1_00
#	diversity_indexシリーズ追加
##############################

#シンプソンの多様度指数
diversity_index.simpson=function(seq)return( 1-sum((seq/sum(seq))^2) )
#シャノンの多様度指数
diversity_index.shannon=function(seq)return( -sum(seq[seq>0]/sum(seq[seq>0])*log(seq[seq>0]/sum(seq[seq>0]))) )
