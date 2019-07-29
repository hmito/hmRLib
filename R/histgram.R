#' Translate axis of histgram sequence into another axis
#' @description This function translate the histgram sequence with a certain axis into another histgram sequence with a different axis.
#' @param src.hist Source histgram sequence of float values.
#' @param src.breakseq Source histgram breaks. The length is length(src.hist)+1.
#' @param trg.breakseq Target histgram breaks.
#' @return Target histgram sequence of float values.
#' @export
#' @examples
#' x = c(1,2,3)
#' x.ax = c(0,1,2,3)
#' y.ax = c(0,0.5,1,1.5,2,2.5)
#' hist_axis_translate(x,x.ax,y.ax)
#' # c(0.5,0.5,1,1,1.5)
hist_axis_translate=function(src.hist,src.breakseq,trg.breakseq){
	src.ln = length(src.breakseq)-1
	trg.ln = length(trg.breakseq)-1

	trg1 = matrix(rep(trg.breakseq[-length(trg.breakseq)],times=src.ln),trg.ln,src.ln)
	trg2 = matrix(rep(trg.breakseq[-1],times=src.ln),trg.ln,src.ln)

	src1 = matrix(rep(src.breakseq[-length(src.breakseq)],each=trg.ln),trg.ln,src.ln)
	src2 = matrix(rep(src.breakseq[-1],each=trg.ln),trg.ln,src.ln)

	dif = ifelse(trg2<src2,trg2,src2) - ifelse(trg1>src1,trg1,src1)

	dif[dif<0]=0
	dif=dif/(src2-src1)

	return(as.numeric(dif%*%src.hist))
}

#' Translate histgram matrix with min values and max values to a unified histgram matrix (i.e., same breaks)
#' @description This function translate the histgram set with different min/max values into a histgram with a common breaks.
#' @param minseq min values of source histgram matrix.
#' @param maxseq max values of source histgram matrix.
#' @param histmx source histgram matrix.
#' @param minval min value of a unified histrgam.
#' @param maxval max value of a unified histrgam.
#' @param len length of a unified histrgam.
#' @return List of new unified histgram matrix (hist), breaks (break) and mid values of each element (mids)
#' @export
minmaxhist_to_hist=function(minseq, maxseq, histmx, minval=NA, maxval=NA, len=NA){
	if(is.na(minval)){
		minval = min(minseq)
	}
	if(is.na(maxval)){
		maxval = max(maxseq)
	}
	if(is.na(len)){
		len = ncol(histmx)
	}
	breaks = seq(minval,maxval,length=len+1)

	if(any(minseq==maxseq)){
		return(NULL)
	}

	z = matrix(0,nrow(histmx),len)
	for(i in 1:nrow(histmx)){
		z[i,] = hmRLib::hist_axis_translate(histmx[i,],seq(minseq[i],maxseq[i],length=ncol(histmx)+1),breaks)
	}

	return(list(hist=z, breaks=breaks, mids=breaks[-length(breaks)]+diff(breaks)/2))
}

#' Smoothing histgram sequence by using simple moving average method
#' @description This function translate the histgram sequence into the smoothing data.
#' @param hist.data Histgram sequence
#' @param width Width of the simple moving average method. width=1 means no smoothing. This value should be an intergral value.
#' @return Smoothing histgram sequence
#' @examples
#' hist.data = c(1,5,7,5,5,4,3,1,0,0,0,4,4,8,5,5,5,5,3,3,3,7,7,8,3,3,1,3,5,0,0,2,3,3,0)
#' smooth = hist_smoothing(hist.data,3)
#' plot(hist.data,type="b")
#' lines(1:length(hist.data),smooth,col="red",lwd=2)
#' @export
hist_smoothing = function(hist.data,width){
	data = c(rep(0,floor((width-1)/2)),hist.data,rep(0,ceiling((width-1)/2)))
	#	shist = numeric(length(hist.data) - width + 1)
	shist = numeric(length(hist.data))
	for(i in 1:width){
		shist = shist + data[c(rep(FALSE,width-i),rep(TRUE,length(data)-width+1),rep(FALSE,i-1))]
	}

	shist = shist * sum(hist.data)/sum(shist)
	return(shist)
	#	return(list(data=shist,axis=seq(1+(width-1)/2,length(hist.data)-(width-1)/2,length=length(hist.data)-width+1)))
}

#' Find peaks in given histgram by truncing at a threshold values.
#' @description Find peaks in given histgram. The peaks are devided by the majority of peak devision with changing threshold of the ignoring values [min,max].
#' @param hist.data The target histgram
#' @param min The minmum value of the threshold.
#' @param max The maximum value of the threshold.
#' @param n The step of the threshold change.
#' @return Table of the peaks with lower number, upper number, top number (maximum position within the peak) and frequency of each peaks.
#' @examples
#' hist.data = c(1,5,7,5,5,4,3,1,0,0,0,4,4,8,5,5,5,5,3,3,3,7,7,8,3,3,1,3,5,0,0,2,3,3,0)
#' peaks = hist_find_peaks.minvalue(hist.data,0,6)
#' plot(hist.data,type="b",lwd=2)
#' for(i in 1:nrow(peaks)){
#' 	segments(peaks$top[i],-1,peaks$top[i],10,col="red")
#' 	polygon(
#' 		c(peaks$lower[i],peaks$lower[i],peaks$upper[i],peaks$upper[i]),c(-1,10,10,-1),
#' 		col=rgb(1,0,0,0.2),border=NA
#' 	)
#' }
#' @export
hist_find_peaks.minvalue = function(hist.data,min,max,n=101){
	threshold.mx = matrix(rep(seq(min,max,length=n),each=length(hist.data)+2),length(hist.data)+2,n)
	hist.data.mx = matrix(rep(c(0,hist.data,0),times=n),length(hist.data)+2,n)
	exist.mx = hist.data.mx>threshold.mx

	result = apply(exist.mx[-1,]!=exist.mx[-nrow(exist.mx),],2,sum)/2
	result.table = table(result)
	peak.num = as.integer(names(result.table)[order(result.table,decreasing=TRUE)[1]])

	threshold.no = min((1:n)[result==peak.num])
	exist.seq = exist.mx[,threshold.no]

	peaks = data.frame("lower"=NULL,"upper"=NULL,"top"=NULL,"freq"=NULL)
	if(peak.num!=0){
		boundary = 1

		lower = (1:(length(exist.seq)-1))[exist.seq[-1]&(!exist.seq[-length(exist.seq)])]
		upper = (0:(length(exist.seq)))[(!exist.seq[-1])&exist.seq[-length(exist.seq)]]

		for(peak.pos in 1:peak.num){
			this.lower = min((boundary:lower[peak.pos])[hist.data[boundary:lower[peak.pos]]>0])
			if(peak.pos<peak.num){
				boundary = (upper[peak.pos]:lower[peak.pos+1])[order(hist.data[upper[peak.pos]:lower[peak.pos+1]])[1]]
			}else{
				boundary = length(hist.data)
			}
			this.upper = max((upper[peak.pos]:(boundary-1))[hist.data[upper[peak.pos]:(boundary-1)]>0])
			this.top = (this.lower:this.upper)[order(hist.data[this.lower:this.upper],decreasing = TRUE)[1]]
			this.freq = sum(hist.data[this.lower:this.upper])

			peaks = rbind(peaks,data.frame("lower"=this.lower,"upper"=this.upper,"top"=this.top,"freq"=this.freq))
		}
	}

	return(peaks)
}

#' Find peaks in given histgram by checking the valley of the data
#' @description Find peaks in given histgram. The peaks are devided by the valley which are detected by the sloop.
#' @param hist.data The target histgram
#' @param min.ratio The minmum ratio for being detected as a valley. The ratio is calculated by the min([peaks values in both sides]/[valley value]).
#' @param min.value The minmum value for being detected as a valley.
#' @param interval The step of the slope.
#' @return Table of the peaks with lower number, upper number, top number (maximum position within the peak) and frequency of each peaks.
#' @examples
#' hist.data = c(1,5,7,5,5,4,3,1,0,0,0,4,4,8,5,5,5,5,3,3,3,7,7,8,3,3,1,3,5,0,0,2,3,3,0)
#' peaks = hist_find_peaks.slope(hist.data,0.5)
#' plot(hist.data,type="b",lwd=2)
#' for(i in 1:nrow(peaks)){
#' 	segments(peaks$top[i],-1,peaks$top[i],10,col="red")
#' 	polygon(
#' 		c(peaks$lower[i],peaks$lower[i],peaks$upper[i],peaks$upper[i]),c(-1,10,10,-1),
#' 		col=rgb(1,0,0,0.2),border=NA
#' 	)
#' }
#' @export
hist_find_peaks.slope = function(hist.data, min.ratio = 0.3, min.value = NA, interval = 1){
	if(is.na(min.ratio)){
		min.ratio = 1.0
	}
	if(is.na(min.value)){
		min.value = max(hist.data)
	}

	hist.data = c(0,hist.data,0)
	slope = hist.data[-seq(1,interval)] - hist.data[-seq(length(hist.data),length(hist.data)-interval+1)]
	slope.pos = 1:length(slope)+interval/2

	is_up = (slope[-length(slope)]<=0) & (slope[-1]>0)
	is_down = (slope[-length(slope)]<0) & (slope[-1]>=0)

	valley.code = is_up*1 + is_down*2
	selvalley.no = seq(1,length(valley.code))[valley.code>0]
	selvalley = valley.code[selvalley.no]

	valley.lower.no = selvalley.no[selvalley==3 | c((selvalley[-length(selvalley)]==2 & selvalley[-1]==1),FALSE)]
	valley.upper.no = selvalley.no[selvalley==3 | c(FALSE,(selvalley[-length(selvalley)]==2 & selvalley[-1]==1))]

	boundaries = c(0,floor((slope.pos[valley.lower.no-1] + slope.pos[valley.upper.no])/2),length(hist.data))

	#list up peaks based on boundaries
	#	peaks = data.frame("lower"=NA,"upper"=NA,"top"=NA,"freq"=NA)
		peaks = data.frame("lower"=NULL,"upper"=NULL,"top"=NULL,"freq"=NULL)
	for(pos in 2:length(boundaries)){
		range = (boundaries[pos-1]+1):boundaries[pos]
		this.lower = min(range[hist.data[range]>0])-1
		this.upper = max(range[hist.data[range]>0])-1
		this.top = range[order(hist.data[range],decreasing = TRUE)[1]]-1
		this.freq = sum(hist.data[range])

		peaks = rbind(peaks,data.frame("lower"=this.lower,"upper"=this.upper,"top"=this.top,"freq"=this.freq))
	}

	#marge peaks based on the minratio
	while(nrow(peaks)>1){
		peaks.value = hist.data[peaks$top+1]
		valley.value = ifelse(
			peaks$lower[-1] - peaks$upper[-nrow(peaks)] > 1,
			0,
			ifelse(
				hist.data[peaks$lower[-1]+1]>hist.data[peaks$upper[-nrow(peaks)]+1],
				hist.data[peaks$upper[-nrow(peaks)]+1],
				hist.data[peaks$lower[-1]+1]
			)
		)
		valley.ratio = ifelse(peaks.value[-1]>peaks.value[-nrow(peaks)],valley.value/peaks.value[-nrow(peaks)], valley.value/peaks.value[-1])

		if(all(valley.ratio < min.ratio & valley.value < min.value)){
			break
		}else{
			marge.no = order(valley.ratio,decreasing = TRUE)[1]
			peaks$upper[marge.no] = peaks$upper[marge.no+1]
			peaks$top[marge.no] = ifelse(peaks.value[marge.no]>=peaks.value[marge.no+1],peaks$top[marge.no],peaks$top[marge.no+1])
			peaks$freq[marge.no] = peaks$freq[marge.no] + peaks$freq[marge.no+1]
			peaks = peaks[-(marge.no+1),]
		}
	}

	return(peaks)
}
