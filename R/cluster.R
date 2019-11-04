#' Categorize multi dimension data based on the relative distance vs cluster size using kmeans function
#' @description Based on the kmeans clustering algorithm, find optimal cluster num by keeping multiple cluster are not overlapped.
#' @param data Matrix data for clustering. Row is element and Col is axis.
#' @param rate Threshold rate of clustersize / distance
#' @param iter Number of iteration for kmeans.
#' @param kmax Max cluster number.
#' @return List with size: number of cluster and group: #cluster of each element.
#' @importFrom stats kmeans var
#' @export
distlim_kmeans_clustering = function(data, rate = 2, iter=10, kmax=10){
	cls = NULL
	for(i in 1:kmax){
		tmpcls = kmeans(data,i)
		for(j in 1:iter){
			bes = kmeans(data,i)
			if(tmpcls$betweenss/(tmpcls$betweenss+tmpcls$tot.withinss) < bes$betweenss/(bes$betweenss+bes$tot.withinss)){
				tmpcls = bes
			}
		}

		#distance between clusters
		distmx = matrix(0,i,i)
		#sum of size between clusters on the line between center
		sizemx = matrix(0,i,i)

		for(j in 1:i){
			for(k in 1:i){
				if(j==k)next
				subj = data[tmpcls$cluster==j,]#-matrix(tmpcls$centers[j,],sum(tmpcls$cluster==j),2,byrow=TRUE)
				subk = data[tmpcls$cluster==k,]#-tmpcls$centers[k,]
				vec = (tmpcls$centers[j,]-tmpcls$centers[k,])
				vec = matrix(vec/sqrt(sum(vec*vec)),2,1)
				nxj = as.numeric(subj%*%vec)
				nxk = as.numeric(subk%*%vec)
				sizemx[j,k] = sqrt(var(nxj))+sqrt(var(nxk))
				distmx[j,k] = abs(mean(nxj) - mean(nxk))
			}
		}

		#all cluster distance should be larger than the size of clusters
		#	i.e., all clusters should be enough far from other clusters.
		condmx = (sizemx*rate < distmx)
		diag(condmx) = TRUE
		if(!all(condmx))break

		cls = list(size=i, group=tmpcls$cluster)
	}
	return(cls)
}

#' Categorize multi dimension data based on the relative distance vs cluster size using hclust function
#' @description Based on the h-clustering algorithm, find optimal cluster num by keeping multiple cluster are not overlapped.
#' @param data Matrix data for clustering. Row is element and Col is axis.
#' @param thr Threshold value of the relative distance/size of clusters.
#' @param kmax Max cluster number.
#' @param method Clustering method in hclust.
#' @return List with size: number of cluster and group: #cluster of each element.
#' @importFrom stats hclust dist  cutree
#' @export
distlim_hclust_clustering = function(data, thr = 5.0, kmax=10, method = "ward.D2"){
	#get sequence of x,y values
	#xval = as.numeric(zdat$x)
	#yval = as.numeric(zdat$y)
	#data = cbind(scale(xval),scale(yval))

	cls = hclust(dist(data),method = method)

	minval = Inf
	for(i in 2:kmax){
		group = cutree(cls,k=i)
		center = NULL
		for(j in 1:i){
			if(sum(group==j)==1){
				center = rbind(center,data[group==j,])
			}else{
				center = rbind(center,apply(data[group==j,],2,mean))
			}
		}

		#distance between clusters
		distmx = matrix(0,i,i)
		#sum of size between clusters on the line between center
		sizemx = matrix(0,i,i)

		for(j in 1:i){
			for(k in 1:i){
				if(j==k)next
				subj = data[group==j,]#-matrix(tmpcls$centers[j,],sum(tmpcls$cluster==j),2,byrow=TRUE)
				subk = data[group==k,]#-tmpcls$centers[k,]
				vec = (center[j,]-center[k,])
				vec = matrix(vec/sqrt(sum(vec*vec)),2,1)
				nxj = as.numeric(subj%*%vec)
				nxk = as.numeric(subk%*%vec)
				sizemx[j,k] = sqrt(var(nxj))+sqrt(var(nxk))
				distmx[j,k] = abs(mean(nxj) - mean(nxk))
			}
		}
		minval = c(minval,min(distmx/sizemx,na.rm = TRUE))
	}

	size = max((1:length(minval))[minval>thr])
	#	size = order(minval,decreasing = TRUE)[1]
	group = cutree(cls,k=size)
	return(list(size = size, group = group))
}
