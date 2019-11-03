#' Categorize multi dimension data based on the relative distance vs cluster size
#' @description Based on the kmeans clustering algorithm, find optimal cluster num by keeping multiple cluster are not overlapped.
#' @param data Matrix data for clustering. Row is element and Col is axis.
#' @param rate Threshold rate of clustersize / distance
#' @param iter Number of iteration for kmeans.
#' @param kmax Max cluster number.
#' @return List with size: number of cluster and cluster: #cluster of each element.
#' @importFrom stats kmeans var
#' @export
overlap_check_kmeans = function(data, rate = 2, iter=10, kmax=10){
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

		cls = list(size=i, cluster=tmpcls$cluster)
	}
	return(cls)
}

