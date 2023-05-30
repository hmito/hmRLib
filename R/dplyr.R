#' reserve columns with the given names in the table
#' @description the column is generated when there are not.
#' @param data table
#' @param col_names column names for reserve
#' @return data with the reserved columns
#' @importFrom magrittr %>%
#' @export
reserve_cols = function(data,col_names){
	ans = col_names %>%
		purrr::map_dfc(~tibble::tibble(!!.x := NA)) %>%
		dplyr::bind_rows(data)
	return(ans[-1,])
}

#' reduce multiple columns into single column.
#' @description reduce multiple columns into single column by funton .reduce.
#' @param data table
#' @param col column
#' @param ... merged columns
#' @param .keep whether the merged column are kept or removed.
#' @param .before location of column before brabra.
#' @param .after location of column after brabra.
#' @param .reduce reduce function; default is the first column which is not NA
#' @return data with the reseved columns
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
reduce_cols = function(data, col, ..., .keep = FALSE, .before=NULL, .after=NULL, .reduce = function(x,y){return(x)}){
	encol = dplyr::enquo(col)
	col  = substitute(col)
	cols = as.list(substitute(list(...)))[-1]
	.before = dplyr::enquo(.before)
	.after = dplyr::enquo(.after)
	if(length(cols)==0)return(data)

	if(all(col != names(data))){
		data = dplyr::mutate(data, !!encol := data[[cols[[1]]]],.before=!!.before,.after=!!.after)
		rmcols = as.character(cols)
		cols = cols[-1]
	}else{
		#data = dplyr::relocate(data, !!encol,.before=!!.before,.after=!!.after)
		rmcols = as.character(cols)
	}

	for(merged_col in cols){
		data[[col]] = ifelse(!is.na(data[[col]]),
									ifelse(!is.na(data[[merged_col]]),
											 .reduce(data[[col]],data[[merged_col]]),
										data[[col]]
									), data[[merged_col]])
	}

	if(!.keep){
		data = dplyr::select(data, -which(names(data) %in% rmcols))
	}

	return(data)
}

