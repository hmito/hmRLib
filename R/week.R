weekno.origin = as.Date("1990-01-01")

#' Return weekno orogin date
#' @description Return weekno orogin date (default is 1990-01-01)
#' @return Date class
#' @export
weekno_origin = function(){return(weekno.origin)}

#' Calculate weekno from Date
#' @description Calculate weekno from Date
#' @param Date the focal date
#' @return Week number
#' @export
date_to_weekno = function(Date){
	floor(as.integer(Date-weekno.origin)/7) + 1
}

#' Calculate weekno from Year, month and Day
#' @description Calculate weekno from Year, month and Day
#' @param year the year of the focal date
#' @param month the month of the focal date
#' @param day the date of the focal date
#' @return Week number
#' @export
ymd_to_weekno = function(year,month,day){
	date_to_weekno(as.Date(sprintf("%d-%02d-%02d",year,month,day)))
}

#' Calculate weekno from year and week number on the year.
#' @description Calculate weekno from Date
#' @param year the year of the focal week
#' @param week the week of the focal week
#' @return Week number
#' @export
yw_to_weekno = function(year,week){
	date_to_weekno(as.Date(sprintf("%04d-01-04",year))) -1 + week
}

#' Calculate date range from week number
#' @description Calculate date range of the focal week number.
#' @param wno the week number of the focal week
#' @return data frame with from (first date) and to (last date)
#' @export
weekno_to_date = function(wno){
	return(data.frame(from=as.Date(weekno.origin+7*(wno-1)),to=as.Date(weekno.origin+7*(wno-1)+6)))
}

#' Calculate first date from week number
#' @description Calculate first date of the focal week number.
#' @param wno the week number of the focal week
#' @return first date
#' @export
weekno_to_firstdate = function(wno){
	return(as.Date(weekno.origin+7*(wno-1)))
}
