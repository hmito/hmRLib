jpera_traits = data.frame(
	#stringi::stri_escape_unicode("target character")
	jpname = c("\u660e\u6cbb","\u5927\u6b63","\u662d\u548c","\u5e73\u6210","\u4ee4\u548c"),
	name = c("M","T","S","H","R"),
	origin=c(1867,1911,1925,1988,2018),
	begin=c("1868-10-23","1912-07-30","1926-12-25","1989-01-08","2019-05-01"),
	end=c("1912-07-30","1926-12-25","1989-01-08","2019-05-01","2099-12-31"))
Date.origin = as.Date("2000-01-01") - as.integer(as.Date("2000-01-01"))

#' get Date type from Japanese Era name
#' @description Work like as.Date but Japanese Era can be allowed, which is represented as \%E in format. Default formats are "R1-04-03" or "R1.04.03".
#' @param str target character or sequence of character.
#' @param tryFormats format of str for reading JP era and other date data. \%E mean era. The other options follows as.Date function.
#' @return Date class
#' @export
jpera_to_date = function(str, tryFormats = c("%E-%m-%d", "%E.%m.%d")){
	jperaFormats = rex::escape(tryFormats)
	jperaFormats = stringr::str_replace_all(jperaFormats, "%[a-zA-DF-Z]",".+")
	pattern = sprintf("([%s]%s)(\\d{1,2})",
							paste(c(stringr::str_to_lower(jpera_traits$name),
									  stringr::str_to_upper(jpera_traits$name)),collapse = ""),
							paste0(paste0("|",jpera_traits$jpname),collapse="")
	)
	jperaFormats = stringr::str_replace_all(jperaFormats, "%E",pattern)
	jperaFormats = sprintf("^%s$",jperaFormats)
	str = stringr::str_remove(stringr::str_remove(str,"^\\s*"),"\\s*$")

	Match = stringr::str_match_all(str,pattern)
	for(i in 1:length(str)){
		target = Match[[i]]
		if(nrow(target)==0)next

		replaces = sprintf("%d",
								 hmRLib::replace_by(stringr::str_to_upper(target[,2]),c(jpera_traits$name,jpera_traits$jpname),
								 			  rep(jpera_traits$origin,times=2)+as.integer(target[,3]),NA)
		)
		names(replaces) = target[,1]
		str[i]=stringr::str_replace_all(str[i],replaces)
	}

	ADFormats = stringr::str_replace_all(tryFormats,"%E","%Y")
	return(as.Date(unlist(purrr::map(str,as.Date,optional=TRUE,tryFormats = ADFormats)),origin=Date.origin))
}

#' transform Japanese-Era string into AD-year string with two digit.
#' @description transform Japanese-Era string into AD-year string with two digit (i.e., R1 -> 19)
#' @param str target character or sequence of character.
#' @return transformed str
#' @export
jpera_to_y2str = function(str){
	pattern = sprintf("([%s]%s)(\\d{1,2})",
							paste(c(stringr::str_to_lower(jpera_traits$name),
									  stringr::str_to_upper(jpera_traits$name)),collapse = ""),
							paste0(paste0("|",jpera_traits$jpname),collapse="")
	)
	Match = stringr::str_match_all(str,pattern)

	for(i in 1:length(str)){
		target = Match[[i]]
		if(nrow(target)==0)next

		replaces = hmRLib::replace_by(
			stringr::str_to_upper(target[,2]),
			c(jpera_traits$name,jpera_traits$jpname),
			sprintf("%02d",(rep(jpera_traits$origin,times=2)+as.integer(target[,3]))%%100),
			"??")
		names(replaces) = target[,1]
		str[i]=stringr::str_replace_all(str[i],replaces)
	}
	return(str)
}

#' transform Japanese-Era string into AD-year string.
#' @description transform Japanese-Era string into AD-year string (i.e., R1 -> 2019)
#' @param str target character or sequence of character.
#' @return transformed str
#' @export
jpera_to_ystr = function(str){
	pattern = sprintf("([%s]%s)(\\d{1,2})",
							paste(c(stringr::str_to_lower(jpera_traits$name),stringr::str_to_upper(jpera_traits$name)),collapse = ""),
							paste0(paste0("|",jpera_traits$jpname),collapse="")
	)
	Match = stringr::str_match_all(str,pattern)

	for(i in 1:length(str)){
		target = Match[[i]]
		if(nrow(target)==0)next

		replaces =  hmRLib::replace_by(
			stringr::str_to_upper(target[,2]),
			c(jpera_traits$name,jpera_traits$jpname),
			sprintf("%d",rep(jpera_traits$origin,times=2)+as.integer(target[,3])),
			"??")
		names(replaces) = target[,1]
		str[i]=stringr::str_replace_all(str[i],replaces)
	}
	return(str)
}
