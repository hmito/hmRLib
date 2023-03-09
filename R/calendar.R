jpera_traits = data.frame(
	#stringi::stri_escape_unicode("target character")
	jpname = c("\u660e\u6cbb","\u5927\u6b63","\u662d\u548c","\u5e73\u6210","\u4ee4\u548c"),
	name = c("M","T","S","H","R"),
	origin=c(1867,1911,1925,1988,2018),
	begin=c("1868-10-23","1912-07-30","1926-12-25","1989-01-08","2019-05-01"),
	end=c("1912-07-30","1926-12-25","1989-01-08","2019-05-01","2099-12-31")
)
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
							paste(c(stringr::str_to_lower(jpera_traits$name),
									  stringr::str_to_upper(jpera_traits$name)),collapse = ""),
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

#' transform string to ymd data table.
#' @description transform string to ymd data table like data.frame(y=2022,m=5,d=12). NA will be filled for uncertain elements.
#' @param str target character or sequence of character.
#' @param Date.beg potentially start date; used for validation and estimation.
#' @param Date.end potentially end date; used for validation and estimation.
#' @param use.jpera Allow Japanese era case (e.g., R2.4.5 or 令和3年4月11日).
#' @param use.exceldate Allow to check the value which is originally excel date but occationally transformed to normal value.
#' @param exceldate.origin Origin of excel date. In default, it is 1900-01-01 if the file is originally created on Windows and 1904-01-01 on Mac.
#' @return transformed data.frame
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_sub
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_length
#' @export
str_to_ymd = function(str, Date.beg = NULL, Date.end = NULL, use.jpera=TRUE, use.exceldate=FALSE,exceldate.origin = as.Date("1904-01-01")){
	if(is.null(Date.beg)){
		Date.beg = as.Date("1970-01-01")
	}
	if(is.null(Date.end)){
		Date.end = as.Date("2069-12-31")
	}
	Candidate_y = as.integer(format(Date.beg,"%Y")):as.integer(format(Date.end,"%Y"))

	str = hmRLib::str_number_to_han(str_remove_all(str_remove_all(str,"^\\s+"),"\\s+$"))

	fn = function(Str){
		ymd = data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_)
		ans = NULL

		if(is.na(Str))return(ymd)

		if(str_detect(Str,"\u5e74|\u6708|\u65e5")){
			#年月日の場合
			if(use.jpera){
				ymd$y = Str %>% str_extract("(.*[0-9]{1,4})\u5e74",1) %>% hmRLib::jpera_to_ystr() %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
			}else{
				ymd$y = Str %>% str_extract("(.*[0-9]{1,4})\u5e74",1) %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
			}
			ymd$m = Str %>% str_extract("([0-9]{1,2})\u6708",1) %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
			ymd$d = Str %>% str_extract("([0-9]{1,2})\u65e5",1) %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()

			#二けた表記の場合、可能性のある年で救済
			ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})

			if(ymd$y %in% c(NA,Candidate_y) & ymd$m %in% c(NA,1:12) & ymd$d %in% c(NA,1:31))return(ymd)
			return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
		}

		#?やXは未定値として扱うためゼロ置換
		Str = str_replace_all(Str,"[?X]","0")

		Str.split = str_split(Str,"[\\s\\t\\.\\-/,]")[[1]]
		Str.split = Str.split[Str.split!=""]
		if(length(Str.split)>=2){
			#Check English
			EnMonth.seq = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
			EnMonth = str_detect(str_to_lower(Str.split),paste(EnMonth.seq,collapse="|"))
			if(sum(EnMonth)==1){
				for(i in 1:12){
					if(str_detect(str_to_lower(Str.split[EnMonth]),EnMonth.seq[i])){
						ymd$m = i
						break
					}
				}
				Str.split = Str.split[!EnMonth]

				if(length(Str.split)>=2){
					ymd$d = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$y = Str.split[2] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
						if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
						ans = ymd
					}

					ymd$d = Str.split[2] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$y = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
						if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
						ans = ymd
					}

					if(!is.null(ans))return(ans)
				}

				ymd$d = NA_integer_
				ymd$y = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				ymd$d = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				ymd$y = NA_integer_
				if(ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}else{
				if(length(Str.split)>=3){
					if(use.jpera){
						ymd$y = Str.split[1] %>% hmRLib::jpera_to_ystr() %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}else{
						ymd$y = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}
					ymd$m = Str.split[2] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$d = Str.split[3] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
					if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(0,1:12) & ymd$d %in% c(0,1:31)){
						if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
						#0の場合はNAで埋め込み
						if(ymd$m==0)ymd$m=NA
						if(ymd$d==0)ymd$d=NA
						ans = ymd
					}

					#3点あるときは、YMDを優先
					if(!is.null(ans))return(ans)

					if(use.jpera){
						ymd$y = Str.split[3] %>% hmRLib::jpera_to_ystr() %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}else{
						ymd$y = Str.split[3] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}
					ymd$m = Str.split[2] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$d = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
					if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
						if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
						ans = ymd
					}

					#DMYは一般的ではないが、フルに情報がそろっていればOKとする
					if(!is.null(ans))return(ans)
				}

				if(use.jpera){
					ymd$y = Str.split[1] %>% hmRLib::jpera_to_ystr() %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}else{
					ymd$y = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
				ymd$m = Str.split[2] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(use.jpera){
					ymd$y = Str.split[2] %>% hmRLib::jpera_to_ystr() %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}else{
					ymd$y = Str.split[2] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
				ymd$m = Str.split[1] %>% str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}
		}

		Str.int = str_extract(Str,"^[^0-9]*([0-9]{4,8})[^0-9]*+$",1)
		if(!is.na(Str.int)){
			if(str_length(Str.int)==8){
				ymd$y = as.integer(str_sub(Str.int,1,4))
				ymd$m = as.integer(str_sub(Str.int,5,6))
				ymd$d = as.integer(str_sub(Str.int,7,8))


				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(0,1:12) & ymd$d %in% c(0,1:31)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					#0の場合はNAで埋め込み
					if(ymd$m==0)ymd$m=NA
					if(ymd$d==0)ymd$d=NA
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}else if(str_length(Str.int)==6){
				ymd$y = as.integer(str_sub(Str.int,1,2))
				ymd$m = as.integer(str_sub(Str.int,3,4))
				ymd$d = as.integer(str_sub(Str.int,5,6))
				#y二けたの場合は救済
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})

				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(0,1:12) & ymd$d %in% c(0,1:31)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					#0の場合はNAで埋め込み
					if(ymd$m==0)ymd$m=NA
					if(ymd$d==0)ymd$d=NA
					ans = ymd
				}

				if(!is.null(ans))return(ans)

				ymd$y = as.integer(str_sub(Str.int,1,4))
				ymd$m = as.integer(str_sub(Str.int,5,6))
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}else if(str_length(Str.int)==4){
				ymd$y = as.integer(Str.int)
				ymd$m = NA_integer_
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				ymd$y = NA_integer_
				ymd$m = as.integer(str_sub(Str.int,1,2))
				ymd$d = as.integer(str_sub(Str.int,3,4))
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
				if(ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				ymd$y = as.integer(str_sub(Str.int,1,2))
				ymd$m = as.integer(str_sub(Str.int,3,4))
				ymd$d = NA_integer_
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}

			if(use.exceldate){
				exceldate.beg = as.integer(Date.beg-exceldate.origin)
				exceldate.end = as.integer(Date.end-exceldate.origin)

				dateval = as.integer(Str.int) %>% suppressWarnings()
				if(exceldate.beg<= dateval & dateval <=exceldate.end){
					date = exceldate.origin+dateval
					ymd$y = format(date,"%Y") %>% as.integer()
					ymd$m = format(date,"%m") %>% as.integer()
					ymd$d = format(date,"%d") %>% as.integer()
					return(ymd)
				}
			}
		}
		return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
	}

	return(purrr::map_dfr(str, fn))
}

#' transform ymd data table to string.
#' @description transform ymd data table like data.frame(y=2022,m=5,d=12) to string. NA will be filled by na.fill.
#' @param ymd data.frame with column y, m and d for year, month and day.
#' @param format output format with y,Y,m and d.
#' @param na.fill filled character for missing element.
#' @return transformed str
#' @importFrom magrittr %>%
#' @export
ymd_to_str = function(ymd, format="%Y.%m.%d", na.fill="?"){
	format %>%
		stringr::str_replace_all("%%","%!") %>%
		stringr::str_replace_all("%Y",dplyr::if_else(is.na(ymd$y),strrep(na.fill,4),sprintf("%04d",ymd$y))) %>%
		stringr::str_replace_all("%y",dplyr::if_else(is.na(ymd$y),strrep(na.fill,2),sprintf("%02d",ymd$y))) %>%
		stringr::str_replace_all("%m",dplyr::if_else(is.na(ymd$m),strrep(na.fill,2),sprintf("%02d",ymd$m))) %>%
		stringr::str_replace_all("%d",dplyr::if_else(is.na(ymd$d),strrep(na.fill,2),sprintf("%02d",ymd$d))) %>%
		stringr::str_replace_all("%!","%") %>%
		return()
}
