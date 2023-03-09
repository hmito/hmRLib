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
jpera_to_date = function(str, tryFormats = c("%E-%m-%d", "%E.%m.%d"),...){
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
	return(as.Date(unlist(purrr::map(str,as.Date,optional=TRUE,tryFormats = ADFormats)),origin=Date.origin,...))
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
#' @export
str_to_ymd = function(str, is.range = FALSE, Date.beg = NULL, Date.end = NULL, use.jpera=TRUE, use.exceldate=FALSE,exceldate.origin = as.Date("1904-01-01")){
	#str = c("令和5年2月3日","H23年2月3日","H23.2.4-H23.3.5","　　２０２１年４月２日～２０２１年４月１０日","　　２０２１年４月２日～１０日", "2nd - 10th Dec. 2021", "R2.4.3","from 2021.2.3 to 2022.3.1", "2021-03-02~2021-10-31", "2021.04.23-05.01")
	#Data = readRDS("D:/LocalEES/dat/annual_report/annual_report_2017-2021.rds")
	#strbase = data.frame(str = c(Data$misc$date,  Data$conf_talk$date)) %>%  dplyr::filter(!(stringr::str_detect(str,"^[0-9]{4}$") | stringr::str_detect(str,"^[0-9]{8}$"))) %>%dplyr::pull(str) %>% unique()
	#str = c(str,strbase)

	str.full = c("2012.3.4-2012.5.6","2012年3月4日-2012年5月6日","H24.3.4-H24.5.6","H24年3月4日-H24年5月6日","平成24年3月4日-平成24年5月6日",
					 "4th March, 2012 - 6th May, 2012","4-mar-2012~6-may-2012","4Mar2012 - 6May2012","2012 3/4 - 2012 5/6",
					 "March 4th, 2012 - May 6th, 2012","mar-4-2012~may-6-2012",
					 "2012.3.4-5.6","2012年3月4日-5月6日","H24.3.4-5.6","H24年3月4日-5月6日","平成24年3月4日-5月6日",
					 "4th March - 6th May, 2012","4-mar~6-may-2012","4Mar-6May2012",
					 "March 4th - May 6th, 2012","mar-4~may-6-2012",
					 "2012.3.4-5","2012年3月4-5日","H24.3.4-5","H24年3月4日-5日","平成24年3月4-5日",
					 "4th - 5th March, 2012","4~5-mar-2012","4-5Mar2012",
					 "March 4th - 5th, 2012","mar 4~5 2012"
	)
	str =c(str.full,"20120304-20120506","20120304-506","20120304-6","12.03.04-12.05.06","12.03.04","2012.03-04","2012年3月-4月",
			 "H24.03-04","H24年3月-4月","2012.03","H24.3","H24年3月","2012年3月",
			 "2012.3.X-2012.5.X","????年3月4日-????年5月6日")

	# basic character update
	str = str %>%
		hmRLib::str_to_han() %>%
		stringr::str_remove_all("\\(.+\\)") %>%
		stringr::str_replace_all("\\s+"," ") %>%
		stringr::str_replace_all("\\sof\\s"," ") %>%
		stringr::str_to_lower() %>%
		stringr::str_remove_all("\u3002") %>%
		stringr::str_replace_all("\u3001",",") %>%
		stringr::str_replace_all("/",".") %>%
		return()

	#remove multiple - by .
	str = dplyr::if_else(stringr::str_count(str,"-")!=1, stringr::str_replace_all(str,"[\\-]","."),str)

	# range-split update
	#	default:~ single"-", "to", "から" can be used
	str = str %>%
		stringr::str_replace("-","~") %>%
		stringr::str_remove_all("(^|\\s)from(\\s|$)") %>%
		stringr::str_replace("(^|\\s)to(\\s|$)", "~") %>%
		stringr::str_remove_all("\u307e\u3067") %>%
		stringr::str_replace("\u304b\u3089","~") %>%
		return()

	# add-split update
	#  default:& single"," "\u30fb" "、"\uff65 "and" "および" "及び" "と"
	# ","はかなり有望な候補だが、ここでは保留
	str = str %>%
		stringr::str_replace_all("\\s*and\\s*","&") %>%
		stringr::str_replace_all("[\u30fb\uff65]","&") %>%
		stringr::str_replace_all("(\u304a\u3088\u3073|\u53ca\u3073|\u3068)","&") %>%
		return()

	# missing value
	#  fill 0
	str = str %>%
		stringr::str_replace_all("[\\?x]","x")

	# remove week information
	str = str %>%
		stringr::str_remove_all(
			paste0("(",paste0(c("\u65e5","\u6708","\u706b","\u6c34","\u6728","\u91d1","\u571f"),collapse="|"),")(\u66dc|\u66dc\u65e5),?")
		) %>%
		stringr::str_remove_all(
			"(monday|tuesday|wednesday|thursday|friday|saturday|sunday),?"
		) %>%
		stringr::str_remove_all(
			"(mon|tue|wed|thu|fri|sat|sun)\\.?,?"
		) %>%
		stringr::str_replace_all(
			"(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*,?\\s?","\\1 "
		)

	# 最後に基本的なスペース除去
	str = str %>%
		stringr::str_remove("^\\s+") %>%
		stringr::str_remove("\\s+$") %>%
		stringr::str_replace("\\s*,\\s*",",") %>%
		stringr::str_replace("\\s*~\\s*","~") %>%
		stringr::str_replace("\\s*&\\s*","&") %>%
		return()

	if(is.null(Date.beg)){
		Date.beg = as.Date("1970-01-01")
	}
	if(is.null(Date.end)){
		Date.end = as.Date("2069-12-31")
	}
	Year.beg = as.integer(strftime(Date.beg,"%Y"))
	Year.end = as.integer(strftime(Date.end,"%Y"))
	Month.beg = as.integer(strftime(Date.beg,"%Y"))*12 + as.integer(strftime(Date.beg,"%m"))-1
	Month.end = as.integer(strftime(Date.end,"%Y"))*12 + as.integer(strftime(Date.end,"%m"))-1
	#Candidate_y = as.integer(format(Date.beg,"%Y")):as.integer(format(Date.end,"%Y"))

	YMD_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		y = suppressWarnings(as.integer(ymd[,1]))
		m = suppressWarnings(as.integer(ymd[,2]))
		d = suppressWarnings(as.integer(ymd[,3]))
		y[y==0]=NA
		m[m==0]=NA
		d[d==0]=NA

		is.ymd = !(is.na(y)|is.na(m)|is.na(d))
		Date = as.Date(sprintf("%d-%d-%d",y,m,d),format="%Y-%m-%d",optional=TRUE)
		Date[Date.beg > Date | Date > Date.end]=NA

		is.ym = !(is.na(y)|is.na(m))&is.na(d)
		Month = y*12+m-1
		Month[Month.beg > Month | Month > Month.end]=NA
		Month[1>m | m>12] = NA

		is.y = !(is.na(y)) & is.na(m) & is.na(d)
		Year = y
		Year[Year.beg > Year| Year > Year.end]=NA

		is.md = is.na(y) & !(is.na(m) | is.na(d))
		mdDate = as.Date(sprintf("2000-%s-%s",m,d),format="%Y-%m-%d",optional=TRUE)
		mdDate2 = hmRLib::find_unique(sprintf("%d.%d",m,d),Date.beg + sequence(dplyr::if_else(Date.end - Date.beg > 365+366,integer(1),as.integer(Date.end - Date.beg))),function(x,y){x==strftime(y,"%m.%d")})
		mdDate[!is.na(mdDate)] =


			return(dplyr::case_when(
				is.ymd ~ strftime(Date,"%Y.%m.%d"),
				is.ym ~ sprintf("%04d.%02d.??",as.integer(Month/12),Month%%12+1),
				is.y ~ sprintf("%04d.??.??",Year),
				is.md ~ strftime(mdDate,"????.%m.%d"),
				TRUE ~ NA_character_
			))
	}
	YMDYMD_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		return(stringr::str_c(YMD_to_str(ymd[,1:3]),YMD_to_str(ymd[,4:6]),sep = "-"))
	}
	YjMD_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		ymd[,1] = hmRLib::jpera_to_ystr(ymd[,1])
		return(YMD_to_str(ymd))
	}
	YjMDYjMD_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		return(stringr::str_c(YjMD_to_str(ymd[,1:3]),YjMD_to_str(ymd[,4:6]),sep = "-"))
	}
	YMeDe_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		ymd[,2] = hmRLib::replace_by(ymd[,2],c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),1:12)
		ymd[,3] = stringr::str_remove_all(ymd[,3],"[^0-9]")
		return(YMD_to_str(ymd))
	}
	YMeDeYMeDe_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		return(stringr::str_c(YMeDe_to_str(ymd[,1:3]),YMeDe_to_str(ymd[,4:6]),sep = "-"))
	}
	Y2MD_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		y = (Year.beg:Year.end)[hmRLib::find_unique(as.integer(ymd[,1]),Year.beg:Year.end,function(x,y){x==y%%100})]
		ymd[,1] = dplyr::if_else(is.na(y),NA_character_,sprintf("%d",y))
		return(YMD_to_str(ymd))
	}
	YMDYMD_to_str = function(ymd){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		return(stringr::str_c(YMD_to_str(ymd[,1:3]),YMD_to_str(ymd[,4:6]),sep = "-"))
	}

	p = list(
		Y = "([0-2x][0-9x]{3})",
		Y2 = "([0-9x]{2})",
		to = "[~&]",
		sep = "[\\.\\-\\s/]",
		M = "([0-2x]?[0-9x])",
		D = "([0-3x]?[0-9x])",
		M2 = "([0-2x][0-9x])",
		D2 = "([0-3x][0-9x])",
		beg = "(?:^|[^a-z])",
		end = "(?:$|[^0-9])",
		Y_JP = sprintf("((?:%s)[0-6x]?[0-9x])",paste0(jpera_traits$jpname,collapse="|")),
		Y_jp = sprintf("([%s][0-6x]?[0-9x])",stringr::str_to_lower(paste0(jpera_traits$name,collapse=""))),
		M_en = "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)",
		D_en = "([0-3x]?[0-9x](?:th|nd|rd)?)",
		space = "[\\s\\.,]*"
	)


	ans = rep(NA_character_,length=length(str))
	ans_level = rep(Inf,length=length(str))


	#range detect mode
	if(is.range){
		level = 0 #yyyy.mm.dd-yyyy.mm.dd
		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,p$sep,p$M,p$sep,p$D,p$to,p$Y,p$sep,p$M,p$sep,p$D,p$end),
											 c(1,2,3,4,5,6)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,"\u5e74",p$M,"\u6708",p$D,"\u65e5",p$to,p$Y,"\u5e74",p$M,"\u6708",p$D,"\u65e5"),
											 c(1,2,3,4,5,6)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$D_en,p$space,p$M_en,p$space,p$Y,p$to,p$D_en,p$space,p$M_en,p$space,p$Y,p$end),
											 c(3,2,1,6,5,4)) %>% YMeDeYMeDe_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$M_en,p$space,p$D_en,p$space,p$Y,p$to,p$M_en,p$space,p$D_en,p$space,p$Y,p$end),
											 c(3,1,2,6,4,5)) %>% YMeDeYMeDe_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		if(use.jpera){
			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_jp,p$sep,p$M,p$sep,p$D,p$to,p$Y_jp,p$sep,p$M,p$sep,p$D,p$end),
												 c(1,2,3,4,5,6)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level

			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_jp,"\u5e74",p$M,"\u6708",p$D,"\u65e5",p$to,p$Y_jp,"\u5e74",p$M,"\u6708",p$D,"\u65e5"),
												 c(1,2,3,4,5,6)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level

			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_JP,"\u5e74",p$M,"\u6708",p$D,"\u65e5",p$to,p$Y_JP,"\u5e74",p$M,"\u6708",p$D,"\u65e5"),
												 c(1,2,3,4,5,6)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level
		}

		level = 1 #dd.mm.yyyy-dd.mm.yyyy
		cand = stringr::str_extract(str,
											 paste0(p$beg,p$D,p$sep,p$M,p$sep,p$Y,p$to,p$D,p$sep,p$M,p$sep,p$Y,p$end),
											 c(3,2,1,6,5,4)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		level = 2 #yyyy.mm.dd-mm.dd

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,p$sep,p$M,p$sep,p$D,p$to,p$M,p$sep,p$D,p$end),
											 c(1,2,3,1,4,5)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,"\u5e74",p$M,"\u6708",p$D,"\u65e5",p$to,p$M,"\u6708",p$D,"\u65e5"),
											 c(1,2,3,1,4,5)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$D_en,p$space,p$M_en,p$to,p$D_en,p$space,p$M_en,p$space,p$Y,p$end),
											 c(5,2,1,5,4,3)) %>% YMeDeYMeDe_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$M_en,p$space,p$D_en,p$to,p$M_en,p$space,p$D_en,p$space,p$Y,p$end),
											 c(5,1,2,5,3,4)) %>% YMeDeYMeDe_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		if(use.jpera){
			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_jp,p$sep,p$M,p$sep,p$D,p$to,p$M,p$sep,p$D,p$end),
												 c(1,2,3,1,4,5)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level

			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_jp,"\u5e74",p$M,"\u6708",p$D,"\u65e5",p$to,p$M,"\u6708",p$D,"\u65e5"),
												 c(1,2,3,1,4,5)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level

			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_JP,"\u5e74",p$M,"\u6708",p$D,"\u65e5",p$to,p$M,"\u6708",p$D,"\u65e5"),
												 c(1,2,3,1,4,5)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level
		}


		level = 3 #dd.mm-dd.mm.yyyy
		cand = stringr::str_extract(str,
											 paste0(p$beg,p$D,p$sep,p$M,p$to,p$D,p$sep,p$M,p$sep,p$Y,p$end),
											 c(5,2,1,5,4,3)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		level = 4 #yyyy.mm.dd-dd

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,p$sep,p$M,p$sep,p$D,p$to,p$D,p$end),
											 c(1,2,3,1,2,4)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,"\u5e74",p$M,"\u6708",p$D,"\u65e5","?",p$to,p$D,"\u65e5"),
											 c(1,2,3,1,2,4)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$D_en,p$to,p$D_en,p$space,p$M_en,p$space,p$Y,p$end),
											 c(4,3,1,4,3,2)) %>% YMeDeYMeDe_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$M_en,p$space,p$D_en,p$to,p$D_en,p$space,p$Y,p$end),
											 c(4,1,2,4,1,3)) %>% YMeDeYMeDe_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		if(use.jpera){
			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_jp,p$sep,p$M,p$sep,p$D,p$to,p$D,p$end),
												 c(1,2,3,1,2,4)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level

			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_jp,"\u5e74",p$M,"\u6708",p$D,"\u65e5","?",p$to,p$D,"\u65e5"),
												 c(1,2,3,1,2,4)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level

			cand = stringr::str_extract(str,
												 paste0(p$beg,p$Y_JP,"\u5e74",p$M,"\u6708",p$D,"\u65e5","?",p$to,p$D,"\u65e5"),
												 c(1,2,3,1,2,4)) %>% YjMDYjMD_to_str()
			pos = !is.na(cand)&ans_level>=level
			ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
			ans_level[pos] = level
		}

		level = 5 #dd-dd.mm.yyyy

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$D,p$to,p$D,p$sep,p$M,p$sep,p$Y,p$end),
											 c(4,3,1,4,3,2)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		level = 6 #yyyymmdd-yyyymmdd

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,p$M,p$D2,p$to,p$Y,p$M,p$D2,p$end),
											 c(1,2,3,4,5,6)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		level = 7 #yyyymmdd-mmdd

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,p$M,p$D2,p$to,p$M,p$D2,p$end),
											 c(1,2,3,1,4,5)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		level = 8 #yyyymmdd-dd

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y,p$M,p$D2,p$to,p$D,p$end),
											 c(1,2,3,1,2,4)) %>% YMDYMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level
	}

	# YYYY.MM.DD
	level = 100
	cand = stringr::str_extract(str,
										 paste0(p$beg,p$Y,p$sep,p$M,p$sep,p$D,p$end),
										 c(1,2,3)) %>% YMD_to_str()
	pos = !is.na(cand)&ans_level>=level
	ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
	ans_level[pos] = level

	cand = stringr::str_extract(str,
										 paste0(p$beg,p$Y,"\u5e74",p$M,"\u6708",p$D,"\u65e5"),
										 c(1,2,3)) %>% YMD_to_str()
	pos = !is.na(cand)&ans_level>=level
	ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
	ans_level[pos] = level

	cand = stringr::str_extract(str,
										 paste0(p$beg,p$D_en,p$space,p$M_en,p$space,p$Y,p$end),
										 c(3,2,1)) %>% YMeDe_to_str()
	pos = !is.na(cand)&ans_level>=level
	ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
	ans_level[pos] = level

	cand = stringr::str_extract(str,
										 paste0(p$beg,p$M_en,p$space,p$D_en,p$space,p$Y,p$end),
										 c(3,1,2)) %>% YMeDe_to_str()
	pos = !is.na(cand)&ans_level>=level
	ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
	ans_level[pos] = level

	if(use.jpera){
		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y_jp,p$sep,p$M,p$sep,p$D,p$end),
											 c(1,2,3)) %>% YjMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y_jp,"\u5e74",p$M,"\u6708",p$D,"\u65e5"),
											 c(1,2,3)) %>% YjMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level

		cand = stringr::str_extract(str,
											 paste0(p$beg,p$Y_JP,"\u5e74",p$M,"\u6708",p$D,"\u65e5"),
											 c(1,2,3)) %>% YjMD_to_str()
		pos = !is.na(cand)&ans_level>=level
		ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
		ans_level[pos] = level
	}

	level = 101 #dd.mm.yyyy
	cand = stringr::str_extract(str,
										 paste0(p$beg,p$D,p$sep,p$M,p$sep,p$Y,p$end),
										 c(3,2,1)) %>% YMD_to_str()
	pos = !is.na(cand)&ans_level>=level
	ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
	ans_level[pos] = level

	level = 102 #yyyymmdd

	cand = stringr::str_extract(str,
										 paste0(p$beg,p$Y,p$M,p$D2,p$end),
										 c(1,2,3)) %>% YMD_to_str()
	pos = !is.na(cand)&ans_level>=level
	ans[pos] = dplyr::if_else(ans_level[pos]==level,NA_character_,cand[pos])
	ans_level[pos] = level

	#TODO: 残る処理は、Y2.M.D-Y2.M.D、Y2.M.D、Y.M-Y.M、Y.M、Y2MD-Y2MD|YM-Y?M、Y2MD|YM、Y | MD-M?D | Y2M-Y2?M、Y | MD | Y2M、Excel

	sstr = stringr::str_split(Str,"[~&]")[[1]]
	#fn(sstr[2],NULL,NULL,NULL)
	fn = function(Str,Hint,Date.beg,Date.end){
		ymd = data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_)
		ans = NULL

		if(is.na(Str))return(ymd)

		if(stringr::str_detect(Str,"[0-9]+\u5e74[0-9]+\u6708[0-9]+\u65e5")){
			#年月日の場合
			Str = stringr::str_remove_all(Str,"\\s")
			if(stringr::str_split(Str,"[~&]")[[1]])
				if(use.jpera){
					#年度の場合
					if(str_detect("\u5e74\u5ea6")){
						IsFY = TRUE
						ymd$y = Str %>% stringr::str_extract("(.*[0-9]{1,4})\u5e74\u5ea6",1) %>% hmRLib::jpera_to_ystr() %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}else{
						IsFY = FALSE
						ymd$y = Str %>% stringr::str_extract("(.*[0-9]{1,4})\u5e74",1) %>% hmRLib::jpera_to_ystr() %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}
				}else{
					if(str_detect("\u5e74\u5ea6")){
						IsFY = TRUE
						ymd$y = Str %>% stringr::str_extract("(.*[0-9]{1,4})\u5e74\u5ea6",1) %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}else{
						IsFY = FALSE
						ymd$y = Str %>% stringr::str_extract("(.*[0-9]{1,4})\u5e74",1) %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}
				}
			ymd$m = Str %>% stringr::str_extract("([0-9]{1,2})\u6708",1) %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
			ymd$d = Str %>% stringr::str_extract("([0-9]{1,2})\u65e5",1) %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()

			#二けた表記の場合、可能性のある年で救済
			ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})

			if(ymd$y %in% c(NA,Candidate_y) & ymd$m %in% c(NA,1:12) & ymd$d %in% c(NA,1:31))return(ymd)
			return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
		}

		#?やXは未定値として扱うためゼロ置換
		Str = stringr::str_replace_all(Str,"[?X]","0")

		Str.split = stringr::str_split(Str,"[\\s\\t\\.\\-/,]")[[1]]
		Str.split = Str.split[Str.split!=""]
		if(length(Str.split)>=2){
			#Check English
			EnMonth.seq = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
			EnMonth = stringr::str_detect(stringr::str_to_lower(Str.split),paste(EnMonth.seq,collapse="|"))
			if(sum(EnMonth)==1){
				for(i in 1:12){
					if(stringr::str_detect(stringr::str_to_lower(Str.split[EnMonth]),EnMonth.seq[i])){
						ymd$m = i
						break
					}
				}
				Str.split = Str.split[!EnMonth]

				if(length(Str.split)>=2){
					ymd$d = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$y = Str.split[2] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
						if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
						ans = ymd
					}

					ymd$d = Str.split[2] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$y = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
						if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
						ans = ymd
					}

					if(!is.null(ans))return(ans)
				}

				ymd$d = NA_integer_
				ymd$y = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				ymd$d = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
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
						ymd$y = Str.split[1] %>% hmRLib::jpera_to_ystr() %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}else{
						ymd$y = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}
					ymd$m = Str.split[2] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$d = Str.split[3] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
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
						ymd$y = Str.split[3] %>% hmRLib::jpera_to_ystr() %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}else{
						ymd$y = Str.split[3] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					}
					ymd$m = Str.split[2] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$d = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
					ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
					if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
						if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
						ans = ymd
					}

					#DMYは一般的ではないが、フルに情報がそろっていればOKとする
					if(!is.null(ans))return(ans)
				}

				if(use.jpera){
					ymd$y = Str.split[1] %>% hmRLib::jpera_to_ystr() %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}else{
					ymd$y = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
				ymd$m = Str.split[2] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(use.jpera){
					ymd$y = Str.split[2] %>% hmRLib::jpera_to_ystr() %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}else{
					ymd$y = Str.split[2] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				}
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
				ymd$m = Str.split[1] %>% stringr::str_remove_all("[^0-9]") %>% as.integer() %>% suppressWarnings()
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}
		}

		Str.int = stringr::str_extract(Str,"^[^0-9]*([0-9]{4,8})[^0-9]*+$",1)
		if(!is.na(Str.int)){
			if(stringr::str_length(Str.int)==8){
				ymd$y = as.integer(stringr::str_sub(Str.int,1,4))
				ymd$m = as.integer(stringr::str_sub(Str.int,5,6))
				ymd$d = as.integer(stringr::str_sub(Str.int,7,8))


				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(0,1:12) & ymd$d %in% c(0,1:31)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					#0の場合はNAで埋め込み
					if(ymd$m==0)ymd$m=NA
					if(ymd$d==0)ymd$d=NA
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}else if(stringr::str_length(Str.int)==6){
				ymd$y = as.integer(stringr::str_sub(Str.int,1,2))
				ymd$m = as.integer(stringr::str_sub(Str.int,3,4))
				ymd$d = as.integer(stringr::str_sub(Str.int,5,6))
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

				ymd$y = as.integer(stringr::str_sub(Str.int,1,4))
				ymd$m = as.integer(stringr::str_sub(Str.int,5,6))
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y) & ymd$m %in% c(1:12)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				if(!is.null(ans))return(ans)
				return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
			}else if(stringr::str_length(Str.int)==4){
				ymd$y = as.integer(Str.int)
				ymd$m = NA_integer_
				ymd$d = NA_integer_
				if(ymd$y %in% c(Candidate_y)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				ymd$y = NA_integer_
				ymd$m = as.integer(stringr::str_sub(Str.int,1,2))
				ymd$d = as.integer(stringr::str_sub(Str.int,3,4))
				ymd$y = purrr::map_int(ymd$y,function(x){if(is.na(x))return(x);if(sum(x==Candidate_y%%100)!=1)return(x);Candidate_y[x==Candidate_y%%100]})
				if(ymd$m %in% c(1:12) & ymd$d %in% c(1:31)){
					if(!is.null(ans)) return(data.frame(y=NA_integer_,m=NA_integer_,d=NA_integer_))
					ans = ymd
				}

				ymd$y = as.integer(stringr::str_sub(Str.int,1,2))
				ymd$m = as.integer(stringr::str_sub(Str.int,3,4))
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
