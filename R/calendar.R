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
#' @param ... arguments passing to as.Date function
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
	str = str %>%
		stringr::str_remove("^\\s*") %>%
		stringr::str_remove("\\s*$") %>%
		return()

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

#' formatting date string
#' @description unify valid format of date
#' @param datestr target character or sequence of character.
#' @param Date.beg potentially begin date; used for validation and estimation.
#' @param Date.end potentially end date; used for validation and estimation.
#' @param strict return NA if read datestr is out of Date.beg-Date.end range
#' @param use.range Allow range date format
#' @param use.jpera Allow Japanese era case (e.g., R2.4.5 or 令和3年4月11日).
#' @param use.exceldate Allow to check the value which is originally excel date but occationally transformed to normal value.
#' @param exceldate.origin Origin of excel date. In default, it is 1900-01-01 if the file is originally created on Windows and 1904-01-01 on Mac.
#' @return transformed date string like "2021.03.04-2023.05.31"
#' @importFrom magrittr %>%
#' @export
datestr_format_full = function(datestr, Date.beg = NULL, Date.end = NULL, strict = TRUE, use.range=TRUE, use.jpera=TRUE, use.exceldate=FALSE,exceldate.origin = as.Date("1904-01-01")){
	# basic character update
	datestr = datestr %>%
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
	datestr = dplyr::if_else(stringr::str_count(datestr,"-")!=1, stringr::str_replace_all(datestr,"[\\-]","."),datestr)

	# range-split update
	#	default:~ single"-", "to", "から" can be used
	datestr = datestr %>%
		stringr::str_replace("-","~") %>%
		stringr::str_remove_all("(^|\\s)from(\\s|$)") %>%
		stringr::str_replace("(^|\\s)to(\\s|$)", "~") %>%
		stringr::str_remove_all("\u307e\u3067") %>%
		stringr::str_replace("\u304b\u3089","~") %>%
		return()

	# add-split update
	#  default:& single"," "\u30fb" "、"\uff65 "and" "および" "及び" "と"
	# ","はかなり有望な候補だが、ここでは保留
	datestr = datestr %>%
		stringr::str_replace_all("\\s*and\\s*","&") %>%
		stringr::str_replace_all("[\u30fb\uff65]","&") %>%
		stringr::str_replace_all("(\u304a\u3088\u3073|\u53ca\u3073|\u3068)","&") %>%
		return()

	# missing value
	#  fill 0
	datestr = datestr %>%
		stringr::str_replace_all("[\\?x]","x")

	# remove week information
	datestr = datestr %>%
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
	datestr = datestr %>%
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

	# これを正規表現に放り込むので、エスケープのエスケープが必須
	P = list(
		Y = "([0-2x][0-9x]{3})",
		Y2 = "([0-9x]{2})",
		to = "[~&]",
		sep = "[\\\\.\\\\-\\\\s]",
		space = "[\\\\s\\\\.,]*",
		M = "([0-2x]?[0-9x])",
		D = "([0-3x]?[0-9x])",
		M2 = "([0-2x][0-9x])",
		D2 = "([0-3x][0-9x])",
		beg = "(?:^|[^a-z0-9])",
		end = "(?:$|[^a-z0-9])",
		Y_JP = sprintf("((?:%s)[0-6x]?[0-9x])",paste0(jpera_traits$jpname,collapse="|")),
		Y_jp = sprintf("([%s][0-6x]?[0-9x])",stringr::str_to_lower(paste0(jpera_traits$name,collapse=""))),
		M_en = "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)",
		D_en = "([0-3x]?[0-9x](?:th|nd|rd)?)"
	)
	YMD_to_str = function(ymd, opt=NULL){
		if(is.vector(ymd)){
			if("-MD" %in% opt){
				ymd = cbind(matrix(ymd,ncol=1), matrix(NA_character_,nrow=length(ymd),ncol=1), matrix(NA_character_,nrow=length(ymd),ncol=1))
			}else{
				ymd = matrix(ymd,nrow=1)
			}
		}
		if("-Y" %in% opt){
			ymd = cbind(matrix(NA_character_,nrow=nrow(ymd),ncol=1), ymd)
		}
		if("-D" %in% opt){
			ymd = cbind(ymd, matrix(NA_character_,nrow=nrow(ymd),ncol=1))
		}
		if("Yj" %in% opt){
			ymd[,1] = hmRLib::jpera_to_ystr(ymd[,1])
		}
		if("Y2" %in% opt){
			ymd1 = (Year.beg:Year.end)[hmRLib::find_unique(suppressWarnings(as.integer(ymd[,1])),Year.beg:Year.end,function(x,y){x==y%%100})]
			ymd[,1] = dplyr::if_else(is.na(ymd1),NA_character_,sprintf("%d",ymd1))
		}
		if("Me" %in% opt){
			ymd[,2] = hmRLib::replace_by(ymd[,2],c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),1:12)
		}
		if("De" %in% opt){
			ymd[,3] = stringr::str_remove_all(ymd[,3],"[^0-9]")
		}

		y = suppressWarnings(as.integer(ymd[,1]))
		m = suppressWarnings(as.integer(ymd[,2]))
		d = suppressWarnings(as.integer(ymd[,3]))
		y[y==0]=NA
		m[m==0]=NA
		d[d==0]=NA

		is.ymd = !(is.na(y)|is.na(m)|is.na(d))
		Date = as.Date(sprintf("%d-%d-%d",y,m,d),format="%Y-%m-%d",optional=TRUE)
		DateStr = strftime(Date,"%Y.%m.%d")
		if(strict){
			DateStr[Date.beg > Date | Date > Date.end]=NA
		}

		is.ym = !(is.na(y)|is.na(m))&is.na(d)
		Month = y*12+m-1
		MonthStr = sprintf("%04d.%02d.??",as.integer(Month/12),Month%%12+1)
		MonthStr[1>m | m>12] = NA
		if(strict){
			MonthStr[Month.beg > Month | Month > Month.end]=NA
		}

		is.y = !(is.na(y)) & is.na(m) & is.na(d)
		Year = y
		YearStr = sprintf("%04d.??.??",Year)
		if(strict){
			YearStr[Year.beg > Year| Year > Year.end]=NA
		}

		is.md = is.na(y) & !(is.na(m) | is.na(d))
		mdDate = as.Date(sprintf("2000-%s-%s",m,d),format="%Y-%m-%d",optional=TRUE)
		TestDates = Date.beg + sequence(dplyr::if_else(Date.end - Date.beg > 365+366,integer(1),as.integer(Date.end - Date.beg)+1L),from=0)
		mdDate2 = TestDates[hmRLib::find_unique(sprintf("%02d.%02d",m,d),TestDates,function(x,y){x==strftime(y,"%m.%d")})]
		mdStr = dplyr::if_else(!is.na(mdDate2),strftime(mdDate2,"%Y.%m.%d"),strftime(mdDate,"????.%m.%d"))

		return(dplyr::case_when(
			is.ymd ~ DateStr,
			is.ym ~ MonthStr,
			is.y ~ YearStr,
			is.md ~ mdStr,
			TRUE ~ NA_character_
		))
	}
	YMDYMD_to_str = function(ymd, opt=NULL){
		if(is.vector(ymd)){
			ymd = matrix(ymd,nrow=1)
		}
		return(stringr::str_c(YMD_to_str(ymd[,1:(ncol(ymd)/2)],opt),YMD_to_str(ymd[,1:(ncol(ymd)/2)+(ncol(ymd)/2)],opt),sep = "-"))
	}
	try_cand = function(pack, pattern){
		opt = c(
			dplyr::if_else(stringr::str_detect(pattern,"%Yj|%YJ"),"Yj",NA_character_),
			dplyr::if_else(stringr::str_detect(pattern,"%Y2"),"Y2",NA_character_),
			dplyr::if_else(stringr::str_detect(pattern,"%Me"),"Me",NA_character_),
			dplyr::if_else(stringr::str_detect(pattern,"%De"),"De",NA_character_),
			dplyr::case_when(
				!stringr::str_detect(pattern,"%M|%D")~"-MD",
				!stringr::str_detect(pattern,"%D")~"-D",
				!stringr::str_detect(pattern,"%Y")~"-Y",
				TRUE ~ NA_character_
			)
		)

		pickup = stringr::str_extract_all(pattern,"%[YMD]",simplify = TRUE) %>% as.vector()
		second_apper = duplicated(pickup)
		if(any(second_apper)){
			to_str = YMDYMD_to_str
			pickup[second_apper] = stringr::str_c(pickup[second_apper],"2")
			pickup_order = hmRLib::find_unique(c("%Y","%M","%D","%Y2","%M2","%D2"),pickup)
			pickup_order[c(4,5,6)][is.na(pickup_order[c(4,5,6)])]=pickup_order[c(1,2,3)][is.na(pickup_order[c(4,5,6)])]
			pickup_order = pickup_order[!is.na(pickup_order)]
		}else{
			to_str = YMD_to_str
			pickup_order = hmRLib::find_unique(c("%Y","%M","%D"),pickup)
			pickup_order = pickup_order[!is.na(pickup_order)]
		}

		opt = opt[!is.na(opt)]
		cand = stringr::str_extract(
			pack$datestr,
			pattern %>%
				stringr::str_replace_all("\\-",P$to) %>%
				stringr::str_replace_all("\\.",P$sep) %>%
				stringr::str_replace_all("_",P$space) %>%
				stringr::str_replace_all("%Yj@",paste0(P$Y_jp,"\u5e74")) %>%
				stringr::str_replace_all("%YJ@",paste0(P$Y_JP,"\u5e74")) %>%
				stringr::str_replace_all("%Y@",paste0(P$Y,"\u5e74")) %>%
				stringr::str_replace_all("%M@",paste0(P$M,"\u6708")) %>%
				stringr::str_replace_all("%D@",paste0(P$D,"\u65e5")) %>%
				stringr::str_replace_all("%Yj",P$Y_jp) %>%
				stringr::str_replace_all("%YJ",P$Y_JP) %>%
				stringr::str_replace_all("%Y2",P$Y2) %>%
				stringr::str_replace_all("%Y",P$Y) %>%
				stringr::str_replace_all("%Me",P$M_en) %>%
				stringr::str_replace_all("%De",P$D_en) %>%
				stringr::str_replace_all("%D2",P$D2) %>%
				stringr::str_replace_all("%M2",P$M2) %>%
				stringr::str_replace_all("%M",P$M) %>%
				stringr::str_replace_all("%D",P$D) %>%
				stringr::str_replace("^",P$beg) %>%
				stringr::str_replace("$",P$end) %>%
				return(),pickup_order) %>% to_str(opt)
		pos = !is.na(cand)&pack$ans_level>=pack$level
		pack$ans[pos] = dplyr::if_else(pack$ans_level[pos]==pack$level,NA_character_,cand[pos])
		pack$ans_level[pos] = pack$level
		return(pack)
	}

	pack = list(
		datestr = datestr,
		level = Inf,
		ans = rep(NA_character_,length=length(datestr)),
		ans_level = rep(Inf,length=length(datestr))
	)

	#range detect mode
	if(use.range){
		pack$level = 100 #YYYY.M.D-YYYY.M.D
		pack = try_cand(pack,"%Y.%M.%D-%Y.%M.%D")
		pack = try_cand(pack,"%Y@%M@%D@-%Y@%M@%D@")
		pack = try_cand(pack,"%Y_%Me_%De-%Y_%Me_%De")
		pack = try_cand(pack,"%De_%Me_%Y-%De_%Me_%Y")
		pack = try_cand(pack,"%Me_%De_%Y-%Me_%De_%Y")
		if(use.jpera){
			pack = try_cand(pack,"%Yj.%M.%D-%Yj.%M.%D")
			pack = try_cand(pack,"%Yj@%M@%D@-%Yj@%M@%D@")
			pack = try_cand(pack,"%YJ@%M@%D@-%YJ@%M@%D@")
		}

		pack$level = 110
		pack = try_cand(pack,"%D.%M.%Y-%D.%M.%Y")

		pack$level = 200
		pack = try_cand(pack,"%Y.%M.%D-%M.%D")
		pack = try_cand(pack,"%Y@%M@%D@-%M@%D@")
		pack = try_cand(pack,"%Y_%Me_%De-%Me_%De")
		pack = try_cand(pack,"%De_%Me-%De_%Me_%Y")
		pack = try_cand(pack,"%Me_%De-%Me_%De_%Y")
		if(use.jpera){
			pack = try_cand(pack,"%Yj.%M.%D-%M.%D")
			pack = try_cand(pack,"%Yj@%M@%D@-%M@%D@")
			pack = try_cand(pack,"%YJ@%M@%D@-%M@%D@")
		}

		pack$level = 210
		pack = try_cand(pack,"%D.%M-%D.%M.%Y")

		pack$level = 300 #yyyy.mm.dd-dd
		pack = try_cand(pack,"%Y.%M.%D-%D")
		pack = try_cand(pack,"%Y@%M@%D@?-%D@")
		pack = try_cand(pack,"%Y_%Me_%De-%De")
		pack = try_cand(pack,"%De-%De_%Me_%Y")
		pack = try_cand(pack,"%Me_%De-%De_%Y")
		if(use.jpera){
			pack = try_cand(pack, "%Yj.%M.%D-%D")
			pack = try_cand(pack, "%Yj@%M@%D@?-%D@")
			pack = try_cand(pack, "%YJ@%M@%D@?-%D@")
		}

		pack$level = 310 #dd-dd.mm.yyyy
		pack = try_cand(pack, "%D-%D.%M.%Y")

		pack$level = 400 #yyyymmdd-yyyymmdd
		pack = try_cand(pack, "%Y%M%D2-%Y%M%D2")

		pack$level = 410 #yyyymmdd-mmdd
		pack = try_cand(pack,"%Y%M%D2-%M%D2")

		pack$level = 420 #yyyymmdd-dd
		pack = try_cand(pack,"%Y%M%D2-%D")
	}

	# YYYY.MM.DD
	pack$level = 1100
	pack = try_cand(pack, "%Y.%M.%D")
	pack = try_cand(pack, "%Y@%M@%D@")
	pack = try_cand(pack, "%Y_%Me_%De")
	pack = try_cand(pack, "%De_%Me_%Y")
	pack = try_cand(pack, "%Me_%De_%Y")
	if(use.jpera){
		pack = try_cand(pack,"%Yj.%M.%D")
		pack = try_cand(pack,"%Yj@%M@%D@")
		pack = try_cand(pack,"%YJ@%M@%D@")
	}


	pack$level = 1110 #dd.mm.yyyy
	pack = try_cand(pack,"%D.%M.%Y")

	pack$level = 1400 #yyyymmdd
	pack = try_cand(pack, "%Y%M%D2")

	pack$level = 2000 #yy.mm.dd-yy.mm.dd
	pack = try_cand(pack, "%Y2.%M.%D-%Y2.%M.%D")

	pack$level = 2010 #yy.mm.dd-mm.dd
	pack = try_cand(pack,"%Y2.%M.%D-%M.%D")

	pack$level = 2020 #yy.mm.dd-dd
	pack = try_cand(pack,"%Y2.%M.%D-%D")

	pack$level = 2100 #yy.mm.dd
	pack = try_cand(pack,"%Y2.%M.%D")

	if(use.range){
		pack$level = 3000 #yyyy.mm-yyyy.mm
		pack = try_cand(pack, "%Y.%M-%Y.%M")
		pack = try_cand(pack, "%Y@%M@-%Y@%M@")
		pack = try_cand(pack, "%Y_%Me-%Y_%Me")
		pack = try_cand(pack, "%Me_%Y-%Me_%Y")
		if(use.jpera){
			pack = try_cand(pack,"%Yj.%M-%Yj.%M")
			pack = try_cand(pack,"%Yj@%M@-%Yj@%M@")
			pack = try_cand(pack,"%YJ@%M@-%YJ@%M@")
		}
		pack$level = 3010
		pack = try_cand(pack,"%M.%Y-%M.%Y")
		pack$level = 3020 #yyyy.mm-yyyy.mm
		pack = try_cand(pack, "%Y.%M-%M")
		pack = try_cand(pack, "%Y@%M@-%M@")
		pack = try_cand(pack, "%Y_%Me-%Me")
		pack = try_cand(pack, "%Me_%Me_%Y")
		if(use.jpera){
			pack = try_cand(pack,"%Yj.%M-%M")
			pack = try_cand(pack,"%Yj@%M@-%M@")
			pack = try_cand(pack,"%YJ@%M@-%M@")
		}
		pack$level = 3030
		pack = try_cand(pack,"%M-%M.%Y")
	}

	pack$level = 3100 #yyyy.mm
	pack = try_cand(pack, "%Y.%M")
	pack = try_cand(pack, "%Y@%M@")
	pack = try_cand(pack, "%Y_%Me")
	pack = try_cand(pack, "%Me_%Y")
	if(use.jpera){
		pack = try_cand(pack,"%Yj.%M")
		pack = try_cand(pack,"%Yj@%M@")
		pack = try_cand(pack,"%YJ@%M@")
	}
	pack$level = 3110
	pack = try_cand(pack,"%M.%Y")

	if(use.range){
		pack$level = 4000 #yymmdd-yymmdd or yyyymm-yyyymm
		pack = try_cand(pack, "%Y2%M2%D2-%Y2%M2%D2")
		pack = try_cand(pack,"%Y%M2-%Y%M2")

		pack$level = 4010 #yymmdd-mmdd or yyyymm-yyyymm
		pack = try_cand(pack, "%Y2%M2%D2-%M2%D2")

		pack$level = 4020 #yymmdd-dd or yyyymm-mm
		pack = try_cand(pack, "%Y2%M2%D2-%D2")
		pack = try_cand(pack,"%Y%M2-%M2")
	}
	pack$level = 4100 #yymmdd
	pack = try_cand(pack, "%Y2%M2%D2")
	pack = try_cand(pack, "%Y%M2")

	if(use.range){
		pack$level = 5000 #yyyy.mm-yyyy.mm
		pack = try_cand(pack, "%M@%D@-%M@%D@")
		pack = try_cand(pack, "%Me_%De-%Me_%De")
		pack = try_cand(pack, "%De_%Me-%De_%Me")

		pack$level = 5020 #yyyy.mm-yyyy.mm
		pack = try_cand(pack, "%M@%D@?-%D@")
		pack = try_cand(pack, "%Me_%De-%De")
		pack = try_cand(pack, "%De-%De_%Me")
	}

	pack$level = 5100 #yyyy.mm
	pack = try_cand(pack, "%M@%D@")
	pack = try_cand(pack, "%De_%Me")
	pack = try_cand(pack, "%Me_%De")

	if(use.range){
		pack$level = 5200 #yy.mm-yy.mm or mm.dd-mm.dd
		pack = try_cand(pack, "%Y2.%M-%Y2.%M")
		pack = try_cand(pack, "%M.%D-%M.%D")
		pack = try_cand(pack, "%Y2.%M-%M")
		pack = try_cand(pack, "%M.%D-%D")
	}
	pack$level = 5300 #yy.mm or mm.dd
	pack = try_cand(pack, "%Y2.%M")
	pack = try_cand(pack, "%M.%D")

	if(use.range){
		pack$level = 6000 #yyyy-yyyy or yymm-yymm or mmdd-mmdd
		pack = try_cand(pack, "%Y-%Y")
		pack = try_cand(pack,"%Y2%M2-%Y2%M2")
		pack = try_cand(pack,"%M%D2-%M%D2")

		pack$level = 6020 #yymmdd-dd or yyyymm-mm
		pack = try_cand(pack, "%Y2%M2-%M2")
		pack = try_cand(pack,"%M%D2-%D2")
	}
	pack$level = 6100 #yymmdd
	pack = try_cand(pack, "%Y")
	pack = try_cand(pack, "%Y2%M2")
	pack = try_cand(pack, "%M%D2")

	#try excel
	pack$level = 10000 #excel date value
	if(use.exceldate){
		exceldate.beg = as.integer(Date.beg-exceldate.origin)
		exceldate.end = as.integer(Date.end-exceldate.origin)

		dateval = pack$datestr %>% as.integer() %>% suppressWarnings()
		cand = dplyr::if_else(exceldate.beg<= dateval & dateval <=exceldate.end,strftime(exceldate.origin+dateval,"%Y.%m.%d"),NA_character_)
		pos = !is.na(cand)&pack$ans_level>=pack$level
		pack$ans[pos] = dplyr::if_else(pack$ans_level[pos]==pack$level,NA_character_,cand[pos])
		pack$ans_level[pos] = pack$level
	}

	return(pack$ans)
}

#' transform string to ymd data table.
#' @description transform string to ymd data table like data.frame(y=2022,m=5,d=12) or pair of such data.frame. NA will be filled for uncertain elements.
#' @param datestr target character or sequence of character.
#' @param Date.beg potentially start date; used for validation and estimation.
#' @param Date.end potentially end date; used for validation and estimation.
#' @param strict return NA if read datestr is out of Date.beg-Date.end range
#' @param use.range Allow range date format
#' @param use.jpera Allow Japanese era case (e.g., R2.4.5 or 令和3年4月11日).
#' @param use.exceldate Allow to check the value which is originally excel date but occationally transformed to normal value.
#' @param exceldate.origin Origin of excel date. In default, it is 1900-01-01 if the file is originally created on Windows and 1904-01-01 on Mac.
#' @return transformed data.frame
#' @importFrom magrittr %>%
#' @export
datestr_to_ymd = function(datestr, Date.beg = NULL, Date.end = NULL, strict = TRUE, use.range=TRUE, use.jpera=TRUE, use.exceldate=FALSE,exceldate.origin = as.Date("1904-01-01")){
	table = datestr_format_full(datestr,Date.beg,Date.end,strict,use.range,use.jpera,use.exceldate,exceldate.origin) %>%
		stringr::str_split("[\\.\\-]",simplify = TRUE) %>%
		return()

	if(ncol(table)==3){
		ans = data.frame(
			y=as.integer(table[,1]) %>% suppressWarnings(),
			m=as.integer(table[,2]) %>% suppressWarnings(),
			d=as.integer(table[,3]) %>% suppressWarnings()
		)
	}else if(ncol(table)==6){
		beg = data.frame(
			y=as.integer(table[,1]) %>% suppressWarnings(),
			m=as.integer(table[,2]) %>% suppressWarnings(),
			d=as.integer(table[,3]) %>% suppressWarnings()
		)
		end = data.frame(
			y=as.integer(table[,4]) %>% suppressWarnings(),
			m=as.integer(table[,5]) %>% suppressWarnings(),
			d=as.integer(table[,6]) %>% suppressWarnings()
		)
		endna = is.na(end$y) & is.na(end$m) & is.na(end$d)
		end[endna,]=beg[endna,]
		ans = list(beg=beg,end=end)
	}
	return(ans)
}

#' transform ymd data table to string.
#' @description transform ymd data table like data.frame(y=2022,m=5,d=12) to string. NA will be filled by na.fill.
#' @param ymd data.frame with column y, m and d for year, month and day.
#' @param dateformat output format with Y, y, M, m, D, d. See detais.
#' @param range_sep separation of range if ymd is range format.
#' @param na.fill filled character for missing element.
#' @param mode determining data filling mode = c("full","maximum","compact","short","minimum"). see detail.
#' @return transformed str
#' @details
#' format of each mode is like the following.
#' 	maximum: always range mode e.g., 2012.03.04-2012.03.04 2023.04.05-2023.05.06
#' 	full (default): range mode is used only when beg and end is different e.g., 2012.03.04 2023.04.05-2023.05.06
#' 	compact: range mode remove duplicating information e.g., 2023.04.05-05.06 2023.04.??
#' 	minimum: (information loss can occur depending on dateformat) remove NA and duplicating info e.g., 2023.04-05
#' dateformat can use the following special characters.
#'		(percent)Yj(at): NOT WORKING
#'		(percent)YJ(at): NOT WORKING
#'		(percent)Y(at): YYYY年
#'		(percent)y(at): YY年
#'		(percent)M(at): MM月
#'		(percent)m(at): M月
#'		(percent)D(at): DD日
#'		(percent)d(at): D日
#'		(percent)Y: YYYY
#'		(percent)y: YY
#'		(percent)M: MM
#'		(percent)m: M
#'		(percent)D: DD
#'		(percent)d: D
#' @importFrom magrittr %>%
#' @export
ymd_to_datestr = function(ymd, dateformat="%Y.%M.%D", range_sep = "-", na.fill="?",mode = "full"){
	if(!is.null(ymd$beg) & !is.null(ymd$end)){
		if(mode=="maximum"){
			return(paste0(ymd_to_datestr(ymd$beg,dateformat,range_sep,na.fill,mode),
							  range_sep,
							  ymd_to_datestr(ymd$beg,dateformat,range_sep,na.fill,mode)))
		}else if(mode=="full"){
			beg = ymd_to_datestr(ymd$beg,dateformat,range_sep,na.fill,mode)
			end = ymd_to_datestr(ymd$end,dateformat,range_sep,na.fill,mode)
			return(
				dplyr::if_else(beg==end,beg,paste0(beg,range_sep,end))
			)
		}else if(mode=="compact" | mode=="minimum"){
			beg = ymd_to_datestr(ymd$beg,dateformat,range_sep,na.fill,mode)
			endYMD = ymd_to_datestr(ymd$end,dateformat,range_sep,na.fill,mode)
			endMD = ymd_to_datestr(ymd$end,stringr::str_remove(dateformat,"[^@a-zA-Z0-9]*%[Yy][^%]*"),range_sep,na.fill,mode)
			endD = ymd_to_datestr(ymd$end,stringr::str_remove(dateformat,"[^@a-zA-Z0-9]*%[Yy][^%]*") %>% stringr::str_remove("[^@a-zA-Z0-9]*%[Mm][^%]*"),range_sep,na.fill,mode)

			return(
				dplyr::case_when(
					ymd$beg$y!=ymd$end$y ~ paste0(beg,range_sep,endYMD),
					ymd$beg$m!=ymd$end$m ~ paste0(beg,range_sep,endMD),
					ymd$beg$d!=ymd$end$d ~ paste0(beg,range_sep,endD),
					TRUE ~ beg
				)
			)
		}
	}else{
		if(mode == "minimum"){
			endYMD = ymd_to_datestr(ymd,dateformat,range_sep,na.fill,"full")
			endMD = ymd_to_datestr(ymd,stringr::str_remove(dateformat,"%[Yy][^%]*"),range_sep,na.fill,"full")
			endYM = ymd_to_datestr(ymd,stringr::str_remove(dateformat,"[^@a-zA-Z0-9]*%[Dd][^%]*"),range_sep,na.fill,"full")
			endY = ymd_to_datestr(ymd,stringr::str_remove(dateformat,"[^@a-zA-Z0-9]*%[Dd][^%]*") %>% stringr::str_remove("[^@a-zA-Z0-9]*%[Mm][^%]*"),range_sep,na.fill,"full")

			dplyr::case_when(
				!(is.na(ymd$y)|is.na(ymd$m)|is.na(ymd$d)) ~ endYMD,
				!(is.na(ymd$y)|is.na(ymd$m)) ~ endYM,
				!(is.na(ymd$m)|is.na(ymd$d)) ~ endMD,
				!(is.na(ymd$y)) ~ endY,
				TRUE ~ NA_character_
			)
		}else{
			dateformat %>%
				stringr::str_replace_all("%%","%!") %>%
				stringr::str_replace_all("%Y@",dplyr::if_else(!is.na(ymd$y),sprintf("%d\u5e74",ymd$y),sprintf("%s\u5e74",strrep(na.fill,4)))) %>%
				stringr::str_replace_all("%y@",dplyr::if_else(!is.na(ymd$y),sprintf("%02d\u5e74",ymd$y%%100),sprintf("%s\u5e74",strrep(na.fill,2)))) %>%
				stringr::str_replace_all("%M@",dplyr::if_else(!is.na(ymd$m),sprintf("%02d\u6708",ymd$m),sprintf("%s\u6708",strrep(na.fill,2)))) %>%
				stringr::str_replace_all("%D@",dplyr::if_else(!is.na(ymd$d),sprintf("%02d\u65e5",ymd$d),sprintf("%s\u65e5",strrep(na.fill,2)))) %>%
				stringr::str_replace_all("%m@",dplyr::if_else(!is.na(ymd$m),sprintf("%d\u6708",ymd$m),sprintf("%s\u6708",strrep(na.fill,1)))) %>%
				stringr::str_replace_all("%d@",dplyr::if_else(!is.na(ymd$d),sprintf("%d\u65e5",ymd$d),sprintf("%s\u65e5",strrep(na.fill,1)))) %>%
				stringr::str_replace_all("%Y",dplyr::if_else(!is.na(ymd$y),sprintf("%d",ymd$y),sprintf("%s",strrep(na.fill,4)))) %>%
				stringr::str_replace_all("%y",dplyr::if_else(!is.na(ymd$y),sprintf("%02d",ymd$y%%100),sprintf("%s",strrep(na.fill,2)))) %>%
				stringr::str_replace_all("%M",dplyr::if_else(!is.na(ymd$m),sprintf("%02d",ymd$m),sprintf("%s",strrep(na.fill,2)))) %>%
				stringr::str_replace_all("%D",dplyr::if_else(!is.na(ymd$d),sprintf("%02d",ymd$d),sprintf("%s",strrep(na.fill,2)))) %>%
				stringr::str_replace_all("%m",dplyr::if_else(!is.na(ymd$m),sprintf("%d",ymd$m),sprintf("%s",strrep(na.fill,1)))) %>%
				stringr::str_replace_all("%d",dplyr::if_else(!is.na(ymd$d),sprintf("%d",ymd$d),sprintf("%s",strrep(na.fill,1)))) %>%
				stringr::str_replace_all("%!","%") %>%
				return()
		}
	}
}

#' formatting datastr table to string.
#' @description formatting datastr by using datestr_to_ymd and ymd_to_datestr
#' @param datestr target character or sequence of character.
#' @param dateformat output format with Y, M, D, y, m, d
#' @param range_sep separation of range if ymd is range format.
#' @param na.fill filled character for missing element.
#' @param mode determining data filling mode = c("full","maximum","compact","short","minimum"). see detail.
#' @param Date.beg potentially start date; used for validation and estimation.
#' @param Date.end potentially end date; used for validation and estimation.
#' @param strict return NA if read str is out of Date.beg-Date.end range
#' @param use.range Allow range date format
#' @param use.jpera Allow Japanese era case (e.g., R2.4.5 or 令和3年4月11日).
#' @param use.exceldate Allow to check the value which is originally excel date but occationally transformed to normal value.
#' @param exceldate.origin Origin of excel date. In default, it is 1900-01-01 if the file is originally created on Windows and 1904-01-01 on Mac.
#' @return transformed str
#' @details
#' format of each mode is like the following.
#' 	maximum: always range mode e.g., 2012.03.04-2012.03.04 2023.04.05-2023.05.06
#' 	full (default): range mode is used only when beg and end is different e.g., 2012.03.04 2023.04.05-2023.05.06
#' 	compact: range mode remove duplicating information e.g., 2023.04.05-05.06 2023.04.??
#' 	minimum: (information loss can occur depending on dateformat) remove NA and duplicating info e.g., 2023.04-05
#' dateformat can use the following special characters.
#'		(percent)Yj(at): NOT WORKING
#'		(percent)YJ(at): NOT WORKING
#'		(percent)Y(at): YYYY年
#'		(percent)y(at): YY年
#'		(percent)M(at): MM月
#'		(percent)m(at): M月
#'		(percent)D(at): DD日
#'		(percent)d(at): D日
#'		(percent)Y: YYYY
#'		(percent)y: YY
#'		(percent)M: MM
#'		(percent)m: M
#'		(percent)D: DD
#'		(percent)d: D
#' @importFrom magrittr %>%
#' @export
datestr_format = function(datestr, dateformat="%Y.%M.%D", range_sep = "-", na.fill="?",mode = "full",Date.beg = NULL, Date.end = NULL, strict = TRUE, use.range=TRUE, use.jpera=TRUE, use.exceldate=FALSE,exceldate.origin = as.Date("1904-01-01")){
	datestr %>%
		datestr_to_ymd(Date.beg,Date.end,strict,use.range,use.jpera,use.exceldate,exceldate.origin) %>%
		ymd_to_datestr(dateformat,range_sep,na.fill,mode)
}
