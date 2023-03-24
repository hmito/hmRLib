#' Replace hankaku-kana by zenkaku-kana
#' @description Replace hankaku-kana by zenkaku-kana.
#' @param x target character
#' @return replaced character
#' @export
str_kana_to_zen <- function(x){
	# character変換
	if (!is.character(x)){ x <- as.character(x) }

	# 濁点、半濁点文字の置換
	dh = c("\uff76\uff9e","\uff77\uff9e","\uff78\uff9e","\uff79\uff9e","\uff7a\uff9e","\uff7b\uff9e","\uff7c\uff9e","\uff7d\uff9e","\uff7e\uff9e","\uff7f\uff9e","\uff80\uff9e","\uff81\uff9e","\uff82\uff9e","\uff83\uff9e","\uff84\uff9e","\uff8a\uff9e","\uff8b\uff9e","\uff8c\uff9e","\uff8d\uff9e","\uff8e\uff9e","\uff8a\uff9f","\uff8b\uff9f","\uff8c\uff9f","\uff8d\uff9f","\uff8e\uff9f")
	dz = c("\u30ac","\u30ae","\u30b0","\u30b2","\u30b4","\u30b6","\u30b8","\u30ba","\u30bc","\u30be","\u30c0","\u30c2","\u30c5","\u30c7","\u30c9","\u30d0","\u30d3","\u30d6","\u30d9","\u30dc","\u30d1","\u30d4","\u30d7","\u30da","\u30dd")
#	dh <- c("ｶﾞ","ｷﾞ","ｸﾞ","ｹﾞ","ｺﾞ","ｻﾞ","ｼﾞ","ｽﾞ","ｾﾞ","ｿﾞ","ﾀﾞ","ﾁﾞ","ﾂﾞ","ﾃﾞ","ﾄﾞ","ﾊﾞ","ﾋﾞ","ﾌﾞ","ﾍﾞ","ﾎﾞ","ﾊﾟ","ﾋﾟ","ﾌﾟ","ﾍﾟ","ﾎﾟ")
#	dz <- c("ガ","ギ","グ","ゲ","ゴ","ザ","ジ","ズ","ゼ","ゾ","ダ","ヂ","ヅ","デ","ド","バ","ビ","ブ","ベ","ボ","パ","ピ","プ","ペ","ポ")
	for( i in 1:length(dz) ){ x <- gsub(dh[i],dz[i],x) }

	# 1bite文字の置換
	x <- chartr("\uff71\uff72\uff73\uff74\uff75\uff76\uff77\uff78\uff79\uff7a\uff7b\uff7c\uff7d\uff7e\uff7f\uff80\uff81\uff82\uff83\uff84\uff85\uff86\uff87\uff88\uff89\uff8a\uff8b\uff8c\uff8d\uff8e\uff8f\uff90\uff91\uff92\uff93\uff94\uff95\uff96\uff97\uff98\uff99\uff9a\uff9b\uff9c\uff66\uff9d\uff61\uff62\uff63\uff64\uff65\uff66\uff67\uff68\uff69\uff6a\uff6b\uff6c\uff6d\uff6e\uff6f\uff70",
					"\u30a2\u30a4\u30a6\u30a8\u30aa\u30ab\u30ad\u30af\u30b1\u30b3\u30b5\u30b7\u30b9\u30bb\u30bd\u30bf\u30c1\u30c4\u30c6\u30c8\u30ca\u30cb\u30cc\u30cd\u30ce\u30cf\u30d2\u30d5\u30d8\u30db\u30de\u30df\u30e0\u30e1\u30e2\u30e4\u30e6\u30e8\u30e9\u30ea\u30eb\u30ec\u30ed\u30ef\u30f2\u30f3\u3002\u300c\u300d\u3001\u30fb\u30f2\u30a1\u30a3\u30a5\u30a7\u30a9\u30e3\u30e5\u30e7\u30c3\u30fc",
					x)
#	x <- chartr("ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ"
#					, "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー"
#					, x)
	return(x)
}

#' Replace zenkaku-alphabet by hankaku-alphabet
#' @description Replace zenkaku-alphabet by hankaku-alphabet
#' @param x target character
#' @return replaced character
#' @export
str_alphabet_to_han = function(x){
	# character変換
	if (!is.character(x)){ x <- as.character(x) }

	# 1bite文字の置換
	# ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＺ
	x <- chartr("\uff41\uff42\uff43\uff44\uff45\uff46\uff47\uff48\uff49\uff4a\uff4b\uff4c\uff4d\uff4e\uff4f\uff50\uff51\uff52\uff53\uff54\uff55\uff56\uff57\uff58\uff59\uff5a\uff21\uff22\uff23\uff24\uff25\uff26\uff27\uff28\uff29\uff2a\uff2b\uff2c\uff2d\uff2e\uff2f\uff30\uff31\uff32\uff33\uff34\uff35\uff36\uff37\uff38\uff3a",
					"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
					x)

	return(x)
}

#' Replace zenkaku-number by hankaku-number
#' @description Replace zenkaku-number by hankaku-number
#' @param x target character
#' @return replaced character
#' @export
str_number_to_han = function(x){
	# character変換
	if (!is.character(x)){ x <- as.character(x) }

	# 1bite文字の置換
	# ０１２３４５６７８９
	x <- chartr("\uff10\uff11\uff12\uff13\uff14\uff15\uff16\uff17\uff18\uff19",
					"0123456789",
					x)

	return(x)
}

#' Replace zenkaku-symbol by hankaku-symbol
#' @description Replace zenkaku-symbol by hankaku-symbol
#' @param x target character
#' @return replaced character
#' @export
str_symbol_to_han = function(x){
	# character変換
	if (!is.character(x)){ x <- as.character(x) }

	# 1bite文字の置換
	# （）｛｝［］＜＞“”！？，．／＠＃＄％＾＆＊ー―～＝＿＋｜￥
	x <- chartr("\uff08\uff09\uff5b\uff5d\uff3b\uff3d\uff1c\uff1e\u201c\u201d\uff01\uff1f\uff0c\uff0e\uff0f\uff20\uff03\uff04\uff05\uff3e\uff06\uff0a",
					'(){}[]<>""!?,./@#$%^&*',
					x)
	# ‘’
	x <- chartr("\u2015\uff5e\uff1d\uff3f\uff0b\uff5c\uffe5\u2018",
					"-~=_+|\\'",
					x)
	x <- chartr("\u2019","'",x)
	return(x)
}

#' Replace zenkaku alphabet, number and symbols by hankaku
#' @description Replace zenkaku alphabet, number and symbols by hankaku
#' @param x target character
#' @return replaced character
#' @export
str_to_han = function(x){
	x %>%
		str_alphabet_to_han() %>%
		str_number_to_han() %>%
		str_symbol_to_han() %>%
		stringr::str_replace_all("\u3000"," ") %>%
		return()
}

#' Check similarity of given string with target.
#' @description Check similarity of given string with target.
#' @param string character for checking similarity
#' @param target character for compared target
#' @param similarity threshold similarity
#' @return logical: TRUE if string is enough similar with target.
#' @importFrom utils adist
#' @export
str_similar = function(string,target,similarity=3){
	apply(adist(string,target)<=similarity,1,any)
}
#' Wherher the given string appear first time in the given string vector
#' @description str can be uniqued by using string[str_first_appear(string)].
#' @param string target character vector.
#' @param similarity threshold similarity
#' @return logical: TRUE if the character element appear first time.
#' @importFrom utils adist
#' @export
str_first_appear = function(string,similarity=3){
	if(length(string)==0)return(logical(0))
	if(length(string)==1)return(TRUE)
	return(
		apply(apply((adist(string,string)<similarity),2,cumsum)>0,2,function(x){min((1:length(x))[x])})==1:length(string)
	)
}
#' Generate random character sequences
#' @description return n strings with length len.
#' @param n number of str
#' @param len length of each str
#' @param lower_case logical: include lower case alphabets
#' @param upper_case logical: include upper case alphabets
#' @param number logical: include numbers
#' @return sequences of string.
#' @export
rand_char = function(n,len,lower_case=TRUE,upper_case=TRUE,number=TRUE){
	cand = NULL
	if(number){
		cand = c(cand,"0","1","2","3","4","5","6","7","8","9")
	}
	if(lower_case){
		cand = c(cand,"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
	}
	if(upper_case){
		cand = c(cand,"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
	}
	if(n<=0)return(NULL)
	ans = NULL
	for(i in 1:n){
		ans = c(ans,paste(sample(cand,len),collapse=""))
	}
	return(ans)
}
