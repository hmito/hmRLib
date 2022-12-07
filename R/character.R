#' Replace hankaku-kana by zenkaku-kana
#' @description Replace hankaku-kana by zenkaku-kana.
#' @param x target character
#' @return replaced character
#' @export
hankana2zenkana <- function(x){
	# character変換
	if (!is.character(x)){ x <- as.character(x) }

	# 濁点、半濁点文字の置換
	dh <- c("ｶﾞ","ｷﾞ","ｸﾞ","ｹﾞ","ｺﾞ","ｻﾞ","ｼﾞ","ｽﾞ","ｾﾞ","ｿﾞ","ﾀﾞ","ﾁﾞ","ﾂﾞ","ﾃﾞ","ﾄﾞ","ﾊﾞ","ﾋﾞ","ﾌﾞ","ﾍﾞ","ﾎﾞ","ﾊﾟ","ﾋﾟ","ﾌﾟ","ﾍﾟ","ﾎﾟ")
	dz <- c("ガ","ギ","グ","ゲ","ゴ","ザ","ジ","ズ","ゼ","ゾ","ダ","ヂ","ヅ","デ","ド","バ","ビ","ブ","ベ","ボ","パ","ピ","プ","ペ","ポ")
	for( i in 1:length(dz) ){ x <- gsub(dh[i],dz[i],x) }

	# 1bite文字の置換
	x <- chartr("ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ"
					, "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー"
					, x)
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
	apply(adist(string,target)<similarity,1,any)
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
