#' Create red-blue color set.
#' @description Create red-blue based color set.
#' @param n Size of color set.
#' @param low The hue at the sides of the color set.
#' @param high The hue at the center of the color set.
#' @return Created color set.
#' @importFrom grDevices rgb
#' @export
rb.colors=function(n,low=0.25,high=0.90){
	if(n%%2==1){
		n = n-1
		return(c(rgb(1,seq(1-high,1-low,length=n/2),seq(1-high,1-low,length=n/2),1),"white",rgb(seq(1-low,1-high,length=n/2),seq(1-low,1-high,length=n/2),1,1)))
	}else{
		return(c(rgb(1,seq(1-high,1-low,length=n/2),seq(1-high,1-low,length=n/2),1),rgb(seq(1-low,1-high,length=n/2),seq(1-low,1-high,length=n/2),1,1)))
	}
}

#' Create cold color set.
#' @description Create cold-color based color set.
#' @param n Size of color set.
#' @param val.white Strength of white color.
#' @param rate.white Changing rate of the white color
#' @return Created color set.
#' @importFrom grDevices hsv
#' @export
cold.colors=function (n, val.white=0.2,rate.white=0.4) {
	hsv(seq(3/6,4/6,length=n),c(seq(val.white,1.0,length=round(n*rate.white)),rep(1.0,length=n-round(n*rate.white))),1)
}

#' Create green color set.
#' @description Create green-color based color set.
#' @param n Size of color set.
#' @param val.white Strength of white color.
#' @param rate.white Changing rate for bright side.
#' @param rate.dark Changing rate for dark side.
#' @return Created color set.
#' @export
green.colors=function (n, val.white=0.2,rate.white=0.5,rate.dark=0.5) {
	hsv(seq(1/6,2/6,length=n),c(seq(val.white,1.0,length=round(n*rate.white)),rep(1.0,length=n-round(n*rate.white))),seq(1.0,rate.dark,length=n))
}

#' CUD v3 color set as the color universal design
#' @description CUD v3 is the color palet for the color universal design, i.e., considring the accessibility for color blinders.
#' @return List of colors
#' @export
cud.colorset=function(){
	list(
		red="#ff2800",
		yellow="#faf500",
		green="#35a16b",
		blue="#0041ff",
		skyblue="#66ccff",
		pink="#ff99a0",
		orange="#ff9900",
		purple="#9a0079",
		brown="#663300",
		light_red="#ffd1d1",
		light_yellow="#ffff99",
		light_yellowgreen="#cbf266",
		light_skyblue="#b4ebfa",
		light_brown="#edc58f",
		light_green="#87e7b0",
		light_purple="#c7b2de",
		white="#ffffff",
		lightgrey="#c8c8cb",
		grey="#7f878f",
		black="#000000"
	)
}

#' CUD v3 color palette as the color universal design
#' @description CUD v3 is the color palet for the color universal design, i.e., considring the accessibility for color blinders.
#' @param n Size of color set.
#' @return Created color set.
#' @export
cud.pallete = function(n=10){
	set = cud.colorset()
	col = c(set$blue, set$red, set$yellow, set$skyblue, set$green,set$orange, set$brown, set$black, set$grey,set$lightgrey)
	return(col[1:n])
}

#' matlab color palette as the color universal design
#' @description Return color palette used in matlab.
#' @param n Size of color set.
#' @return Created color set.
#' @export
matlab.pallete = function(n=10){
	col = c(rgb(0,0.4470,0.7410),rgb(0.8500,0.3250,0.0980), rgb(0.9290,0.6940,0.1250), rgb(0.4940,0.1840,0.5560), rgb(0.4660,0.6740,0.1880), rgb(0.3010,0.7450,0.9330), rgb(0.6350,0.0780,0.1840), "black","grey","darkgrey")
	return(col[1:n])
}

coloropt.pallete = function(n=7){
	if(n>7)warning("max coloropt length is 7.")
	col = c(rgb(63/255,83/255,211/255),rgb(221/255,179/255,16/255),rgb(181/255,29/255,20/255),
	rgb(0/255,190/255,255/255),rgb(251/255,73/255,176/255),rgb(0/255,178/255,93/255),rgb(202/255,202/255,202/255))
	return(col[1:n])
}

