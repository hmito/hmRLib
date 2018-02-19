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

#' CUD v3 color palet as the color universal design
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
