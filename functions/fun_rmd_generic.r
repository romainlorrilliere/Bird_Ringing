### vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster","sf","data.table","ggsn","gridExtra","OpenStreetMap","ggrepel","move")

vecPackage=c("kableExtra","knitr","tools")

ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "https://pbil.univ-lyon1.fr/CRAN/",dependencies=TRUE)
     library(p,character.only = TRUE)

}





my_kable_print <- function(d,caption="",bootstrap_options = "hover",position="center" , font_size=11,full_width = FALSE,fixed_thead = TRUE,scroll=TRUE,scroll_height = "300px", scroll_width = "100%") {

    k <- kableExtra::kable_styling(knitr::kable(d,caption = caption),bootstrap_options = ,bootstrap_options, full_width = full_width,fixed_thead = fixed_thead, position=position,font_size=font_size)
    if(scroll)
        k <- kableExtra::scroll_box(k,width = scroll_width, height = scroll_height)

    return(k)
}
