#' Explore linear trends
#'
#' Calculates linear trend and significance on a monthly basis for each lake
#'
#' @param record dataframe with estimated historical record of chl-a levels
#' @param lake string, Name of Lake
#' @import ggplot2
#' @import lubridate
#' @export

monthly_trend <- function(record,lake){
  record$Year <- year(record$ImageDate)
  record$Month <- as.factor(months(record$ImageDate))
  for(i in unique(record$Month)){
    print(paste(i,"Slope (Trend):"))
    monthlymod <- lm(FieldValue~Year, data=record[(record$Month==i),])
    print((monthlymod$coefficients[,2]))
    print(paste("P-value",summary(monthlymod)$coefficients[2,4]))
  }
  ggplot(record,aes(x=ImageDate,y=FieldValue))+
    geom_point(aes(x=ImageDate,y=FieldValue,col=Month))+
    geom_line(aes(x=ImageDate,y=FieldValue,col=Month))+
    theme_bw()+
    ylab(expression(paste("Chl-a (",mu,"g/L)")))+
    xlab("Date")+
    ggtitle(paste("Chl-Levels by Month: ",lake))+
    scale_color_discrete(name="Month")+
    theme(legend.position="bottom")

}


#' DOY for Max Chl
#'
#' Calculates DOY for maximum chl-a for each station
#'
#' @param record dataframe with estimated historical record of Chl-a levels
#' @param lake string, Name of Lake
#' @import lubridate
#' @importFrom plyr ddply
#' @export

doy_max_chl <- function(record,lake){
  record$Year <- as.factor(year(record$ImageDate))
  annualmaxrecord <- ddply(record,c('Year','StationID'),function(x) x[which(x$FieldValue==max(x$FieldValue)),])
  annualmax <- data.frame(DOYmax=yday(annualmaxrecord$ImageDate),
                          Year=year(annualmaxrecord$ImageDate),
                          FieldValue=annualmaxrecord$FieldValue,
                          StationID=as.factor(annualmaxrecord$StationID))

  print(summary(lm(DOYmax~Year,data=annualmax)))

  ggplot(annualmax,aes(x=Year,y=DOYmax))+
    geom_point(aes(x=Year,y=DOYmax,col=StationID))+
    geom_line(aes(x=Year,y=DOYmax,col=StationID))+
    geom_smooth(method = "lm", se = FALSE,col='black')+
    theme_bw()+
    ylab("Day of Year")+
    ggtitle(paste("Occurrence of Maximum Chl-Levels: ",lake))+
    scale_color_discrete(name="Station ID")+
    theme(legend.position="bottom")
}
