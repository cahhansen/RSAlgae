#' Explore linear trends by station
#'
#' Calculates linear trend and significance by station over the period of record
#'
#' @param record dataframe with estimated historical record of chl-a levels
#' @import lubridate
#' @export

station_trend <- function(record){
  record$Year <- year(record$ImageDate)
  record$Yearnorm <- record$Year-min(record$Year)
  stations <- unique(as.character(record$StationID))
  stationresults <- data.frame(Station=rep(NA,length(stations)),Trend=rep(NA,length(stations)),PValue=rep(NA,length(stations)))
  for(i in seq(1,length(stations))){
    temp.results <- summary(lm(FieldValue~Yearnorm, data=record[(record$StationID==stations[i]),]))
    stationresults$Station[i] <- as.character(stations[i])
    stationresults$Trend[i] <- temp.results$coefficients[2,1]
    stationresults$PValue[i] <- temp.results$coefficients[2,4]
  }
return(stationresults)

}





#' Explore linear trends
#'
#' Calculates linear trend and significance on a monthly basis for each lake
#'
#' @param record dataframe with estimated historical record of chl-a levels
#' @param lake string, Name of Lake
#' @import ggplot2
#' @import lubridate
#' @import RColorBrewer
#' @export
#'

monthly_trend <- function(record,lake){
  record$Year <- year(record$ImageDate)
  record$Month <- ordered(as.factor(months(record$ImageDate)))
  record$Month <- ordered(record$Month, levels = c("May","June","July","August","September"))
  record$Yearnorm <- record$Year-min(record$Year)
  for(i in unique(record$Month)){
    monthlymod <- lm(FieldValue~Yearnorm, data=record[(record$Month==i),])
    print(i)
    print(summary(monthlymod))
  }
  ggplot(record,aes(x=ImageDate,y=FieldValue))+
    geom_point(aes(x=ImageDate,y=FieldValue,col=Month))+
    geom_line(aes(x=ImageDate,y=FieldValue,col=Month))+
    theme_bw()+
    ylab(expression(paste("Chl-a (",mu,"g/L)")))+
    xlab("Date")+
    ggtitle(paste("Chl-Levels by Month: ",lake))+
    scale_color_brewer(palette="Blues",
                       name="Month",
                       breaks=c("May","June","July","August","September"),
                       labels=c("May","June","July","August","September"))+
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
#'

doy_max_chl <- function(record,lake){
  record$Year <- (year(record$ImageDate))
  record$Yearnorm <- record$Year-min(record$Year)
  record$Year <- as.factor(record$Year)
  annualmaxrecord <- ddply(record,c('Year','StationID'),function(x) x[which(x$FieldValue==max(x$FieldValue)),])
  annualmax <- data.frame(DOYmax=yday(annualmaxrecord$ImageDate),
                          Year=year(annualmaxrecord$ImageDate),
                          FieldValue=annualmaxrecord$FieldValue,
                          StationID=as.factor(annualmaxrecord$StationID),
                          Yearnorm=annualmaxrecord$Yearnorm)

  print(summary(lm(DOYmax~Yearnorm,data=annualmax)))

  stations <- unique(as.character(annualmax$StationID))
  stationresults <- data.frame(Station=rep(NA,length(stations)),Trend=rep(NA,length(stations)),PValue=rep(NA,length(stations)))
  for(i in seq(1,length(stations))){
    temp.results <- summary(lm(FieldValue~Yearnorm, data=annualmax[(annualmax$StationID==stations[i]),]))
    stationresults$Station[i] <- as.character(stations[i])
    stationresults$Trend[i] <- temp.results$coefficients[2,1]
    stationresults$PValue[i] <- temp.results$coefficients[2,4]
  }


  p <- ggplot(annualmax,aes(x=Year,y=DOYmax))+
    geom_point(aes(x=Year,y=DOYmax,col=StationID))+
    geom_line(aes(x=Year,y=DOYmax,col=StationID))+
    geom_smooth(method = "lm", se = FALSE,col='black')+
    theme_bw()+
    ylab("Day of Year")+
    ggtitle(paste("Occurrence of Maximum Chl-Levels: ",lake))+
    scale_color_discrete(name="Station ID")+
    theme(legend.position="bottom")
  p
  return(list(stationresults,p))
}

#' Extreme Trends
#'
#' Calculates trend and statistical significance for occurrence of extreme values
#'
#' @param record dataframe with estimated historical record of Chl-a levels
#' @param lake string, Name of Lake
#' @param extreme numerical value of the threshold above which are "extreme" levels
#' @import lubridate
#' @import RColorBrewer
#' @import ggplot2
#' @export
#'

extreme_trends <- function(record,lake,extreme=mean(record$FieldValue),plot="station"){
  record$Year <- (year(record$ImageDate))
  record$Yearnorm <- record$Year-min(record$Year)
  record$Year <- as.factor(record$Year)
  record.sub <- record[(record$FieldValue>extreme),]

  print(summary(lm(FieldValue~Yearnorm,data=record.sub)))
  if(plot=="station"){
    p=ggplot(record.sub,aes(x=ImageDate,y=FieldValue))+
      geom_point(aes(x=ImageDate,y=FieldValue,col=as.factor(StationID)))+
      theme_bw()+
      ylab(expression(paste("Chl-a (",mu,"g/L)")))+
      xlab("Date")+
      ggtitle(paste("Extreme Chl-Levels by Station: ",lake))+
      theme(legend.position="bottom")+
      scale_color_discrete(name="Station ID")
  }
  if(plot=="month"){
    record.sub$Month <- ordered(as.factor(months(record.sub$ImageDate)))
    record.sub$Month <- ordered(record.sub$Month, levels = c("May","June","July","August","September"))
    p=ggplot(record.sub,aes(x=ImageDate,y=FieldValue))+
      geom_point(aes(x=ImageDate,y=FieldValue,col=Month))+
      theme_bw()+
      ylab(expression(paste("Chl-a (",mu,"g/L)")))+
      xlab("Date")+
      ggtitle(paste("Extreme Chl-Levels by Month: ",lake))+
      scale_color_brewer(palette="Blues",
                         name="Month",
                         breaks=c("May","June","July","August","September"),
                         labels=c("May","June","July","August","September"))+
      theme(legend.position="bottom")
  }
  print(p)



}
