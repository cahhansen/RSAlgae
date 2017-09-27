#' Explore linear trends by station
#'
#' Calculates linear trend and significance by station over the period of record
#'
#' @param record dataframe with estimated historical record of chl-a levels
#' @import lubridate
#' @import RColorBrewer
#' @import ggplot2
#' @import stats
#' @export

station_trend <- function(record){
  record$Year <- year(record$ImageDate)
  record$Yearnorm <- record$Year-min(record$Year)
  stations <- unique(as.character(record$StationID))
  stationresults <- data.frame(Station=rep(NA,length(stations)),Trend=rep(NA,length(stations)),PValue=rep(NA,length(stations)),LowerCI=rep(NA,length(stations)),UpperCI=rep(NA,length(stations)))
  for(i in seq(1,length(stations))){
    temp.mod <- lm(FieldValue~Yearnorm, data=record[(record$StationID==stations[i]),])
    temp.results <- summary(temp.mod)
    stationresults$Station[i] <- as.character(stations[i])
    stationresults$Trend[i] <- temp.results$coefficients[2,1]
    stationresults$PValue[i] <- temp.results$coefficients[2,4]
    stationresults$LowerCI[i] <- confint(temp.mod)[2]
    stationresults$UpperCI[i] <- confint(temp.mod)[4]
  }

  p <- ggplot(record,aes(x=ImageDate,y=FieldValue))+
    geom_point(aes(x=ImageDate,y=FieldValue,col=as.factor(StationID)))+
    theme_bw()+
    ylab(expression(paste("Chl-a (",mu,"g/L)")))+
    scale_color_discrete(name="Station ID")
}


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
return(list(Plot=p,Results=stationresults))

}




#' DOY for Max Chl
#'
#' Calculates DOY for maximum chl-a for each station
#'
#' @param record dataframe with estimated historical record of Chl-a levels
#' @param lake string, Name of Lake
#' @param by.station, option to look at DOY for max chl by station
#' @import lubridate
#' @importFrom plyr ddply
#' @export


doy_max_chl <- function(record,lake,by.station=FALSE){
  record$Year <- (year(record$ImageDate))
  record$Yearnorm <- record$Year-min(record$Year)
  record$Year <- as.factor(record$Year)
  annualmaxrecord <- ddply(record,c('Year'),function(x) x[which(x$FieldValue==max(x$FieldValue)),])
  annualmax <- data.frame(DOYmax=yday(annualmaxrecord$ImageDate),
                          Year=year(annualmaxrecord$ImageDate),
                          FieldValue=annualmaxrecord$FieldValue,
                          StationID=as.factor(annualmaxrecord$StationID))

  p1 <- ggplot(annualmax,aes(x=Year,y=DOYmax))+
    geom_point(aes(x=Year,y=DOYmax))+
    geom_smooth(method = "lm", se = FALSE,col='red')+
    theme_bw()+
    ylab("Day of Year")+
    ggtitle(paste("Occurrence of Maximum Chl-Levels: ",lake))+
    theme(legend.position="none")

  annualmaxrecord.station <- ddply(record,c('Year','StationID'),function(x) x[which(x$FieldValue==max(x$FieldValue)),])
  annualmax.station <- data.frame(DOYmax=yday(annualmaxrecord.station$ImageDate),
                                  Year=year(annualmaxrecord.station$ImageDate),
                                  FieldValue=annualmaxrecord.station$FieldValue,
                                  StationID=as.factor(annualmaxrecord.station$StationID),
                                  Yearnorm=annualmaxrecord.station$Yearnorm)

  stations <- unique(as.character(annualmax.station$StationID))
  stationresults <- data.frame(Station=rep(NA,length(stations)),Trend=rep(NA,length(stations)),PValue=rep(NA,length(stations)))
  for(i in seq(1,length(stations))){
    temp.results <- summary(lm(FieldValue~Yearnorm, data=annualmax.station[(annualmax.station$StationID==stations[i]),]))
    stationresults$Station[i] <- as.character(stations[i])
    stationresults$Trend[i] <- temp.results$coefficients[2,1]
    stationresults$PValue[i] <- temp.results$coefficients[2,4]
  }


  p2 <- ggplot(annualmax.station,aes(x=Year,y=DOYmax))+

  print(summary(lm(DOYmax~Year,data=annualmax)))

  ggplot(annualmax,aes(x=Year,y=DOYmax))+

  print(summary(lm(DOYmax~Year,data=annualmax)))

  ggplot(annualmax,aes(x=Year,y=DOYmax))+

    geom_point(aes(x=Year,y=DOYmax,col=StationID))+
    theme_bw()+
    ylab("Day of Year")+
    ggtitle(paste("Occurrence of Maximum Chl-Levels by Station: ",lake))+
    scale_color_discrete(name="Station ID")+
    theme(legend.position="bottom")

  if(by.station==TRUE){
    return(list(stationresults,p2))
  }else{
    print(summary(lm(DOYmax~Yearnorm,data=annualmax)))
    return(p1)
  }

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

    stations <- unique(as.character(record.sub$StationID))
    stationresults <- data.frame(Station=rep(NA,length(stations)),Trend=rep(NA,length(stations)),PValue=rep(NA,length(stations)))
    for(i in seq(1,length(stations))){
      temp.results <- summary(lm(FieldValue~Yearnorm, data=record.sub[(record.sub$StationID==stations[i]),]))
      stationresults$Station[i] <- as.character(stations[i])
      stationresults$Trend[i] <- temp.results$coefficients[2,1]
      stationresults$PValue[i] <- temp.results$coefficients[2,4]
    }
    print(stationresults)
  }
  if(plot=="month"){
    record.sub$Month <- ordered(as.factor(months(record.sub$ImageDate)))
    record.sub$Month <- ordered(record.sub$Month, levels = c("May","June","July","August","September"))
    for(i in unique(record.sub$Month)){
      monthlymod <- lm(FieldValue~Yearnorm, data=record.sub[(record.sub$Month==i),])
      print(i)
      print(summary(monthlymod))
    }

    p=ggplot(record.sub,aes(x=ImageDate,y=FieldValue))+
      geom_point(aes(fill=Month),pch=21,colour="black")+
      theme_bw()+
      ylab(expression(paste("Chl-a (",mu,"g/L)")))+
      xlab("Date")+
      ggtitle(paste("Extreme Chl-Levels by Month: ",lake))+
      scale_fill_brewer(palette="Blues",
                         name="Month",
                         breaks=c("May","June","July","August","September"),
                         labels=c("May","June","July","August","September"))+
      theme(legend.position="bottom")
  }
  print(p)
}
