#' calculate trends in the occurrence of a maximum event
#'
#'
#' @param data  with estimated values, dates, location identifiers
#' @param value string, name of column with water quality values
#' @param date string, name of column with dates
#' @param location string, name of column with location identifiers
#' @import mblm
#' @import ggplot2
#' @import lubridate
#' @export



doy.max.trend <- function(data,date,value,location){
  data$date <- data[,date]
  data$value <- data[,value]
  data$location <- data[,location]
  data$Year <- (year(data$date))

  data$Year <- as.factor(data$Year)
  annualmaxdata <- ddply(data,c('Year'),function(x) x[which(x$value==max(x$value)),])
  annualmax <- data.frame(DOYmax=yday(annualmaxdata$date),
                          Year=year(annualmaxdata$date),
                          Value=annualmaxdata$value,
                          LocationID=as.factor(annualmaxdata$location))
  annualmax$Yearnorm <- annualmax$Year-min(annualmax$Year)

  fit <- with(annualmax,mblm(DOYmax~Yearnorm))

  doyplot <- ggplot(annualmax,aes(x=Yearnorm,y=DOYmax))+
    geom_point(aes(x=Yearnorm,y=DOYmax))+
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], col='red')+
    theme_bw()+
    ylab("Day of Year")+
    xlab("Year in record")+
    ggtitle(paste("Occurrence of Maximum Chl-Levels ",min(annualmax$Year),"-",max(annualmax$Year)))+
    theme(legend.position="none")
  return(list(summary(fit),doyplot))
}

