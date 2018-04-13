#' plotrecord.errors
#'
#' Plots estimated record with error bars
#'
#' @param data Dataframe with estimated values,dates, location identifiers, lower and upper bounds (lower and upper)
#' @param value string, name of column with water quality values
#' @param date string, name of column with imagery dates
#' @param location string, name of column with location identifiers
#' @param ylab string, label for y axis
#' @return plots the estimated record with error bars
#' @import ggplot2
#' @export

plotrecord.errors <- function(data, value, date, location, ylab=expression(paste("Chl-a (",mu,"g/L)"))){

  data$date <- as.Date(data[[,date]])
  data$value <- data[[,value]]
  data$location <- data[[,location]]
  ggplot2::ggplot(data)+geom_point(aes(x=date,y=value,col=as.factor(location)))+
    geom_errorbar(aes(x=date,ymin=lower,ymax=upper), width=0.2)+
    theme_bw()+scale_color_discrete(name="Location")+
    ggtitle("Modeled Record with Confidence Intervals")+
    xlab("Date")+
    ylim(0,1000)+
    theme(legend.position="bottom")+
    scale_x_date(limits = c(as.Date(paste0(min(year(data$date)),"-1-1")), as.Date(paste0(max(year(data$date)),"-12-31"))))
}

#' plotrecord.cal
#'
#' Plots estimated record with calibrated data
#'
#' @param data Dataframe with estimated values (value), dates (ImageDate), location identifier
#' @param caldata Dataframe with data used in Calibration (value, ImageDate, and Lake column)
#' @param value string, name of column with water quality values
#' @param date string, name of column with imagery dates
#' @param location string, name of column with location identifiers
#' @param ylab string, label for y axis
#' @return plot of estimated record with data used for calibration
#' @import ggplot2
#' @export

plotrecord.cal <- function(data,caldata,value,date,location,ylab=expression(paste("Chl-a (",mu,"g/L)"))){
  data$date <- as.Date(data[[,date]])
  data$value <- data[[,value]]
  data$location <- as.factor(data[[,location]])
  caldata$date <- as.Date(caldata[[,date]])
  caldata$value <- caldata[[,value]]
  ggplot2::ggplot()+geom_point(data=data,aes(x=date,y=value,col=location))+
    geom_point(data=caldata,aes(x=date,y=value))+
    theme_bw()+scale_color_discrete(name="Location")+
    ggtitle("Modeled Values")+
    xlab("Date")+
    theme(legend.position="bottom")+
    scale_x_date(limits = c(as.Date(paste0(min(year(data$ImageDate)),"-1-1")), as.Date(paste0(max(year(data$ImageDate)),"-12-31"))))
}



#' plotrecord
#'
#' Plots estimated and observed data
#'
#' @param data Dataframe with estimated values
#' @param obsdata Dataframe with Observed Data
#' @param datavalue string, name of column with values in estimated dataframe
#' @param obsdatavalue string, name of column with values in observed dataframe
#' @param date string, name of column with date of imagery used for estimating values (must be date class)
#' @param obsdate string, name of column with date of observation (must be date class)
#' @param datacolors vector of length=2, specifying colors for estimated and observed datasets
#' @param datashapes vector of length=2, specifying shapes for estimated and observed datasets
#' #' @param lake string, Name of Lake
#' @param labels optional for plotting
#' @param ylab string, label for y axis
#' @return plot of estimated and observed data
#' @import ggplot2
#' @export

plotrecord <- function(data,datavalue,date,obsdata,obsdatavalue,obsdate,datacolors=c("grey","red"),datashapes=c(1,2),lake="",ylab=expression(paste("Chl-a (",mu,"g/L)"))){
  data$value <- data[,datavalue]
  data$Date <- data[,date]
  obsdata$value <- obsdata[,obsdatavalue]
  obsdata$Date <- obsdata[,obsdate]
  obsdata <- subset(obsdata, value >= 0)
  combinedf <- data.frame(Date=c(data$ImageDate,obsdata$Date),
                          Value=c(data$value,obsdata$value),
                          Dataset=c(rep("Estimated",nrow(data)),rep("Observed",nrow(obsdata))))

  names(datacolors) <- c("Estimated","Observed")
  names(datashapes) <- c("Estimated","Observed")
  combinedf$Dataset <- as.factor(combinedf$Dataset)
  p <- ggplot2::ggplot(data=combinedf,aes(x=Date,y=Value))+
    geom_point(aes(color=Dataset,shape=Dataset))+
    theme_bw()+
    scale_x_date(limits = c(as.Date(paste0(min(lubridate::year(data$ImageDate)),"-1-1")), as.Date(paste0(max(lubridate::year(data$ImageDate)),"-12-31"))))+
    ggtitle(paste(lake,"Historical Record"))+
    xlab("Date")+
    theme(legend.position="bottom")+
    scale_color_manual(values=datacolors)+
    scale_shape_manual(values=datashapes,
                       name="Dataset")
  print(p)

}
