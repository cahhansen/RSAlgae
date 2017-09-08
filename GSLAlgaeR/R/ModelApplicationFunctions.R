#' Apply model to historical remotely sensed record
#'
#' Apply GLM to remotely sensed record
#'
#' @param histdata dataframe with reflectance values
#' @param model GLM (.Rdata)
#' @param season vector of months to include in the season
#' @param threshold numeric value at which to limit the allowed estimate)
#' @export


apply_mod_seasonal <- function(histdata,model,season,threshold){
  histdata$ImageDate <- as.Date(histdata$ImageDate)
  histdata$Month <- months(histdata$ImageDate)
  histdatasub <- histdata[(histdata$Month %in% season),]
  subpredicted <- predict(model,newdata=histdatasub,type='response',se.fit=T)
  histdatasub$FieldValue <- subpredicted$fit
  histdatasub$lower <- histdatasub$FieldValue-1.96*subpredicted$se.fit
  histdatasub$upper <- histdatasub$FieldValue+1.96*subpredicted$se.fit
  histdatasub <- histdatasub[(histdatasub$FieldValue<threshold),]
  return(histdatasub)
}

#' Plot record
#'
#' Plots estimated record with error bars
#'
#' @param histdata Dataframe with estimated values (FieldValue), dates (ImageDate), lake (Lake), lower and upper bounds (lower and upper)
#' @import ggplot2
#' @import lubridate
#' @export

plot_est_record.errors <- function(histdata){
  ggplot(histdata)+geom_point(aes(x=ImageDate,y=FieldValue,col=as.factor(StationID)))+
  geom_errorbar(aes(x=ImageDate,ymin=lower,ymax=upper), width=0.2)+
  theme_bw()+scale_color_discrete(name="Station ID")+
  ggtitle("Modeled Record with Confidence Intervals")+
  ylab(expression(paste("Chl-a (",mu,"g/L)")))+xlab("Date")+
  theme(legend.position="bottom")+
  scale_x_date(limits = c(as.Date(paste0(min(year(histdata$ImageDate)),"-1-1")), as.Date(paste0(max(year(histdata$ImageDate)),"-12-31"))))
}

#' Plot record
#'
#' Plots estimated record with calibrated data
#'
#' @param histdata Dataframe with estimated values (FieldValue), dates (ImageDate), lake (Lake), lower and upper bounds (lower and upper)
#' @param caldata Dataframe with data used in Calibration (FieldValue, ImageDate, and Lake column)
#' @import ggplot2
#' @import lubridate
#' @export

plot_est_record.cal <- function(histdata,caldata){
  caldata$ImageDate <- as.Date(caldata$ImageDate)
  ggplot()+geom_point(data=histdata,aes(x=ImageDate,y=FieldValue,col=as.factor(StationID)))+
    geom_point(data=caldata,aes(x=ImageDate,y=FieldValue))+
    theme_bw()+scale_color_discrete(name="Station ID")+
    ggtitle("Modeled Values")+
    ylab(expression(paste("Chl-a (",mu,"g/L)")))+xlab("Date")+
    theme(legend.position="bottom")+
    scale_x_date(limits = c(as.Date(paste0(min(year(histdata$ImageDate)),"-1-1")), as.Date(paste0(max(year(histdata$ImageDate)),"-12-31"))))
}



#' Plot record
#'
#' Plots estimated and observed data
#'
#' @param histdata Dataframe with estimated values (FieldValue), dates (ImageDate), lake (Lake), lower and upper bounds (lower and upper)
#' @param caldata Dataframe with data used in Calibration (FieldValue, ImageDate, and Lake column)
#' @import ggplot2
#' @import lubridate
#' @export

plot_entire_record <- function(histdata,obsdata,lake){
  obsdata$Date <- as.Date(obsdata$Date)
  obsdata <- subset(obsdata, Value >= 0)
  histdata$Dataset <- as.character(histdata$Dataset)
  combinedf <- data.frame(Date=c(histdata$ImageDate,obsdata$Date),
                          Value=c(histdata$FieldValue,obsdata$Value),
                          Dataset=c(histdata$Dataset,rep("Observed",nrow(obsdata))))
  ggplot()+geom_point(data=combinedf,aes(x=Date,y=Value,col=as.factor(Dataset)))+
    theme_bw()+scale_color_discrete(name="Dataset")+
    ggtitle(paste("Historical Record",":",lake))+
    ylab(expression(paste("Chl-a (",mu,"g/L)")))+xlab("Date")+
    theme(legend.position="bottom")+
    scale_x_date(limits = c(as.Date(paste0(min(year(histdata$ImageDate)),"-1-1")), as.Date(paste0(max(year(histdata$ImageDate)),"-12-31"))))
}
