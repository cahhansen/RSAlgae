#' lagpad
#'
#' Lags a variable by a time step
#'
#' @param x climate data
#' @param k time step to lag (postive results in a forward shift, negative results in backwards shift)
#' @export


lagpad <- function(x, k) {
  i<-is.vector(x)

  if(is.vector(x)) x<-matrix(x) else x<-matrix(x,nrow(x))
  if(k>0) {
    x <- rbind(matrix(rep(NA, k*ncol(x)),ncol=ncol(x)), matrix(x[1:(nrow(x)-k),], ncol=ncol(x)))
    x[is.na(x)] <- 0
    x[x==-9999] <- NA
  }
  else {
    x <- rbind(matrix(x[(-k+1):(nrow(x)),], ncol=ncol(x)),matrix(rep(NA, -k*ncol(x)),ncol=ncol(x)))
    x[is.na(x)] <- 0
    x[x==-9999] <- NA
    }
  if(i) x[1:length(x)] else x

}

#' climate.factor.effect
#'
#' Evaluates difference in values based on climate conditions
#'
#' @param record dataframe with estimated historical record of water quality parameter
#' @param imagedate string, name of coloumn with the date of the estimate (date of remotely sensed imagery)
#' @param value string, name of column with estimated or field-sampled water quality parameter
#' @param climaterecord dataframe with climate variables
#' @param climatevar character, name of climate variable (column) of interest
#' @param maxlag numeric, number of days to lag the climate effect
#' @param noevent numeric, threshold for whether an event occured
#' @param alternative character string specifying alternative hypothesis ("two.sided","greater","less")
#' @param overall boolean, TRUE: all locations, FALSE: by each location. Default is TRUE
#' @param months months an optional character string for if the t-test should be month specific
#' @param location string, name of column with unique location identifier, used if overall is FALSE
#' @param ylabel string, optional label for plot
#' @return results of wilcox test for differences in mean values
#' @import ggplot2
#' @import lubridate
#' @export
#'

climate.factor.effect <- function(record,imagedate,value,climaterecord,
                                  climatevar,maxlag,noevent,alternative="two.sided",overall=TRUE,months=NULL,location="",ylabel="Average Value"){
  record$ImageDate <- record[,imagedate]
  ImageDate <- record$ImageDate

  lag <- seq(0,maxlag,1)
  if(overall==TRUE){
    if(!is.null(months)){
      climaterecord$Month <- months(climaterecord$Date)
      #Create results dataframe
      results <- data.frame(Month=rep(months,8),Threshold=NA,Lag=rep(0:maxlag,times=1,each=length(months)),Event=NA,NoEvent=NA,PValue=NA)
      for(j in months){
        if(noevent!=0){
          climaterecordsub <- climaterecord[(climaterecord$Month==j),]
          noevent <- mean(climaterecordsub[,climatevar],na.rm=TRUE)
        }
        for(i in lag){
          climaterecord$templag <- lagpad(x=climaterecord[,climatevar],k=i)
          climateeventdates <- climaterecord[(climaterecord$templag>noevent),'Date']
          record.events <- subset(record,ImageDate %in% climateeventdates)
          record.noevents <- subset(record,!(ImageDate %in% climateeventdates))
          record.events$Month <- months(record.events$ImageDate)
          record.noevents$Month <- months(record.noevents$ImageDate)
          record.events.sub <- subset(record.events, Month %in% j)
          record.noevents.sub <- subset(record.noevents, Month %in% j)
          #Wilcox Test (Does not assume normal distributions)
          wilcoxresults <- tryCatch(wilcox.test(record.events.sub[,value],record.noevents.sub[,value],alternative), error=function(e) NULL)
          results[results$Lag==i & results$Month==j,"Threshold"] <- noevent
          results[results$Lag==i & results$Month==j,"Event"] <- mean(record.events.sub[,value],na.rm=TRUE)
          results[results$Lag==i & results$Month==j,"NoEvent"] <- mean(record.noevents.sub[,value],na.rm=TRUE)
          if(!is.null(wilcoxresults)){
            results[results$Lag==i & results$Month==j,"PValue"] <- wilcoxresults$p.value
          }


        }
      }
    }else{
      #Create results dataframe
      results <- data.frame(Lag=seq(0,maxlag,1),Event=NA,NoEvent=NA,PValue=NA)
      for(i in lag){
        climaterecord$templag <- lagpad(x=climaterecord[,climatevar],k=i)
        climateeventdates <- climaterecord[(climaterecord$templag>noevent),'Date']
        record.events <- subset(record,ImageDate %in% climateeventdates)
        record.noevents <- subset(record,!(ImageDate %in% climateeventdates))
        results[i+1,"Event"] <- mean(record.events[,value],na.rm=TRUE)
        results[i+1,"NoEvent"] <- mean(record.noevents[,value],na.rm=TRUE)
        results[i+1,"PValue"] <- wilcox.test(record.events[,value],record.noevents[,value],alternative)$p.value
      }
      #Plot Results
      ggplot2::ggplot(data=results)+
        geom_line(aes(x=Lag,y=Event,color='a'))+
        geom_line(aes(x=Lag,y=NoEvent,color='b'))+
        theme_bw()+
        xlab("Lag (Days)")+
        ylab(ylabel)+
        scale_color_manual(name="",
                           values=c('a'='red','b'='blue'),
                           labels=c("Event","No Event"))
    }
  }else{
    #Get location info
    locationlist <- unique(record[,location])
    #Create Results dataframe
    results <- data.frame(Location=rep(locationlist,8),Lag=rep(0:7,times=1,each=length(locationlist)),Event=NA,NoEvent=NA,PValue=NA)
    for(j in locationlist){
      recordsub <- record[(record[,location]==j),]
      for(i in lag){
        climaterecord$templag <- lagpad(x=climaterecord[,climatevar],k=i)
        climateeventdates <- climaterecord[(climaterecord$templag>noevent),'Date']
        record.events <- subset(recordsub,ImageDate %in% climateeventdates)
        record.noevents <- subset(recordsub,!(ImageDate %in% climateeventdates))
        results[(results$Lag==i & results$Location==j),"Event"] <- mean(record.events[,value],na.rm=TRUE)
        results[(results$Lag==i & results$Location==j),"NoEvent"] <- mean(record.noevents[,value],na.rm=TRUE)
        results[(results$Lag==i & results$Location==j),"PValue"] <- wilcox.test(record.events[,value],record.noevents[,value],alternative)$p.value
      }
    }
  }
  return(results)
}
