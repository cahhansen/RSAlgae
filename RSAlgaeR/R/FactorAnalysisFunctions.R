#' lagpad
#'
#' Lags a variable by a time step
#'
#' @param x climate data
#' @param k time step to lag (positive results in a forward shift, negative results in backwards shift)
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

#' weather.effect
#'
#' Evaluates immediate difference in values based on weather conditions
#'
#' @param wqrecord dataframe with estimated historical record of water quality parameter
#' @param imagedatecol string, name of column with the date of the estimate (date of remotely sensed imagery)
#' @param valuecol string, name of column with estimated or field-sampled water quality parameter
#' @param climaterecord dataframe with climate variables
#' @param climatevarcol character, name of climate variable (column) of interest
#' @param climatedatecol string, name of column with the date of the climate observation
#' @param maxlag numeric, number of days to lag the climate effect
#' @param noevent numeric, threshold for whether an event occurred
#' @param alternative character string specifying alternative hypothesis ("two.sided","greater","less")
#' @param overall boolean, TRUE: all locations, FALSE: by each location. Default is TRUE
#' @param months months an optional character string for if the t-test should be month specific
#' @param locationcol string, name of column with unique location identifier, used if overall is FALSE
#' @param ylabel string, optional label for plot
#' @return results of wilcox test for differences in mean values (and, if overall, boxplots of water quality data)
#' @import ggplot2
#' @import lubridate
#' @examples
#' data(estimatedrecord)
#' data(climatedata)
#' effectresults <- weather.effect(wqrecord=estimatedrecord,imagedatecol="ImageDate",
#' valuecol="EstChlValue",climaterecord=climatedata,climatevarcol="TMAX",climatedatecol="DATE",
#' maxlag=7,noevent=16,months=c("July"))
#' @export
#'

weather.effect <- function(wqrecord,imagedatecol,valuecol,climaterecord,
                                  climatevarcol,climatedatecol,maxlag,noevent,alternative="two.sided",overall=TRUE,months=NULL,locationcol="",ylabel="Average Value"){
  #Format data frames
  wqrecord$ImageDate <- wqrecord[,imagedatecol]
  climaterecord$Month <- months(climaterecord[,climatedatecol])
  lag <- seq(0,maxlag,1)
  if(overall==TRUE){
    if(!is.null(months)){
      #Create results dataframe
      results <- data.frame(Month=rep(months,8),Threshold=NA,Lag=rep(0:maxlag,times=1,each=length(months)),Event=NA,NoEvent=NA,PValue=NA)
      for(j in months){
        if(noevent!=0){
          climaterecordsub <- climaterecord[(climaterecord$Month==j),]
          noevent <- mean(climaterecordsub[,climatevarcol],na.rm=TRUE)
        }
        for(i in lag){
          climaterecord$templag <- lagpad(x=climaterecord[,climatevarcol],k=i)
          climateeventdates <- climaterecord[(climaterecord$templag>noevent),'Date']
          record.events <- subset(wqrecord,ImageDate %in% climateeventdates)
          record.noevents <- subset(wqrecord,!(ImageDate %in% climateeventdates))
          record.events$Month <- months(record.events$ImageDate)
          record.noevents$Month <- months(record.noevents$ImageDate)
          record.events.sub <- subset(record.events, Month %in% j)
          record.noevents.sub <- subset(record.noevents, Month %in% j)
          #Wilcox Test (Does not assume normal distributions)
          wilcoxresults <- tryCatch(wilcox.test(record.events.sub[,valuecol],record.noevents.sub[,valuecol],alternative), error=function(e) NULL)
          results[results$Lag==i & results$Month==j,"Threshold"] <- noevent
          results[results$Lag==i & results$Month==j,"Event"] <- mean(record.events.sub[,valuecol],na.rm=TRUE)
          results[results$Lag==i & results$Month==j,"NoEvent"] <- mean(record.noevents.sub[,valuecol],na.rm=TRUE)
          if(!is.null(wilcoxresults)){
            results[results$Lag==i & results$Month==j,"PValue"] <- wilcoxresults$p.value
          }


        }
      }
    }else{
      #Create results dataframe for analysis (not by month)
      results <- data.frame(Lag=seq(0,maxlag,1),Event=NA,NoEvent=NA,PValue=NA)
      for(i in lag){
        climaterecord$templag <- lagpad(x=climaterecord[,climatevarcol],k=i)
        climateeventdates <- climaterecord[(climaterecord$templag>noevent),'Date']
        record.events <- subset(wqrecord,ImageDate %in% climateeventdates)
        record.noevents <- subset(wqrecord,!(ImageDate %in% climateeventdates))
        results[i+1,"Event"] <- mean(record.events[,valuecol],na.rm=TRUE)
        results[i+1,"NoEvent"] <- mean(record.noevents[,valuecol],na.rm=TRUE)
        results[i+1,"PValue"] <- wilcox.test(record.events[,valuecol],record.noevents[,valuecol],alternative)$p.value
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
    #Get info for analysis by location
    locationlist <- unique(wqrecord[,locationcol])
    #Create Results dataframe
    results <- data.frame(Location=rep(locationlist,8),Lag=rep(0:7,times=1,each=length(locationlist)),Event=NA,NoEvent=NA,PValue=NA)
    for(j in locationlist){
      recordsub <- wqrecord[(wqrecord[,locationcol]==j),]
      for(i in lag){
        climaterecord$templag <- lagpad(x=climaterecord[,climatevarcol],k=i)
        climateeventdates <- climaterecord[(climaterecord$templag>noevent),'Date']
        record.events <- subset(recordsub,ImageDate %in% climateeventdates)
        record.noevents <- subset(recordsub,!(ImageDate %in% climateeventdates))
        results[(results$Lag==i & results$Location==j),"Event"] <- mean(record.events[,valuecol],na.rm=TRUE)
        results[(results$Lag==i & results$Location==j),"NoEvent"] <- mean(record.noevents[,valuecol],na.rm=TRUE)
        results[(results$Lag==i & results$Location==j),"PValue"] <- wilcox.test(record.events[,valuecol],record.noevents[,valuecol],alternative)$p.value
      }
    }
  }
  return(results)
}

#' climate.effect
#'
#' Evaluates effects on HAB measures based on seasonal climate conditions
#'
#' @param record dataframe with both record of water quality parameter and climate data (annual)
#' @param valuecol string, name of column with estimated or field-sampled water quality parameter
#' @param climatevarcol character, name of climate variable (column) of interest
#' @param alternative character string specifying alternative hypothesis ("two.sided","greater","less")
#' @return results of wilcox test for differences in mean values (and, if overall, boxplots of water quality data)
#' @import ggplot2
#' @import lubridate
#' @examples
#' data(utahlake_hab_climatedata)
#' effectresults <- climate.effect(record=utahlake_hab_climatedata,
#' valuecol="MaxAvgChl",climatevarcol="TotalWinterPrecip")
#' @export
#'

climate.effect <- function(record,valuecol,climatevarcol,alternative="two.sided"){
  record$Value <- record[,valuecol]
  record$Climate <- record[,climatevarcol]
  #Calculate threshold
  threshold <- mean(record$Climate,na.rm=T)
  #Create thresholds
  abovesub <- record[(record$Climate>=threshold),"Value"]
  abovesubmean <- mean(abovesub,na.rm=T)
  belowsub <- record[(record$Climate<threshold),"Value"]
  belowsubmean <- mean(belowsub,na.rm=T)
  meandiff <- wilcox.test(abovesub,belowsub)
  corresult <- cor.test(record$Value,record$Climate,method = 'kendall')

  return(list(MeanAboveAvg=abovesubmean,
              MeanBelowAvg=belowsubmean,
              WilcoxTest=meandiff,
              KendallTau=corresult))
}
