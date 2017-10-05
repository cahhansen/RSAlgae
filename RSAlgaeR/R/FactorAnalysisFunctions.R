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


#' climate.factor.compare
#'
#' Performs a t-test on water quality dataset for various climate variables
#'
#' @param record dataframe with estimated historical record of water quality
#' @param value string, name of column with estimated or field-sampled water quality parameter
#' @param climaterecord dataframe with climate variables
#' @param climatevar character, name of climate variable (column) of interest
#' @param lag integer, number of days to lag the climate variable
#' @param noevent numeric, threshold for whether an event occured
#' @param alternative character string specifying alternative hypothesis ("two.sided","greater","less")
#' @param months an optional character string for if the t-test should be month specific
#' @import stats
#' @import lubridate
#' @export


climate.factor.compare <- function(record,value,climaterecord,climatevar,lag=NULL,noevent,alternative="two.sided",months=NULL){
  record$value <- record[,value]
  #If the variable is lagged:
  if(!is.null(lag)){
    #Lag climatevar
    laggedvar <- lagpad(climaterecord[,climatevar],lag)
    climaterecord[,paste(climatevar,'_lag')] <- laggedvar
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[laggedvar>noevent,'Date']
    record.events <- subset(record,ImageDate %in% climateeventdates)
    record.noevents <- subset(record,!(ImageDate %in% climateeventdates))
  }else{
    #If the variable is not lagged:
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[(climaterecord[,climatevar]>noevent),'Date']
    record.events <- subset(record,ImageDate %in% climateeventdates)
    record.noevents <- subset(record,!(ImageDate %in% climateeventdates))
  }

  #If the analysis should be done for a specific month:
  if(!is.null(months)){
    print(months)
    record.events$Month <- months(record.events$ImageDate)
    record.noevents$Month <- months(record.noevents$ImageDate)
    record.events.sub <- subset(record.events, Month %in% c(months))
    record.noevents.sub <- subset(record.noevents, Month %in% c(months))
    print(paste("Number Events:",nrow(record.events.sub),",Number non-Events:",nrow(record.noevents.sub)))
    #Wilcox Test (Does not assume normal distributions)
    results <- tryCatch(wilcox.test(record.events.sub$value,record.noevents.sub$value,alternative), error=function(e) NULL)

    print(paste("Mean Events:",mean(record.events.sub$value)))
    print(paste("Mean non-Events:", mean(record.noevents.sub$value)))

    if(nrow(record.noevents.sub)>0 & nrow(record.events.sub)>0){
      record.events.sub$Event <- 'Event'
      record.noevents.sub$Event <- 'No Event'
      recordcompare <- rbind(record.events.sub,record.noevents.sub)
      #Box and whisker plot of the two comparison groups
      p <- ggplot(recordcompare)+
        geom_boxplot(aes(x=as.factor(Event),y=value))+
        ggtitle(paste("Comparison of Chl Values for Climate Events: ",months))+
        ylab(expression(paste("Chl-a ",mu,"g/L")))+
        xlab("")+
        theme_bw()+
        annotate("text",x="Event",y=-8,label=paste("Mean:",round(mean(record.events.sub$value),2)))+
        annotate("text",x="No Event",y=-8,label=paste("Mean:",round(mean(record.noevents.sub$value),2)))
    }else{
      recordcompare <- rbind(record.events.sub,record.noevents.sub)
      #Box and whisker plot of all of the data together
      p <- ggplot(recordcompare)+
        geom_boxplot(aes(x=Lake,y=value))+
        ggtitle(paste("Distribution of Chl Values for: ",months))+
        ylab(expression(paste("Chl-a ",mu,"g/L")))+
        xlab("")+
        theme_bw()+
        annotate("text",x=unique(recordcompare$Lake),y=-8,label=paste("Mean:",round(mean(recordcompare$value),2)))
    }

  }else{
    #Wilcox Test (Does not assume normal distributions)
    print(paste("Number Events:",nrow(record.events),",Number non-Events:",nrow(record.noevents)))
    results <- wilcox.test(record.events$value,record.noevents$value,alternative)

    #Box and whisker plot of the two comparison groups
    record.events$Event <- 'Event'
    record.noevents$Event <- 'No Event'
    recordcompare <- rbind(record.events,record.noevents)
    p <- ggplot(recordcompare)+
      geom_boxplot(aes(x=as.factor(Event),y=value))+
      ggtitle(paste("Comparison of Values for Climate Events: Overall"))+
      ylab(expression(paste("Chl-a ",mu,"g/L")))+
      xlab("")+
      theme_bw()+
      annotate("text",x="Event",y=-8,label=paste("Mean:",round(mean(record.events$value),2)))+
      annotate("text",x="No Event",y=-8,label=paste("Mean:",round(mean(record.noevents$value),2)))
    }
  testtype <- function(alternative){
    switch(alternative,
           "two.sided" = "Difference in means is not equal to 0",
           "greater" = "Difference in means is greater than 0. Mean for event is greater than mean for non-event.",
           "less" = "Difference in means is less than 0. Mean for event is less than mean for non-event.")
  }
  if(!is.null(results)){
    if(results$p.value>=0.05){
      writeLines(paste("Not a statistically significant difference.\nP-value:",results$p.value))
    }else{
      writeLines(paste(testtype(alternative),"\nP-value:",results$p.value))
    }
  }
  return(p)
}

#' climate.factor.location
#'
#' Performs a t-test on water quality dataset for various climate variables (by location)
#'
#' @param record dataframe with estimated historical record of water quality levels
#' @param value string, name of column with estimated or field-sampled water quality parameter
#' @param climaterecord dataframe with climate variables
#' @param climatevar character, name of climate variable (column) of interest
#' @param lag integer, number of days to lag the climate variable
#' @param noevent numeric, threshold for whether an event occured
#' @param alternative character string specifying alternative hypothesis ("two.sided","greater","less")
#' @param location vector, list of unique location identifiers to iterate over
#' @import stats
#' @import lubridate
#' @export
#'


climate.factor.location <- function(record,value, climaterecord,climatevar,lag=NULL,noevent,alternative="two.sided",location){
  record$value <- record[,value]
  #If the variable is lagged:
  if(!is.null(lag)){
    #Lag climatevar
    laggedvar <- lagpad(climaterecord[,climatevar],lag)
    climaterecord[,paste(climatevar,'_lag')] <- laggedvar
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[laggedvar>noevent,'Date']
    record.events <- subset(record,ImageDate %in% climateeventdates)
    record.noevents <- subset(record,!(ImageDate %in% climateeventdates))
  }else{
    #If the variable is not lagged:
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[(climaterecord[,climatevar]>noevent),'Date']
    record.events <- subset(record,ImageDate %in% climateeventdates)
    record.noevents <- subset(record,!(ImageDate %in% climateeventdates))
  }
  stationresults <- data.frame(Station=rep(NA,length(location)),PValue=rep(NA,length(location)))
  if(nrow(record.noevents.sub)>0 & nrow(record.events.sub)>0){
    recordcompare <- rbind(record.events.sub,record.noevents.sub)

    for(i in seq(1,length(location))){
      temp.data.event <- record.events[(record.events$StationID==location[i]),]
      temp.data.noevent <- record.noevents[(record.noevents$StationID==location[i]),]
      temp.results <- (wilcox.test(temp.data.event$value,temp.data.noevent$value,alternative="greater"))
      stationresults$Station[i] <- as.character(location[i])
      stationresults$PValue[i] <- temp.results$p.value
      stationresults$Event[i] <- mean(temp.data.event$value)
      stationresults$NoEvent[i] <- mean(temp.data.noevent$value)
    }
  }


  return(stationresults)
}
