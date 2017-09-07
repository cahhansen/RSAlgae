#' Lag variable
#'
#' Lags a variable by a time step
#'
#' @param x climate data
#' @param k time step to lag (postive results in a forward shift, negative results in backwards shift)
#' @export
#'


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


#' Climate Factor Impact
#'
#' Performs a t-test on chl-a dataset for various climate variables
#'
#' @param chlrecord dataframe with estimated historical record of chl-a levels
#' @param climaterecord dataframe with climate variables
#' @param climatevar character, name of climate variable (column) of interest
#' @param lag integer, number of days to lag the climate variable
#' @param noevent numeric, threshold for whether an event occured
#' @param alternative character string specifying alternative hypothesis ("two.sided","greater","less")
#' @param months an optional character string for if the t-test should be month specific
#' @import stats
#' @import lubridate
#' @export
#'


climate_factor_compare <- function(chlrecord,climaterecord,climatevar,lag=NULL,noevent,alternative="two.sided",months=NULL){
  #If the variable is lagged:
  if(!is.null(lag)){
    #Lag climatevar
    laggedvar <- lagpad(climaterecord[,climatevar],lag)
    climaterecord[,paste(climatevar,'_lag')] <- laggedvar
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[laggedvar>noevent,'Date']
    chlrecord.events <- subset(chlrecord,ImageDate %in% climateeventdates)
    chlrecord.noevents <- subset(chlrecord,!(ImageDate %in% climateeventdates))
  }else{
    #If the variable is not lagged:
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[(climaterecord[,climatevar]>noevent),'Date']
    chlrecord.events <- subset(chlrecord,ImageDate %in% climateeventdates)
    chlrecord.noevents <- subset(chlrecord,!(ImageDate %in% climateeventdates))
  }

  #If the analysis should be done for a specific month:
  if(!is.null(months)){
    print(months)
    chlrecord.events$Month <- months(chlrecord.events$ImageDate)
    chlrecord.noevents$Month <- months(chlrecord.noevents$ImageDate)
    chlrecord.events.sub <- subset(chlrecord.events, Month %in% c(months))
    chlrecord.noevents.sub <- subset(chlrecord.noevents, Month %in% c(months))
    print(paste("Number Events:",nrow(chlrecord.events.sub),",Number non-Events:",nrow(chlrecord.noevents.sub)))
    #Wilcox Test (Does not assume normal distributions)
    results <- tryCatch(wilcox.test(chlrecord.events.sub$FieldValue,chlrecord.noevents.sub$FieldValue,alternative), error=function(e) NULL)


    if(nrow(chlrecord.noevents.sub)>0 & nrow(chlrecord.events.sub)>0){
      chlrecord.events.sub$Event <- 'Event'
      chlrecord.noevents.sub$Event <- 'No Event'
      chlrecordcompare <- rbind(chlrecord.events.sub,chlrecord.noevents.sub)
      #Box and whisker plot of the two comparison groups
      p <- ggplot(chlrecordcompare)+
        geom_boxplot(aes(x=as.factor(Event),y=FieldValue))+
        ggtitle(paste("Comparison of Chl Values for Climate Events: ",months))+
        ylab(expression(paste("Chl-a ",mu,"g/L")))+
        xlab("")+
        theme_bw()+
        annotate("text",x="Event",y=-8,label=paste("Mean:",round(mean(chlrecord.events.sub$FieldValue),2)))+
        annotate("text",x="No Event",y=-8,label=paste("Mean:",round(mean(chlrecord.noevents.sub$FieldValue),2)))
    }else{
      chlrecordcompare <- rbind(chlrecord.events.sub,chlrecord.noevents.sub)
      #Box and whisker plot of all of the data together
      p <- ggplot(chlrecordcompare)+
        geom_boxplot(aes(x=Lake,y=FieldValue))+
        ggtitle(paste("Distribution of Chl Values for: ",months))+
        ylab(expression(paste("Chl-a ",mu,"g/L")))+
        xlab("")+
        theme_bw()+
        annotate("text",x=unique(chlrecordcompare$Lake),y=-8,label=paste("Mean:",round(mean(chlrecordcompare$FieldValue),2)))
    }

  }else{
    #Wilcox Test (Does not assume normal distributions)
    print(paste("Number Events:",nrow(chlrecord.events),",Number non-Events:",nrow(chlrecord.noevents)))
    results <- wilcox.test(chlrecord.events$FieldValue,chlrecord.noevents$FieldValue,alternative)

    #Box and whisker plot of the two comparison groups
    chlrecord.events$Event <- 'Event'
    chlrecord.noevents$Event <- 'No Event'
    chlrecordcompare <- rbind(chlrecord.events,chlrecord.noevents)
    p <- ggplot(chlrecordcompare)+
      geom_boxplot(aes(x=as.factor(Event),y=FieldValue))+
      ggtitle(paste("Comparison of Values for Climate Events: Overall"))+
      ylab(expression(paste("Chl-a ",mu,"g/L")))+
      xlab("")+
      theme_bw()+
      annotate("text",x="Event",y=-8,label=paste("Mean:",round(mean(chlrecord.events$FieldValue),2)))+
      annotate("text",x="No Event",y=-8,label=paste("Mean:",round(mean(chlrecord.noevents$FieldValue),2)))
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

#' Climate Factor Impact
#'
#' Performs a t-test on chl-a dataset for various climate variables
#'
#' @param chlrecord dataframe with estimated historical record of chl-a levels
#' @param climaterecord dataframe with climate variables
#' @param climatevar character, name of climate variable (column) of interest
#' @param lag integer, number of days to lag the climate variable
#' @param noevent numeric, threshold for whether an event occured
#' @param alternative character string specifying alternative hypothesis ("two.sided","greater","less")
#' @param stations list of unique stations to iterate over
#' @import stats
#' @import lubridate
#' @export
#'


climate_factor_station <- function(chlrecord,climaterecord,climatevar,lag=NULL,noevent,alternative="two.sided",stations){
  #If the variable is lagged:
  if(!is.null(lag)){
    #Lag climatevar
    laggedvar <- lagpad(climaterecord[,climatevar],lag)
    climaterecord[,paste(climatevar,'_lag')] <- laggedvar
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[laggedvar>noevent,'Date']
    chlrecord.events <- subset(chlrecord,ImageDate %in% climateeventdates)
    chlrecord.noevents <- subset(chlrecord,!(ImageDate %in% climateeventdates))
  }else{
    #If the variable is not lagged:
    #Subset into groups based on climate/weather events
    climateeventdates <- climaterecord[(climaterecord[,climatevar]>noevent),'Date']
    chlrecord.events <- subset(chlrecord,ImageDate %in% climateeventdates)
    chlrecord.noevents <- subset(chlrecord,!(ImageDate %in% climateeventdates))
  }
  stationresults <- data.frame(Station=rep(NA,length(stations)),PValue=rep(NA,length(stations)))
  if(nrow(chlrecord.noevents.sub)>0 & nrow(chlrecord.events.sub)>0){
    chlrecordcompare <- rbind(chlrecord.events.sub,chlrecord.noevents.sub)

    for(i in seq(1,length(stations))){
      temp.data.event <- chlrecord.events[(chlrecord.events$StationID==stations[i]),]
      temp.data.noevent <- chlrecord.noevents[(chlrecord.noevents$StationID==stations[i]),]
      temp.results <- (wilcox.test(temp.data.event$FieldValue,temp.data.noevent$FieldValue,alternative="greater"))
      stationresults$Station[i] <- as.character(stations[i])
      stationresults$PValue[i] <- temp.results$p.value
      stationresults$Event[i] <- mean(temp.data.event$FieldValue)
      stationresults$NoEvent[i] <- mean(temp.data.noevent$FieldValue)
    }
  }


  return(stationresults)
}
