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
#' Evaluates difference in values based on weather conditions at quartiles and locations
#'
#' @param wqrecord dataframe with estimated historical record of water quality parameter
#' @param imagedatecol string, name of column with the date of the estimate (date of remotely sensed imagery)
#' @param wqvarcol string, name of column with estimated or field-sampled water quality parameter
#' @param climaterecord dataframe with climate variables
#' @param climatevarcol character, name of climate variable (column) of interest
#' @param climatedatecol string, name of column with the date of the climate observation
#' @param maxlag numeric, number of days to lag the climate effect
#' @param noevent numeric, threshold for whether an event occurred
#' @param monthlist vector, an optional vector containing the months to split the data on
#' @param locationcol string, name of column with unique location identifier, used if overall is FALSE
#' @return results of kruskal wallis test for differences in mean values (and, if overall, boxplots of water quality data)
#' @import lubridate
#' @examples
#' data(estimatedrecord)
#' data(climatedata)
#' effectresults <- weather.effect(wqrecord=estimatedrecord,imagedatecol="ImageDate",wqvarcol="EstChlValue",
#' climaterecord=climatedata,climatevarcol="PRCP",climatedatecol="DATE",
#' maxlag=7,noevent=0,monthlist=c("June","July","August"),locationcol="StationID")
#' @export
#'

weather.effect <- function(wqrecord,imagedatecol,wqvarcol,
                           climaterecord,climatedatecol,climatevarcol,
                           maxlag=0,noevent=NA,monthlist=NULL,locationcol=NULL){
  #Format data frames of water quality record and climate record
  wqrecord$Date <- wqrecord[,imagedatecol]
  wqrecord$wqVar <- wqrecord[,wqvarcol]
  wqrecord$Location <- wqrecord[,locationcol]
  wqrecordsub <- wqrecord[,c("Location","Date","wqVar")]
  climaterecord$climVar <- climaterecord[,climatevarcol]
  climaterecord$Date <- climaterecord[,climatedatecol]
  climaterecordsub <- climaterecord[,c("Date","climVar")]
  lag <- seq(0,maxlag,1)

  if(!is.null(monthlist)){
    #Find average values for each location, month, and time lag and evaluate whether differences are significant
    #Create results dataframe
    locationlist <- unique(wqrecordsub$Location)
    results <- data.frame(Location=rep(locationlist,times=length(lag),each=length(monthlist)),
                          Month=rep(monthlist,times=length(lag)),
                          Lag=rep(0:maxlag,each=length(monthlist)*length(locationlist)),
                          NoEvent=NA,Q1=NA,Q2=NA,Q3=NA,Q4=NA,PValue=NA,
                          nNoEvent=NA,nQ1=NA,nQ2=NA,nQ3=NA,nQ4=NA)
    for(k in locationlist){
      for(j in monthlist){
        for(i in lag){
          #Lag the climate data
          climaterecordsub$lagclimVar <- lagpad(x=climaterecordsub$climVar,k=i)
          #Limit to only data at the location of interest
          wqrecordsub2 <- subset(wqrecordsub,Location==k)
          #Merge climate and water quality data
          wqclimatedata <- merge(climaterecordsub,wqrecordsub2, by.x="Date",by.y="Date")
          wqclimatedata$Month <- as.factor(months(wqclimatedata$Date))
          qbreaks <- quantile(wqclimatedata[(wqclimatedata$lagclimVar>noevent & wqclimatedata$Month==j),"lagclimVar"],
                              prob=c(0,0.25,0.5,0.75,1))
          wqclimatedata$Class <- cut(wqclimatedata$lagclimVar,
                                     breaks=c(noevent,unname(qbreaks)),
                                     labels=c("No Event","0-25%","25-50%","50-75%","75-100%"),
                                     include.lowest = TRUE)
          #Calculate the mean water quality value for each quantile
          results[(results$Location==k & results$Month==j & results$Lag==i),"NoEvent"] <- mean(subset(wqclimatedata,Class=="No Event" & Month==j,select=wqVar)[[1]],na.rm=TRUE)
          results[(results$Location==k & results$Month==j & results$Lag==i),"nNoEvent"] <- length(subset(wqclimatedata,Class=="No Event" & Month==j,select=wqVar)[[1]])
          results[(results$Location==k & results$Month==j & results$Lag==i),"Q1"] <- mean(subset(wqclimatedata,Class=="0-25%" & Month==j,select=wqVar)[[1]],na.rm=TRUE)
          results[(results$Location==k & results$Month==j & results$Lag==i),"nQ1"] <- length(subset(wqclimatedata,Class=="0-25%" & Month==j,select=wqVar)[[1]])
          results[(results$Location==k & results$Month==j & results$Lag==i),"Q2"] <- mean(subset(wqclimatedata,Class=="25-50%" & Month==j,select=wqVar)[[1]],na.rm=TRUE)
          results[(results$Location==k & results$Month==j & results$Lag==i),"nQ2"] <- length(subset(wqclimatedata,Class=="25-50%" & Month==j,select=wqVar)[[1]])
          results[(results$Location==k & results$Month==j & results$Lag==i),"Q3"] <- mean(subset(wqclimatedata,Class=="50-75%" & Month==j,select=wqVar)[[1]],na.rm=TRUE)
          results[(results$Location==k & results$Month==j & results$Lag==i),"nQ3"] <- length(subset(wqclimatedata,Class=="50-75%" & Month==j,select=wqVar)[[1]])
          results[(results$Location==k & results$Month==j & results$Lag==i),"Q4"] <- mean(subset(wqclimatedata,Class=="75-100%" & Month==j,select=wqVar)[[1]],na.rm=TRUE)
          results[(results$Location==k & results$Month==j & results$Lag==i),"nQ4"] <- length(subset(wqclimatedata,Class=="75-100%" & Month==j,select=wqVar)[[1]])
          #Kruskal-Walls Test (Non-parametric version of ANOVA)
          results[(results$Location==k & results$Month==j & results$Lag==i),"PValue"] <- kruskal.test(wqVar~Class,data=wqclimatedata[(wqclimatedata$Month==j & wqclimatedata$Location==k),])$p.value
        }
      }
    }
  }else{
    #Find average values for each location,and time lag and evaluate whether differences are significant
    #Create results dataframe
    locationlist <- unique(wqrecordsub$Location)
    results <- data.frame(Location=rep(locationlist,times=length(lag)),
                          Lag=rep(0:maxlag,each=length(locationlist)),
                          NoEvent=NA,Q1=NA,Q2=NA,Q3=NA,Q4=NA,PValue=NA,
                          nNoEvent=NA,nQ1=NA,nQ2=NA,nQ3=NA,nQ4=NA)
    for(k in locationlist){
      for(i in lag){
        #Lag the climate variable
        climaterecordsub$lagclimVar <- lagpad(x=climaterecordsub$climVar,k=i)
        #Limit to only data at the location of interest
        wqrecordsub2 <- subset(wqrecordsub,Location==k)
        #Merge climate and water quality data
        wqclimatedata <- merge(climaterecordsub,wqrecordsub2, by.x="Date",by.y="Date")
        #Calculate values at quantiles and classify the data accordingly
        qbreaks <- quantile(wqclimatedata[(wqclimatedata$lagclimVar>noevent),"lagclimVar"],
                            prob=c(0,0.25,0.5,0.75,1))
        wqclimatedata$Class <- cut(wqclimatedata$lagclimVar,
                                   breaks=c(noevent,unname(qbreaks)),
                                   labels=c("No Event","0-25%","25-50%","50-75%","75-100%"),
                                   include.lowest = TRUE)
        #Calculate the mean water quality value for each quantile
        results[(results$Location==k & results$Lag==i),"NoEvent"] <- mean(subset(wqclimatedata,Class=="No Event",select=wqVar)[[1]],na.rm=TRUE)
        results[(results$Location==k & results$Lag==i),"Q1"] <- mean(subset(wqclimatedata,Class=="0-25%",select=wqVar)[[1]],na.rm=TRUE)
        results[(results$Location==k & results$Lag==i),"Q2"] <- mean(subset(wqclimatedata,Class=="25-50%",select=wqVar)[[1]],na.rm=TRUE)
        results[(results$Location==k & results$Lag==i),"Q3"] <- mean(subset(wqclimatedata,Class=="50-75%",select=wqVar)[[1]],na.rm=TRUE)
        results[(results$Location==k & results$Lag==i),"Q4"] <- mean(subset(wqclimatedata,Class=="75-100%",select=wqVar)[[1]],na.rm=TRUE)
        results[(results$Location==k & results$Lag==i),"nNoEvent"] <- length(subset(wqclimatedata,Class=="No Event",select=wqVar)[[1]])
        results[(results$Location==k & results$Lag==i),"nQ1"] <- length(subset(wqclimatedata,Class=="0-25%",select=wqVar)[[1]])
        results[(results$Location==k & results$Lag==i),"nQ2"] <- length(subset(wqclimatedata,Class=="25-50%",select=wqVar)[[1]])
        results[(results$Location==k & results$Lag==i),"nQ3"] <- length(subset(wqclimatedata,Class=="50-75%",select=wqVar)[[1]])
        results[(results$Location==k & results$Lag==i),"nQ4"] <- length(subset(wqclimatedata,Class=="75-100%",select=wqVar)[[1]])

        #Kruskal-Walls Test (Non-parametric version of ANOVA)
        results[(results$Location==k & results$Lag==i),"PValue"] <- kruskal.test(wqVar~Class,data=wqclimatedata)$p.value
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
