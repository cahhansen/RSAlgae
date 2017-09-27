#' Explore linear trends in the historic record
#'
#' Calculates linear trend and significance by month over the period of record
#'
#' @param record dataframe with record of chl-a levels. Must have columns: FieldValue, ImageDate.
#' @param by options for subsetting the data. month: to look at plots on a monthly basis; interval: plot the trend/confidence intervals for different lengths of the historic record
#' @import lubridate
#' @import ggplot2
#' @import reshape2
#' @import RColorBrewer
#' @importFrom cowplot plot_grid
#' @import stats
#' @export

longterm_trend <- function(record,by=""){
  year.start <- min(lubridate::year(record$ImageDate))
  year.end <- max(lubridate::year(record$ImageDate))
  record$year.norm <- lubridate::year(record$ImageDate)-year.start+1

  if(by=="month"){
    record$Month <- ordered(as.factor(months(record$ImageDate)))
    month.names <- c("May","June","July","August","September")
    record$Month <- ordered(record$Month, levels = month.names)
    results <- data.frame(Month=month.names,Trend=rep(NA,5),PValue=rep(NA,5),LowerCI=rep(NA,5),UpperCI=rep(NA,5))
    for(i in seq(1,5,1)){
      record.sub <- record[(record$Month==month.names[i]),]
      monthlymod <- lm(FieldValue~year.norm, data=record.sub)
      results$Trend[i] <- summary(monthlymod)$coefficients[2,1]
      results$LowerCI[i] <- confint(monthlymod)[2]
      results$UpperCI[i] <- confint(monthlymod)[4]
      results$PValue[i] <- summary(monthlymod)$coefficients[2,4]
      assign(paste("Plot",i, sep=''), ggplot(record.sub,aes(x=ImageDate,y=FieldValue))+
               geom_point(aes(x=ImageDate,y=FieldValue))+
               theme_bw()+
               ylab(expression(paste("Chl-a (",mu,"g/L)")))+
               xlab("Date")+
               ggtitle(i)+
               theme(legend.position="none")+
               geom_smooth(method = "lm", se = FALSE,col='red'))
      print(paste(month.names[i],"Trend:",summary(monthlymod)$coefficients[2,1],"PValue:",summary(monthlymod)$coefficients[2,4],"CI:",round(confint(monthlymod)[2],2),"-",round(confint(monthlymod)[4],2)))
    }
    p1 <- plot_grid(Plot1,Plot2,Plot3,Plot4,Plot5, align='h')

    p2 <- ggplot(record,aes(x=ImageDate,y=FieldValue))+
      geom_point(aes(fill=Month),pch=21,colour="black")+
      ylab(expression(paste("Chl-a (",mu,"g/L)")))+
      xlab("Date")+
      theme_bw()+
      scale_fill_brewer(palette="Blues",
                        name="Month",
                        breaks=c("May","June","July","August","September"),
                        labels=c("May","June","July","August","September"))+
      theme(legend.position="bottom")
    return(list(Plot1=p1,Plot2=p2,Results=results))
  }
  if(by=="interval"){
    results <- data.frame(Year=seq(year.start+1,year.end),Trend=rep(NA,year.end-year.start),LowerCI=rep(NA,year.end-year.start),UpperCI=rep(NA,year.end-year.start))
    for(j in seq(year.start+1,year.end)){
      record.sub <- record[(record$ImageDate<paste0(j+1,'-12-31')),]
      temp.mod <- lm(FieldValue~year.norm, data=record.sub)
      temp.results <- summary(temp.mod)
      results$Trend[j-year.start] <- temp.results$coefficients[2,1]
      results$LowerCI[j-year.start] <- confint(temp.mod)[2]
      results$UpperCI[j-year.start] <- confint(temp.mod)[4]
      results$PValue[j-year.start] <- temp.results$coefficients[2,4]
    }
    plotresults = melt(results[,c(1,2,3,4)], id.var="Year")
    p3 <- ggplot(data=plotresults)+
      geom_line(aes(x=Year,y=value,colour=variable))+
      theme_bw()+
      ylab(expression(paste("Trend in Chl(",mu,"g/L)/year")))+
      xlab("End Year for Interval")+
      theme(legend.position="bottom")+
      scale_colour_manual(name="",
                          breaks=c("Trend","UpperCI"),
                          values=c('Trend'='blue','UpperCI'='red','LowerCI'='red'),
                          labels=c("Trend","Confidence Interval"))
    return(list(Plot=p3,Results=results))
  }
  if(by==""){
    temp.mod <- lm(lm(FieldValue~year.norm, data=record))
    print(paste("Overall Trend:",summary(temp.mod)$coefficients[2,1],"PValue:",summary(temp.mod)$coefficients[2,4],"CI:",round(confint(temp.mod)[2],2),"-",round(confint(temp.mod)[4],2)))

    p4 <- ggplot(data=record,aes(x=ImageDate,y=FieldValue))+
      geom_point(colour='black')+
      geom_smooth(method = "lm", se = FALSE,col='red')+
      theme_bw()+
      ylab(expression(paste("Chl(",mu,"g/L)")))+
      xlab("Date")+
      theme(legend.position="none")

    results <- data.frame(Trend=summary(temp.mod)$coefficients[2,1],PValue=summary(temp.mod)$coefficients[2,4],LowerCI=confint(temp.mod)[2],UpperCI=confint(temp.mod)[4])
    return(list(Plot=p4,Results=results))
  }
}

