#' apply.mod.seasonal
#'
#' Apply GLM to remotely sensed record
#'
#' @param df dataframe with reflectance values
#' @param datecol string, name of column with imagery dates
#' @param model calibrated GLM
#' @param season vector of months to include in the season
#' @param threshold numeric value above which is considered unreasonable/noise
#' @return dataframe of predicted values and confidence intervals
#' @examples
#' data(srdataforapplication)
#' data(utahsummermod)
#' estdata <- apply.mod.seasonal(df=srdataforapplication,
#' datecol="ImageDate",model=utahsummermod,season=c("July","August","September"),threshold=500)
#' @export


apply.mod.seasonal <- function(df, datecol, model, season, threshold){
  df$ImageDate <- as.Date(df[,datecol])
  df$Month <- months(df$ImageDate)
  dfsub <- df[(df$Month %in% season),]
  subpredicted <- predict(model,newdata=dfsub,type='response',se.fit=T)
  dfsub$Value <- subpredicted$fit
  dfsub$Lower <- dfsub$Value-1.96*subpredicted$se.fit
  dfsub$Upper <- dfsub$Value+1.96*subpredicted$se.fit
  dfsub <- dfsub[(dfsub$Value<threshold),]
  return(dfsub)
}

