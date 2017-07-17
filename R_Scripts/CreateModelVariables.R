
CreateModelVariables <- function(filename){

  library(ggplot2)
  library(reshape2)
  
  #Load surface reflectance data 
  ls57=read.csv(filename)
  
  #Plot the range of reflectance data by band
  ls57format=melt(ls57,id.vars='Lake', measure.vars=c('Blue','Green','Red','NIR','SWIR1','SWIR2'))
  plotls57=ggplot(ls57format)+ geom_boxplot(aes(x=Lake, y=value,color=variable))+ylab("Reflectance (0-10000)")+
    ggtitle("Landsat 5-7 Surface Reflectance Values by Band and by Lake")+ 
    scale_colour_manual(values = c("blue","green3","red","lightcoral","orange","gold"),name="Band")+theme_bw()
  plotls57
  ###################################################################################################################################
  ##Make R dataframes for model development and application
  dataframe=ls57
  dataframe=dataframe[,-1]
  #Create interaction variables
  dataframe$Green_Blue=dataframe$Green/dataframe$Blue
  dataframe$Red_Blue=dataframe$Red/dataframe$Blue
  dataframe$Red_Green=dataframe$Red/dataframe$Green
  dataframe$Red_NIR=dataframe$Red/dataframe$NIR
  dataframe$Red_SWIR1=dataframe$Red/dataframe$SWIR1
  dataframe$Green_SWIR1=dataframe$Green/dataframe$SWIR1
  dataframe$Blue_SWIR1=dataframe$Blue/dataframe$SWIR1
  dataframe$Red_SWIR2=dataframe$Red/dataframe$SWIR2
  dataframe$Green_SWIR2=dataframe$Green/dataframe$SWIR2
  dataframe$Blue_SWIR2=dataframe$Blue/dataframe$SWIR2
  dataframe$NIR_SWIR1=dataframe$NIR/dataframe$SWIR1
  dataframe$NIR_SWIR2=dataframe$NIR/dataframe$SWIR2
  dataframe$NIR_Blue=dataframe$NIR/dataframe$Blue
  dataframe$NIR_Green=dataframe$NIR/dataframe$Green
  dataframe$NDVI=(dataframe$NIR-dataframe$Red)/(dataframe$NIR+dataframe$Red)
  dataframe$avgRGB=(dataframe$Red+dataframe$Blue+dataframe$Green)/3
  dataframe$avgSWIR=(dataframe$SWIR1+dataframe$SWIR2)/2
  
  return(dataframe)
}


