weather<-function(airport,startdate,enddate){ 
  
  if(!("RCurl" %in% rownames(installed.packages()))) {install.packages("RCurl")}
  
  library("RCurl")   
  
  baseURL<-"https://www.wunderground.com/history/airport"
  
 suffixURL <- 'DailyHistory.html?HideSpecis=1'
 weather<-read.table("",colClasses=c("numeric","numeric","numeric"),col.names=c("Maxtemp","Mintemp","Precip_mm"))
 
 for(i in seq(as.Date(startdate), as.Date(enddate), by="days")) {
  
   i <- format(as.Date(i,origin = "1970-01-01"),'%Y/%m/%d')
  
  url<-getURL(paste(baseURL,airport,i,suffixURL,sep = "/"))

  Maxtemp<-as.numeric(substr(sub(".*Max Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url), nchar(sub(".*Max Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url))-1, nchar(sub(".*Max Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url))))
  Mintemp<-as.numeric(substr(sub(".*Min Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url), nchar(sub(".*Min Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url))-1, nchar(sub(".*Min Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url))))
  rainfall<-as.numeric(substr(sub(".*Precipitation</span></td> *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url),nchar(sub(".*Precipitation</span></td> *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url))-3,nchar(sub(".*Precipitation</span></td> *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url))))
  

  
  Maxtemp<-ifelse(!is.na(Maxtemp),Maxtemp,as.numeric(substr(sub(".*Max Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url), nchar(sub(".*Max Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url)), nchar(sub(".*Max Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url)))))
  Mintemp<-ifelse(!is.na(Mintemp),Mintemp,as.numeric(substr(sub(".*Min Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url), nchar(sub(".*Min Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url)), nchar(sub(".*Min Temperature *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url)))))
  rainfall<-ifelse(!is.na(rainfall),rainfall,as.numeric(substr(sub(".*Precipitation</span></td> *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url),nchar(sub(".*Precipitation</span></td> *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url))-2,nchar(sub(".*Precipitation</span></td> *(.*?) *</span><span class=\"wx-unit\".*", "\\1", url)))))
  
  weather<-rbind(weather,list(Maxtemp=Maxtemp,Mintemp=Mintemp,Precip_mm=rainfall))
  
 }
 
 weather$Date<-seq(as.Date(startdate), as.Date(enddate), by="days")
 return(weather)
}
