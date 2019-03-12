library(dplyr)
library(tidyr)
library(lubridate)
library(RODBC)

#set work directory
workdir<-"D:/Job/Applause/"     ##update the work directory here
filelist<-c('bugs','devices','testers','tester_device') ##update file names here

setwd(workdir)
#import mulitiple csv files
for (i in filelist){
  
  df<-read.csv(file=paste0(i,'.csv'),header = T,strip.white = T,stringsAsFactors = F)
  
  #assign new name for files
  assign(i, df)
  
  rm(df)
} 

#join file
#tester & tester_device
t_td<-merge(testers,tester_device,by='testerId',all.y = T)

#tester & tester_device & device
t_td_d<-merge(t_td,devices,by='deviceId',all.x = T)

#combine all files
comb<-merge(t_td_d,bugs,by=c('testerId','deviceId'),all.y = T)


#############################################################

#matching function for two criterias
matching<-function(cy,de){
  
  if(length(cy)==1&&cy=='All'){
    
    ifelse(length(de)==1&de=='All',
           output<-comb%>%group_by(firstName,lastName)%>%summarise(Experience=n()),
           output<-comb%>%filter(description%in%de)%>%group_by(firstName,lastName)%>%
             summarise(Experience=n())
    )
    
    output<-arrange(output,-Experience)
    
  }else if(length(cy)==1&&cy!='All'){
    
    ifelse(length(de)==1&de=='All',
           output<-comb%>%filter(country%in%cy)%>%group_by(firstName,lastName)%>%
             summarise(Experience=n()),
           output<-comb%>%filter(description%in%de,country%in%cy)%>%group_by(firstName,lastName)%>%
             summarise(Experience=n())
    )
    output<-arrange(output,-Experience)
    
  }else if(length(cy)>1){
    
    ifelse(length(de)==1&de=='All',
           output<-comb%>%filter(country%in%cy)%>%group_by(firstName,lastName)%>%
             summarise(Experience=n()),
           output<-comb%>%filter(description%in%de,country%in%cy)%>%group_by(firstName,lastName)%>%
             summarise(Experience=n())
    )
    output<-arrange(output,-Experience)
  }
  
  #output$tester<-paste(output$firstName,output$lastName)
  
  return(as.data.frame(output))
  #output<-output%>%select(tester,Experience)
}

#wrap functions together and prompt to users
search_tester<-function(){
  
  c<-readline("Please Enter Country by Comma:")
  d<-readline("Please Enter Device by Comma:")
  country<-unlist(strsplit(c,','))
  
  device<-unlist(strsplit(d,','))
  
  matching(country,device)
  
}

###########################################
#Run this function to match testers
search_tester()




