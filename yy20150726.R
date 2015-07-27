library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(relaimpo)
library(RSQLite)

library(xlsx)
library(RMySQL)
library(ggplot2) #add for ggplot
library(reshape2)
library(dplyr)

#library(bigmemory)


##deal with CSV file
##1. delete the "," in number
##2. change the date to 
par(mfrow=c(1,1),mar=c(4,4,0,0))
mycolor=rainbow(20)


# about the space
gc()

#----------------------------------------------------------------------------------------------
#1# original data
if (F){
  conn <- dbConnect(MySQL(), dbname = "thdata", username="thdata_user", password="gbgj53GD2s2gy64wRT",host="121.43.197.34",port=3306)
  yydb201545 = dbGetQuery(conn,"select kid,sid,liveuid,users,typename,date from simple_online_duowan")
  dbDisconnect(conn)
  
  
  conn <- dbConnect(MySQL(), dbname = "yy", username="root", password="123456",host="127.0.0.1",port=3306)
  #yydb20150102 = dbGetQuery(conn,"select kid,sid,liveuid,users,typename,date from yy20150102")
  yydb20150203 = dbGetQuery(conn,"select kid,sid,liveuid,users,typename,date from yy20150203")
  yydb201504 = dbGetQuery(conn,"select kid,sid,liveuid,users,typename,date from yy201504")
  dbDisconnect(conn)
  
  #class(yydb20150102$date) = c('POSIXt','POSIXct')
  #yydb20150102$mon=as.integer(format(as.Date(yydb20150102$date),"%Y%m"))
  
  class(yydb20150203$date) = c('POSIXt','POSIXct')
  yydb20150203$mon=as.integer(format(as.Date(yydb20150203$date),"%Y%m"))
  
  class(yydb201504$date) = c('POSIXt','POSIXct')
  yydb201504$mon=as.integer(format(as.Date(yydb201504$date),"%Y%m"))
  
  class(yydb201545$date) = c('POSIXt','POSIXct')
  yydb201545$mon=as.integer(format(as.Date(yydb201545$date),"%Y%m"))
  
  
  #yy=rbind(yydb20150102,yydb20150203,yydb201504)
  yy=rbind(yydb20150203,yydb201504)
  
  yy=rbind(yy,yydb201545)  
  save(file="./yy.Rdata",yy)
  
  yy201502=filter(yy,mon==201502)
  yy201503=filter(yy,mon==201503)
  yy201504=filter(yy,mon==201504)
  yy201505=filter(yy,mon==201505)
  #yy201502[seq(1,nrow(yy201502),10000),"date"]
  save(file="yy201502.Rdata",yy201502)
  save(file="yy201503.Rdata",yy201503)
  save(file="yy201504.Rdata",yy201504)
  save(file="yy201505.Rdata",yy201505)
  
  
  conn <- dbConnect(MySQL(), dbname = "yy", username="root", password="123456",host="127.0.0.1",port=3306)
  
#20141001 20141101
  (bunixstamp=as.numeric(as.POSIXct("2014-10-01")))
  (eunixstamp=as.numeric(as.POSIXct("2014-11-01")))


yy201410 = dbGetQuery(conn,'select kid,sid,liveuid,users,typename,date from duowan_yy where (date<UNIX_TIMESTAMP("2014-12-26") )&& (date>=UNIX_TIMESTAMP("2014-12-25"))')
yy201410 = dbGetQuery(conn,"select kid,sid,liveuid,users,typename,date from duowan_yy where date>=1412092800 && date<1417363200")
yy201410 = dbGetQuery(conn,"select kid,sid,liveuid,users,typename,date from duowan_yy")

class(yy201410$date) = c('POSIXt','POSIXct')
yy201410$mon=as.integer(format(as.Date(yy201410$date),"%Y%m"))
head(yy201410)  
tail(yy201410)  
dbDisconnect(conn)
  
  
}


if (F){

bdate="2015-04-01"
edate="2015-07-31"
(bunixstamp=as.numeric(as.POSIXct(bdate)))
(eunixstamp=as.numeric(as.POSIXct(edate)))

conn <- dbConnect(MySQL(), dbname = "thdata", username="thdata_user", password="gbgj53GD2s2gy64wRT",host="121.43.197.34",port=3306)
yy = dbGetQuery(conn,"select kid,sid,liveuid,users,typename,date from simple_online_duowan where date>=1427817600 && date<=1438272000")
dbDisconnect(conn)

class(yy$date) = c('POSIXt','POSIXct')
yy$mon=as.integer(format(as.Date(yy$date),"%Y%m"))

save(file=paste(bdate,edate,"yy.Rdate"),yy)

}



#----------------------------------------------------------------------------------------------
#2# load yy
#gc()
#load("./yy.Rdata")
yy=tbl_df(yy)

#----------------------------------------------------------------------------------------------
#3# select the data
resid.list=list()
reuid.list=list()
resbytype.list=list()
monthlist=c(201505,201506)
breaks=c(100000,50000,10000)

for(monthnow in monthlist){

  mdpre=filter(yy,mon==monthnow-1) #previous
  mdnew=filter(yy,mon==monthnow) #new
  mdpre=select(mdpre,date,mon,sid,liveuid,typename,users)
  mdnew=select(mdnew,date,mon,sid,liveuid,typename,users)
  mdpre=tbl_df(mdpre)
  mdnew=tbl_df(mdnew)
  
  mdnew$sidnew=0
  mdnew$siddel=0
  mdnew$uidnew=0
  mdnew$uiddel=0
  
  mdpre$sidnew=0
  mdpre$siddel=0
  mdpre$uidnew=0
  mdpre$uiddel=0
  
  mdnew[!(mdnew$sid %in% mdpre$sid),"sidnew"]=1
  mdpre[!(mdpre$sid %in% mdnew$sid),"siddel"]=1
  mdnew[!(mdnew$liveuid %in% mdpre$liveuid),"uidnew"]=1
  mdpre[!(mdpre$liveuid %in% mdnew$liveuid),"uiddel"]=1
  
  #-----------------------------whole without typename classify----------------------------
  sidnewtot=group_by(mdnew,sid)
  sidnewadd=group_by(filter(mdnew,sidnew==1),sid)
  sidpredel=group_by(filter(mdpre,siddel==1),sid)
  
  uidnewtot=group_by(mdnew,liveuid)
  uidnewadd=group_by(filter(mdnew,uidnew==1),liveuid)
  uidpredel=group_by(filter(mdpre,uiddel==1),liveuid)
  #length(group_size(gsidnew))
  
  smrtotsid=summarise(sidnewtot,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
  smraddsid=summarise(sidnewadd,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
  smrdelsid=summarise(sidpredel,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
  
  smrtotuid=summarise(uidnewtot,nuid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
  smradduid=summarise(uidnewadd,nuid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
  smrdeluid=summarise(uidpredel,nuid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))

  resid=data.frame(month=NA,project=NA,uniquesid=NA, sumuser=NA,uniqueuid=NA,nmu10=NA,smu10=NA,nmu5=NA,smu5=NA,nmu1=NA,smu1=NA,nmu0=NA,smu0=NA,
                   stringsAsFactors =F)
  resid[1,]=data.frame(month=monthnow,project=as.character("total"),uniquesid=length(unique(sidnewtot$sid)),sumuse=sum(smrtotsid$meanuser),uniqueuid=length(unique(uidnewtot$liveuid)),
                       nmu10=sum(smrtotsid$meanuser>=breaks[1]),NA,
                       nmu5=sum(smrtotsid$meanuser>=breaks[2] & smrtotsid$meanuser<breaks[1]),NA,
                       nmu1=sum(smrtotsid$meanuser>=breaks[3] & smrtotsid$meanuser<breaks[2]),NA,
                       nmu0=sum(smrtotsid$meanuser<breaks[3]),NA,stringsAsFactors =F)

  if (nrow(filter(smrtotsid,meanuser>=breaks[1]))==0) resid[1,"smu10"]=0 else 
    resid[1,"smu10"]=sum(select(filter(smrtotsid,meanuser>=breaks[1]),meanuser))
  if (nrow(filter(smrtotsid,meanuser>=breaks[2] & meanuser<breaks[1]))==0) resid[1,"smu5"]=0   else 
    resid[1,"smu5"]=sum(select(filter(smrtotsid,meanuser>=breaks[2] & meanuser<breaks[1]),meanuser))
  if (nrow(filter(smrtotsid,meanuser>=breaks[3] & meanuser<breaks[2]))==0) resid[1,"smu1"]=0   else 
    resid[1,"smu1"]=sum(select(filter(smrtotsid,meanuser>=breaks[3] & meanuser<breaks[2]),meanuser))
  if (nrow(filter(smrtotsid,meanuser<breaks[3]))==0) resid[1,"smu0"]=0   else 
    resid[1,"smu0"]=sum(select(filter(smrtotsid,meanuser<breaks[3]),meanuser))
  
  resid[2,]=data.frame(monthnow,as.character("newadd"),length(unique(sidnewadd$sid)),sum(smraddsid$meanuser), length(unique(uidnewadd$liveuid)),
                       nmu10=sum(smraddsid$meanuser>=breaks[1]),NA,
                       nmu5=sum(smraddsid$meanuser>=breaks[2] & smraddsid$meanuser<breaks[1]),NA,
                       nmu1=sum(smraddsid$meanuser>=breaks[3] & smraddsid$meanuser<breaks[2]),NA,
                       nmu0=sum(smraddsid$meanuser<breaks[3]),NA,stringsAsFactors =F)
  
  if (nrow(filter(smraddsid,meanuser>=breaks[1]))==0) resid[2,"smu10"]=0 else 
    resid[2,"smu10"]=sum(select(filter(smraddsid,meanuser>=breaks[1]),meanuser))
  if (nrow(filter(smraddsid,meanuser>=breaks[2] & meanuser<breaks[1]))==0) resid[2,"smu5"]=0   else 
    resid[2,"smu5"]=sum(select(filter(smraddsid,meanuser>=breaks[2] & meanuser<breaks[1]),meanuser))
  if (nrow(filter(smraddsid,meanuser>=breaks[3] & meanuser<breaks[2]))==0) resid[2,"smu1"]=0   else 
    resid[2,"smu1"]=sum(select(filter(smraddsid,meanuser>=breaks[3] & meanuser<breaks[2]),meanuser))
  if (nrow(filter(smraddsid,meanuser<breaks[3]))==0) resid[2,"smu0"]=0   else 
    resid[2,"smu0"]=sum(select(filter(smraddsid,meanuser<breaks[3]),meanuser))
  
  
  resid[3,]=data.frame(monthnow,as.character("delete"),length(unique(sidpredel$sid)),sum(smrdelsid$meanuser), length(unique(uidpredel$liveuid)), NA, NA, NA,NA,NA, NA, NA, NA,stringsAsFactors =F)
  resid=transform(resid,psmu10=smu10/sumuser,psmu5=smu5/sumuser,psmu1=smu1/sumuser,psmu0=smu0/sumuser)
  
  write.csv(file=paste(monthnow,"sid user distribution specification.csv"),top_n(arrange(smrtotsid,desc(meanuser)),resid[1,"nmu10"]+resid[1,"nmu5"]+resid[1,"nmu1"],meanuser))
#  write.csv(file=paste(monthnow,"sid user number  summary.csv"),resid)

  resid.list[[monthnow-monthlist[1]+1]]=resid
  names(resid.list)[monthnow-monthlist[1]+1]=as.character(monthnow)
  resid.list
  
  #========================================================================================
  
  reuid=data.frame(month=NA,project=NA,uniquesid=NA, sumuser=NA,uniqueuid=NA,nmu10=NA,smu10=NA,nmu5=NA,smu5=NA,nmu1=NA,smu1=NA,nmu0=NA,smu0=NA,
                   stringsAsFactors =F)
  reuid[1,]=data.frame(month=monthnow,project=as.character("total"),uniquesid=length(unique(sidnewtot$sid)),sumuse=sum(smrtotuid$meanuser),uniqueuid=length(unique(uidnewtot$liveuid)),
                       nmu10=sum(smrtotuid$meanuser>=breaks[1]),NA,
                       nmu5=sum(smrtotuid$meanuser>=breaks[2] & smrtotuid$meanuser<breaks[1]),NA,
                       nmu1=sum(smrtotuid$meanuser>=breaks[3] & smrtotuid$meanuser<breaks[2]),NA,
                       nmu0=sum(smrtotuid$meanuser<breaks[3]),NA,stringsAsFactors =F)
  
  if (nrow(filter(smrtotuid,meanuser>=breaks[1]))==0) reuid[1,"smu10"]=0 else 
    reuid[1,"smu10"]=sum(select(filter(smrtotuid,meanuser>=breaks[1]),meanuser))
  if (nrow(filter(smrtotuid,meanuser>=breaks[2] & meanuser<breaks[1]))==0) reuid[1,"smu5"]=0   else 
    reuid[1,"smu5"]=sum(select(filter(smrtotuid,meanuser>=breaks[2] & meanuser<breaks[1]),meanuser))
  if (nrow(filter(smrtotuid,meanuser>=breaks[3] & meanuser<breaks[2]))==0) reuid[1,"smu1"]=0   else 
    reuid[1,"smu1"]=sum(select(filter(smrtotuid,meanuser>=breaks[3] & meanuser<breaks[2]),meanuser))
  if (nrow(filter(smrtotuid,meanuser<breaks[3]))==0) reuid[1,"smu0"]=0   else 
    reuid[1,"smu0"]=sum(select(filter(smrtotuid,meanuser<breaks[3]),meanuser))
  
  reuid[2,]=data.frame(monthnow,as.character("newadd"),length(unique(sidnewadd$sid)),sum(smradduid$meanuser), length(unique(uidnewadd$liveuid)),
                       nmu10=sum(smradduid$meanuser>=breaks[1]),NA,
                       nmu5=sum(smradduid$meanuser>=breaks[2] & smradduid$meanuser<breaks[1]),NA,
                       nmu1=sum(smradduid$meanuser>=breaks[3] & smradduid$meanuser<breaks[2]),NA,
                       nmu0=sum(smradduid$meanuser<breaks[3]),NA,stringsAsFactors =F)
  
  if (nrow(filter(smradduid,meanuser>=breaks[1]))==0) reuid[2,"smu10"]=0 else 
    reuid[2,"smu10"]=sum(select(filter(smradduid,meanuser>=breaks[1]),meanuser))
  if (nrow(filter(smradduid,meanuser>=breaks[2] & meanuser<breaks[1]))==0) reuid[2,"smu5"]=0   else 
    reuid[2,"smu5"]=sum(select(filter(smradduid,meanuser>=breaks[2] & meanuser<breaks[1]),meanuser))
  if (nrow(filter(smradduid,meanuser>=breaks[3] & meanuser<breaks[2]))==0) reuid[2,"smu1"]=0   else 
    reuid[2,"smu1"]=sum(select(filter(smradduid,meanuser>=breaks[3] & meanuser<breaks[2]),meanuser))
  if (nrow(filter(smradduid,meanuser<breaks[3]))==0) reuid[2,"smu0"]=0   else 
    reuid[2,"smu0"]=sum(select(filter(smradduid,meanuser<breaks[3]),meanuser))
  
  
  reuid[3,]=data.frame(monthnow,as.character("delete"),length(unique(sidpredel$sid)),sum(smrdelsid$meanuser), length(unique(uidpredel$liveuid)), NA, NA, NA,NA,NA, NA, NA, NA,stringsAsFactors =F)
  reuid=transform(reuid,psmu10=smu10/sumuser,psmu5=smu5/sumuser,psmu1=smu1/sumuser,psmu0=smu0/sumuser)
  
  write.csv(file=paste(monthnow,"uid user distribution specification.csv"),top_n(arrange(smrtotuid,desc(meanuser)),reuid[1,"nmu10"]+reuid[1,"nmu5"]+reuid[1,"nmu1"],meanuser))
#  write.csv(file=paste(monthnow,"uid user number summary.csv"),reuid)
  
  reuid.list[[monthnow-monthlist[1]+1]]=reuid
  names(reuid.list)[monthnow-monthlist[1]+1]=as.character(monthnow)
  reuid.list

  
  #-----------------------------by type statistic of the total date-----------------------------
  
  type.range=names(table(mdnew$typename))
  type=c("entertain","fin","game","love","ting")
  resbytype=as.data.frame(matrix(NA,9,3+length(type)))
  names(resbytype)=c("month","project","valuename",type)
  resbytype$month=monthnow
  resbytype$project=rep(c("total","newadd","deletle"),each=3)
  resbytype$valuename=rep(c("uniquesid","sumuser","uniqueuid"),3)
  resbytype
  
  for (i in 1:length(type)) {
    #for (i in 1:1) {
    
    tname=type[i]
    
    sidnewtot=group_by(filter(mdnew,typename==tname),sid)
    sidnewadd=group_by(filter(mdnew,typename==tname & sidnew==1),sid)
    sidpredel=group_by(filter(mdpre,typename==tname & siddel==1),sid)
    
    uidnewtot=group_by(filter(mdnew,typename==tname),liveuid)
    uidnewadd=group_by(filter(mdnew,typename==tname & uidnew==1),liveuid)
    uidpredel=group_by(filter(mdpre,typename==tname & uiddel==1),liveuid)
    #length(group_size(gsidnew))
    
    smrtotsid=summarise(sidnewtot,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    smraddsid=summarise(sidnewadd,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    smrdelsid=summarise(sidpredel,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    
    resbytype[,3+i]=c(length(unique(sidnewtot$sid)),sum(smrtotsid$meanuser),length(unique(uidnewtot$liveuid)),
                      length(unique(sidnewadd$sid)),sum(smraddsid$meanuser), length(unique(uidnewadd$liveuid)),
                      length(unique(sidpredel$sid)),sum(smrdelsid$meanuser), length(unique(uidpredel$liveuid)))
    

    
    smrtotuid=summarise(uidnewtot,nuid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    smradduid=summarise(uidnewadd,nuid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    smrdeluid=summarise(uidpredel,nuid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    
    if (length(unique(uidnewadd$liveuid))>0) write.csv(file=paste(monthnow,tname,"newadd liveuid top100.csv",sep="-"),
                                                       top_n(arrange(smradduid,desc(meanuser)),100,meanuser))
    
  }
  
  resbytype
  resbytype.list[[monthnow-monthlist[1]+1]]=resbytype
  names(resbytype.list)[monthnow-monthlist[1]+1]=as.character(monthnow)
  resbytype.list
}


resid.table=resid.list[[1]]
reuid.table=resid.list[[1]]
for (i in (2:length(resid.list))) {
  if (length(resid.list)<2) break
  resid.table=rbind(resid.table,resid.list[[i]])
  reuid.table=rbind(reuid.table,reuid.list[[i]])
}

resbytype.table=resbytype.list[[1]]
for (i in (2:length(resbytype.list))) {
  if (length(resbytype.list)<2) break
  resbytype.table=rbind(resbytype.table,resbytype.list[[i]])
}
write.csv(file=paste("sid user number summary.csv"),resid.table)
write.csv(file=paste("uid user number summary.csv"),reuid.table)
write.csv(file=paste("user number summary by type.csv"),resbytype.table)

save(file="./resbytype.Rdata",resid.list,reuid.list,resbytype.list)




#-----------------------------daily with typename classify-----------------------------
dayres.list=list()
monthlist=c(201504:201506)
monthlist
  
md=tbl_df(select(filter(yy,mon %in% monthlist),date,typename,sid,liveuid,users)) #previous
md$date=as.Date(md$date)
type=names(table(md$typename))
days=unique(as.Date(md$date))
uniquesid=data.frame(date=days,totuniquesid=NA,entertain=NA,fin=NA,game=NA,love=NA,ting=NA)
uniqueuid=data.frame(date=days,totuniqueuid=NA,entertain=NA,fin=NA,game=NA,love=NA,ting=NA)
meanuser=data.frame(date=days,summeanuser=NA,entertain=NA,fin=NA,game=NA,love=NA,ting=NA)

for (n in 1:length(days)){
  
  dayth=days[n]

  ddnew=filter(md,date==dayth)
  #ddnew=md
  smrddnew=summarise(group_by(ddnew,sid),nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
  
  uniquesid[n,"totuniquesid"]=n_distinct(ddnew$sid)
  uniqueuid[n,"totuniqueuid"]=n_distinct(ddnew$liveuid)
  meanuser[n,"summeanuser"]=sum(smrddnew$meanuser)
      
  for (i in 1:length(type)) {
    #for (i in 1:1) {
    tname=type[i]
    
    sidnewtot=filter(ddnew,typename==tname)
    sidnewtot=group_by(sidnewtot,sid)

    uniquesid[n,ncol(uniquesid)-length(type)+i]=n_distinct(sidnewtot$sid)
    uniqueuid[n,ncol(uniqueuid)-length(type)+i]=n_distinct(sidnewtot$liveuid)
    
    smrtotsid=summarise(sidnewtot,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    meanuser[n,ncol(uniquesid)-length(type)+i]=sum(smrtotsid$meanuser)
    
  }
}
  

dayres.list=list(uniquesid,uniqueuid,meanuser)
names(dayres.list)=c("uniquesid","uniqueuid","meanuser")
dayres.list
save(file="dayres.Rdata",dayres.list)

write.csv(file=paste("uniquesid daily summary.csv"),dayres.list[[1]])
write.csv(file=paste("uniqueuid daily summary.csv"),dayres.list[[2]])
write.csv(file=paste("meanuser daily summary.csv"),dayres.list[[3]])












test=tbl_df(select(filter(yy,mon %in% c(201505)),date,typename)) #previous
length(which(test$typename=="entertain"))



#-----------------test hour data test with typename classify-------------------------

load("yy201502.Rdata")
yyall=tbl_df(yy201502)
monthlist=c(201502:201502)

houres.list=list()
md=tbl_df(select(filter(yyall,mon %in% monthlist),date,typename,sid,liveuid,users)) #previous
type=names(table(md$typename))

stime=as.POSIXlt(min(md$date))
endtime=as.POSIXlt(max(md$date))
seqtime=seq(stime,endtime-3600,3600)

uniquesid=data.frame(date=seqtime,totuniquesid=NA,entertain=NA,fin=NA,game=NA,love=NA,ting=NA)
uniqueuid=data.frame(date=seqtime,totuniqueuid=NA,entertain=NA,fin=NA,game=NA,love=NA,ting=NA)
meanuser=data.frame(date=seqtime,summeanuser=NA,entertain=NA,fin=NA,game=NA,love=NA,ting=NA)

for (n in 1:length(seqtime)) {
#n=1
  
  yytime=as.POSIXlt(seqtime[n])
  
  yynextime=as.POSIXlt(yytime)
  yynextime$hour=yytime$hour+1
  ddnew=filter(md, date>= yytime & date<yynextime)
  
  smrddnew=summarise(group_by(ddnew,sid),nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
  uniquesid[n,"totuniquesid"]=n_distinct(ddnew$sid)
  uniqueuid[n,"totuniqueuid"]=n_distinct(ddnew$liveuid)
  meanuser[n,"summeanuser"]=sum(smrddnew$meanuser)
  
  for (i in 1:length(type)) {
    #for (i in 1:1) {
    tname=type[i]
    
    sidnewtot=filter(ddnew,typename==tname)
    sidnewtot=group_by(sidnewtot,sid)
    
    uniquesid[n,ncol(uniquesid)-length(type)+i]=n_distinct(sidnewtot$sid)
    uniqueuid[n,ncol(uniqueuid)-length(type)+i]=n_distinct(sidnewtot$liveuid)
    
    smrtotsid=summarise(sidnewtot,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
    meanuser[n,ncol(uniquesid)-length(type)+i]=sum(smrtotsid$meanuser)
    
  }
  
}
  
write.xlsx(file="./doc/yy_feb_data_mean_result_byhour.xlsx",meanuser)
write.xlsx(file="./doc/yy_feb_data_uid_result_byhour.xlsx",uniqueuid)
write.xlsx(file="./doc/yy_feb_data_sid_result_byhour.xlsx",uniquesid)



#----------------------------------------------------------------------------------------------
#4# make the statistic



#----------------------------------------------------------------------------------------------
#5# gather the reuslt





save(file="./yy.Rdata",yy)
