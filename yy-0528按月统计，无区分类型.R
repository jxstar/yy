library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(relaimpo)

library(xlsx)
library(RSQLite)
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
  
  #yy=rbind(yydb20150102,yydb20150203,yydb201504)
  yy=rbind(yydb20150203,yydb201504)
  save(file="./yy.Rdata",yy)
  
  
  yy201502=filter(yy,mon==201502)
  yy201503=filter(yy,mon==201503)
  yy201504=filter(yy,mon==201504)
  #yy201502[seq(1,nrow(yy201502),10000),"date"]
  save(file="yy201502.Rdata",yy201502)
  save(file="yy201503.Rdata",yy201503)
  save(file="yy201504.Rdata",yy201504)
}


#----------------------------------------------------------------------------------------------
#2# load yy
gc()
load("./yy.Rdata")
yy=tbl_df(yy)

#----------------------------------------------------------------------------------------------
#3# select the data
monthnow=201503

mdpre=filter(yy,mon==monthnow-1) #previous
mdnew=filter(yy,mon==monthnow) #new
mdpre=select(mdpre,kid,date,mon,sid,liveuid,typename,users)
mdnew=select(mdnew,kid,date,mon,sid,liveuid,typename,users)
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

sidnewtot=group_by(mdnew,sid)
sidnewadd=group_by(filter(mdnew,sidnew==1),sid)
sidpredel=group_by(filter(mdpre,siddel==1),sid)
uidnewtot=group_by(mdnew,liveuid)
uidnewadd=group_by(filter(mdnew,uidnew==1),liveuid)
uidpredel=group_by(filter(mdpre,uiddel==1),liveuid)
#length(group_size(gsidnew))

smrtot=summarise(sidnewtot,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
smradd=summarise(sidnewadd,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))
smrdel=summarise(sidpredel,nsid=n(),nuser=sum(users),meanuser=mean(users),maxuser=max(users),minuser=min(users))

resid=data.frame(month=NA,project=NA,uniquesid=NA, sumuser=NA,uniqueuid=NA,nmu8=NA,smu8=NA,nmu5=NA,smu5=NA,nmu1=NA,smu1=NA,nmu0=NA,smu0=NA,
                 stringsAsFactors =F)
resid[1,]=data.frame(month=monthnow,project=as.character("total"),uniquesid=length(unique(sidnewtot$sid)),sumuse=sum(smrtot$meanuser),uniqueuid=length(unique(uidnewtot$liveuid)),
                     nmu8=sum(smrtot$meanuser>=80000),smu8=sum(select(filter(smrtot,meanuser>=80000),meanuser)),
                     nmu5=sum(smrtot$meanuser>=50000 & smrtot$meanuser<80000),smu5=sum(select(filter(smrtot,meanuser>=50000 & meanuser<80000),meanuser)),
                     nmu1=sum(smrtot$meanuser>=10000 & smrtot$meanuser<50000),smu1=sum(select(filter(smrtot,meanuser>=10000 & meanuser<50000),meanuser)),
                     nmu0=sum(smrtot$meanuser<10000),smu0=sum(select(filter(smrtot,meanuser<10000),meanuser)),stringsAsFactors =F)
resid[2,]=data.frame(monthnow,as.character("newadd"),length(unique(sidnewadd$sid)),sum(smradd$meanuser), length(unique(uidnewadd$liveuid)),NA,NA, NA, NA, NA, NA, NA, NA,stringsAsFactors =F)
resid[3,]=data.frame(monthnow,as.character("delete"),length(unique(sidpredel$sid)),sum(smrdel$meanuser), length(unique(uidpredel$liveuid)),NA,NA, NA, NA, NA, NA, NA, NA,stringsAsFactors =F)

resid

if (F){
resid[2,]=data.frame(monthnow,as.character("add"),length(unique(sidnewadd$sid)),sum(smradd$meanuser),
                     sum(smradd$meanuser>=80000),sum(select(filter(smradd,meanuser>=80000),meanuser)),
                     sum(smradd$meanuser>=50000 & smradd$meanuser<80000),sum(select(filter(smradd,meanuser>=50000 & meanuser<80000),meanuser)),
                     sum(smradd$meanuser>=10000 & smradd$meanuser<50000),sum(select(filter(smradd,meanuser>=10000 & meanuser<50000),meanuser)),
                     sum(smradd$meanuser<10000),sum(select(filter(smradd,meanuser<10000),meanuser)),
                     length(unique(uidnewadd$liveuid)))
resid[3,]=data.frame(monthnow,"del",length(unique(sidpredel$sid)),sum(smrdel$meanuser),
                     sum(smrdel$meanuser>=80000),sum(select(filter(smrdel,meanuser>=80000),meanuser)),
                     sum(smrdel$meanuser>=50000 & smrdel$meanuser<80000),sum(select(filter(smrdel,meanuser>=50000 & meanuser<80000),meanuser)),
                     sum(smrdel$meanuser>=10000 & smrdel$meanuser<50000),sum(select(filter(smrdel,meanuser>=10000 & meanuser<50000),meanuser)),
                     sum(smrdel$meanuser<10000),sum(select(filter(smrdel,meanuser<10000),meanuser)),
                     length(unique(uidpredel$liveuid)))
}
save(file="201503result.Rdata",sidnewtot,sidnewadd,sidpredel,uidnewtot,uidnewadd,uidpredel,smrtot,smradd,smrdel,resid)

resid
save

head(group_size(sid.group))

#----------------------------------------------------------------------------------------------
#4# make the statistic



#----------------------------------------------------------------------------------------------
#5# gather the reuslt





save(file="./yy.Rdata",yy)
