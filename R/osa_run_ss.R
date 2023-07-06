
## wrappers for Stock synthesis OSA diagnostics
## Created by Steve Barbeaux 7/6/2023


OSA_run_SS<-function(model="Model23.1.0",ages=0:12, fleet=2, sex=1,stck='EBS_COD',surv='EBSSHELF'){
   require(data.table)
   mods1<-r4ss::SSgetoutput(dirvec=model)
   age<-data.table(mods1[[1]]$agedbase[,c(1,6,16:19)])[Bin%in%ages & Fleet==fleet & Sex==sex]
   age<-data.table(melt(age,c('Yr','Fleet','Bin')))
   o<-age[variable=='Obs']
   o<-dcast(o,Yr~Bin)
   p<-age[variable=='Exp']
   p<-dcast(p,Yr~Bin)
   pearson<-age[variable=='Pearson']
   pearson<-dcast(pearson,Yr~Bin)
   years<-o$Yr
   o <- as.matrix(o[,-1])
   p <- as.matrix(p[,-1])
   pearson <- as.matrix(pearson[,-1])
   Neff<-round(data.table(mods1[[1]]$agedbase)[Bin==0 & Sex==sex]$effN)
   plot_osa_comps(o,p, pearson, ages=ages, years=years, Neff=Neff, stock=paste("A",sex,stck,model,sep="_"), survey=surv)
  }



  OSA_run_SS_length<-function(model="Model23.1.0",lengths=seq(3.5,75.5,by=1), fleet=2, sex=1, stck='EBS_COD',surv='EBSSHELF'){
   mods1<-r4ss::SSgetoutput(dirvec=model)
   age<-data.table(mods1[[1]]$lendbase[,c(1,6,16:19)])[Bin%in%lengths & Fleet==fleet &Sex==sex]
   age<-data.table(melt(age,c('Yr','Fleet','Bin')))
   o<-age[variable=='Obs']
   o<-dcast(o,Yr~Bin)
   p<-age[variable=='Exp']
   p<-dcast(p,Yr~Bin)
   pearson<-age[variable=='Pearson']
   pearson<-dcast(pearson,Yr~Bin)
   years<-o$Yr
   o <- as.matrix(o[,-1])
   p <- as.matrix(p[,-1])
   pearson <- as.matrix(pearson[,-1])
   Neff<-round(data.table(mods1[[1]]$lendbase)[Bin==lengths[1]& Fleet==fleet & Sex==sex]$effN)
   plot_osa_comps(o,p, pearson, ages=lengths, years=years, Neff=Neff, stock=paste("L",sex,stck,model,sep="_"), survey=surv)
  }

