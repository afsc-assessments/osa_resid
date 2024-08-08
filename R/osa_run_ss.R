
## wrappers for Stock synthesis OSA diagnostics
## Created by Steve Barbeaux 7/6/2023


OSA_run_SS<-function(model="Model23.1.0",ages=0:12, fleet=2, sx=1,stck='EBS_COD',surv='EBSSHELF'){
   require(data.table)
   library(compResidual)
   mods1<-r4ss::SSgetoutput(dirvec=model)
   age<-data.table::data.table(mods1[[1]]$agedbase[,c(1,6,13,16:19)])[Bin%in%ages & Fleet==fleet & Sex==sx]
   age<-data.table::data.table(melt(age,c('Yr','Fleet','Sex','Bin')))
   o<-age[variable=='Obs']
   o<-maditr::dcast(o,Yr~Bin)
   p<-age[variable=='Exp']
   p<-maditr::dcast(p,Yr~Bin)
   pearson<-age[variable=='Pearson']
   pearson<-maditr::dcast(pearson,Yr~Bin)
   years<-o$Yr
   o <- as.matrix(o[,-1])
   p <- as.matrix(p[,-1])
   pearson <- as.matrix(pearson[,-1])
   Neff<-round(data.table::data.table(mods1[[1]]$agedbase)[Bin==ages[1] & Sex==sx]$effN)
   plot_osa_comps2(o, p, pearson, index=ages, index_label='age', years=years, Neff=Neff, 
   stock=paste("A_sex=",sx,stck,model,sep="_"), survey=surv)
  }



  OSA_run_SS_length<-function(model=mods[1],lengths=seq(4.5,119.5,by=5), fleet=2, sx=1, stck='EBS_COD',surv='EBSSHELF'){
   library(compResidual)
   mods1<-r4ss::SSgetoutput(dirvec=model)
   age<-data.table::data.table(mods1[[1]]$lendbase[,c(1,6,13,16:19)])[Bin%in%lengths & Fleet==fleet &Sex==sx]
   age<-data.table::data.table(melt(age,c('Yr','Fleet','Sex','Bin')))
   o<-age[variable=='Obs']
   o<-maditr::dcast(o,Yr~Bin)
   p<-age[variable=='Exp']
   p<-maditr::dcast(p,Yr~Bin)
   pearson<-age[variable=='Pearson']
   pearson<-maditr::dcast(pearson,Yr~Bin)
   years<-o$Yr
   o <- as.matrix(o[,-1])
   p <- as.matrix(p[,-1])
   pearson <- as.matrix(pearson[,-1])
   Neff<-round(data.table::data.table(mods1[[1]]$lendbase)[Bin==lengths[1]& Fleet==fleet & Sex==sx]$effN)
   plot_osa_comps2(o,p, pearson, index=lengths, index_label='length bin', years=years, Neff=Neff, 
   stock=paste("L_sex=",sx,stck,model,sep="_"), survey=surv)
  }




 for(i in 3:length(mods)){
    OSA_run_SS_length(model=mods[i],lengths=seq(4.5,85.5,by=5), fleet=2, sx=1, stck='EBS_COD',surv='EBSSHELF')
 }


 for(i in 1:2){
    OSA_run_SS_length(model=mods[i],lengths=seq(4.5,85.5,by=5), fleet=1, sx=1, stck='EBS_COD',surv='Fishery')
 }


obs=o; exp=p;index=ages;stock=paste("A_sex=",sx,stck,model,sep="_"); survey=surv