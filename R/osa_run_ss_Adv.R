
## wrappers for Stock synthesis OSA diagnostics
## Created by Steve Barbeaux 8/8/2024 mostly stolen from work by Cole Monnahan

## Run OSA analysis for age composition
#' @model directory of the model
#' @ages ages over which to do the analysis, should match the model
#' @fleet fleet number
#' @sx sex to do the analysis
#' @N1 number of draws to be conducted
### example  
## mods=dir()[1:4]  ##In the model directory with four models
## OSA_run_SS_age(model=mods[1],ages=0:12, fleet=2, sx=1, N1=100, stck='EBS_COD',surv='EBSSHELF')


OSA_run_SS_age<-function(model=mods[1],ages=0:12, fleet=2, sx=1, N1=100, stck='EBS_COD',surv='EBSSHELF'){
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
   plot_osa_comps2(o, p, pearson, index=ages, index_label=paste0(model,'_age'), years=years, Neff=Neff, 
   stock=paste("A_sex=",sx,stck,model,sep="_"), survey=surv,N=N1)
  }

## Run OSA analysis for length composition
#' @model directory of the model
#' @lengths length bins over which to do the analysis, should match the model
#' @fleet fleet number
#' @sx sex to do the analysis
#' @N1 number of draws to be conducted
### example  
## mods=dir()[1:4]  ##In the model directory with four models
## OSA_run_SS_length(model=mods[1],lengths=seq(4.5,119.5,by=5),fleet=2, sx=1, N1=100, stck='EBS_COD',surv='EBSSHELF')
  
  OSA_run_SS_length<-function(model=mods[1],lengths=seq(4.5,119.5,by=5), fleet=2, sx=1, stck='EBS_COD',surv='EBSSHELF',N1=100){
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
   plot_osa_comps2(o,p, pearson, index=lengths, index_label=paste0(model,'_length bin'), years=years, Neff=Neff, 
   stock=paste("L_sex=",sx,stck,model,sep="_"), survey=surv,N=N1)
  }




#' Explore OSA residuals for multinomial composition data and
#' compare to Pearson
#' @param obs,exp,pearson the observed, expected and Pearson
#'   residual matrices with rows as years and columns as ages (or
#'   lengths)
#' @param index,years vectors giving the index of ages (or length) and years
#' @param index_label character value indicating 'age' or 'length bin' depending on comp type
#' @param stock,survey characters given the stock and survey,
#' used to create filename stock_survey.pdf
#' @param outpath folder name for output, e.g. 'figs'
#' @return returns nothing but creates a PDF file in the working
#' directory
#'
plot_osa_comps2 <- function(obs, exp, pearson, index, years, index_label, Neff,
                           stock, survey, outpath = '',N=N1){

  stopifnot(all.equal(nrow(obs), nrow(exp), nrow(pearson),
                      length(years)))
  stopifnot(all.equal(ncol(obs), ncol(exp), ncol(pearson), length(index)))

  filename <- paste0(stock,"_",survey,"_", gsub('\\s', '_', index_label), ".pdf")

  if(is.null(outpath)) {
    pdf(here::here(filename), onefile=TRUE, width=7, height=7)
  } else {
    pdf(here::here(outpath, filename), onefile=TRUE, width=7, height=7)
  }

  on.exit(dev.off())
  ## Neff <- ceiling(Neff)
  o1 <- round(Neff*obs/rowSums(obs),0); p=exp/rowSums(exp)
  ## default output
  res1<-list()
  sdnr1<-list()
  for(i in 1:N){
    res1[[i]] <- resMulti(t(o1), t(p))
    sdnr1[[i]]<-sd(res1[[i]])
  }
  sdnr<-data.table(do.call(rbind,sdnr1))
  names(sdnr)="sdnr"
  sdnr$ID<-1:nrow(sdnr)
  sdnr<-sdnr[order(sdnr),]

  HCI<-sqrt(qchisq(.95, (length(res1[[1]])-1))/(length(res1[[1]])-1))
  LCI<-sqrt(qchisq(.05, (length(res1[[1]])-1))/(length(res1[[1]])-1))
  n=1
  if(N>1){n=trunc(N/2)}

  
  psdnr<-ggplot(sdnr,aes(x=sdnr))+geom_histogram(color="black",fill="salmon")
  psdnr<-psdnr+geom_vline(data=sdnr[n,],aes(xintercept =sdnr), colour="blue",linewidth=1.5,linetype=3)+theme_bw(base_size=16)+labs(x="OSA SDNR",y="Count")
  psdnr<-psdnr+geom_vline(data=sdnr[n,],aes(xintercept =HCI), colour="red",linewidth=1.5,linetype=2)
  psdnr<-psdnr+geom_vline(data=sdnr[n,],aes(xintercept =LCI), colour="red",linewidth=1.5,linetype=2)
  print(psdnr)

   
   res=res1[[sdnr$ID[n]]]

  if(!all(is.finite(res))){
    warning("failed to calculate OSA residuals for ", stock)
    return(NULL)
  }
  par(mfrow=c(2,2))
  plot_res(x=res,o=obs,e=p,pr=pearson)
  
}

#' @x Residual object as returned from one of the residual functions
#' @o observed
#' @e expected
#' @pr pearson residuals

## stole key pieces from compResidual:::plot.cres
plot_res <- function(x=res,o=o1 ,e=p, pr=pearson){
  library(compResidual)  
  Neff <- colSums(o)
  ehat <- Neff*e
  V <- Neff*e*(1-e)
  
  nbins <- nrow(o)
  nyrs<-nrow(e)
  pearson<-pr
  if(!all(is.finite(x))){
    ind <- (!is.finite(x))
    message("the following  were not finite")
    message("observed counts= ", paste(o[-nbins,][ind], collapse=' '))
    message("expected counts= ", paste(round(ehat[-nbins,][ind],2), collapse=' '))
    message("Pearson resid= ", paste(round(pearson[-nbins,][ind],2), collapse=' '))
    message("OSA resid= ", paste(round(x[ind],2), collapse=' '))
    stop('non-finite residuals')
    pearson[2,17]
    pearson[!is.finite(x)]
    pearson[ind]
    x[ind]
    o[-nbins,][ind]
    ehat[-nbins,][ind]
    pearson[-nbins,][ind]
  }
  pearson=t(pearson)
  xname <- 1:nyrs
  yname <- 1:nbins
  add_legend <- function(x, cex.text = 1, ...) {
    zscale <- c(-3,-2,-1,0,1,2,3)
    zscale <- pretty(x, min.n = 4)
    uu <- par("usr")
    yy <- rep(uu[3] + 0.03 * (uu[4] - uu[3]), length(zscale))
    xx <- seq(uu[1] + 0.1 * (uu[2] - uu[1]), uu[1] + 0.4 * (uu[2] - uu[1]), length = length(zscale))
    text(xx, yy, labels = zscale, cex = cex.text)
    colb <- ifelse(zscale < 0, rgb(1, 0, 0, alpha = 0.5),
                   rgb(0, 0, 1, alpha = 0.5))
    bs <- 1
    if ("bubblescale" %in% names(list(...)))
      bs <- list(...)$bubblescale
    points(xx, yy+3*(1/length(yy)),
           cex = sqrt(abs(zscale))/max(sqrt(abs(zscale)), na.rm = TRUE) * 5 * bs,
           pch = 19, col = colb)
  }
  ymax <- max(o)


  sample <- rep(1:ncol(x), each = nrow(x))
  composition <- rep(1:nrow(x), ncol(x))
  xTicks <- pretty(1:ncol(x))
  xTicks <- xTicks[xTicks != 0]
  yTicks <- pretty(1:nrow(x))
  yTicks <- yTicks[yTicks != 0]
  plotby(sample, composition, x, bubblescale = 0.3, xlab = "",
         yaxt = "n", xaxt = "n", ylab='')
  mtext(text='OSA residuals', line=0, cex=.8)
  axis(1, at = xTicks, labels = xname[xTicks])
  axis(2, at = yTicks, labels = yname[yTicks])
  add_legend(x, cex.text = 0.8, bubblescale = 0.3)

  sample <- rep(1:ncol(pearson), each = nrow(pearson))
  composition <- rep(1:nrow(pearson), ncol(pearson))
  xTicks <- pretty(1:ncol(pearson))
  xTicks <- xTicks[xTicks != 0]
  yTicks <- pretty(1:nrow(pearson))
  yTicks <- yTicks[yTicks != 0]
  plotby(sample, composition, pearson, bubblescale = 0.3, xlab = "",
         yaxt = "n", xaxt = "n", ylab='n')
  mtext(text='Pearson residuals', line=0, cex=.8)
  axis(1, at = xTicks, labels = xname[xTicks])
  axis(2, at = yTicks, labels = yname[yTicks])
  add_legend(pearson, cex.text = 0.8, bubblescale = 0.3)

  q1 <- qqnorm(x, plot=FALSE)
  q2 <- qqnorm(pearson, plot=FALSE)
  plot(0,0,xlim=range(c(q1$x,q2$x)),ylim=range(c(q1$y,q2$y)),
       type='n', xlab='', ylab='')
  mtext(text='QQ plots', line=0, cex=.8)

  ##qqnorm(x, col = 1, main = "")
  with(q1, points(x,y, col=1))
  with(q2, points(x,y, col=2))
  abline(0, 1)
  legend("topleft", col = 1:2, legend = c("OSA", "Pearson"), pch = 1, bty = "n")
  sdnrs <- c(paste('OSA SDNR=',sprintf('%.2f', sd(x))),
    paste('Pearson SDNR=',sprintf('%.2f', sd(pearson))))
  legend('bottomright', legend=sdnrs, bty='n')
  oagg <- colSums(o)
  eagg <- colSums(e)
  nbins <- ncol(o)
  plot(1:nbins, y=oagg, ylim=c(0, max(oagg,eagg)), type='b',
       xlab='', ylab='')
  lines(1:nbins, y=eagg, col=2)
  mtext(text='Aggregated fits', line=0, cex=.8)
}
