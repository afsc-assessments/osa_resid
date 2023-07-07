#' Explore OSA residuals for multinomial composition data and
#' compare to Pearson
#' @param obs,exp,pearson the observed, expected and Pearson
#'   residual matrices with rows as years and columns as ages (or
#'   lengths)
#' @param ages,years vectors giving the ages and years
#' @param stock,survey characters given the stock and survey,
#' used to create filename stock_survey.pdf
#' @return returns nothing but creates a PDF file in the working
#' directory
#'
plot_osa_comps <- function(obs, exp, pearson, ages, years, Neff,
                           stock, survey){
  stopifnot(all.equal(nrow(obs), nrow(exp), nrow(pearson),
                      length(years)))
  stopifnot(all.equal(ncol(obs), ncol(exp), ncol(pearson),
                           length(ages)))
  filename <- paste0(stock,"_",survey,".pdf")
  pdf(here::here(filename), onefile=TRUE, width=7, height=7)
  on.exit(dev.off())
  ## Neff <- ceiling(Neff)
  o <- round(Neff*obs/rowSums(obs),0); p=exp/rowSums(exp)
  ## default output
  res <- resMulti(t(o), t(p))
  if(!all(is.finite(res))){
    warning("failed to calculate OSA residuals for ", stock)
    return(NULL)
  }
  plot(res)
  ## compare to Pearson side by side
  mat <- t(matrix(res, nrow=nrow(res), ncol=ncol(res)))
  dimnames(mat) <- list(year=years, age=ages[-1])
  reslong <- reshape2::melt(mat, value.name='resid')
  g1 <- ggplot(reslong, aes(year, age, size=abs(resid),
                            color=resid>0)) + geom_point() +
    ggtitle('OSA w/o age 1') + ylim(range(ages))
  dimnames(pearson) <- list(year=years, age=ages)
  pearsonlong <- reshape2::melt(pearson, value.name='resid')
  g2 <- ggplot(pearsonlong, aes(year, age, size=abs(resid),
                                color=resid>0)) + geom_point() +
    ggtitle('Pearson')
  print(cowplot::plot_grid(g1,g2, nrow=2))

  ## ind is age/len bin to drop
  for(ind in 1:length(ages)){
    ## assumes first column dropped so put it there
    ages2 <- ages[-ind]
    o2 <- cbind(o[,ind], o[,-ind])
    p2 <- cbind(p[,ind], p[,-ind])
    res <- resMulti(t(o2), t(p2))
    ## not sure why these fail sometimes?
    if(!all(is.finite(res))) {warning('failed when ind=',ind); break}
    mat <- t(matrix(res, nrow=nrow(res), ncol=ncol(res)))
    dimnames(mat) <- list(year=years, age=ages2)
    reslong <- reshape2::melt(mat, value.name='resid')
    g <- ggplot(reslong, aes(year, age, size=abs(resid),
                             color=resid>0)) + geom_point()+
      ggtitle(paste('OSA w/o age',ages[ind])) + ylim(range(ages))
    print(g)
  }
  message("wrote file ", filename)
}
