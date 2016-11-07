
doPCA <- function(dataMat,colorBy=NA, ...){
  x1 <- scale(dataMat)
  pca.res <- prcomp(x1, center=F, scale=F)
  percent_variation <- pca.res$sdev^2/sum(pca.res$sdev^2) * 100
  PCs <- data.frame(pca.res$x[,c(1:5)])
  
  plotlist <- lapply(1:4, function(i){
    x <- paste0('PC',i)
    y <- paste0('PC',i+1)
    if(class(colorBy) != "logical"){
      p <- ggplot(data=PCs, aes_string(x=x,y=y,color=colorBy))
    }else{
      p <- ggplot(data=PCs, aes_string(x=x,y=y))
    }
    p <- p + geom_point(size=.5, alpha=.7) + theme_bw(base_size = 14)
    p <- p + xlab(paste0(x,' - (', round(percent_variation[i],2), '%)' ))  + ylab(paste0(y,' - ( ', round(percent_variation[i+1],2), '%)' ))
  })
  
  if(class(colorBy) != "logical"){
    grid_arrange_shared_legend(plotlist, ncol = 2, nrow = 2)
  }else{
    do.call(grid.arrange, c(plotlist, list(ncol = 2)))
  }
}
