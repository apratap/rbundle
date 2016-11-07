######################
library("gridExtra")
library("ggplot2")
library("grid")
#####################

panel.correlation <- function(x, y, corMethod="spearman", digits=2, prefix="", cex.cor, col="black",...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,method=corMethod)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  if(abs(r) > .8 ) cex.cor = cex.cor + .5
  if(abs(r) > .9) col = "red"
  text(0.5, 0.5, txt, cex = cex.cor, col = col )
}

panel.smooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                          cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = col.smooth, ...)
}

corPlot <- function(data_matrix,...){
  m <- as.matrix(data_matrix)
  #remove any rows with NA
  to_keep <- !apply(m,1, function(x) any(is.na(x)))
  m <- m[to_keep,]
  pairs(m, upper.panel=panel.correlation, lower.panel=panel.smooth, ...)
}
