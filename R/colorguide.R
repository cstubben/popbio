colorguide <- function(col, main="", border=FALSE)
{
  col<-rev(col)   ## reverse order of colors to print top to bottom
  op<-par(mar=c(1,1,1.5,1))
  plot(c(1,100),c(1,100), type = "n", axes = FALSE,  xlab="", ylab="" , main=main)
  x<-100/length(col)
  i <- seq(1,100,x)
  ## draw rectangles at left of plot
  rect(0, i, 20, x+i, col=col, border=border)
  mid<-diff(i)[1]/2
  text(20, i+mid, col, pos=4)
  par(op)
}

