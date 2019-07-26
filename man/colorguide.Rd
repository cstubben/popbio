\name{colorguide}
\alias{colorguide}
\title{ Plot a simple guide to colors }
\description{
  Plots a simple guide to colors with color blocks and names arranged vertically
}
\usage{
colorguide(col, main = "", border = FALSE)
}
\arguments{
  \item{col}{ A vector of colors }
  \item{main}{ Title  }
  \item{border}{ Type of border around rectangles }
}
%\details{ }
\value{ A plot of colors and names }
%\references{  }
\author{ Chris Stubben }
%\note{  }
\examples{
op<-par(mfrow=c(2,2))
colorguide(palette(), "Palette colors")
## 657 built-in colors
## RED
reds<-grep("red", colors(), value=TRUE)
## sorted alphabetically
colorguide(reds, "Reds sorted alphabetically")
# GREEN
greens<-grep("green", colors(), value=TRUE)
RGBColors <- col2rgb(greens)
RGBOrder <- order( RGBColors[2,], RGBColors[3,], RGBColors[1,]  )
colorguide(greens[RGBOrder][1:30], "Greens sorted by RGB")
## light blues
colorguide(grep("light.*blue", colors(), value=TRUE) , "Light blues")
## Functions
colorguide(rainbow(16, end=.7)  , "Rainbow colors")
colorguide(heat.colors(16)  , "Heat.colors")
## colorRampPalette
jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
  "yellow", "#FF7F00", "red", "#7F0000"))
colorguide(rev(jet.colors(16)), "Jet colors from Matlab")
blue2red<-colorRampPalette(c('blue','lightyellow','red'))
colorguide(blue2red(16), "Blue to Red colors")
par(op)
}
\keyword{ color }
