% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projection.matrix.R
\name{projection.matrix}
\alias{projection.matrix}
\title{Construct projection matrix models using transition frequency tables}
\usage{
projection.matrix(
  transitions,
  stage = NULL,
  fate = NULL,
  fertility = NULL,
  sort = NULL,
  add = NULL,
  TF = FALSE
)
}
\arguments{
\item{transitions}{a stage-fate data frame with stage or age class in the
current census,  fate in the subsequent census, and one or more fertility columns}

\item{stage}{a column name or position of the stage column in the stage-fate
data frame.  Defaults to "stage".}

\item{fate}{name of the fate column in the stage-fate data frame. Defaults to "fate"}

\item{fertility}{one or more names of fertility columns in the stage-fate
data frame.  By default, any column names matching stage class names are
assumed to contain individual fertilities}

\item{sort}{a vector listing stage classes that correspond to the rows and
columns of the desired projection matrix.  Currently, names in this vector
must match a level in the stage column.  Also, this option should only be
used if stages are not \code{\link{ordered}}, since the default is to sort by
\code{\link{levels}} in the stage column.}

\item{add}{a vector listing row, column and value, used to add
\emph{estimated}  transtions to the transition matrix (e.g., a transition
from seed bank to  seedling).  May be repeated.}

\item{TF}{output separate transition (T) and fertility (F) matrices.  Default
is FALSE and outputs a single projection matrix A}
}
\value{
The default output is a single projection matrix A. If the TF flag is
true, then a list with 2 items where A=T+F
}
\description{
Construct an age or stage-structure projection model from a transition table
listing stage in time \emph{t}, fate in time \emph{t+1}, and one or more
individual fertility columns.
}
\details{
The state transition rates are estimated using transition frequency tables
(see section 6.1.1, Caswell 2001), so this technique will most likely apply
to demographic studies of plants or other sessile organisms where individuals
are tagged and then consistently relocated in annual censuses.  The fertility
rates are calculated by averaging individuals fertilities by stage class;
therefore, some care should be taken to correctly estimate individual
fertilities based on the timing of the census.
}
\note{
Individual fertilities should be the total number of offspring at the
end of the census interval.  Therefore, fertilites should include offspring
survival in a prebreeding censuses (and more than one offspring class may be
present).  In a postbreeding census, new offspring were born just before the
census, so the fertility rate is just the number of offspring in this case.
}
\examples{
trans01 <- subset(merge(test.census, test.census, by = "plant", sort =FALSE),
                    year.x==2001 & year.y==2002 )
## Add individual fertilities using "anonymous reproduction"  based on the
## proportional reproductive outputs of flowering plants and the total number
## of seedlings at the end of the projection interval
trans01$seedferts <- trans01$fruits.x/sum(trans01$fruits.x) * 5
trans01
stages <- c("seedling", "vegetative", "reproductive")
## three ways to specify columns
projection.matrix(trans01, stage.x, stage.y, seedferts, stages)
projection.matrix(trans01, 3, 6, 8, c(3,4,2))
projection.matrix(trans01, "stage.x", "stage.y", "seedferts", stages)
## BEST to use column default (fertility column (seedling) now matches stage class name)
names(trans01)[c(3, 6, 8)] <- c("stage", "fate", "seedling")
# AND order stages in dataframe
trans01$stage <- ordered(trans01$stage, stages)
projection.matrix(trans01)
projection.matrix(trans01, TF=TRUE)
## Example using Aquilegia data
sf <- subset(aq.trans, year==1998 & plot==909, c(year, plant, stage, fruits, fate))
## rows and columns of final matrix
levels(sf$stage)
## seedlings next year
seedlings <- nrow(subset(aq.trans, plot==909 & year==1999 & stage=="recruit"))
## ADD individual fertility estimates for recruits and seeds assuming seed bank and
## new seeds contribute to a common germinant pool with equal chance of recruitment
seed.survival <- .4
seed.bank.size <- 1000
seeds.per.fruit <- 50
seeds.from.plants <- sum(sf$fruits)*seeds.per.fruit
recruitment.rate <- seedlings/(seed.bank.size + seeds.from.plants)
## add two fertility columns
sf$recruit <- sf$fruits/sum(sf$fruits) * seeds.from.plants * recruitment.rate
sf$seed <- sf$fruits * seeds.per.fruit * seed.survival
## add seed bank survival and seed bank recruitment rate to transition matrix
A <- projection.matrix(sf, add=c(1,1, seed.survival, 2,1, recruitment.rate ))
A
max(Re(eigen(A)$values))

}
\author{
Chris Stubben
}
