#' Annual census data for Aquilegia chrysantha
#'
#' Demography census data from \emph{Aquilegia chrysantha} in Fillmore Canyon,
#' Organ Mountains, New Mexico, 1996-2003.
#'
#' This sample data set includes census data from 10 of the 15 total demography
#' plots established in 1995.
#'
#' @format A data frame with 2853 rows on the following 8 variables:
#' \describe{
#'   \item{plot}{Plot number}
#'   \item{year}{Year of census}
#'   \item{plant}{Plant id number}
#'   \item{status}{Plant status recorded in field: dead, dormant, recruit0
#'    (with cotyledons only), recruit1, flowering or vegetative. }
#'   \item{rose}{Total number of rosettes}
#'   \item{leaf}{Total number of leaves}
#'   \item{infl}{Total number of infloresences or flowering stalks}
#'   \item{fruits}{Total number of mature fruits}
#' }
#' @source Data set owners: Brook Milligan, Chris Stubben, Allan Strand
#' @seealso \code{\link{aq.trans}} for annual transitions with stage and fate
#' in same row
#' @examples
#' head2(aq.census)
#' sv <- table(aq.census$status, aq.census$year)
#' sv
#' stage.vector.plot(sv[-1, ], prop = FALSE)
"aq.census"

#' Annual transition data for Aquilegia chrysantha
#'
#' Transition data listing stages and fates from \emph{Aquilegia chrysantha}  in
#' Fillmore Canyon, Organ Mountains, New Mexico, 1996-2003.
#'
#' The five stage classes include seeds in the seed bank, new recruits or
#' seedlings, small vegetative plants with 1 rosette, large vegetative plants
#' with 2 or more rosettes, and flowering plants. Stage classes were assigned to
#' census plants using a combination of status and size data recorded in the
#' field.  See \code{demo(stage.classify)} for more details.
#'
#' @format A data frame with 1637 rows on the following 9 variables:
#' \describe{
#'     \item{\code{plot}}{Plot number}
#'     \item{\code{year}}{Staring year of census}
#'     \item{\code{plant}}{Plant id number}
#'      \item{\code{stage}}{Initial stage class with ordered factor
#'      levels \code{seed} < \code{recruit} < \code{small} < \code{large}
#'      < \code{flower}.}
#'     \item{\code{leaf}}{Total number of leaves}
#'     \item{\code{rose}}{Total number of rosettes}
#'     \item{\code{fruits}}{Total number of mature fruits}
#'     \item{\code{fate}}{Final stage class or fate with levels
#'     \code{seed} < \code{recruit} < \code{small} < \code{large} <
#'     \code{flower} < \code{dead}}
#'     \item{\code{rose2}}{Final number of rosettes}
#'   }
#' @source Data set owners: Brook Milligan, Chris Stubben, Allan Strand
#' @seealso \code{\link{aq.census}}
#' @examples
#' head2(aq.trans)
#' sv <- table(aq.trans$stage, aq.trans$year)
#' addmargins(sv)
#' stage.vector.plot(sv[-1, ], prop = FALSE, main = "Aquilegia stage vectors")
#' ## plot proportions with barplot
#' ## use xpd to draw legend outside plot boundaries
#' op <- par(mar = c(5, 4, 4, 1), xpd = TRUE)
#' x <- barplot(prop.table(sv[-1, ], 2),
#'   las = 1, col = 1:4, ylim = c(0, 1),
#'   xaxt = "n", space = .5, xlab = "Year", ylab = "Proportion in stage class"
#' )
#' yrs <- substr(colnames(sv), 3, 4)
#' axis(1, x, yrs)
#' legend(2.7, 1.25, rev(rownames(sv)[-1]), fill = 4:1, bty = "n", ncol = 2)
#' par(op)
"aq.trans"

#' Projection matrices for a tropical understory herb
#'
#' Projection matrices for a tropical understory herb (\emph{Calathea
#' ovandensis}) for plots 1-4 in years 1982-1985 and the pooled matrix.
#' Matrices were constructed using a post-breeding census with 8 size classes:
#' seed, seedling, juvenile, pre-reproductive, and 4 reproductive classes
#' divided by leaf area.
#'
#' @format A list of 17 matrices ordered by plot then year, with the pooled
#' matrix last.
#' @source Table 7 in Horvitz and Schemske (1995).  The pooled matrix is from
#' Table 8.
#' @references Horvitz, C.C. and D.W. Schemske. 1995. Spatiotemporal variation
#' in demographic transitions of a tropical understory herb: Projection matrix
#' analysis. Ecological Monographs 65:155-192.
#' @examples
#' calathea
#' ## Single matrix
#' calathea[[11]]
#' image2(calathea[[11]], text.cex = .8)
#' title(paste("Calathea", names(calathea[11])), line = 3)
#' ## MEAN matrix (exclude pooled matrix)
#' mean(calathea[-17])
#' ## all plot 1
#' calathea[1:4]
#' ## all 1982 matrices
#' calathea[ grep("1982", names(calathea)) ]
#' # OR
#' # calathea[seq(1,16,4)]
#' # split(calathea, 1:4)[[1]]
#' ## Growth rates -see Figure 7
#' x <- sapply(calathea[-17], lambda)
#' x <- matrix(x, nrow = 4, byrow = TRUE, dimnames = list(paste("plot", 1:4), 1982:1985))
#' x
#' matplot2(x, type = "b", ylab = "Growth rate", main = "Calathea growth rates")
"calathea"

#' Population sizes of grizzly bears in Yellowstone from 1959-1997
#'
#' Estimated number of adult female grizzly bears in the Greater Yellowstone
#' population from 1959-1997.
#'
#' @format A data frame with 39 rows on the following 2 variables.
#'   \describe{
#'     \item{\code{year}}{ Year of census}
#'     \item{\code{N}}{ Estimated number of female grizzlies}
#'   }
#' @source Table 3.1 in Morris and Doak 2002. Original data from Eberhardt et
#' al. 1986 and Haroldson 1999.
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#' @examples
#' grizzly
#' ## plot like Fig 3.6 (p. 66)
#' plot(grizzly$year, grizzly$N,
#'   type = "o", pch = 16, las = 1, xlab = "Year",
#'   ylab = "Adult females", main = "Yellowstone grizzly bears"
#' )
#' ## calcualte  log(Nt+1/Nt)
#' nt <- length(grizzly$N) ## number transitions
#' logN <- log(grizzly$N[-1] / grizzly$N[-nt])
#' ## Mean and var
#' c(mean = mean(logN), var = var(logN))
#' ## or using linear regression
#' ## transformation for unequal variances (p. 68)
#' x <- sqrt(grizzly$year[-1] - grizzly$year[-length(grizzly$year)])
#' y <- logN / x
#' mod <- lm(y ~ 0 + x)
#' summary(mod)
#' ## plot like Fig 3.7
#' plot(x, y,
#'   xlim = c(0, 1.2), ylim = c(-.3, .3), pch = 16, las = 1,
#'   xlab = expression(paste("Sqrt time between censuses ", (t[t + 1] - t[i])^{
#'     1 / 2
#'   })),
#'   ylab = expression(log(N[t + 1] / N[t]) / (t[t + 1] - t[i])^{
#'     1 / 2
#'   }),
#'   main = expression(paste("Estimating ", mu, " and ", sigma^2, " using regression"))
#' )
#' abline(mod)
#' ## MEAN (slope)
#' mu <- coef(mod)
#' ## VAR (mean square in analysis of variance table)
#' sig2 <- anova(mod)[["Mean Sq"]][2]
#' c(mean = mu, var = sig2)
#' ## Confidence interval for mean  (page 72)
#' confint(mod, 1)
#' ## Confidence interval for sigma 2  (equation 3.13)
#' df1 <- length(logN) - 1
#' df1 * sig2 / qchisq(c(.975, .025), df = df1)
#' ## test for outliers using dffits (p.74)
#' dffits(mod)[dffits(mod) > 2 * sqrt(1 / 38) ]
#' ## plot like  fig 3.11
#' plot(grizzly$N[-nt], logN,
#'   pch = 16, xlim = c(20, 100), ylim = c(-.3, .3), las = 1,
#'   xlab = "Number of females in year T",
#'   ylab = expression(log(N[t + 1] / N[t])),
#'   main = "Grizzly log population growth rates"
#' )
#' cor(grizzly$N[-nt], logN)
#' abline(lm(logN ~ grizzly$N[-nt]), lty = 3)
"grizzly"

#' Correlation matrices for Hudsonia vital rates
#'
#' Within year and between year correlation matrices from \emph{Hudsonia
#' montana} vital rates.  Correlations were calculated from first 13 growth and
#' survival rates only, since fertility rates vary little.
#'
#' @format A list with 2 correlation matrices, corrin (within year correlation)
#' and corrout (between year correlation).
#' @source The correlation matrices in Morris and Doak 2002 include some
#' correlations > 1.  A corrected set of correlations was sent by the D. Doak on
#' 8/4/2007.
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#' @seealso \code{\link{vitalsim}}
#' @examples
#' hudcorrs
"hudcorrs"

#' Projection matrices for mountain golden heather
#'
#' Projection matrices for the mountain golden heather (\emph{Hudsonia montana})
#' for years 1985 through 1988 with 6 size classes: seeds, seedlings, and 4 size
#' classes divided by plant area.
#'
#' @format A list of 4 matrices from 1985-1988
#' @source Table 6.7 in Morris and Doak 2002.  The original data is from Frost
#' 1990.
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#' @examples
#' hudsonia
#' sapply(hudsonia, lambda)
#' ## mean matrix
#' x <- mean(hudsonia)
#' image2(x, mar = c(1, 4, 5.5, 1))
#' title("Hudsonia mean matrix", line = 2.5)
#' lambda(x)
#' # variance
#' var2(hudsonia)
"hudsonia"

#' Best Kendall estimates of Hudsonia vital rate means and variances
#'
#' Best Kendall estimates of vital rate means (9 growth, 4 survival, and 11
#' fertility rates) for \emph{Hudsonia montana}.
#'
#' @format A data frame with 24 rows and 2 columns:
#' \describe{
#'   \item{\code{mean}}{ vital rate means}
#'   \item{\code{var}}{ vital rate variances}
#' }
#' @source Data  listed in Box 8.10 for the \code{\link{vitalsim}} function.
#' See also Table 8.5 in Morris and Doak (2002).
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#' @examples
#' hudvrs
#' hudmxdef(hudvrs$mean)
"hudvrs"

#' Projection matrices for monkeyflower
#'
#' Pooled and annual projection matrices of central and marginal populations of
#' monkeyflowers (\emph{Mimulus cardinalis} and \emph{M. lewisii})
#'
#' Matrix constructed using a post-breeding census with four stage classes:
#' Seeds, small non-reproductive, large non-reproductive, and reproductive.
#'
#' @format A data frame with 32 matrices, arranged with one matrix per row
#'    \describe{
#'     \item{\code{species}}{M. cardinalis or M. lewisii}
#'     \item{\code{site}}{Study site}
#'     \item{\code{year}}{Start year of projection interval or pooled for all three years}
#'     \item{\code{a11}}{matrix element a11;  seed to seed transition or seed bank survival}
#'     \item{\code{a12}}{matrix element a12;  small nr to seed - fertility}
#'     \item{\code{a13}}{matrix element a13;  large nr to seed - fertility}
#'     \item{\code{a14}}{matrix element a14;  reprod to seed - fertility}
#'     \item{\code{a21}}{matrix element a21;  seed to small nr - growth}
#'     \item{\code{a22}}{matrix element a22;  small nr to small nr -stasis}
#'     \item{\code{a23}}{matrix element a23;  large nr to small nr - regress}
#'     \item{\code{a24}}{matrix element a24;  reprod to small nr - regress}
#'     \item{\code{a31}}{matrix element a31;  seed to large nr - growth  }
#'     \item{\code{a32}}{matrix element a32;  small nr to large nr - growth  }
#'     \item{\code{a33}}{matrix element a33;  large nr to large nr - stasis  }
#'     \item{\code{a34}}{matrix element a34;  reprod to large nr - regress  }
#'     \item{\code{a41}}{matrix element a41;  seed to reprod - growth  }
#'     \item{\code{a42}}{matrix element a42;  small nr to reprod - growth  }
#'     \item{\code{a43}}{matrix element a43;  large nr to reprod - growth  }
#'     \item{\code{a44}}{matrix element a44;  reprod to reprod - stasis  }
#'   }
#' @source \url{http://www.esapubs.org/archive/ecol/E087/126/appendix-E.htm}
#' @references Amy Lauren Angert. 2006. Demography of central and marginal
#' populations of monkeyflowers (\emph{Mimulus cardinalis} and \emph{M.
#' lewisii}). Ecology 87:2014-2025.
#' @examples
#' monkeyflower
#' ## convert M. cardinalis rows to list of 16 matrices
#' A <- subset(monkeyflower, species == "cardinalis")
#' # use as.matrix to convert data.frame to numeric matrix
#' A <- split(as.matrix(A[, 4:19]), paste(A$site, A$year))
#' stages <- c("seed", "sm.nr", "lg.nr", "repro")
#' ## convert to list of 16 matrices
#' A <- lapply(A, matrix, nrow = 4, byrow = TRUE, dimnames = list(stages, stages))
#' A[8]
#' image2(A[[8]], round = 8, mar = c(1, 3, 4.5, 1))
#' title(paste("M. cardinalis - ", names(A[8])), line = 2.5)
#' ## plot like figure 1A
#' x <- matrix(sapply(A, lambda), ncol = 4)
#' colnames(x) <- c("BU", "CA", "RP", "WA")
#' rownames(x) <- c(2000:2002, "pooled")
#' x <- x[, c(1, 3, 4, 2)]
#' colrs <- gray(0:3 / 3)[c(1, 3, 2, 4)]
#' barplot(x, beside = TRUE, las = 1, col = colrs, ylim = c(0, 2),
#'   ylab = "Population growth rate", main = "Mimulus cardinalis")
#' box()
#' abline(h = 1, lwd = .5)
#' legend(1, 1.95, rownames(x), fill = colrs, bty = "n")
"monkeyflower"

#' Population densities for the sugarbeet cyst nematode
#'
#' A time-series of population vectors for the sugarbeet cyst nematode
#' \emph{Heterodera schachtii}. Individuals were classified into three stages
#' (J2, J3+J4, and adult) and densities (per 60 cc of soil) were averaged over
#' four replicates, measured every two days, for 10 days. .
#'
#' @format A matrix listing densities from  3 stage classes over 6 time periods
#' @source Example 6.3 in Caswell (2001).
#' @seealso \code{\link{QPmat}}
#' @references Caswell, H. 2001. Matrix population models. Construction,
#' Analysis and interpretation. 2nd ed. Sinauer, Sunderland, Massachusetts.
#' @examples
#' nematode
#' stage.vector.plot(nematode,
#'   prop = FALSE, log = "y", ylim = c(.3, 200),
#'   xlab = "Time", ylab = "Nematode density"
#' )
"nematode"

#' Projection matrix for teasel
#'
#' Projection matrix with six stage classes for the plant teasel
#'
#' @format A 6 x 6 matrix
#' @source Example 5.2 in Caswell 2001.
#' @references Caswell, H. 2001. Matrix population models. Construction,
#' Analysis and interpretation. 2nd ed. Sinauer, Sunderland, Massachusetts.
#' @examples
#' teasel
#' image2(teasel, mar = c(1, 3.5, 5, 1), box.offset = .1)
#' title("Teasel projection matrix", line = 2.5)
#' # fertilities for a monocarpic plant in a prebreeding census in last column
#' splitA(teasel, r = 1:6, c = 6)
#' lambda(teasel)
"teasel"

#' Census data for hypothetical plant
#'
#' Three years of census data for a hypothetical plant with three stage classes
#'
#' @format A data frame with 41 census rows and 4 columns:
#' \describe{
#'  \item{\code{plant}}{Plant id number}
#'  \item{\code{year}}{Year of census}
#'  \item{\code{stage}}{Stage class: seedling, vegetative, or reproductive}
#'  \item{\code{fruits}}{Total number of fruits}
#' }
#' @examples
#' test.census
#' stages <- c("seedling", "vegetative", "reproductive")
#' ## Cross-tabulate stage vectors and order rows by stage
#' sv <- table(test.census$stage, test.census$year)[stages, ]
#' sv
#' stage.vector.plot(sv)
#' ## set xaxt='n' to avoid fractions of a year (2002.5)
#' stage.vector.plot(sv, prop = FALSE, xaxt = "n", las = 1)
#' axis(1, 2001:2003, c(2001, 2002, 2003))
#' ## Convert census data to state-fate transition table using reshape
#' reshape(test.census, direction = "wide", idvar = "plant", timevar = "year")
#' ## Convert census data  to state-fate transition table using merge
#' trans <- subset(
#'   merge(test.census, test.census, by = "plant", sort = FALSE),
#'   year.x == year.y - 1
#' )
#' trans
#' ## Format column and row names
#' trans <- trans[, c(1:4, 6)]
#' colnames(trans)[2:5] <- c("year", "stage", "fruits", "fate")
#' rownames(trans) <- 1:nrow(trans)
#' ## Order stage and fate columns
#' trans$stage <- ordered(trans$stage, levels = stages)
#' trans$fate <- ordered(trans$fate, levels = c(stages, "dead"))
#' ## Select transitions for 2001-2002 and count offspring (seedlings)
#' trans01 <- subset(trans, year == 2001)
#' seedlings <- nrow(subset(test.census, year == 2002 & stage == "seedling"))
#' ## Add individual fertilities using "anonymous reproduction"  based on the
#' ## proportional reproductive outputs of flowering plants and the total number
#' ## of seedlings at the end of the projection interval
#' trans01$seedling <- trans01$fruits / sum(trans01$fruits) * seedlings
#' trans01
#' ##  Create transition frequency table  and build T matrix
#' tf <- table(trans01$fate, trans01$stage)
#' tf
#' ## remove "dead" fate from matrix
#' ## T.mat<-prop.table(tf,2)[-4,]
#' T.mat <- prop.table(tf, 2)[stages, ]
#' T.mat
#' ## Summarize stage-specific fertility rates and build F matrix
#' fert <- tapply(trans01$seedling, trans01$stage, mean)
#' fert
#' F.mat <- T.mat * 0
#' F.mat[1, ] <- fert
#' F.mat
#' ## The final projection matrix is just
#' T.mat + F.mat
#' ## OR use projection matrix function -
#' projection.matrix(trans01)
"test.census"

#' Projection matrices for desert tortoise
#'
#' Projection matrices for the desert tortoise \emph{Gopherus agassizii} with 4
#' different fertility estimates (low, medium low, medium high, and high)
#'
#' @format A list of 4 matrices
#'
#' @source Table 5 in Doak et al (1994).  Used by Caswell (2001) in chapter 9 on
#' sensitivity analysis.
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#' @examples
#' tortoise
#' A <- tortoise[["med.high"]]
#' # log color scale not needed
#' image2(A, mar = c(1, 3.5, 5, 1), log = FALSE, box.off = .1)
#' title("Tortoise projection matrix", line = 3)
#' splitA(A)
#' lambda(A)
#' sapply(tortoise, lambda)
"tortoise"

#' Projection matrix for killer whale
#'
#' Projection matrix for killer whales with 4 size classes: yearling, juvenile,
#' mature and post-reproductive
#'
#' @format A 4 x 4 matrix
#'
#' @source Example 5.1 in Caswell (2001)
#' @references Caswell, H. 2001. Matrix population models. Construction,
#' Analysis and interpretation. 2nd ed. Sinauer, Sunderland, Massachusetts.
#' @examples
#' whale
#' splitA(whale)
#' lambda(whale)
#' sensitivity(whale)
#' # plot sensitivity
#' matplot2(sensitivity(whale),
#'   type = "b", legend = "topleft", ltitle = "Fate",
#'   main = "Killer Whale sensitivity"
#' )
"whale"

#' Survirvorship data for adult and juvenile Acorn Woodpeckers
#'
#' Number of juvenile and adult Acorn Woodpeckers and survival in the Water
#' Canyon, New Mexico population, reconstructed from Stacey and Taper (1992).
#'
#' @format A data frame with 18 rows and 4 columns
#' \describe{
#'   \item{\code{rate}}{ Adult or juvenile stage}
#'   \item{\code{year}}{ Year}
#'   \item{\code{start}}{ Total number of starting individuals}
#'   \item{\code{surv}}{ Number surviving to spring}
#' }
#' @source Stacey, P.B., and M. Taper. 1992. Environmental variation and the
#' persistence of small populations. Ecological Applications 2: 18-29.
#' @references Akcakaya, H. R. 2002. Estimating the variance of survival rates
#' and fecundities. Animal Conservation 5: 333-336.
#' Kendall, B. E. 1998. Estimating the magnitude of environmental stochasticity
#' in survivorship data. Ecological Applications 8(1): 184-193.
#' @seealso \code{\link{Kendall}} and \code{\link{varEst}}
#' @examples
#' woodpecker
#' x <- subset(woodpecker, rate == "adult")
#' plot(x$year, x$start,
#'   type = "o", pch = 16,
#'   ylab = "Number of adults", xlab = "Year",
#'   main = "Acorn Woodpeckers in Water Canyon"
#' )
#' ## stage-specific survival rate
#' x <- aggregate(
#'   list(Nstart = woodpecker$start, Nsurv = woodpecker$surv),
#'   list(stage = woodpecker$rate), sum
#' )
#' x$survival <- x[, 3] / x[, 2]
#' x
"woodpecker"
