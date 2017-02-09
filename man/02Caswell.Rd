\name{02.Caswell}
\alias{02.Caswell}
\alias{Caswell}
\title{Converted Matlab functions from Caswell (2001) } 
\description{ 

\bold{Chapter 2}.  Age-classified matrix models
\describe{
\item{ \code{\link{pop.projection}} }{ section 2.2. Projection of
    population growth rates.  }
}

\bold{Chapter 4}.  Stage-classified matrix models
\describe{
\item{ \code{\link{lambda}} }{ section 4.4. Returns the dominant eigenvalue    }
\item{ \code{\link{stable.stage}} }{ section 4.5. Returns the stable
    stage distribution (right eigenvector)    }
\item{ \code{\link{reproductive.value}} }{ section 4.6. Returns the
    reproductive value (left eigenvector)   }
\item{ \code{\link{damping.ratio}} }{ section 4.7. Returns the damping
  ratio }
\item{ \code{\link{eigen.analysis}} }{ section 4.8. Computes
    eigenvalues and vectors, including the dominant eigenvalue , 
    stable stage distribution, reproductive value, damping
    ratio, sensitivities, and elasticities.  Since version 2.0, these
    are now included as separate functions as well }
}

\bold{Chapter 5}.  Events in the Life Cycle
\describe{
\item{ \code{\link{fundamental.matrix}} }{section 5.3.1.
    Calculate age-specific survival from a stage classified matrix
    using the fundamental matrix N }
\item{ \code{\link{net.reproductive.rate}} }{section 5.3.4.
    Calculate the net reproductive rate of a stage classified matrix
    using the dominant eigenvalue of the matrix R.  }
\item{ \code{\link{generation.time}} }{ section 5.3.5. Calculate the
    generation time of a stage-classified matrix  }
}
Age-specific  survivorship and fertility curves in Fig 5.1 and 5.2 are
now included in \code{demo(Caswell)}.  


\bold{Chapter 6}. Parameter estimation
\describe{
\item{ \code{\link{projection.matrix}} }{ section 6.1.1. Estimate
    vital rates and construct a projection matrix using transtion
    frequency tables}
\item{ \code{\link{QPmat}} }{ section 6.2.2. Construct a projection
    matrix from a time series of  individuals per stage using Wood's
    quadratic programming method.  Requires \code{quadprog} library.}
}


\bold{Chapter 9}.  Sensitivity analysis
\describe{
\item{ \code{\link{sensitivity}} }{ section 9.1. Calculate sensitivities }
\item{ \code{\link{elasticity}} }{ section 9.2. Calculate elasticities }
\item{ \code{\link{secder}} }{ section 9.7. Second derivatives of eigenvalues }
}


\bold{Chapter 10}.  Life Table Response Experiments
\describe{
\item{ \code{\link{LTRE}} }{ section 10.1 and 10.2.  Fixed designs in
    LTREs.  See  \code{demo(Caswell)} for variance
    decomposition in random design (Fig 10.10). } 
}


\bold{Chapter 12}.  Statistical inference
\describe{
\item{ \code{\link{boot.transitions}} }{ section 12.1.4. Resample
    observed census transitions in a stage-fate data frame   }
\item{ \code{\link{resample}} }{ section 12.1.5.2. Resample transitions
    in a projction matrix from a multinomial distribution (and fertilites from a log
    normal)} 
}


\bold{Chapter 14}.  Environmental stochasticity
\describe{
\item{ \code{\link{stoch.growth.rate}} }{ section 14.3. Calculate the
    log stochastic growth rate by simulation and Tuljapukar's approximation  }
\item{ \code{\link{stoch.sens}} }{ section 14.4.1. Senstivity and elasticity of 
stochastic growth rate from numerical simultations }
\item{ \code{\link{stoch.projection}} }{  section 14.5.3. Project
    stochastic growth from a sequence of matrices in a uniform and
    nonuniform environment }
}


\bold{Chapter 15}.  Demographic stochasticity
\describe{
\item{ \code{\link{multiresultm}} }{ section 15.1.3. Incorporate
    demographic stochasticity into population projections. The
    example uses the \link{whale} dataset to create a plot like figure
    15.3. }
}

}
\author{Chris Stubben}

\keyword{documentation}
