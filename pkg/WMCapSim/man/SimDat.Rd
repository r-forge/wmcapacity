\name{SimDat}
\alias{SimDat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulation of working memory change detection data}
\description{
This function simulates data from a change detection experiment,
commonly used in working memory research. It assumes that the models
described by Morey (in preparation) are the true model. This is useful
for analysis testing.
}
\usage{
SimDat(Nsub = 23, Ntrl = 60, SetSizes = c(2, 5, 8), muK = 3.1, sdK = 1, muZ = 2.9, sdZ = 0.46, muIntG = 0.77, sdIntG = 0.39, muSlpG = 1, sdSlpG = 0.44, prch = qlogis(c(0.3, 0.5, 0.7)), KEffect = c(0, -2), GEffect = c(0, -0.7))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nsub}{Number of participants to simulate}
  \item{Ntrl}{Total number of trials in a condition.}
  \item{SetSizes}{A vector of array set sizes}
  \item{muK}{The population mean capacity}
  \item{sdK}{The population standard deviation capacity}
  \item{muZ}{The population mean logit zone-out parameter}
  \item{sdZ}{The population standard deviation zone-out parameter}
  \item{muIntG}{ ~~Describe \code{muIntG} here~~ }
  \item{sdIntG}{ ~~Describe \code{sdIntG} here~~ }
  \item{muSlpG}{ ~~Describe \code{muSlpG} here~~ }
  \item{sdSlpG}{ ~~Describe \code{sdSlpG} here~~ }
  \item{prch}{ ~~Describe \code{prch} here~~ }
  \item{KEffect}{ ~~Describe \code{KEffect} here~~ }
  \item{GEffect}{ ~~Describe \code{GEffect} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
