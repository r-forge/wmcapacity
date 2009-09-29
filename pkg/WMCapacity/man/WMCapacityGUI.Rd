     \name{WMCapacityGUI}
     \Rdversion{1.1}
     \alias{WMCapacityGUI}
     \title{Start GUI for working memory capacity estimation.}
     \description{
       \code{WMCapacityGUI} starts the graphical user interface for building and
     estimating parameters of working memory models. This is the main
     interface for the \code{WMCapacity} package.
     }
     \usage{
     WMCapacityGUI(data=NULL,filename=NULL,setup=NULL,skipInstr=FALSE,name="Analysis",interface=TRUE,doOutput=TRUE, storePred=FALSE)
     }
     \arguments{
       \item{data}{a data frame containing the trial-by-trial data to be
     analyzed.}
       \item{filename}{the name of a CSV file containing the
     trial-by-trial data to be analyzed.}
       \item{setup}{object of class \code{WM2Package}, containing
	 all the necessary information necessary to start an analysis.}
       \item{skipInstr}{logical value determining whether initial
	 instruction screen is skipped.}
       \item{name}{analysis name, used for naming output files.}
       \item{interface}{logical value determining whether the MCMC and
	 output settings are queried via the interface (only used if
     setup is nonnull)}
       \item{doOutput}{logical value determining whether output options
	 are displayed.}
       \item{storePred}{logical value determining whether predicted
     probabilities are stored in the MCMC chains.}
     }
     \details{This function starts the GUI for estimating working memory
     capacity from change detection data, using the hierarchical
     Bayesian models described in Morey (in preparation).

     There are a number of ways of passing data to the function. If you
     have the trial-by-trial data already loaded in R, you can pass the
     data via the \code{data} argument. If it is in a file, you can load
     it by passing the path via the \code{filename} argument. If you
     have analyzed the data before and wish to repeat the analysis, you
     can pass the \code{WM2Package} object via the \code{setup}
     argument. If all these arguments are \code{NULL}, the GUI will
     display a file browser so you can select the file.

     For further details, click on the Help button in the GUI, or see
     the user's manual at \url{http://drsmorey.org/research/rdmorey/}.
   }

     \value{An object of class \code{WM2Package}.}
     \seealso{
       \code{\link{WMCapacityConsole}}, for the non-GUI interface
     (useful for simulations).
     }
     \examples{
     \dontrun{
     ## load Visual Array data set (Rouder et al., 2008)
     data(VisualArray)

     WMCapacityGUI(data=VisualArray)
     }
     }
     \keyword{nonlinear}
