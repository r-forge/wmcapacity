     \name{WMCapacityConsole}
     \Rdversion{1.1}
     \alias{WMCapacityConsole}
     \title{Start GUI for working memory capacity estimation.}
     \description{
       \code{WMCapacityConsole} is the non-GUI interface for
       estimating parameters of working memory models. This function is
     useful for simulations.
     }
     \usage{
     WMCapacityConsole(data=NULL,filename=NULL,setup,name="Analysis")
     }
     \arguments{
       \item{data}{a data frame containing the trial-by-trial data to be
     analyzed.}
       \item{filename}{the name of a CSV file containing the
     trial-by-trial data to be analyzed.}
       \item{setup}{object of class \code{WM2Package}, containing
	 all the necessary information necessary to start an analysis.}
       \item{name}{analysis name, used for naming output files.}
     }
     \details{This function is the non-GUI interface for estimating working memory
     capacity from change detection data, using the hierarchical
     Bayesian models described in Morey (in preparation).

     There are a number of ways of passing data to the function. If you
     have the trial-by-trial data already loaded in R, you can pass the
     data via the \code{data} argument. If it is in a file, you can load
     it by passing the path via the \code{filename} argument. The
     \code{setup} argument is required; the passed object will contain
     the details of the model to be fitted.

     The best way to use this function is to fit the model to data using
     the GUI interface \code{\link{WMCapacityGUI}}, saving the returned
     object. Then, new data can be analyzed by passing the object via
     the \code{setup} argument. Note that for this to work, the new data
     must have the same column names as the old data.

     For further details, see the user's manual at \url{drsmorey.org/research/rdmorey/}.
   }

     \value{An object of class \code{WM2Package}.}
     \seealso{
       \code{\link{WMCapacityGUI}}, for the GUI interface.
     }
     
     \examples{
     \dontrun{
     ## load Visual Array data set (Rouder et al., 2008)
     data(VisualArray)

     WMoutput = WMCapacityGUI(data=VisualArray)
     ## this is redundant, but it shows how it works
     WMCapacityConsole(data=VisualArray, setup=WMoutput)
     }
     }
     \keyword{nonlinear}
