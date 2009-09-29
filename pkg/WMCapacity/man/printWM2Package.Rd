     \name{printWM2Package}
     \Rdversion{1.1}
     \alias{printWM2Package}
     \alias{summary.WM2Package}
     \alias{plot.WM2Package}
      \title{Methods for WM2Package objects}
     \description{These functions provide show, summary, and plot
     methods for objects of class \code{WM2Package}, which are returned
     from WMCapacity analyses.
      } 
     
     \usage{
     printWM2Package(object)
     
     summary.WM2Package(object,...)
    
     plot.WM2Package(x,...)
     }
     

     \arguments{
       \item{object}{an object of class \code{WM2Package}}
        \item{x}{an object of class \code{WM2Package}}
       }
     \details{
     The function \code{printWM2Package} is the show method for
       WM2Package objects. It gives a little information about the
       analysis, including the DIC.

     The \code{summary} method gives more detailed output about the analysis.

     The \code{plot} method plots the chains of the effect parameters
     using the \code{plot.mcmc} method.

     }

     \value{Functions invisibly return the object.}
     \seealso{
       \code{\link{WMCapacityGUI}}, for fitting the working memory models.
     }
     \examples{
     \dontrun{
     ## load Visual Array data set (Rouder et al., 2008)
     data(VisualArray)

     output = WMCapacityGUI(data=VisualArray)
     
     # plot the chains from the analysis 
     plot(output)
     }
     
     }
\keyword{nonlinear}
\keyword{internal}
