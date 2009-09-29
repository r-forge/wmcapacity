     \name{RPredVals}
     \Rdversion{1.1}
     \alias{RPredVals}
     \title{Compute predicted values for working memory models in the package WMCapacity}
     \description{
       The \code{RPredVals} function computes predicted response probabilities for a
     specified working memory model and design, given values of the effect
     parameters. This function is useful for model checking.} 
     
     \usage{
     RPredVals(x,setup)
     }
     

     \arguments{
       \item{x}{the vector containing values at which to evaluate the function.}
       \item{setup}{an object of class \code{WM2Package}, containing the model information.}
       }
     \details{
     The \code{RPredVals} function computes predicted probabilities for the
       model and design specified by the \code{setup} argument. The
       probability of responding ``change'' in a change detection task
       is predicted for each trial.

     For further details, see the user's manual at \url{http://drsmorey.org/research/rdmorey/}.
   }

     \value{A vector of predicted probabilities; each element
     corresponds to a trial.}
     \seealso{
       \code{\link{WMCapacityGUI}}, for fitting the working memory models.
     }
     \examples{
     \dontrun{
     ## load Visual Array data set (Rouder et al., 2008)
     data(VisualArray)

     output = WMCapacityGUI(data=VisualArray)
     
     # compute log-likelihood at posterior mean 
     RPredVals(output@output@par[,5],output)
     }
     
     }
     \keyword{nonlinear}
