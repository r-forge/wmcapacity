     \name{RlogLikelihood}
     \Rdversion{1.1}
     \alias{RlogLikelihood}
     \alias{RlogPosterior}
     \alias{RgradLogPosterior}
     \alias{RlogPosteriorWithCov}
     \alias{RgradLogPosteriorWithCov}
     \title{Compute various quantities relating to the working memory models in  the package WMCapacity}
     \description{
       These functions will compute the log-likelihood, log-posterior, and the gradient of the log-posterior for the multinomial tree models of working memory capacity in the package \code{WMCapacity}. These functions are not meant to be called by the user.} 
     
     \usage{
     RlogLikelihood(x,setup)
     
     RlogPosterior(x,setup)
    
     RgradLogPosterior(x,setup)
     
     RlogPosteriorWithCov(x,setup,precList,means)
         
     RgradLogPosteriorWithCov(x,setup,precList,means)
     }
     

     \arguments{
       \item{x}{the vector containing values at which to evaluate the function.}
       \item{setup}{an object of class \code{WM2Package}, containing the model information.}
       \item{precList}{a list of length equal to the number of covariance matrices modelled. Each element of the list must be a square matrix, representing a precision matrix of appropriate size. The sizes are determined by the model specification.}
       \item{means}{A matrix with rows corresponding to the precision matrices, and columns correpsonding to the parameters involved in the precision matrices. The number of rows will be the number of precision matrices modelled, and the number of columns will be equal to the number of columns in the largest precision matrix.}
       }
     \details{
     The \code{RlogLikelihood} function computes the log-likelihood of the model at the given values. This is used internally for computing the deviance information criterion (DIC) value.

     The \code{RlogPosterior} function computes the (negative) log-posterior of the model at the given values. This is used internally for obtaining starting values for MCMC.

     The \code{RgradLogPosterior} function computes the (negative) gradient of the log-posterior of the model at the given values. This is used internally for obtaining starting values for MCMC.

     The \code{RlogPosteriorWithCov} and \code{RgradLogPosteriorWithCov} compute the (negative) log-posterior and (negative) gradient, respectively. The difference between these two functions and the two above (without covariances) is that these two functions are designed for models with covariance matrices defined. The log-posterior and gradient will be conditioned on the given covariance matrices and means.

     For further details, see the user's manual at \url{http://drsmorey.org/research/rdmorey/}.
   }

     \value{For the likelihood and posterior functions, a single value. For the gradient functions, a vector of the same length as the parameter vector. Each element repesents the partial derivative at the given value.}
     \seealso{
       \code{\link{WMCapacityGUI}}, for fitting the working memory models.
     }
     \examples{
     \dontrun{
     ## load Visual Array data set (Rouder et al., 2008)
     data(VisualArray)

     output = WMCapacityGUI(data=VisualArray)
     
     # compute log-likelihood at posterior mean 
     RlogLikelihood(output@output@par[,5],output)
     }
     
     }
\keyword{nonlinear}
\keyword{internal}
