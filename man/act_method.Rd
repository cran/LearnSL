% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perceptron.R
\name{act_method}
\alias{act_method}
\title{Activation Function}
\usage{
act_method(method, x)
}
\arguments{
\item{method}{Activation function to be used. It must be one of
\code{"step"}, \code{"sine"}, \code{"tangent"}, \code{"linear"}, \code{"relu"},
\code{"gelu"} or \code{"swish"}.}

\item{x}{Input value to be used in the activation function.}
}
\value{
List with the weights of the inputs.
}
\description{
Upon a received input, calculates the output based on the
selected activation function
}
\details{
Formulae used:

\describe{
\item{\emph{step}}{
\deqn{f(x) = \begin{cases}
     0 & \text{if } x < \text{threshold} \\
     1 & \text{if } x \geq \text{threshold}
   \end{cases}}}
\item{\emph{sine}}{\deqn{f(x) = \sinh(x)}}
\item{\emph{tangent}}{\deqn{f(x) = \tanh(x)}}
\item{\emph{linear}}{\deqn{x}}
\item{\emph{relu}}{
\deqn{f(x) = \begin{cases}
     x & \text{if } x > 0 \\
     0 & \text{if } x \leq 0
   \end{cases}}}
\item{\emph{gelu}}{\deqn{f(x) = \frac{1}{2} \cdot x \cdot \left(1 + \tanh\left(\sqrt{\frac{2}{\pi}} \cdot (x + 0.044715 \cdot x^3)\right)\right)}}
\item{\emph{swish}}{\deqn{f(x) = \frac{x}{1 + e^{-x}}}}
}
}
\examples{
# example code
act_method("step", 0.3)
act_method("gelu", 0.7)

}
\author{
Víctor Amador Padilla, \email{victor.amador@edu.uah.es}
}
