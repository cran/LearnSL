#' @title Perceptron
#'
#' @description Binary classification algorithm that learns to separate
#'  two classes of data points by finding an optimal
#'  decision boundary (hyper plane) in the feature space.
#'
#' @param training_data Data frame with already classified observations. Each
#' column represents a parameter of the values. The last column contains the
#' output, this means, the expected output when the other column values are
#' inputs. Each row is a different observation. It works as training data.
#' @param to_clasify Vector containing the parameters of the new value that we want to
#' classify.
#' @param activation_method Activation function to be used. It must be one of
#' \code{"step"}, \code{"sine"}, \code{"tangent"}, \code{"linear"}, \code{"relu"},
#' \code{"gelu"} or \code{"swish"}.
#' @param max_iter Maximum epoch during the training phase.
#' @param learning_rate Value at which the perceptron will learn from previous epochs mistakes.
#' @param details Boolean value. If it is set to "TRUE" multiple clarifications
#' and explanations are printed along the code
#' @param waiting If TRUE while \code{details} = TRUE. The code will stop in each
#' "block" of code and wait for the user to press "enter" to continue.
#'
#' @return List with the weights of the inputs.
#'
#' @details Functioning:
#'
#' \describe{
#'  \item{\emph{Step 1}}{Generate a random weight for each independent variable.}
#'  \item{\emph{Step 2}}{Check if the weights classify correctly. If they do, go to step 4}
#'  \item{\emph{Step 3}}{Adjust weights based on the error between the expected output and the real output.
#'  If max_iter is reached go to step 4. If not, go to step 2.}
#'  \item{\emph{Step 4}}{Return the weights and use them to classify the new value}
#' }
#'
#' @examples
#' # example code
#' perceptron(db_per_or, c(1, 1, 1), "gelu", 1000, 0.1)
#' perceptron(db_per_and, c(0,0,1), "swish", 1000, 0.1, TRUE, FALSE)
#'
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
perceptron <- function(training_data, to_clasify, activation_method, max_iter, learning_rate, details = FALSE, waiting = TRUE){
  if(details){
    console.log("\nEXPLANATION")
    hline()
    hline()
    console.log("\nStep 1:")
    console.log("    - Generate a random weight for each variable.")
    console.log("Step 2:")
    console.log("    - Check if the weight classify correctly. If they do, go to step 4")
    console.log("Step 3:")
    console.log("    - Adjust weights based on the error between the expected output and the real output.")
    console.log("    - If max_iter is reached go to step 4. If not, go to step 2.")
    console.log("Step 4:")
    console.log("    - Return the weigths and use them to classigy the new value\n")
    hline()
    hline()
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
  }
  weigths <- per_training(training_data, activation_method, max_iter, learning_rate, details, waiting)
  clasificacion <- as.numeric(act_method(activation_method,sum(weigths * to_clasify)) > 0.5)
  if (details){
    hline()
    console.log("\nStep 4:\n")
    console.log(paste("Predicted value:", clasificacion, "\n"))
    console.log("Final weigths:")
    print(weigths)
  }
  return(weigths)
}

#' @importFrom stats runif
per_training <- function(training_data, activation_method, max_iter, learning_rate, details, waiting){
  env <- new.env()
  env$weigths <- runif(ncol(training_data)-1, min = -1, max = 1)
  if (details){
    console.log("\nStep 1:")
    console.log(paste("Random weights between -1 and 1 are generated for each variable:"))
    print(env$weigths)
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
    hline()
    console.log("\nSteps 2 and 3:")
  }
  env$is_correct <- FALSE
  sapply(
    1:max_iter,
    function(a){
      if (!env$is_correct){# If every element is classified, we are done
        env$is_correct <- TRUE
        # Verify if every value is correctly classified
        apply(
          training_data,
          1,
          function(b){
            if (env$is_correct){
              inputs <- b[1:length(b)-1]
              expected_output <- b[length(b)]
              output <- act_method(activation_method,sum(env$weigths * inputs))
              if (as.numeric(output > 0.5) != expected_output) {env$is_correct <- FALSE}
            }
          }
        )
        if (!env$is_correct){
          # select a random value from training_data
          row_num <- sample(1:nrow(training_data), 1)
          inputs <- training_data[row_num, 1:ncol(training_data)-1]
          expected_output <- training_data[row_num, ncol(training_data)]

          # calculate output and update weights
          output <- act_method(activation_method,sum(env$weigths * inputs))
          error <- expected_output - output
          env$weigths <- env$weigths + learning_rate * error * inputs
          if(details){
            console.log("Weights do not classify correctly so they get adjusted:")
            print(env$weigths)
            if(waiting){
              invisible(readline(prompt = "Press [enter] to continue"))
              console.log("")
            }
          }
        }
      }
    }
  )
  console.log("")
  return(env$weigths)
}

#' @title Activation Function
#'
#' @description Upon a received input, calculates the output based on the
#' selected activation function
#'
#' @param x Input value to be used in the activation function.
#' @param method Activation function to be used. It must be one of
#' \code{"step"}, \code{"sine"}, \code{"tangent"}, \code{"linear"}, \code{"relu"},
#' \code{"gelu"} or \code{"swish"}.
#'
#' @return List with the weights of the inputs.
#'
#' @details Formulae used:
#'
#' \describe{
#'  \item{\emph{step}}{
#'    \deqn{f(x) = \begin{cases}
#'      0 & \text{if } x < \text{threshold} \\
#'      1 & \text{if } x \geq \text{threshold}
#'    \end{cases}}}
#'  \item{\emph{sine}}{\deqn{f(x) = \sinh(x)}}
#'  \item{\emph{tangent}}{\deqn{f(x) = \tanh(x)}}
#'  \item{\emph{linear}}{\deqn{x}}
#'  \item{\emph{relu}}{
#'    \deqn{f(x) = \begin{cases}
#'      x & \text{if } x > 0 \\
#'      0 & \text{if } x \leq 0
#'    \end{cases}}}
#'  \item{\emph{gelu}}{\deqn{f(x) = \frac{1}{2} \cdot x \cdot \left(1 + \tanh\left(\sqrt{\frac{2}{\pi}} \cdot (x + 0.044715 \cdot x^3)\right)\right)}}
#'  \item{\emph{swish}}{\deqn{f(x) = \frac{x}{1 + e^{-x}}}}
#' }
#'
#' @examples
#' # example code
#' act_method("step", 0.3)
#' act_method("gelu", 0.7)
#'
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
act_method <- function(method, x){
  switch (tolower(method),
          "step"     = as.numeric(x > 0.5),
          "sine"     = (exp(x) - exp(-x)) / 2,
          "tangent"  = (exp(x) - exp(-x)) / (exp(x) + exp(-x)),
          "linear"   = x,
          "relu"     = pmax(x, 0),
          "gelu"     = 0.5 * x * (1 + tanh(sqrt(2 / pi) * (x + 0.044715 * x^3))),
          "swish"    = x / (1 + exp(-x)),
          stop("Unknown activation method")
  )
}
