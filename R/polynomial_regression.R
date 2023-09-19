#' @title Multivariate Polynomial Regression
#'
#' @description Calculates and plots the polynomial regression of a given set of values.
#' Being all of them independent values but one, which is the dependent value.
#' It provides (if asked) information about the process and intermediate values used to calculate the line equation.
#' The approximation depends entirely in the \code{degree} of the equations.
#'
#' @param data x*y data frame with already classified observations. Each column
#' represents a parameter of the values (independent variable). The last column
#' represents the classification value (dependent variable). Each row is a different observation.
#' @param degree Degree of the equations approximation.
#' @param details Boolean value. If it is set to "TRUE" multiple clarifications
#' and explanations are printed along the code
#' @param waiting If TRUE while \code{details} = TRUE. The code will stop in each
#' "block" of code and wait for the user to press "enter" to continue.
#'
#' @return List containing a list for each independent variable,
#'  each one contains the equation coefficients.
#'
#' @examples
#' # example code
#' polynomial_regression(db1rl,4, TRUE, FALSE)
#' polynomial_regression(db1rl,6)
#'
#' @importFrom stats coef lm predict
#' @importFrom graphics lines
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
polynomial_regression <- function(data, degree, details = FALSE, waiting = TRUE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  num_columns <- ncol(data)

  if (details) {
    console.log("\nEXPLANATION (for each independent variable)")
    hline()
    hline()
    console.log("\nStep 1:")
    console.log("    - Create an empty plot with the appropiate limits.")
    console.log("Step 2:")
    console.log("    - Aproximate an equation line that approximates the given values.")
    console.log("      using the lm() function. It employs the least squared error method.")
    console.log("Step 3:")
    console.log("    - Plot the line and the legend.")
    if (waiting) {
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
    hline()
    hline()
    par(mfrow = c(1, 1))

    plot(1, type = "n", xlim = range(data[, num_columns]),
      ylim = range(data[, 1:(num_columns - 1)]),
      main = "Polynomial Regression",
      xlab = colnames(data)[num_columns],
      ylab = "Variables")
    console.log("\nStep 1:")
    console.log("\nAn empty plot is created with appropiate limits\n\n")
    if (waiting) {
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
    console.log("The aproximations of the following equations to the provided values")
    console.log("are done adjusting the coefficients of the line to make it the best-fit possible.\n\n")
  }

  # Initialize empty vectors for legends
  legend_labels <- character((num_columns - 1))
  legend_colors <- integer((num_columns - 1))

  # Iterate through each column (except the last one) as the independent variable
  coefs_list <- list(num_columns - 1)
  for (i in 1:(num_columns - 1)) {
    independent_var <- data[, i]
    dependent_var <- data[, num_columns]

    # Fit polynomial regression
    poly_fit <- lm(independent_var ~ poly(dependent_var, degree, raw = TRUE))

    # Generate points for the regression line
    y_range <- range(dependent_var)
    y_pred <- seq(y_range[1], y_range[2], length.out = 100)
    x_pred <- predict(poly_fit, newdata = data.frame(dependent_var = y_pred))

    # Extract coefficients of the polynomial
    poly_coefs <- coef(poly_fit)

    # Create legend labels with the full equation
    equation <- paste(
      "f(x) =", round(poly_coefs[1], 3),
      ifelse(poly_coefs[2] >= 0, "+", "-"), abs(round(poly_coefs[2], 3)), "x")

    for (d in 3:(degree + 1)) {
      equation <- paste(equation, ifelse(poly_coefs[d] >= 0, "+", "-"), abs(round(poly_coefs[d], 3)), "x^", (d - 1), sep = "")
    }
    coefs <- list(degree + 1)
    coefs[1] <- colnames(data)[i]
    for (d in 1:(degree + 1)){
      coefs[d+1] <- poly_coefs[d]
    }
    coefs_list[[i]] <- coefs
    legend_labels[i] <- paste(colnames(data)[i], ":", equation)
    legend_colors[i] <- i

    if (details) {
      points(dependent_var, independent_var, pch = 16, cex = 1, col = i)
      lines(y_pred, x_pred, col = i, lty = i)

      # Create the legend
      legend("topleft", legend = legend_labels, col = legend_colors,
             pch = 1, lty = 1, bty = 'n', xjust = 1, cex = 0.8)
      hline()
      console.log("Steps 2 and 3:")
      console.log(paste("Equation ( degree",degree,") aproximation for"
                      ,colnames(data)[i],"--> ",equation,"\n\n"))
      if (i != num_columns - 1){
        if (waiting){
          invisible(readline(prompt = "Press [enter] to continue"))
          console.log("")
        }
      }
    }
  }
  return (coefs_list)
}
