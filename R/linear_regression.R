#' @title Multivariate Linear Regression
#'
#' @description Calculates and plots the linear regression of a given set of values.
#' Being all of them independent values but one, which is the dependent value.
#' It provides information about the process and intermediate values used to calculate the line equation.
#'
#' @param data x*y data frame with already classified observations. Each column
#' represents a parameter of the values (independent variable). The last column
#' represents the classification value (dependent variable). Each row is a different observation.
#' @param details Boolean value. If it is set to "TRUE" multiple clarifications
#' and explanations are printed along the code
#' @param waiting If TRUE while \code{details} = TRUE. The code will stop in each
#' "block" of code and wait for the user to press "enter" to continue.
#'
#' @return List containing a list for each independent variable,
#'  each one contains, the variable name, the intercept and the slope.
#'
#' @examples
#' # example code
#' multivariate_linear_regression(db1rl)
#'
#' @importFrom graphics abline legend par points
#' @importFrom stats cov var runif
#' @importFrom utils combn
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
multivariate_linear_regression <- function(data, details = FALSE, waiting = TRUE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))


  num_columns <- ncol(data)

  if(details){
    console.log("\nEXPLANATION (for each independent variable)")
    hline()
    hline()
    console.log("\nStep 1:")
    console.log("    - Calculate mean of the dependent and independet variables.")
    console.log("    - Calculate covariance and the variance of the dependent variable.")
    console.log("      If covariance = 0, print error message.")
    console.log("Step 2:")
    console.log("    - Calculate the intercept and the slope of the equation.")
    console.log("Step 3:")
    console.log("    - Calculate the sum of squared residuals and the sum of squared deviations")
    console.log("      of the independent variable.")
    console.log("    - Calculate the coefficient of determination.")
    console.log("Step 4:")
    console.log("    - Plot the line equation\n")
    if (waiting) {
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
    hline()
    hline()

    par(mfrow = c(1, 1))
    console.log("\nAn empty plot is created with appropiate limits\n\n")
    plot(1, type = "n", xlim = range(data[, num_columns]),
         ylim = range(data[, 1:(num_columns - 1)]),
         main = "Multivariate Linear Regression",
         xlab = colnames(data)[num_columns],  # Use dependent variable as x-axis label
         ylab = "Variables")  # Use "Variables" as y-axis label

    if (waiting) {
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
  }

  # Initialize empty vectors for legends
  legend_labels <- character(num_columns - 1)
  legend_colors <- integer(num_columns - 1)
  variable_names <- character(num_columns - 1)

  reg_params <- list(num_columns - 1)

  dependent_var <- data[, num_columns]
  mean_y <- mean(dependent_var)
  if (details){
    console.log(paste("The mean of ",colnames(data)[num_columns]," is", mean_y,"\n\n"))
  }
  # Iterate through each column (except the last one) as the independent variable
  for (i in 1:(num_columns - 1)) {
    independent_var <- data[, i]

    mean_x <- mean(independent_var)
    covar <- cov(independent_var, dependent_var)
    var_x <- var(independent_var)

    if (details) {
      hline()
      console.log("\nStep 1:")
      console.log(paste(colnames(data)[i],":"))
      console.log(paste("     - Mean =", round(mean_x,3)))
      console.log(paste("     - Covariance =", round(covar,3)))
      console.log(paste("     - Variance =",round(var_x,3), "\n\n"))
      if (waiting) {
        invisible(readline(prompt = "Press [enter] to continue"))
        console.log("")
      }
    }

    if (covar != 0) {
      # Calculate the slope and intercept
      b <- covar / var(dependent_var)
      a <- mean_x - b * mean_y

      ssr <- sum((a + b * dependent_var - mean_x)^2)
      ssy <- sum((independent_var - mean_y)^2)
      rcua <- ssr / ssy

      if (details){
        hline()
        console.log("\nSteps 2 and 3")
        console.log(paste(colnames(data)[i],":"))
        console.log(paste("     - Intercept (a) =", round(a,3)))
        console.log(paste("     - Slope (b) =", round(b,3)))
        console.log(paste("     - Sum of squared residuals (ssr) =", round(ssr,3)))
        console.log(paste("     - Sum of squared deviations of y (ssy) =", round(ssy,3)))
        console.log(paste("They are used to calculate: Coefficient of determination (r^2) =", round(rcua,3), "\n\n"))
        console.log("")
        if (waiting){
          invisible(readline(prompt = "Press [enter] to continue"))
          console.log("")
        }

        # Plot the points and regression line for each column
        points(dependent_var, independent_var, pch = 16, cex = 1, col = i)
        abline(a, b, col = i, lty = i)

        # Store legend labels, colors, and variable names
        legend_labels[i] <- paste(" f(x) =", round(a, 3), "+", round(b, 3), "x")
        legend_colors[i] <- i
        variable_names[i] <- colnames(data)[i]

        legend_text <- paste(variable_names, ": ", legend_labels, sep = "")
        legend("topleft", legend = legend_text, col = legend_colors,
               pch = 1, lty = 1, bty = 'n', xjust = 1, cex = 0.8)

        hline()
        console.log("\nStep 4")
        console.log(paste(colnames(data)[i],":"))
        console.log(paste("Data is plotted and the equation is represented in the legend\n"))
        if (i != num_columns - 1){
          if (waiting) {
            invisible(readline(prompt = "Press [enter] to continue"))
            console.log("")
          }
        }
      }
      reg_params[[i]]<- list(var_name = colnames(data)[i], a = a ,b = b)
    } else {
      stop("Covariance = 0 for column ", i, " infinite slope, no line fits the given data.\n")
    }
  }
  return(reg_params)
}
