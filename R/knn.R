#' @title K-Nearest Neighbors
#'
#' @description This function applies knn algorithm to classify data.
#'
#' @param data Data frame with already classified observations. Each
#' column represents a parameter of the values. The last column contains the
#' output, this means, the expected output when the other column values are
#' inputs. Each row is a different observation.
#' @param ClassLabel String containing the name of the column of the classes we want to classify
#' @param p1 Vector containing the parameters of the new value that we want to
#' classify.
#' @param d_method String with the name of the distance method that will
#' be used. It must be one of \code{"Euclidean"}, \code{"Manhattan"},
#' \code{"Cosine"}, \code{"Chebyshev"}, \code{"Minkowski"}, \code{"Canberra"},
#' \code{"Octile"}, \code{"Hamming"}, \code{"Binary"}or \code{"Jaccard"}. Where
#' both \code{"Hamming"} and \code{"Binary"} use the same method, as it is known
#' by both names.
#' @param k Number of closest values that will be considered in order to classify
#' the new value ("p1").
#' @param p Exponent used in the \code{Minkowski distance}. 3 by default,
#' otherwise if specified.
#' @param details Boolean value. If it is set to "TRUE" multiple clarifications
#' and explanations are printed along the code
#' @param waiting If TRUE while \code{details} = TRUE. The code will stop in each
#' "block" of code and wait for the user to press "enter" to continue.
#'
#' @return Value of the new classified example.
#'
#' @examples
#' # example code
#' knn(db_flowers,"ClassLabel", c(4.7, 1.2, 5.3, 2.1), "chebyshev", 4)
#' knn(db_flowers,"ClassLabel", c(4.7, 1.5, 5.3, 2.1), "chebyshev", 5)
#' knn(db_flowers,"ClassLabel", c(6.7, 1.5, 5.3, 2.1), "Euclidean", 2, details = TRUE, waiting = FALSE)
#' knn(db_per_or,"y", c(1,1,1), "Hamming", 3, details = TRUE, waiting = FALSE)
#'
#' @importFrom graphics pairs
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
knn <- function(data,ClassLabel ,p1, d_method = "euclidean", k, p = 3, details = FALSE, waiting = TRUE) {
  dist <- apply(
    data[, 1:(length(data) - 1)],
    1,
    distance_method,
    p1 = p1,
    d_method = d_method,
    p = p
  )

  neighbors <- sort(dist, index.return = TRUE)$ix[1:k] # k closest values position
  tags <- data[neighbors, length(data)] # class names in k values
  clas <- table(tags)

  my_string <- "New_Value"
  my_list <- list()
  my_list<- c(my_list, p1)
  my_list <- c(my_list, my_string)
  data <- rbind(data, my_list)

  # Extract the features (columns except the last one, which is the class)
  features <- data[, 1:(length(data) - 1)]
  num_dimensions <- ncol(features)

  prediction <- names(clas)[clas == max(clas)][1] #most repeated class

  if(details){
    console.log("\nEXPLANATION")
    hline()
    hline()
    console.log("\nStep 1:")
    console.log("    - Calculate the chosen d_method from the value we want to classify to every other one.")
    console.log("Step 2:")
    console.log("    - Select the k closest neighbors and get their classes.")
    console.log("Step 3:")
    console.log("    - Create a scatterplot matrix with the provided values for visualization purpose")
    console.log("Step 4:")
    console.log("    - Select the most repeated class among the k closest neighbors classes.\n")
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
    hline()
    hline()

    console.log("\nStep 1:")
    console.log("\nDistance from p1 to every other p.")
    print(dist)
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }

    hline()
    console.log("\nStep 2:")
    console.log("\nThese are the first k values classes:")
    print(tags)
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }

    # Create a scatterplot matrix with different colors for each class
    colors <- c("red", "blue", "green", "purple", "orange", "cyan", "magenta", "brown", "gray", "pink")
    class_colors <- colors[match(data[[ClassLabel]], unique(data[[ClassLabel]]))]

    pairs(features, col = class_colors)
    legend("topleft", legend = unique(data[[ClassLabel]]), fill = colors, cex = 0.7, xpd = TRUE, ncol = 1)

    hline()
    console.log("\nStep 3:")
    console.log("\nPlot values.")
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }

    hline()
    console.log("\nStep 4:")
    console.log(paste("\nThe most represented class among the k closes neighbors is", prediction))
    console.log("therefore, that is the new value's predicted class.")
  }
  return (prediction)
}

distance_method <- function(p1, p2, d_method = "euclidean", p = 3){
  switch (tolower(d_method),
          "euclidean" = euclidean_d(p1, p2),
          "manhattan" = manhattan_d(p1, p2),
          "chebyshev" = chebyshev_d(p1, p2),
          "minkowski" = minkowski_d(p1, p2, p),
          "canberra"  = canberra_d(p1, p2),
          "octile"    = octile_d(p1, p2),
          "hamming"   = hamming_d(p1, p2),
          "binary"    = hamming_d(p1, p2),
          "jaccard"   = jaccard_d(p1, p2),
          "cosine"    = cosine_d(p1, p2),
          stop("Unknown distance method")
  )
}

euclidean_d <- function(p1, p2 = p1){
  sqrt(sum((p1 - p2)^2))
}

manhattan_d <- function(p1, p2 = p1){
  sum(p1 - p2)
}

chebyshev_d <- function(p1, p2 = p1){
  max(abs(p1 - p2))
}

minkowski_d <- function(p1, p2 = p1, p = 3){
  (sum(abs(p1 - p2)^p)^(1/p))
}

canberra_d <- function(p1, p2 = p1){
  sum(abs(p1 - p2)/(abs(p1) + abs(p2)))
}

octile_d <- function(p1, p2 = p1){
  ((max(abs(p1 - p2))) + ((sqrt(2) - 1) * (min(abs(p1 - p2)))))
}

hamming_d <- function(p1, p2 = p1){
  sum(p1 != p2)
}

jaccard_d <- function(p1, p2 = p1){
  (1 - (length(intersect(p1, p2)) / length(union(p1, p2))))
}

cosine_d <- function(p1, p2 = p1){
  (sum(p1 * p2) / (sqrt(sum(p1^2)) * sqrt(sum(p2^2))))
}
