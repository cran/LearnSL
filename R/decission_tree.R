#' @title Decision Tree
#'
#' @description This function creates a decision tree based of an example dataset,
#' calculating the best classifier possible in each step. Only creates perfect
#' divisions, this means, if the rule doesn't create a classified group, it is
#' not considered. It is specifically designed for categorical values. Continues values
#' are not recommended as they will be treated as categorical ones.
#'
#' @details If \code{data} is not perfectly classifiable, the code will not finish.
#'
#' @param data A data frame with already classified observations. Each column
#' represents a parameter of the value. Each row is a different observation.
#' The column names in the parameter "data" must not contain the
#' sequence of characters " or ".
#' As this is supposed to be a binary decision rules generator and not a binary
#' decision tree generator, no tree structures are used, except for the
#' information gain formulas.
#' @param classy Name of the column we want the data to be classified by.
#' the set of rules obtained will be calculated according to this.
#' @param m Maximum numbers of child nodes each node can have.
#' @param method The definition of Gain. It must be one of
#' \code{"Entropy"}, \code{"Gini"}or \code{"Error"}.
#' @param details Boolean value. If it is set to "TRUE" multiple clarifications
#' and explanations are printed along the code
#' @param waiting If TRUE while \code{details} = TRUE. The code will stop in each
#' "block" of code and wait for the user to press "enter" to continue.
#'
#' @return Structure of the tree. List with a list per tree level. Each of these
#' contains a list per level node, each of these contains a list with the node's
#' filtered data, the node's id, the father's node id, the height that node is at,
#'  the variable it filters by, the value that variable is filtered by and the information gain of the division
#'
#' @details Available information gain methods are:
#'
#' \describe{
#'  \item{\emph{Entropy}}{The formula to calculate the entropy
#'  works as follows:\eqn{p_{i} = -\sum{f_{i} p_{i} \cdot \log2 p_{i}}}}
#'  \item{\emph{Gini}}{The formula to calculate gini
#'  works as follows:\eqn{p_{i} = 1 -\sum{f_{i} p_{i}^{2}}}}
#'  \item{\emph{Error}}{The formula to calculate error
#'  works as follows:\eqn{p_{i} = 1 -\max{(f_{i} p_{i}})}}
#' }
#' Once the impurity is calculated, the information gain is calculated as follows:
#' \deqn{IG = I_{father} - \sum{\frac{count(sonvalues)}{count(fathervalues)} \cdot I_{son}}}
#'
#' @examples
#' # example code
#' decision_tree(db3, "VehicleType", 5, "entropy", details = TRUE, waiting = FALSE)
#' decision_tree(db2, "VehicleType", 4, "gini")
#'
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
decision_tree <- function (data, classy, m, method = "entropy", details = FALSE, waiting = TRUE){

  if(details){
    console.log("\nEXPLANATION")
    hline()
    hline()
    console.log("\nStep 0:")
    console.log("    - Set the dataframe as parent node. The original dataframe is set as node 0.")
    console.log("\nStep 1:")
    console.log("    - If data is perfectly classified, go to step 4.")
    console.log("    - If data is not classified, create all the possible combinations of values for each variable.")
    console.log("      Each combination stablishs the division of the son nodes, being \"m\"")
    console.log("      numbers of divisions performed.")
    console.log("Step 2:")
    console.log("    - Calculate the information gain for each combination.")
    console.log("      The \"method\" method is used to calculate the information gain.")
    console.log("Step 3:")
    console.log("    - Select the division that offers the most information gain for each variable.")
    console.log("    - Select the division that offers the most information gain among the best of each variable.")
    console.log("    - For each son of the division add the node to the tree and go to step 1 with the filtered dataset.")
    console.log("Step 4:")
    console.log("    - This branch is finished. The next one in preorder will be evaluated\n\n")
    console.log("Step 5:")
    console.log("    - Print results\n\n")
    hline()
    hline()
    console.log("\n IMPORTANT!!\n\n")
    console.log("    - The objective is to understand how decission trees work. The stopping condition is to have PERFECT LEAFES.")
    console.log("      If \"data\" is not perfectly classifiable, the code WILL NOT FINISH!!\n\n")
    console.log("    - It is important to understand that the code flow is recursive,")
    console.log("      meaning the tree is traversed in preorder (first, the root node is visited, then the children from left to right).")
    console.log("      So, when the information is categorized in step 1, this order will be followed. \n\n")
    hline()
    hline()
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
  }

  tree_strctr <- list(list(0))
  result <- aux_decision_tree(data,classy, m, method, tree_strctr, id = 0, id_f = 0, h = 0, details, waiting)
  result <- result[2:length(result)]
  result <- structure(result, class = "tree_struct")

  if(details){
    hline()
    console.log("\nStep 5:")
    console.log("This is the structure of the decission tree:")
    print(result)
  }
  return(result)
}


#' @title Print Tree Structure
#'
#' @description This function prints the structure of a tree, generated by the
#' \code{decision_tree} function.
#'
#' @details It must receive a \code{tree_struct} data type.
#'
#' @param x The tree structure.
#' @param ... Extra useless parameters.
#'
#' @return nothing.
#'
#' @examples
#' # example code
#' print(db_tree_struct)
#'
#' @author Víctor Amador Padilla, \email{victor.amador@@edu.uah.es}
#' @export
print.tree_struct <- function(x, ...){
  for (i in 1:length(x)){
    console.log(paste("Height", i ,"has", length(x[[i]]),"sons, divided by", x[[i]][[1]][[5]],":\n\n"))
    for (j in 1:length(x[[i]])){
      console.log(paste("Son", x[[i]][[j]][[2]] ,"(Whose father node is", x[[i]][[j]][[3]], ") filters by \"", x[[i]][[j]][[6]], "\". It contains:\n"))
      print(x[[i]][[j]][[1]])
      console.log("")
    }
    console.log("")
  }
}


aux_decision_tree <- function (data, classy, m, method, tree_strctr, id, id_f, h, details, waiting){
  if (length(unique(data[, classy])) < 2){
    if (details){
      console.log("\nSteps 1 and 4:")
      console.log("Data is classified.")
    }
    return (tree_strctr)
  }

  if (details){
    console.log("\nStep 0:")
    console.log("\nData:")
    print(data)
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
  }

  if (details){
    hline()
    console.log("\nSteps 1 and 2:")
  }
  candidates <- mapply(
    function (n, name){
      df <- all_combs(unique(n),m)
      df <- gain_method(df, data, classy, method, name)
      df$classifier <- name
      if (details){
        console.log(paste("Combinations for", name))
        print(df)
        console.log("")
      }
      m <- which(df$Gain == max(df$Gain))
      indice_fila_max_dashes <- which.min(sapply(df[m, ], function(column) sum(column == "---")))
      max_value <- df[m, ][indice_fila_max_dashes, ]
      columnas_con_dashes <- colnames(max_value)[apply(max_value == "---", 2, all)]
      max_value <- max_value[, !colnames(max_value) %in% columnas_con_dashes]
      candidato <- list(c(max_value[ 1 : (length(max_value) - 2) ]), as.numeric(max_value[length(max_value)-1]), as.character(max_value[length(max_value)]))
    },
    data[, !colnames(data) %in% classy],  #todas las columnas menos classy
    colnames(data)[!colnames(data) %in% classy], #los nombres de las columnas
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  if (details){
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
  }

  candidates <- t(data.frame(candidates))
  colnames(candidates) <- c("Sons", "Gain", "Classifier")
  max_gain <- list(candidates[which.max(candidates[, "Gain"]), ])

  if(details){
    hline()
    console.log("\nStep 3:")
    console.log("List of best candidates (1 for each variable):")
    print(candidates)
    console.log("\nThe division with the most information gain is chosen:")
    console.log(paste("    - Classifier =",max_gain[[1]][[3]][1]))
    console.log(paste("    - Information gain =",round(max_gain[[1]][[2]][1],3)))
    console.log("    - Sons =")
    print(unlist((max_gain[[1]][[1]])))
    if (waiting){
      invisible(readline(prompt = "Press [enter] to continue"))
      console.log("")
    }
  }

  h <- h + 1
  if (length(tree_strctr)-1 < h){
    tree_strctr <- append(tree_strctr, list(list()))
  }

  name <- max_gain[[1]][[3]]
  for (i in 1:length(max_gain[[1]][[1]])){
    df <- subset(data, get(name) %in% strsplit(as.character(max_gain[[1]][[1]][i]), " ")[[1]])
    rownames(df) <- NULL
    tree_strctr[[1]][[1]]<- tree_strctr[[1]][[1]]+1
    tree_strctr[[h+1]] <- append(tree_strctr[[h+1]], list(list(df, tree_strctr[[1]][[1]], id_f, h, max_gain[[1]][[3]],max_gain[[1]][[1]][[i]],  max_gain[[1]][[2]])))

    tree_strctr <- aux_decision_tree(df, classy, m, method, tree_strctr, tree_strctr[[1]][[1]], tree_strctr[[1]][[1]], h, details, waiting)
  }
  return (tree_strctr)
}

gain_method <- function(da, data, classy, method, name){
  if (is.character(da)){
    df <- as.data.frame(matrix(da, nrow = 1, ncol = 1))
    df$Gain <- 0
    return (df)
  }
  da$Gain <- apply(
    da,
    1,
    function(n, data, classy){
      n <- as.vector(n[n != "---"])
      switch (tolower(method),
              "entropy" = entropy(n, data, classy, name),
              "gini"    = gini(n, data, classy, name),
              "error"   = error(n, data, classy, name),
              stop("Unknown method")
      )
    },
    data = data,
    classy = classy,
    simplify = TRUE
  )
  return(da)
}

entropy <- function(n, data, classy, name){
  ent_hijos <- vector(mode="integer", length = length(n))
  entp <- 0
  valores <- table(data[,classy])
  for (i in 1:length(valores)){
    entp <- entp - (valores[i]/sum(valores)* log2(valores[i]/sum(valores)))
  }
  ganancia = entp[[1]]
  for (i in 1:length(n)){
    subset_data <- table(data[data[[name]] %in% unlist(strsplit(n[i], " ")), ][,classy])
    for (j in subset_data){
      ent_hijos[i] <- ent_hijos[i] - (j/sum(subset_data)* log2(j/sum(subset_data)))
    }
    ganancia = ganancia - ((sum(subset_data)/sum(valores)) * ent_hijos[i])
  }
  return(ganancia)
}

gini <- function(n, data, classy, name){
  gin_hijos <- rep(1, length(n))
  ginp <- 1
  valores <- table(data[,classy])
  for (i in 1:length(valores)){
    ginp <- ginp - (valores[i]/sum(valores) * (valores[i]/sum(valores)))
  }
  ganancia = ginp[[1]]
  for (i in 1:length(n)){
    subset_data <- table(data[data[[name]] %in% unlist(strsplit(n[i], " ")), ][,classy])
    for (j in subset_data){
      gin_hijos[i] <- gin_hijos[i] - ((j/sum(subset_data)) * (j/sum(subset_data)))
    }
    ganancia = ganancia - ((sum(subset_data)/sum(valores)) * gin_hijos[i])
  }
  return(ganancia)
}

error <- function(n, data, classy, name){
  err_hijos <- rep(1, length(n))
  errp <- 1
  valores <- table(data[,classy])
  for (i in 1:length(valores)){
    errp <- errp - (valores[i]/sum(valores) * (valores[i]/sum(valores)))
  }
  ganancia = errp[[1]]
  for (i in 1:length(n)){
    subset_data <- table(data[data[[name]] %in% unlist(strsplit(n[i], " ")), ][,classy])
    err_hijos[i] <- max(subset_data/sum(subset_data))
    ganancia = ganancia - ((sum(subset_data)/sum(valores)) * err_hijos[i])
  }
  return(ganancia)
}

comb <- function(v, vp, k, combinations){
  if (k > 1){
    if (length(vp) == 0){
      v <- c(v,"---")
      comb(v,vp,k-1, combinations)
    }
    else if(length(vp) == 1){
      v <- c(v, paste(vp, collapse = " "))
      comb(v,c(),k-1, combinations)
    }
    else {
      vec <- c(v, paste(vp, collapse = " "), rep(c("---"), each = k-1))
      combinations <- rbind (combinations, vec)
      combi <- vector("list", length(vp))
      l <- ifelse(length(vp)%%2 == 0, length(vp)/2, length(vp)/2 - 0.5)

      for (i in 1:l) {
        temp <- combn(vp, i, paste, collapse = " ")
        if (length(vp)%%2 == 0 && nchar(temp[1]) == l*2 - 1){
          half <- split(temp, f = ifelse(seq_along(temp) <= length(temp)/2, "first", "second"))
          temp <- half$first
        }
        combi[[i]] <- temp
      }
      df <- unlist(combi)
      for (i in 1:length(df)){
        n <- strsplit(df[i], " ")[[1]]
        new_data <- setdiff(vp, n)
        combinations <- comb(c(v,paste(n, collapse = " ")),new_data, k-1, combinations)
      }
      return (combinations)
    }
  }
  else if (k == 1){
    ifelse(length(vp) > 0, v <- c(v, paste(vp, collapse = " ")) , v <- c(v,"---"))
    combinations <- rbind(combinations, v)
    return(combinations)
  }
}

all_combs <- function(vp, k){
  v = c()
  if (k > length(vp)){
    k = length(vp)
  }
  combinations<- data.frame()
  combinations <- comb(v, vp, k, combinations)
  combinations <- apply(combinations, 1, sort, simplify = TRUE)
  combinations <- data.frame(t(combinations))
  combinations <- combinations[!duplicated(combinations), ]
  rownames(combinations) <- NULL

  return(combinations)
}
