#' extract a dataset with only numeric variables from a given dataset
#' @param dataset a dataframe
numericDataset <- function(dataset){
  if(!is.null(dataset)){
    dataset %>% dplyr::select(sapply(X = dataset,FUN = is.numeric) %>% which())
  }
}

#' extract indexes of the numeric variables from a given dataset
#' @param dataset a dataframe
numericIndex <- function(dataset){
  if(!is.null(dataset)){
    sapply(X = dataset,FUN = is.numeric) %>% which()
  }
}

#' extract a dataset with only categorical variables from a given dataset
#' @param dataset a given dataframe
categoricDataset <- function(dataset){
  if(!is.null(dataset)){
    dataset %>% dplyr::select(sapply(X = dataset,FUN = is.factor) %>% which())
  }
}

#' extract position indexes of the categorical variables in a given dataset
#' @param dataset a given dataset
categoricIndex <- function(dataset){
  if(!is.null(dataset)){
    sapply(X = dataset,FUN = is.factor) %>% which()
  }
}

#' extract a dataset with only character variables in a given dataset
#' @param dataset a given dataset
characterDataset <- function(dataset){
  if(!is.null(dataset)){
    dataset %>% dplyr::select(sapply(X = dataset,FUN = is.character) %>% which())
  }
}

#' extract position indexes of the character variables in a given dataset
#' @param dataset a given dataset
characterIndex <- function(dataset){
  if(!is.null(dataset)){
    sapply(X = dataset,FUN = is.character) %>% which()
  }
}
