#' Give the value of the detection limit for a given mineral in input
#' @param col name of the mineral whose detection limit will be given in output
DL <- function(col){
  base::switch(col,
               Li  = 0.25,
               Na = 50,
               Mn = 20,
               Fe = 150,
               Cu = 30,
               Se = 2,
               Rb = 25,
               Sr = 25,
               Mo = 50,
               Ba = 5,
               Re = 0.07,
               Bi = 0.2,
               U  = 0.1
  )
}

#' Looking for the below Detection Limit sign "<" into a character vector or a dataframe
#' @param name dataframe name or variable name of character vector
#' @return a boolean vector or a dataframe of boolean values
is_bDL <- function(name){stringr::str_starts(string = name,pattern = "<")}

fill_bDL <- function(data){
  col_bDL <- c("Li", "Na", "Mn", "Fe", "Cu", "Se", "Rb", "Sr", "Mo", "Ba", "Re", "Bi", "U")
  
  n_mis_row <- apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 1,FUN = n_missing)
  na_bDL <- which(n_mis_row > 0 & n_mis_row < floor(length(col_bDL)/2))
  
  
  data <- dplyr::bind_cols(data %>% dplyr::select(-dplyr::all_of(col_bDL)),
                           apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 2,FUN = function(name){
                             if(anyNA(name[na_bDL])){
                               ifelse(is.na(name),"<DL",name)
                             }else{
                               name
                             }
                           }))
  data <- dplyr::bind_cols(data %>% dplyr::select(-dplyr::all_of(col_bDL)),
                           apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),
                                 MARGIN = 2,
                                 FUN = function(name){
                                   if(any(stringr::str_starts(string = name,pattern = "[<-]"))){
                                     ifelse(stringr::str_starts(string = name,pattern = "[<-]"),"<DL",name)
                                   }else{
                                     name
                                   }
                                 }
                           )
  )
  return(data)
}


#' Looking for any "<" Detection Limit sign in a character vector
#' @param name character vector
any_bDL <- function(name){any(stringr::str_starts(string = name,pattern = "<"),na.rm = TRUE)}
# apply(X = is_bDL.dataframe(data),MARGIN = 2,FUN = any)


#' Count the number of values below the limit of detection
#' @param name variable name of the character vector
#' @param name variable name of a character vector
n_bDL <- function(name){is_bDL(name) %>% sum(na.rm = TRUE)}
#n_bDL <- function(name){stringr::str_starts(string = name,pattern = "[<]") %>% sum(na.rm = TRUE)}

#' Give a randomly selected value between 0 and the Detection Limit value
#' @param name a character vector
#' @param LoD Limit Of Detection value
bDL <- function(name, LoD=0){
  set.seed(12345)
  ifelse(test = is_bDL(name),yes = stats::runif(n=n_bDL(name),min = 0,max = LoD), no = name)
}

#' fill <DL position in a dataset with a specific value given randomly between 0 and the LoD
#' @param data a dataframe
impute_bDL <- function(data){
  
  bDL_cols <- apply(X = data,MARGIN = 2,FUN = any_bDL) %>% which()
  
  if(!purrr::is_empty(bDL_cols)){
    for(bDL_col in names(bDL_cols)){
      data[[bDL_col]] <- base::switch(bDL_col,
                                      Li  = bDL(data[[bDL_col]], LoD = 0.25),
                                      Na = bDL(data[[bDL_col]], LoD = 50),
                                      Mn = bDL(data[[bDL_col]], LoD = 20),
                                      Fe = bDL(data[[bDL_col]], LoD = 150),
                                      Cu = bDL(data[[bDL_col]], LoD = 30),
                                      Se = bDL(data[[bDL_col]], LoD = 2),
                                      Rb = bDL(data[[bDL_col]], LoD = 25),
                                      Sr = bDL(data[[bDL_col]], LoD = 25),
                                      Mo = bDL(data[[bDL_col]], LoD = 50),
                                      Ba = bDL(data[[bDL_col]], LoD = 5),
                                      Re = bDL(data[[bDL_col]], LoD = 0.07),
                                      Bi = bDL(data[[bDL_col]], LoD = 0.2),
                                      U  = bDL(data[[bDL_col]], LoD = 0.1)
      )
    }
  }
  return(data)
}

#' Count the number of missing values in a vector
#' @param name variable name of the character vector
n_missing <- function(name){sum(is.na(name))}

not_NA <- function(name){sum(!is.na(name))}

#' Count the number of missing values and/or values below the detection limit in a vector
#' @param name variable name of the character vector
n_missing_bDL <- function(name){
  sum(stringr::str_starts(string = name,pattern = "[<]") | is.na(name))
}

#' Delete samples with high percentage of missing values. The threshold is given by more than 50% of the numeric predictors.
#' @param data dataframe
rm_na <- function(data){
  col_bDL <- c("Li", "Na", "Mn", "Fe", "Cu", "Se", "Rb", "Sr", "Mo", "Ba", "Re", "Bi", "U")
  if(any(data %>% dplyr::select(dplyr::all_of(col_bDL)) %>% is.na() %>% apply(MARGIN = 1,FUN = all))){
    na_bDL <- data %>% dplyr::select(dplyr::all_of(col_bDL)) %>% is.na() %>% apply(MARGIN = 1,FUN = all) %>% which()
    data <- data %>% dplyr::slice(-na_bDL)
  }
  
  col_iso <- c("d13C","d15N","d2H")
  if(any(data %>% dplyr::select(dplyr::all_of(col_iso)) %>% is.na() %>% apply(MARGIN = 1,FUN = all))){
    na_iso <- data %>% dplyr::select(dplyr::all_of(col_iso)) %>% is.na() %>% apply(MARGIN = 1,FUN = all) %>% which()
    data <- data %>% dplyr::slice(-na_iso)
  }
  
  n_mis_row <- apply(X = data %>% dplyr::select(dplyr::all_of(c(col_bDL,col_iso))),MARGIN = 1,FUN = n_missing)
  if(any(n_mis_row >= floor((length(col_bDL)+length(col_iso))/2))){
    data <- data %>% dplyr::filter(n_mis_row < floor((length(col_bDL)+length(col_iso))/2))
  }
  
  n_mis_column <- apply(X = numericDataset(data),MARGIN = 2,FUN = n_missing)
  if(any(n_mis_column >= floor(nrow(data)/2))){
    data <- data %>% dplyr::select(-dplyr::all_of(names(which(n_mis_column >= floor(nrow(data)/2)))))
  }
  
  return(data)
}

#' Delete samples with high percentage of varialbes below de Detection Limit. 
#' The threshold is given by more than 50% of the numeric predictors.
#' @param data dataframe
rm_bDL <- function(data){
  
  col_bDL <- c("Li", "Na", "Mn", "Fe", "Cu", "Se", "Rb", "Sr", "Mo", "Ba", "Re", "Bi", "U")
  
  n_dl_row <- apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 1,FUN = n_bDL)
  if(any(n_dl_row >= floor(length(col_bDL)/2))){
    data <- data %>% dplyr::filter(n_dl_row < floor(length(col_bDL)/2))
  }
  
  n_dl_column <- apply(X = data %>% dplyr::select(dplyr::all_of(col_bDL)),MARGIN = 2,FUN = n_bDL)
  if(any(n_dl_column >= floor(nrow(data)/2))){
    data <- data %>% dplyr::select(-dplyr::all_of(names(which(n_dl_column >= floor(nrow(data)/2)))))
  }
  
  return(data)
}
