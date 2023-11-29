#' Rename data variable in the grana dataset
#' @param data  grana dataset
rename_data <- function(data){
  if(!is.null(dim(data))){
    if(dim(data)[2]==20){
      data %>% dplyr::rename(anno=colnames(data[1]), numero=colnames(data[2]), matricola=colnames(data[3]),
                             class=colnames(data[4]), "d13C"=colnames(data[5]), "d15N"=colnames(data[6]), 
                             "d2H"=colnames(data[7]),"Li"=colnames(data[8]),  "Na"=colnames(data[9]), 
                             "Mn"=colnames(data[10]), "Fe"=colnames(data[11]), "Cu"=colnames(data[12]), 
                             "Se"=colnames(data[13]), "Rb"=colnames(data[14]), "Sr"=colnames(data[15]), 
                             "Mo"=colnames(data[16]), "Ba"=colnames(data[17]), "Re"=colnames(data[18]), 
                             "Bi"=colnames(data[19]), "U"=colnames(data[20]))
    }else if(dim(data)[2]==19){
      data %>% dplyr::rename(anno=colnames(data[1]), numero=colnames(data[2]), matricola=colnames(data[3]),
                             "d13C"=colnames(data[4]), "d15N"=colnames(data[5]), 
                             "d2H"=colnames(data[6]),"Li"=colnames(data[7]),  "Na"=colnames(data[8]), 
                             "Mn"=colnames(data[9]), "Fe"=colnames(data[10]), "Cu"=colnames(data[11]), 
                             "Se"=colnames(data[12]), "Rb"=colnames(data[13]), "Sr"=colnames(data[14]), 
                             "Mo"=colnames(data[15]), "Ba"=colnames(data[16]), "Re"=colnames(data[17]), 
                             "Bi"=colnames(data[18]), "U"=colnames(data[19]))
    }
  }
  
}

#' Convert data variables to the right data type
#' @param data a dataframe
convert_data_type <- function(data){
  
  for(col in colnames(data)){
    data[[col]] <- base::switch(col,
                                anno = as.character(data[[col]]),
                                numero = as.character(data[[col]]),
                                matricola = as.character(data[[col]]),
                                class = as.factor(data[[col]]),
                                d13C = as.numeric(data[[col]]),
                                d15N = as.numeric(data[[col]]),
                                d2H = as.numeric(data[[col]]),
                                Li  = as.numeric(data[[col]]),
                                Na = as.numeric(data[[col]]),
                                Mn = as.numeric(data[[col]]),
                                Fe = as.numeric(data[[col]]),
                                Cu = as.numeric(data[[col]]),
                                Se = as.numeric(data[[col]]),
                                Rb = as.numeric(data[[col]]),
                                Sr = as.numeric(data[[col]]),
                                Mo = as.numeric(data[[col]]),
                                Ba = as.numeric(data[[col]]),
                                Re = as.numeric(data[[col]]),
                                Bi = as.numeric(data[[col]]),
                                U  = as.numeric(data[[col]])
    )
  }
  return(data)
}


#' Compute some data preprocessing steps such as replace the under detection limit value wiht a random variable between 0 and the detecttion limit,
#' convert data variables and remove records with high number of missing values
#' @param data dataframe
#' @importFrom purrr %>% 
data_preprocessing <- function(data){
  data %>% rename_data() %>% rm_na() %>% fill_bDL()  %>% rm_bDL() %>% 
    impute_bDL() %>% replace_decimal_separator() %>% convert_data_type()
}
