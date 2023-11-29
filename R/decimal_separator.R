#' check if the point exists in a numerical value wrongly formated as a string value
#' @param name  character string of numerical value
is_point_decimal_separator <- function(name){stringr::str_detect(name,"\\.")}

any_comma_decimal_separator <- function(name){any(stringr::str_detect(name,","),na.rm = TRUE)}

replace_decimal_separator <- function(data){
  if(any(apply(data,2,any_comma_decimal_separator))){
    cols <- apply(data,2,any_comma_decimal_separator) %>% which()
    if(!purrr::is_empty(cols)){
      for(col in cols){
        data[[col]] <- stringr::str_replace(data[[col]], ",",".")
      }
    }
  }
  return(data)
}

wrong_decimal_separator_data <- function(data){
  if(any(apply(data,2,any_comma_decimal_separator))){
    cols <- apply(data,2,any_comma_decimal_separator) %>% which()
    b <- base::data.frame()
    for(col in names(cols)){
      a <- data %>% dplyr::filter(stringr::str_detect(data[[col]],","))
      b <- b %>%  dplyr::bind_rows(a) 
    }
    return(b)
  }
}
