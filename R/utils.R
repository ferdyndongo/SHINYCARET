#' Import a database table given a db connection, schema name and table name
#' @param src_name a database connection name
#' @param schema_name a name of schema in the database
#' @param table_name a name of the table to be imported
#' @return a database table
#' @importFrom rlang .data
dbtable <- function(src_name, schema_name, table_name){
  datatable <- dplyr::tbl(src =src_name, dbplyr::in_schema(schema=schema_name, table = table_name)) %>% dplyr::collect()
  if(.data$matricola %in% colnames(datatable)){
    datatable <- datatable %>% dplyr::mutate(prov=factor(stringr::str_sub(string = .data$matricola,start = 1,end = 2)))
  }
  return(datatable)
}

#' variable selection through recursive feature elimination based on random forest
#' @param data a dataset with response variable and quantitative regressors. 
#' If there are categorical variable they have to be transformed in dummy variables
#' prior to use rfe function for variable selection
#' @param catVar the response variable
rfeVar <- function(data, catVar){
  set.seed(1000)
  rfProfile <- caret::rfe(x = numericDataset(data), y=factor(data[[catVar]]), 
                          rfeControl=caret::rfeControl(functions = caret::rfFuncs, method = "cv"))
  return(data %>% dplyr::select(dplyr::all_of(c(caret::predictors(rfProfile), catVar))))
}

#' pre-processing steps used for regressors: zero-variance elimination, near zero-value elimination,
#' highly correlated variable elimination, centering, scaling and bagImpute imputation for missing values.
#' @param data a numerical dataset containing predictor variables.
preprocess <- function(data){
  preProc <- caret::preProcess(data, method=c("zv", "nzv", "corr" ,"center", "scale", "bagImpute"))
  return(stats::predict(preProc, data))
}


#' Local Outlier Factor identification
#' @param X A dataset 
#' @param minPts minimum number of points to be considered given by a knn
#' @return dataframe with local outlier factor scores
lofOut <- function(X, minPts){
  
  lofscores <- data.frame(scores=DDoutlier::LOF(dataset = X, k = minPts))
  return(lofscores)
}


#' write to database from a file excel
#' @param dsn the datasource name connected to database
#' @param filepath the file excel full path
#' @param schemaname the schema's name where the table is stored
#' @param tablename the table's name which will be updated. if not selected a new table will be created in the selected schema
#' @param overwrite logical value: when FALSE and append is FALSE with not selected table, new one is created
#' @param append logical value: if TRUE and overwrite is FALSE, the selected table will be updated
write_to_db <- function(dsn, filepath, schemaname, tablename,overwrite=FALSE,append=TRUE){
  db <- DBI::dbConnect(odbc::odbc(), dsn)
  if (is.character(filepath) && (basename(filepath) %>% stringr::str_ends(pattern = ".xls") |
                                 basename(filepath) %>% stringr::str_ends(pattern = ".xlsx"))){
    data <- readxl::read_excel(path = filepath)
  }else if(is.character(filepath) && basename(filepath) %>% stringr::str_ends(pattern = ".csv")){
    data <- readr::read_csv(file = filepath)
  }else{
    data <- filepath
  }
  destination <- paste(schemaname, tablename, sep = ".")
  odbc::dbWriteTable(conn = db, name =  DBI::SQL(destination), value = data,overwrite=overwrite,append=append)
  odbc::dbDisconnect(db)
}

#' Write to SQLite Database table through Data source Name (DSN)
#' @param dsn the data source name
#' @param data  data
#' @param tablename name for the created table
#' @param overwrite boolean value whether the table is overwritten
#' @param append boolean value wheter the table is updated if TRUE or created if FALSE
file_to_sqlite_dsn <- function(dsn, data, tablename, overwrite=FALSE, append=TRUE){
  
  if(DBI::dbCanConnect(drv = odbc::odbc(), dsn, timeout=10)){
    db <- DBI::dbConnect(odbc::odbc(), dsn)
    if(inherits(db, "SQLite") && !is.null(data)){
      odbc::dbWriteTable(conn = db, name =  tablename, value = data,overwrite=overwrite,append=append)
    }
    odbc::dbDisconnect(db)
  }
  
}


#' Write to SQLite Database table through local SQLite Database File
#' @param dbname database name
#' @param tbname name for the created table
#' @param data  data
#' @param overwrite boolean value whether the table is overwritten
#' @param append boolean value wheter the table is updated if TRUE or created if FALSE
write_to_sqlite_file <- function(dbname, tbname, data, overwrite=FALSE, append=TRUE){
  
  if(RSQLite::dbCanConnect(drv = RSQLite::SQLite(), dbname)){
    con <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname)
    if(inherits(con, "SQLiteConnection") && !is.null(data)){
      RSQLite::dbWriteTable(conn = con, name=tbname, value = data,overwrite=overwrite,append=append)
    }
    RSQLite::dbDisconnect(con)
  }
}

notify <- function(msg, id = NULL) {
  shiny::showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
}
