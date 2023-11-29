#' load data from files with extension xls, xlsx, csv, txt and mdb
#' @param name the name of the file
#' @param path full path of the file
#' @param sheet_name use only for specifying the excel sheetname or the mdb table name
load_file <- function(name, path, sheet_name=NULL) {
  ext <- tools::file_ext(name)
  if (ext %in% c("db", "s3db")) con <- RSQLite::dbConnect(drv = RSQLite::SQLite(),dbname=path)
  dati <- base::switch(ext,
                       xls = readxl::read_excel(path = path,sheet = sheet_name),
                       xlsx = readxl::read_excel(path = path,sheet = sheet_name),
                       csv = readr::read_csv(file = path),
                       txt = utils::read.table(file = path, quote="\"", comment.char=""),
                       tsv = utils::read.delim(file = path),
                       mdb = Hmisc::mdb.get(file = path,tables = sheet_name),
                       RDS = base::readRDS(file = path),
                       db = RSQLite::dbReadTable(conn = con, name = sheet_name),
                       s3db = RSQLite::dbReadTable(conn = con,name = sheet_name),
                       shiny::validate("Invalid file; Please upload a .csv, .tsv, .txt, .xlsx, .xls, .mdb, .db, .s3db or .RDS file")
  )
  if (ext %in% c("db", "s3db")) RSQLite::dbDisconnect(con)
  return(dati)
}

#' extract the name of sheets (workbook) or tables (mdb) in a given file .xls or .mdb
#' @param name the file name
#' @param path the full path of the file 
sheet_list <- function(name, path){
  ext <- tools::file_ext(name)
  if (ext %in% c("db", "s3db")) con <- RSQLite::dbConnect(drv = RSQLite::SQLite(),dbname=path)
  list_tables <- base::switch(ext,
                              xls = readxl::excel_sheets(path = path),
                              xlsx = readxl::excel_sheets(path = path),
                              mdb = Hmisc::mdb.get(file = path,tables = TRUE),
                              db = RSQLite::dbListTables(conn = con),
                              s3db = RSQLite::dbListTables(conn = con, dbname=path),
                              shiny::validate("Invalid file; Please upload a .db, .s3db, .mdb, .xlsx or .xls file")
  )
  if (ext %in% c("db", "s3db")) RSQLite::dbDisconnect(con)
  return (list_tables)
}
