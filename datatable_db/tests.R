library(odbc)
library(RMySQL)
library(config)

# https://www.freesqldatabase.com/extension/?acc=0fP8oRO%2F7x6cvAxrj3NQGQ%3D%3D
config <- config::get(file = "datatable/config.yml")

con <- RMySQL::dbConnect(
  RMySQL::MySQL(), 
  user = config$db_user, 
  password = config$db_password,
  dbname = config$db_name, 
  host = config$db_host
)

DBI::dbGetQuery(con, "SELECT * FROM mtcars_db")

query <- "DELETE FROM mtcars_db WHERE id = 1"
DBI::dbSendQuery(
  con,
  query
)

column_names <- paste0(c(names(mtcars), "id"), collapse = ", ") %>% 
  paste0("(", ., ")")
values <- paste0("'", unlist(c(mtcars[1, ])), "'", collapse = ", ") %>% 
  paste0("(", ., ")")
query <- paste0(
  "INSERT INTO mtcars_db ",
  column_names,
  " VALUES ",
  values
)


DBI::dbSendQuery(
  con,
  query
)

query <- paste0("UPDATE mtcars_db SET ",
                paste0(names(mtcars), "=", "'", unlist(c(mtcars[1, ])), "'", collapse = ", "),
                "WHERE id=3"
)
DBI::dbSendQuery(
  con,
  query
)

UPDATE Customers
SET ContactName = 'Alfred Schmidt', City= 'Frankfurt'
WHERE CustomerID = 1;


odbc::dbListTables(con)
dbRemoveTable(con, "mtcars_db")
data("mtcars")
mtcars <- mtcars %>%
  tibble::rownames_to_column(var = "Car") %>%
  #dplyr::bind_cols(tibble("Buttons" = x)) %>%
  dplyr::mutate(vs = ifelse(vs == 0, "V-shaped", "Straight")) %>%
  dplyr::mutate(am = ifelse(am == 0, "automatic", "manual")) %>% 
  dplyr::mutate(id = dplyr::row_number())
dbWriteTable(con, name = "mtcars_db", value = mtcars, row.names = F)

mtcars <- con %>% 
  dplyr::tbl("mtcars_db") %>% 
  dplyr::collect()

mtcars <- mtcars %>%
  tibble::rownames_to_column(var = "Car") %>%
  #dplyr::bind_cols(tibble("Buttons" = x)) %>%
  dplyr::mutate(vs = ifelse(vs == 0, "V-shaped", "Straight")) %>%
  dplyr::mutate(am = ifelse(am == 0, "automatic", "manual")) %>% 
  dplyr::mutate(id = dplyr::row_number())
