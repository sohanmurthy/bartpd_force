#' A script for simplifying the process of connecting to a postgres server via R

#' Add the following environment variables to your .Renviron file, replacing with values for your postgres server:
#' PSQL_HOST='hostname'
#' PSQL_PORT='5432'
#' PSQL_USER='username'
#' PSQL_PASSWORD='pass'
#' PSQL_DBNAME='dbname'

library(RPostgres)
library(DBI)
library(dplyr)
library(bit64)

getPostgresData <- function(query){
  
  # Setup postgres connection using credentials stored in .Renviron file
    if(!exists("psql_conn")){
    psql_conn <<- dbConnect(RPostgres::Postgres(),  
                          host = Sys.getenv('PSQL_HOST'),
                          port = Sys.getenv('PSQL_PORT'),
                          user = Sys.getenv('PSQL_USER'),
                          password = Sys.getenv('PSQL_PASSWORD'),
                          dbname = Sys.getenv('PSQL_DBNAME')
                          )
    }
  
  # Get results from source and do some bit transforms if necessary
    results <- dbGetQuery(psql_conn, query) %>%
    mutate_if(bit64::is.integer64, as.integer)
  
  return(results)
}

