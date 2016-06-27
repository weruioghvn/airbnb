# Utiliy functions for Airbnb market analysis.
# It's composed of several components.
#   1. database constants

library(RPostgreSQL)


kDriver <- dbDriver("PostgreSQL")
kHost <- "airbnbrdb.cvuvrunlpgtc.us-west-2.rds.amazonaws.com"
kPassword <- "waterice1"
kUser <- "sean"
kDbName <- "airbnbdb"
kPort <- 5432

StringSubstitute <- function(s, ...) {
  # Replaces each occurance of {x} with the named argument in ...
  # So, for example,
  # StringSubstitute('{greeting} {thing}', thing = 'world', greeting = 'hello')
  # returns 'hello world'
  # Substitution happens in the order of ...
  # So, for example StringSubstitute('{a}', a = '{b}', b = 'c')
  # returns 'c'
  # but StringSubstitute('{a}', b = 'c', a = '{b}')
  # returns '{b}'
  # No fancy handling of escaping is done.
  # StringSubstitute('\\{greeting\\}', greeting = 'hello') returns
  # '\\{greeting\\}'

  result = s
  put = list(...)
  if (length(put) == 0) {
    return(result)
  }
  for (i in 1:length(put)) {
    before = paste0('\\{', names(put)[i], '\\}')
    after = put[i]
    result = gsub(before, after, result)
  }
  return(result)
}

GetData <- function(sql) {
    # A thin wrapper on RpostgreSQL in order to data from database

    con <- dbConnect(kDriver,
                     dbname = kDbName,
                     user = kUser,
                     password = kPassword,
                     port = kPort,
                     host = kHost)
    result <- fetch(dbSendQuery(con, sql), n = -1)  # might fail
    dbDisconnect(con)

    return(result)
}

GetDataWithScrapeId <- function(table.name, scrape.id) {
    # A wrapper on GetData except it saves your effort to write
    # the whole SQL query.
    
    sql <- StringSubstitute(
        "SELECT *
         FROM {table_name}
         WHERE scrape_id = {scrape_id};",
        table_name = table.name,
        scrape_id = scrape.id)

    return(GetData(sql))
}

