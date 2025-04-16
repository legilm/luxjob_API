library(DBI)
library(RPostgres)
library(glue)

reduce_quota <- function(token, schema = 'student_gilmar') {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PG_DB"),
    host = Sys.getenv("PG_HOST"),
    port = 5432,
    user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD")
  )
  
  query <- glue::glue_sql("
    UPDATE {`schema`}.api_users
    SET quota = quota - 1
    WHERE token = {token} AND quota > 0
    RETURNING quota
    ",
                          .con = con
  )
  
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  if (nrow(res) == 1) {
    return(res$quota)
  } else {
    return("Quota exhausted or invalid token")
  }
}
