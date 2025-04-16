library(DBI)
library(RPostgres)
library(glue)


verify_token <- function(token, schema = 'student_gilmar') {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PG_DB"),
    host = Sys.getenv("PG_HOST"),
    port = Sys.getenv(5432),
    user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD")
  )
  query <- glue::glue_sql("
    SELECT * FROM student_gilmar.api_users
    WHERE token = {token}
    ",
                          .con = con)

    res <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(!is.null(res) &&
           nrow(res) > 0 && 
           !is.null(res$token) && 
           res$token == token)
}
