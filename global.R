library(RMySQL)

getConnection <- function() {
  
  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(RMySQL::MySQL(), host = "192.168.1.249",user = "root", password = "root",dbname='meituan_comment')
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(RMySQL::MySQL(), host = "192.168.1.249",user = "root", password = "root",dbname='meituan_comment')
  }
  
  return(.connection)
}

