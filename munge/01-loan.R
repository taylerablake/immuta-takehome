conn <- dbConnect(odbc::odbc(), dsn="Immuta SQL Connection")

### Shows the list of exposed tables in Immuta
loan <- dbFetch(dbSendQuery(conn, "SELECT * FROM lending_club_tbl")) %>%
      as_tibble
cache("loan")

