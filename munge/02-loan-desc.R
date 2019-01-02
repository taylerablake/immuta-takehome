### Shows the list of exposed tables in Immuta
loan_desc <- dbFetch(dbSendQuery(conn, "SELECT * FROM lending_club_descriptions")) %>%
      as_tibble
cache("loan_desc")