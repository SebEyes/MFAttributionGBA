#### ---------------------------------------------------------------------
#### Helper: MF_completion (adapted to take MF_DB / MF_syn / npi_code
#### as arguments instead of globals, and with hardened NPI row build)
#### ---------------------------------------------------------------------
MF_completion <- function(database, col_MF, MF_DB, MF_syn, npi_code = "NPI") {
  
  ## --- Normalize MF key columns across all 3 tables ---
  database[[col_MF]] <- trimws(as.character(database[[col_MF]]))
  MF_DB$MF            <- trimws(as.character(MF_DB$MF))
  MF_syn[[1]]          <- trimws(as.character(MF_syn[[1]]))
  MF_syn[[2]]          <- trimws(as.character(MF_syn[[2]]))
  npi_code             <- trimws(as.character(npi_code))
  
  name_to_remove <- names(MF_DB %>% select(-order, -MF))
  
  database$ID <- row.names(database)
  database <- rename(database, "MF" = all_of(col_MF))
  
  name_to_remove <- name_to_remove[name_to_remove %in% names(database)]
  if (length(name_to_remove) > 0) {
    database <- select(database, -all_of(name_to_remove))
  }
  
  database_NPI <- database %>% filter(MF == npi_code)
  database_identified <- database %>% filter(MF != npi_code)
  
  if (any(str_detect(names(database_identified), "^Order$"))) {
    database_identified <- database_identified %>% select(-Order)
  }
  if (any(str_detect(names(database_identified), "^order$"))) {
    database_identified <- database_identified %>% select(-order)
  }
  
  MF_list <- unique(select(database_identified, MF))
  
  MF_list <- merge(
    MF_list, MF_syn,
    by.x = names(MF_list), by.y = names(MF_syn)[1]
  )
  
  MF_list <- merge(
    MF_list, MF_DB,
    by.x = paste(names(MF_syn)[2], "y", sep = "."),
    by.y = "MF",
    all.x = TRUE
  ) %>% select(-paste(names(MF_syn)[2], "y", sep = "."))
  
  database_identified <- merge(
    database_identified, MF_list,
    by.x = "MF", by.y = "MF",
    all.x = TRUE
  )
  
  if (nrow(database_NPI) > 0) {
    
    if (any(str_detect(names(database_NPI), "^Order$"))) {
      database_NPI <- rename(database_NPI, "order" = "Order")
    }
    
    col_toAdd <- names(database_identified)[!(names(database_identified) %in% names(database_NPI))]
    
    to_add <- as.data.frame(matrix(NA, nrow = 1, ncol = length(col_toAdd)))
    names(to_add) <- col_toAdd
    to_add$MF <- npi_code
    
    database_NPI <- merge(
      database_NPI, to_add,
      by.x = "MF", by.y = "MF",
      all.x = TRUE
    )
    database_NPI$taxonRank <- "order"
    database_NPI$kingdom   <- "Animalia"
    database_NPI$phylum    <- "Arthropoda"
  }
  
  output <- rbind(database_identified, database_NPI)
  
  class_order <- MF_DB %>% select(class, order) %>% unique()
  class_order <- rbind(
    class_order,
    data.frame(class = c("Malacostraca"), order = c("Isopoda"))
  ) %>% unique()
  
  output <- output %>% select(-class)
  output <- merge(output, class_order, by = "order", all.x = TRUE, all.y = FALSE)
  
  output <- output %>%
    relocate(class, .after = "phylum") %>%
    relocate(order, .after = "class") %>%
    relocate(MF, .before = "abpcode") %>%
    arrange(ID) %>%
    select(-ID)
  
  return(output)
}
