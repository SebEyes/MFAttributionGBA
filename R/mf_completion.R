#' Complete a working database with MF taxonomy
#'
#' Matches the morphospecies (MF) codes in a working database to the MF
#' reference database via the synonym table and adds the corresponding
#' taxonomy columns. This function can be called directly from R scripts;
#' it does not require running the Shiny app.
#'
#' @param database A data frame containing the working data.
#' @param col_MF Name of the column in `database` containing MF codes.
#' @param MF_DB MF taxonomy reference data frame. It must include `MF`,
#'   `taxonomic_order`, and `class` columns (and compatible taxonomy fields,
#'   including `phylum` and `abpcode`).
#' @param MF_syn Synonym data frame with at least two columns. The first
#'   column contains the old MF codes and the second the corresponding codes
#'   in `MF_DB$MF`.
#' @param npi_code Code used for not-identified records. Defaults to `"NPI"`.
#'
#' @return A data frame with taxonomy columns added to the working database.
#' @export
MF_completion <- function(database, col_MF, MF_DB, MF_syn, npi_code = "NPI") {
  required_reference <- c("MF", "taxonomic_order", "class", "phylum", "abpcode")
  if (!is.data.frame(database)) stop("`database` must be a data frame.", call. = FALSE)
  if (length(col_MF) != 1L || !is.character(col_MF) || !col_MF %in% names(database)) {
    stop("`col_MF` must name a column in `database`.", call. = FALSE)
  }
  if (!is.data.frame(MF_DB) || !all(required_reference %in% names(MF_DB))) {
    stop("`MF_DB` must contain the columns: ", paste(required_reference, collapse = ", "), call. = FALSE)
  }
  if (!is.data.frame(MF_syn) || ncol(MF_syn) < 2L) {
    stop("`MF_syn` must be a data frame with at least two columns.", call. = FALSE)
  }
  if (anyDuplicated(names(database)) || anyDuplicated(names(MF_DB)) || anyDuplicated(names(MF_syn))) {
    stop("Input data frames must have unique column names.", call. = FALSE)
  }

  database[[col_MF]] <- trimws(as.character(database[[col_MF]]))
  MF_DB$MF <- trimws(as.character(MF_DB$MF))
  MF_syn[[1]] <- trimws(as.character(MF_syn[[1]]))
  MF_syn[[2]] <- trimws(as.character(MF_syn[[2]]))
  npi_code <- trimws(as.character(npi_code))

  name_to_remove <- names(dplyr::select(MF_DB, -taxonomic_order, -MF))
  database$ID <- row.names(database)
  database <- dplyr::rename(database, "MF" = dplyr::all_of(col_MF))

  name_to_remove <- name_to_remove[name_to_remove %in% names(database)]
  if (length(name_to_remove) > 0) {
    database <- dplyr::select(database, -dplyr::all_of(name_to_remove))
  }

  database_NPI <- dplyr::filter(database, MF == npi_code)
  database_identified <- dplyr::filter(database, MF != npi_code)
  if ("Order" %in% names(database_identified)) database_identified <- dplyr::select(database_identified, -Order)
  if ("taxonomic_order" %in% names(database_identified)) database_identified <- dplyr::select(database_identified, -taxonomic_order)

  MF_list <- unique(dplyr::select(database_identified, MF))
  MF_list <- merge(MF_list, MF_syn, by.x = names(MF_list), by.y = names(MF_syn)[1])
  synonym_target <- paste(names(MF_syn)[2], "y", sep = ".")
  MF_list <- merge(MF_list, MF_DB, by.x = synonym_target, by.y = "MF", all.x = TRUE)
  MF_list <- dplyr::select(MF_list, -dplyr::all_of(synonym_target))
  database_identified <- merge(database_identified, MF_list, by = "MF", all.x = TRUE)

  if (nrow(database_NPI) > 0) {
    if ("Order" %in% names(database_NPI)) database_NPI <- dplyr::rename(database_NPI, "taxonomic_order" = "Order")
    col_toAdd <- setdiff(names(database_identified), names(database_NPI))
    to_add <- as.data.frame(matrix(NA, nrow = 1, ncol = length(col_toAdd)))
    names(to_add) <- col_toAdd
    to_add$MF <- npi_code
    database_NPI <- merge(database_NPI, to_add, by = "MF", all.x = TRUE)
    database_NPI$taxonRank <- "taxonomic_order"
    database_NPI$kingdom <- "Animalia"
    database_NPI$phylum <- "Arthropoda"
  }

  output <- rbind(database_identified, database_NPI)
  class_order <- unique(dplyr::select(MF_DB, class, taxonomic_order))
  class_order <- unique(rbind(class_order, data.frame(class = "Malacostraca", taxonomic_order = "Isopoda")))
  output <- dplyr::select(output, -class)
  output <- merge(output, class_order, by = "taxonomic_order", all.x = TRUE)
  output <- output |>
    dplyr::relocate(class, .after = "phylum") |>
    dplyr::relocate(taxonomic_order, .after = "class") |>
    dplyr::relocate(MF, .before = "abpcode") |>
    dplyr::arrange(ID) |>
    dplyr::select(-ID)
  output
}
