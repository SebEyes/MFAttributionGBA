#### ---------------------------------------------------------------------
#### Helper: generic file reader (csv or xlsx)
#### ---------------------------------------------------------------------
read_any <- function(filepath, filename, sep = ";") {
  ext <- tolower(file_ext(filename))
  if (ext == "csv") {
    df <- tryCatch(
      read.csv(filepath, sep = sep, header = TRUE,
               stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
      error = function(e) NULL
    )
  } else if (ext %in% c("xlsx", "xls")) {
    df <- tryCatch(as.data.frame(readxl::read_excel(filepath)),
                   error = function(e) NULL)
  } else {
    df <- NULL
  }
  
  # if (!is.null(df)) {
  #   # Force every character/factor column to clean trimmed character
  #   df[] <- lapply(df, function(col) {
  #     if (is.factor(col)) col <- as.character(col)
  #     if (is.character(col)) col <- trimws(col)
  #     col
  #   })
  # }
  df
}