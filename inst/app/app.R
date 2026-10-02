#############################################
#### MF ATTRIBUTION AND COMPLETION - SHINY ####
#############################################

library(shiny)
library(shinyFeedback)
library(DT)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tools)

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

#### ---------------------------------------------------------------------
#### UI
#### ---------------------------------------------------------------------
ui <- fluidPage(
  
  useShinyFeedback(),
  titlePanel("MF Attribution & Taxonomy Completion"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h4("1. Working database"),
      fileInput("wdb_file", "Upload working database (.csv / .xlsx)",
                accept = c(".csv", ".xlsx", ".xls")),
      radioButtons("wdb_sep", "CSV separator (ignored for xlsx)",
                   c(";" = ";", "," = ","), inline = TRUE),
      uiOutput("wdb_col_select"),
      textInput("npi_code", "Code used for 'not identified' MF", value = "NPI"),
      
      hr(),
      h4("2. MF taxonomy reference database"),
      fileInput("mfdb_file", "Upload MF_completeInfo (.csv / .xlsx)",
                accept = c(".csv", ".xlsx", ".xls")),
      radioButtons("mfdb_sep", "CSV separator (ignored for xlsx)",
                   c(";" = ";", "," = ","), inline = TRUE),
      
      hr(),
      h4("3. MF synonym database"),
      fileInput("syn_file", "Upload MF_synonym (.csv / .xlsx)",
                accept = c(".csv", ".xlsx", ".xls")),
      radioButtons("syn_sep", "CSV separator (ignored for xlsx)",
                   c(";" = ";", "," = ","), inline = TRUE),
      
      hr(),
      uiOutput("status_indicator"),
      br(),
      actionButton("run", "Run attribution", class = "btn-primary"),
      br(), br(),
      downloadButton("download", "Download result (.xlsx)")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Working DB preview", DTOutput("wdb_preview")),
        tabPanel("MF reference preview", DTOutput("mfdb_preview")),
        tabPanel("Synonym preview", DTOutput("syn_preview")),
        tabPanel("Result", DTOutput("result_preview")),
        tabPanel(
          "Unmatched MF",
          br(),
          textOutput("unmatched_summary"),
          br(),
          DTOutput("unmatched_preview")
        ),
        tabPanel("Log / Warnings", verbatimTextOutput("log"))
      )
    )
  )
)

#### ---------------------------------------------------------------------
#### SERVER
#### ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## -----------------------------------------------------------
  ## Reactive file reads (with validation / feedback)
  ## -----------------------------------------------------------
  wdb <- reactive({
    req(input$wdb_file)
    df <- read_any(input$wdb_file$datapath, input$wdb_file$name, input$wdb_sep)
    validate(need(!is.null(df), "Could not read working database. Check file format/separator."))
    df
  })
  
  mfdb <- reactive({
    req(input$mfdb_file)
    df <- read_any(input$mfdb_file$datapath, input$mfdb_file$name, input$mfdb_sep)
    validate(need(!is.null(df), "Could not read MF reference database. Check file format/separator."))
    
    req_cols <- c("MF", "order", "class", "phylum", "abpcode")
    missing <- setdiff(req_cols, names(df))
    validate(need(
      length(missing) == 0,
      paste("MF reference file is missing required column(s):", paste(missing, collapse = ", "))
    ))
    df
  })
  
  syn <- reactive({
    req(input$syn_file)
    df <- read_any(input$syn_file$datapath, input$syn_file$name, input$syn_sep)
    validate(need(!is.null(df), "Could not read MF synonym database. Check file format/separator."))
    validate(need(ncol(df) >= 2, "Synonym file must contain at least 2 columns (synonym MF, reference MF)."))
    df
  })
  
  ## -----------------------------------------------------------
  ## Dynamic column picker for working DB
  ## -----------------------------------------------------------
  output$wdb_col_select <- renderUI({
    req(wdb())
    selectInput("col_MF", "Column containing MF codes", choices = names(wdb()))
  })
  
  ## -----------------------------------------------------------
  ## Previews
  ## -----------------------------------------------------------
  output$wdb_preview  <- renderDT(head(wdb(), 50),  options = list(scrollX = TRUE))
  output$mfdb_preview <- renderDT(head(mfdb(), 50), options = list(scrollX = TRUE))
  output$syn_preview  <- renderDT(head(syn(), 50),  options = list(scrollX = TRUE))
  
  ## -----------------------------------------------------------
  ## Status indicator: ready to run?
  ## -----------------------------------------------------------
  files_ready <- reactive({
    !is.null(input$wdb_file) && !is.null(input$mfdb_file) &&
      !is.null(input$syn_file) && !is.null(input$col_MF) && nzchar(input$col_MF)
  })
  
  output$status_indicator <- renderUI({
    if (isTRUE(files_ready())) {
      div(style = "color: green; font-weight: bold;", "\u2713 Ready to run")
    } else {
      div(style = "color: darkorange; font-weight: bold;",
          "\u26A0 Upload all 3 files and select the MF column")
    }
  })
  
  ## -----------------------------------------------------------
  ## Run computation
  ## -----------------------------------------------------------
  result <- eventReactive(input$run, {
    req(wdb(), mfdb(), syn(), input$col_MF)
    
    res <- tryCatch({
      MF_completion(
        database = wdb(),
        col_MF   = input$col_MF,
        MF_DB    = mfdb(),
        MF_syn   = syn(),
        npi_code = input$npi_code
      )
    }, error = function(e) {
      showNotification(paste("Error during attribution:", e$message),
                       type = "error", duration = NULL)
      NULL
    })
    
    if (!is.null(res)) {
      showNotification("Attribution completed successfully.", type = "message")
      updateTabsetPanel(session, "main_tabs", selected = "Result")
    }
    
    res
  })
  
  ## -----------------------------------------------------------
  ## Result preview
  ## -----------------------------------------------------------
  output$result_preview <- renderDT({
    req(result())
    datatable(result(), options = list(scrollX = TRUE))
  })
  
  ## -----------------------------------------------------------
  ## Unmatched MF tab (taxonomy = NA after merge)
  ## -----------------------------------------------------------
  unmatched <- reactive({
    req(result())
    tax_cols <- intersect(c("kingdom", "phylum", "class", "order"), names(result()))
    if (length(tax_cols) == 0) return(result()[0, ])
    result() %>% filter(if_any(all_of(tax_cols), is.na))
  })
  
  output$unmatched_summary <- renderText({
    req(result())
    n <- nrow(unmatched())
    if (n == 0) {
      "All MF codes were successfully matched to a taxonomy."
    } else {
      paste0("\u26A0 ", n, " row(s) could not be fully matched to a taxonomy. ",
             "Check spelling or add missing synonyms in the synonym database.")
    }
  })
  
  output$unmatched_preview <- renderDT({
    req(result())
    datatable(
      unmatched() %>% select(any_of(c("MF"))) %>% distinct() %>%
        bind_cols(unmatched() %>% select(-any_of("MF"))) ,
      options = list(scrollX = TRUE)
    )
  })
  
  ## -----------------------------------------------------------
  ## Log
  ## -----------------------------------------------------------
  output$log <- renderPrint({
    req(result())
    cat("Rows in (working database):", nrow(wdb()), "\n")
    cat("Rows out (result):         ", nrow(result()), "\n")
    cat("Unmatched MF rows:         ", nrow(unmatched()), "\n")
    if (nrow(unmatched()) > 0) {
      cat("\nUnmatched MF codes:\n")
      print(unique(unmatched()$MF))
    }
  })
  
  ## -----------------------------------------------------------
  ## Download handler
  ## -----------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() paste0("MF_attributed_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(result())
      writexl::write_xlsx(result(), path = file)
    }
  )
}

#### ---------------------------------------------------------------------
#### RUN APP
#### ---------------------------------------------------------------------
shinyApp(ui, server)