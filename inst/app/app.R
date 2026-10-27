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
    df <- tryCatch(read.csv(filepath, sep = sep, header = TRUE,
                            stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                   error = function(e) NULL)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- tryCatch(as.data.frame(readxl::read_excel(filepath, col_types = "text")),
                   error = function(e) NULL)
  } else {
    df <- NULL
  }
  df
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
      fileInput("wdb_file", "Upload working database (.csv / .xlsx)", accept = c(".csv", ".xlsx", ".xls")),
      radioButtons("wdb_sep", "CSV separator (ignored for xlsx)", c(";" = ";", "," = ","), inline = TRUE),
      uiOutput("wdb_col_select"),
      textInput("npi_code", "Code used for 'not identified' MF", value = "NPI"),
      hr(),
      h4("2. MF taxonomy reference database"),
      fileInput("mfdb_file", "Upload MF_completeInfo (.csv / .xlsx)", accept = c(".csv", ".xlsx", ".xls")),
      radioButtons("mfdb_sep", "CSV separator (ignored for xlsx)", c(";" = ";", "," = ","), inline = TRUE),
      hr(),
      h4("3. MF synonym database"),
      fileInput("syn_file", "Upload MF_synonym (.csv / .xlsx)", accept = c(".csv", ".xlsx", ".xls")),
      radioButtons("syn_sep", "CSV separator (ignored for xlsx)", c(";" = ";", "," = ","), inline = TRUE),
      hr(),
      uiOutput("status_indicator"),
      br(),
      actionButton("run", "Run attribution", class = "btn-primary"),
      br(), br(),
      downloadButton("download", "Download result (.xlsx)"),
      br(), br(),
      actionButton("exit_app", "Exit application", class = "btn-danger")
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Working DB preview", DTOutput("wdb_preview")),
        tabPanel("MF reference preview", DTOutput("mfdb_preview")),
        tabPanel("Synonym preview", DTOutput("syn_preview")),
        tabPanel("Result", DTOutput("result_preview")),
        tabPanel("Unmatched MF", br(), textOutput("unmatched_summary"), br(), DTOutput("unmatched_preview")),
        tabPanel("Log / Warnings", verbatimTextOutput("log"))
      )
    )
  )
)

#### ---------------------------------------------------------------------
#### SERVER
#### ---------------------------------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$exit_app, {
    stopApp()
  })

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
    req_cols <- c("MF", "taxonomic_order", "class", "phylum", "scientificName")
    missing <- setdiff(req_cols, names(df))
    validate(need(length(missing) == 0,
                   paste("MF reference file is missing required column(s):", paste(missing, collapse = ", "))))
    df
  })
  syn <- reactive({
    req(input$syn_file)
    df <- read_any(input$syn_file$datapath, input$syn_file$name, input$syn_sep)
    validate(need(!is.null(df), "Could not read MF synonym database. Check file format/separator."))
    validate(need(ncol(df) >= 2, "Synonym file must contain at least 2 columns (synonym MF, reference MF)."))
    df
  })
  output$wdb_col_select <- renderUI({
    req(wdb())
    selectInput("col_MF", "Column containing MF codes", choices = names(wdb()))
  })
  output$wdb_preview <- renderDT(head(wdb(), 50), options = list(scrollX = TRUE))
  output$mfdb_preview <- renderDT(head(mfdb(), 50), options = list(scrollX = TRUE))
  output$syn_preview <- renderDT(head(syn(), 50), options = list(scrollX = TRUE))
  files_ready <- reactive({
    !is.null(input$wdb_file) && !is.null(input$mfdb_file) && !is.null(input$syn_file) &&
      !is.null(input$col_MF) && nzchar(input$col_MF)
  })
  output$status_indicator <- renderUI({
    if (isTRUE(files_ready())) div(style = "color: green; font-weight: bold;", "\u2713 Ready to run")
    else div(style = "color: darkorange; font-weight: bold;", "\u26A0 Upload all 3 files and select the MF column")
  })
  result <- reactiveVal()
  observeEvent(input$run, {
    req(wdb(), mfdb(), syn(), input$col_MF)
    res <- tryCatch({
      MFAttributionGBA::MF_completion(database = wdb(), col_MF = input$col_MF,
                                      MF_DB = mfdb(), MF_syn = syn(), npi_code = input$npi_code)
    }, error = function(e) {
      showNotification(paste("Error during attribution:", e$message), type = "error", duration = NULL)
      NULL
    })
    if (!is.null(res)) {
      result(res)
      updateTabsetPanel(session, "main_tabs", selected = "Result")
      showNotification("Attribution completed successfully.", type = "message")
    }
  })
  output$result_preview <- renderDT({ req(result()); datatable(result(), options = list(scrollX = TRUE)) })
  unmatched <- reactive({
    req(result())
    tax_cols <- intersect(c("kingdom", "phylum", "class", "taxonomic_order"), names(result()))
    if (length(tax_cols) == 0) return(result()[0, ])
    result() %>% filter(if_any(all_of(tax_cols), is.na))
  })
  output$unmatched_summary <- renderText({
    req(result())
    n <- nrow(unmatched())
    if (n == 0) "All MF codes were successfully matched to a taxonomy."
    else paste0("\u26A0 ", n, " row(s) could not be fully matched to a taxonomy. Check spelling or add missing synonyms in the synonym database.")
  })
  output$unmatched_preview <- renderDT({
    req(result())
    datatable(unmatched() %>% select(any_of("MF")) %>% distinct() %>%
                bind_cols(unmatched() %>% select(-any_of("MF"))), options = list(scrollX = TRUE))
  })
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
  output$download <- downloadHandler(
    filename = function() paste0("MF_attributed_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(result())
      writexl::write_xlsx(result(), path = file)
    }
  )
}

shinyApp(ui, server)
