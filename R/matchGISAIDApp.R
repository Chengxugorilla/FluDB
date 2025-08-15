#' interactive GISAID match and fill，return data.frame
#' @param strain_df   data.frame，must have Virus column
#' @param GISAID_df   data.frame，must have Isolate name、Isolate ID、Passage details/history columns
#' @return            modified strain_df（data.frame）
#' @importFrom magrittr %>%
#' @importFrom shiny fluidPage tags reactive observeEvent stopApp
#' @importFrom shiny actionButton column titlePanel br
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom dplyr filter %>%
#' @importFrom rlang .data
#' @export

matchGISAIDApp <- function(strain_df, GISAID_df) {
  stopifnot(
    "Virus" %in% names(strain_df),
    all(c("Isolate name", "Isolate ID", "Passage details/history") %in% names(GISAID_df))
  )

  ui <- fluidPage(
    tags$head(tags$style(HTML(".btn{margin-top:10px}"))),
    titlePanel("GISAID match and fill"),
    fluidRow(
      column(4,
             DTOutput("tbl_left"),
             br(),
             actionButton("save_btn", "save and close", class = "btn-success")),
      column(8,
             DTOutput("tbl_right"),
             actionButton("confirm_btn", "confirm and fill", class = "btn-primary"))
    )
  )

  server <- function(input, output, session) {

    left_tbl <- reactiveVal(strain_df)
    sel_left <- reactiveVal(NULL)

    output$tbl_left <- renderDT({
      datatable(left_tbl(),
                selection = list(mode = "single", selected = sel_left()),
                filter = "top",
                rownames = FALSE)
    })

    observeEvent(input$tbl_left_rows_selected, {
      sel_left(input$tbl_left_rows_selected)
    })

    right_tbl <- reactive({
      req(sel_left())
      key <- left_tbl()[sel_left(), "Virus"]           # 原样取出
      GISAID_df %>%
        filter(str_to_upper(.data$`Isolate name`) == str_to_upper(key))
    })

    output$tbl_right <- renderDT({
      datatable(right_tbl(),
                selection = "single",
                rownames = FALSE,
                escape = FALSE)
    })

    observeEvent(input$confirm_btn, {
      req(sel_left(), input$tbl_right_rows_selected)
      new_left <- left_tbl()
      new_left[sel_left(), "Isolate_ID"] <- right_tbl()[input$tbl_right_rows_selected, "Isolate ID"]
      new_left[sel_left(), "ID_Passs"]   <- right_tbl()[input$tbl_right_rows_selected, "Passage details/history"]
      left_tbl(new_left)
      sel_left(sel_left())
    })

    observeEvent(input$save_btn, {
      stopApp(left_tbl())
    })
  }

  runApp(shinyApp(ui, server), launch.browser = TRUE)
}
