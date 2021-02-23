library(shiny)
library(tidyverse)
library(DT)
library(RMySQL)
library(config)
library(odbc)
library(RMySQL)
library(config)

source("modal_dialog.R")

config <- config::get(file = "datatable/config.yml")
con <- RMySQL::dbConnect(
  RMySQL::MySQL(),
  user = config$db_user,
  password = config$db_password,
  dbname = config$db_name,
  host = config$db_host
)

mtcars <- con %>%
  dplyr::tbl("mtcars_db") %>%
  dplyr::collect()

create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
    paste0(
      '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
      .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
      .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
    ))
}

x <- create_btns(mtcars$id)
mtcars <- mtcars %>%
  dplyr::bind_cols(tibble("Buttons" = x))

ui <- fluidPage(

  # div(style = "display: none;", icon("refresh")),
  div(
    class = "container",
    div(
      style = "margin-top: 50px;",
      shiny::actionButton(
        inputId = "add_car",
        label = "Add Row",
        icon = shiny::icon("plus"),
        class = "btn-success"
      )
    )
  ),
  div(
    class = "container",
    style = "margin-top: 50px;",
    DT::DTOutput(outputId = "dt_table", width = "100%")
  ),

  shiny::includeScript("script.js")
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    df = mtcars %>%
      dplyr::select(-id),
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = max(mtcars$id) + 1
  )

  output$dt_table <- DT::renderDT(
    {
      shiny::isolate(rv$df)
    },
    escape = F,
    rownames = FALSE,
    options = list(processing = FALSE)
  )

  proxy <- DT::dataTableProxy("dt_table")
  shiny::observe({
    DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
  })

  ### delete row
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) &
      stringr::str_detect(input$current_id,
        pattern = "delete"
      ))
    rv$dt_row <- which(stringr::str_detect(rv$df$Buttons,
      pattern = paste0("\\b", input$current_id, "\\b")
    ))

    sql_id <- rv$df[rv$dt_row, ][["Buttons"]] %>%
      stringr::str_extract_all(pattern = "delete_[0-9]+") %>%
      unlist() %>%
      readr::parse_number()

    query <- stringr::str_glue("DELETE FROM mtcars_db WHERE id = {sql_id}")
    DBI::dbSendQuery(
      con,
      query
    )

    rv$df <- rv$df[-rv$dt_row, ]
  })

  # when edit button is clicked, modal dialog shows current editable row filled out
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) &
      stringr::str_detect(input$current_id,
        pattern = "edit"
      ))
    rv$dt_row <- which(stringr::str_detect(rv$df$Buttons,
      pattern = paste0("\\b", input$current_id, "\\b")
    ))
    df <- rv$df[rv$dt_row, ]
    modal_dialog(
      car = df$Car, mpg = df$mpg, cyl = df$cyl, 
      hp = df$hp, disp = df$disp, drat = df$drat,
      wt = df$wt, qsec = df$qsec, vs = mtcars$vs, 
      am = mtcars$am, gear = df$gear, carb = df$carb,
      selected_am = df$am, selected_vs = df$vs, edit = TRUE
    )
    rv$add_or_edit <- NULL
  })

  # when final edit button is clicked, table will be changed
  shiny::observeEvent(input$final_edit, {
    shiny::req(!is.null(input$current_id) &
      stringr::str_detect(input$current_id, pattern = "edit") &
      is.null(rv$add_or_edit))

    rv$edited_row <- dplyr::tibble(
      Car = input$car_name,
      mpg = input$mpg,
      cyl = input$cyl,
      disp = input$disp,
      hp = input$hp,
      drat = input$drat,
      wt = input$wt,
      qsec = input$qsec,
      vs = input$vs,
      am = input$am,
      gear = input$gear,
      carb = input$carb,
      Buttons = rv$df$Buttons[rv$dt_row]
    )

    sql_row <- rv$edited_row %>%
      dplyr::select(-Buttons)

    id <- rv$df[rv$dt_row, ][["Buttons"]] %>%
      stringr::str_extract_all(pattern = "delete_[0-9]+") %>%
      unlist() %>%
      readr::parse_number()

    query <- paste0(
      "UPDATE mtcars_db SET ",
      paste0(names(sql_row), "=", "'", unlist(c(sql_row)), "'", collapse = ", "),
      stringr::str_glue("WHERE id = {id}")
    )
    DBI::dbSendQuery(
      con,
      query
    )

    rv$df[rv$dt_row, ] <- rv$edited_row
  })

  shiny::observeEvent(input$add_car, {
    modal_dialog(
      car = "", mpg = "", cyl = "", hp = "", disp = "", drat = "",
      wt = "", qsec = "", vs = mtcars$vs, am = mtcars$am, gear = "", carb = "",
      selected_am = NULL, selected_vs = NULL, edit = FALSE
    )
    rv$add_or_edit <- 1
  })

  shiny::observeEvent(input$final_edit, {
    shiny::req(rv$add_or_edit == 1)
    add_row <- dplyr::tibble(
      Car = input$car_name,
      mpg = input$mpg,
      cyl = input$cyl,
      disp = input$disp,
      hp = input$hp,
      drat = input$drat,
      wt = input$wt,
      qsec = input$qsec,
      vs = input$vs,
      am = input$am,
      gear = input$gear,
      carb = input$carb,
      Buttons = create_btns(rv$keep_track_id)
    )
    rv$df <- add_row %>%
      dplyr::bind_rows(rv$df)

    add_row <- add_row %>%
      dplyr::mutate(id = rv$keep_track_id) %>%
      dplyr::select(-Buttons)

    column_names <- paste0(c(names(add_row)), collapse = ", ") %>%
      paste0("(", ., ")")
    values <- paste0("'", unlist(c(add_row)), "'", collapse = ", ") %>%
      paste0("(", ., ")")
    query <- paste0(
      "INSERT INTO mtcars_db ",
      column_names,
      " VALUES ",
      values
    )
    DBI::dbSendQuery(
      con,
      query
    )

    rv$keep_track_id <- rv$keep_track_id + 1
  })


  ### remove edit modal when close button is clicked or submit button
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  shiny::observeEvent(input$final_edit, {
    shiny::removeModal()
  })
}

onStop(function() {
  dbDisconnect(con)
})

# Run the application
shinyApp(ui = ui, server = server)
