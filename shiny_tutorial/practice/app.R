library(shiny)

boot_cards <- function(title = NULL, body = NULL) {
  boot_cards <-
    div(
      class = "class = col-md-3 my-cards",
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          p(title)
        ),
        div(
          class = "panel-body",
          p(body)
        )
      )
    )
  
  return(boot_cards)

}

ui <- shiny::fluidPage(

  shiny::singleton(
    tags$head(tags$script(src = "test.js"))
  ),

  div(
    class = "row",
    div(
      shiny::sidebarPanel(
        id = "sidebar",
        shiny::textInput(inputId = "header_add", label = "Add Title", placeholder = "Title"),
        shiny::textInput(inputId = "body_add", label = "Add Body", placeholder = "Body"),
        shiny::actionButton(inputId = "remove", label = "Remove Card"),
        shiny::actionButton(inputId = "add", label = "Add Card")
      )
    ),
    div(id = "placeholder")
  ),
  
  shiny::includeScript("www/button_click.js")

)

cards <- function(item, button, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = 'add-remove-cards',
    message = list(
      card = as.character(item),
      add_remove = button
    )
  )
}

server <- function(input, output, session) {

  shiny::observeEvent(input$remove | input$add, {
    
    cards(
      item = boot_cards(
        input$header_add,
        input$body_add
      ), 
      button = input$button_clicked
    )
    
  })

}

shiny::shinyApp(ui, server)
