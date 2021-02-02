modal_dialog <- function(car, mpg, cyl, hp,disp, drat, selected_vs,
                         wt, qsec, vs, am, gear, carb, selected_am, edit) {
  
  if(edit) {
    x <- "Submit Edits"
  } else {
    x <- "Add New Car"
  }
  
  shiny::modalDialog(
    title = "Edit Car",
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        shiny::textInput(inputId = "car_name",
                         label = "Car Type",
                         value = car,
                         placeholder = "Input Car Type", 
                         width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "mpg",
                            label = "Miles Per Gallon",
                            value = mpg, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "cyl",
                            label = "Cylinders",
                            value = cyl, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "hp",
                            label = "Horesepower",
                            value = hp, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "disp",
                            label = "Displacement",
                            value = disp, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "drat",
                            label = "Rear Axle Ratio",
                            value = drat, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "wt",
                            label = "Weight",
                            value = wt, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "qsec",
                            label = "1/4 Mile Time",
                            value = qsec, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(inputId = "vs",
                           label = "Engine", 
                           width = "200px",
                           selected = selected_vs,
                           choices = unique(vs))
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(inputId = "am",
                           label = "Transmission", 
                           width = "200px",
                           selected = selected_am,
                           choices = unique(am))
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "gear",
                            label = "Number of Forward Gears",
                            value = gear, 
                            width = "200px")
      ),
      div(
        style = "display: inline-block;",
        shiny::numericInput(inputId = "carb",
                            label = "Number of Carburetors",
                            value = carb, 
                            width = "200px")
      )
    ),
    size = 'm',
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(inputId = "final_edit",
                          label   = x,
                          icon = shiny::icon("edit"),
                          class = "btn-info"),
      shiny::actionButton(inputId = "dismiss_modal",
                          label   = "Close",
                          class   = "btn-danger")
    )
    
    
  ) %>% shiny::showModal()
  
}
