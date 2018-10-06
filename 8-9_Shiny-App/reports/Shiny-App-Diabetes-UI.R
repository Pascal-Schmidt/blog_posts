library(shiny)
library(ggplot2)
library(rsconnect)
library(here)

diabetes <- read.csv(here::here("docs", "diabetes.csv"))



shinyUI(fluidPage(


  # Create a header
  titlePanel(title = "Checking the Model Assumptions for LDA and QDA for the Diabetes Pima Indians Data Set"),

  sidebarLayout(
    sidebarPanel(


      # dropdown selector
      selectInput("variablex", "Ellipses X - Axis", names(diabetes)[1:8], multiple = FALSE, selected = "Glucose"),

      selectInput("variabley", "Ellipses Y - Axis", names(diabetes)[1:8], multiple = FALSE, selected = "Insulin"),

      selectInput("variableyy", "Boxplot Y - Axis", names(diabetes)[1:8], multiple = FALSE, selected = "Insulin"),

      selectInput("normalq", "Q-Q Plot Normality", names(diabetes)[1:8], multiple = FALSE, selected = "Pregnancies"),

      selectInput("normald", "Distribution Normality", names(diabetes)[1:8], multiple = FALSE, selected = "Pregnancies")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Variance-Covariance Ellipses", htmlOutput("text"), plotOutput("plot")),
        tabPanel("Equal Variance", htmlOutput("text.1"), plotOutput("plot.1")),
        navbarMenu(
          "Normality",
          tabPanel("Q-Q Plots", htmlOutput("text.2"), plotOutput("plot.2"), plotOutput("plot.3")),
          tabPanel("Distribution", htmlOutput("text.3"), plotOutput("plot.4"))
        )
      )
    )
  )
))
