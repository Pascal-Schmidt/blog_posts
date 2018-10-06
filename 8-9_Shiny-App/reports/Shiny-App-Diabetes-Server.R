library(shiny)
library(ggplot2)
library(rsconnect)

shinyServer(function(input, output) {
  output$plot <- renderPlot({
    diabetes <- read.csv("diabetes.csv")
    diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
    diabetes <- na.omit(diabetes) # now we omit all NA values
    diabetes$Outcome <- as.factor(diabetes$Outcome)
    levels(diabetes$Outcome) <- c("No Diabetes", "Diabetes")

    ggplot(diabetes, aes(x = diabetes[, input$variablex], y = diabetes[, input$variabley], col = Outcome)) +
      geom_point() +
      stat_ellipse() +
      ylab(input$variabley) +
      xlab(input$variablex) +
      scale_color_manual(values = c("red", "blue"))
  })

  ######### END OF TAB 1

  ######### START OF TAB 2

  output$plot.1 <- renderPlot({
    diabetes <- read.csv("diabetes.csv")
    diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
    diabetes <- na.omit(diabetes) # now we omit all NA values
    diabetes$Outcome <- as.factor(diabetes$Outcome)
    levels(diabetes$Outcome) <- c("No Diabetes", "Diabetes")


    ggplot(diabetes, aes(x = Outcome, y = diabetes[, input$variableyy], col = Outcome, fill = Outcome)) +
      geom_boxplot(alpha = 0.2) +
      ylab(input$variableyy) +
      scale_color_manual(values = c("red", "blue")) +
      scale_fill_manual(values = c("red", "blue"))
  })

  ######### END OF TAB 2

  ######### START OF TAB 3.1

  output$plot.2 <- renderPlot({
    diabetes <- read.csv("diabetes.csv")
    diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
    diabetes <- na.omit(diabetes) # now we omit all NA values
    diabetes$Outcome <- as.factor(diabetes$Outcome)
    levels(diabetes$Outcome) <- c("No Diabetes", "Diabetes")


    diab.yes <- subset(diabetes, Outcome == "Diabetes")
    qqnorm(diab.yes[, input$normalq], main = "Diabetes Group \n Normal Q-Q Plot")
    qqline(diab.yes[, input$normalq], col = 2)
  })

  output$plot.3 <- renderPlot({
    diabetes <- read.csv("diabetes.csv")
    diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
    diabetes <- na.omit(diabetes) # now we omit all NA values
    diabetes$Outcome <- as.factor(diabetes$Outcome)
    levels(diabetes$Outcome) <- c("No Diabetes", "Diabetes")


    diab.no <- subset(diabetes, Outcome == "No Diabetes")
    qqnorm(diab.no[, input$normalq], main = "Non Diabetes Group \n Normal Q-Q Plot")
    qqline(diab.no[, input$normalq], col = 2)
  })

  ######### START OF TAB 3.1

  output$plot.4 <- renderPlot({
    diabetes <- read.csv("diabetes.csv")
    diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
    diabetes <- na.omit(diabetes) # now we omit all NA values
    diabetes$Outcome <- as.factor(diabetes$Outcome)
    levels(diabetes$Outcome) <- c("No Diabetes", "Diabetes")

    ggplot(diabetes, aes(x = diabetes[, input$normald], y = ..density.., col = Outcome, fill = Outcome)) +
      geom_density(aes(y = ..density..), alpha = 0.1) +
      xlab(input$normald) +
      scale_color_manual(values = c("blue", "red")) +
      scale_fill_manual(values = c("blue", "red"))
  })


  # text output for data description
  output$text <- renderUI({
    tags$div(
      HTML('<p style="color:black; font-size: 12pt">
         Here we can experiment with the ellipses of each variable. 
         Linear Discriminant Analysis assumes equal variance-covariance matrices
         within each group, whereas for Quadratic Discriminant Analysis, the 
         covariance ellispes can differ and it allows different covariance structures within
         each group. The covariance sets the shape of the ellipses. When we experiment
         with the variables, then we can see that most of the ellipses have the same 
         orientation and therefore, have roughly the same covariance marices. 
         However, the variances differ a lot within each group. This is visible
         by looking at the size of the ellipses. Bigger ellispses mean a larger variance. </p>

         <p style="color:black; font-size: 12pt">
         Another assumption is that each group is drawn from a multivariate normal 
         distribution. We can check this assumption by looking if the scatter of the data
         is actually elliptical. For some variables, we notice that this is not always the case.
         </p>')
    )
  })


  output$text.1 <- renderUI({
    tags$div(
      HTML('<p style="color:black; font-size: 12pt">
         With these boxplots, we can check the variances assumption within each group for each variable. The 
         bigger the boxplot is, the larger the variance. The horizontal line inside each boxplot 
         represents the median and the lower end and the upper end are the first and third
         quantile respectively. The points that are visible are considered outliers. </p>')
    )
  })


  output$text.2 <- renderUI({
    tags$div(
      HTML('<p style="color:black; font-size: 12pt">
         With the Q-Q Plots we can further check if the data within each group is drawn from
         a normal distribution. We can assume normality, when the points fall on the red line.
         For a lot of variables this is not the case </p>')
    )
  })


  output$text.3 <- renderUI({
    tags$div(
      HTML('<p style="color:black; font-size: 12pt">
         Here, we are comparing the distributions of each class. These visualizations help determining
         if the variances of the classes are roughly equal. In addition to that, they help determining 
         if each class is drawn from a multivariate normal distribution. From the plots, we can see that 
         a lot of distributions are skewed and that the variances for the classes differ. </p>
         
         <p style="color:black; font-size: 12pt">
         So now that we have gone through all the visualizations, we can say that some assumptions are violated
         for the linear discriminant analysis and also for the quadratic discriminant analysis. The normality
         assumption for some variables is violated. Moreover, the variance for some variables are not equal. However,
         the orientation of each ellipse was roughly equal and so the covariance structure was similar.
         </p>
         
         <p style="color:black; font-size: 12pt">
         I hope you enjoyed the visual approach towards linear and quadratic disciminant analysyis and 
         have learned how to check the assumptions with visualizations. Thank you.
         </p>')
    )
  })
})
