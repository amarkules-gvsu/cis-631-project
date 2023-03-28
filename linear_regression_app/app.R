library(shiny)
library(tidyverse)
library(tidymodels)
library(GGally)

# To Do
#   Add assumption checking - linearity
#   Add help dialog
#   Deploy!

data <- read_csv("linear_regression_data.csv")

transforms <- c("None", "Squared", "Square Root", "Log")

iv_names_clean <- c("Protein, G",
                    "Total lipid (fat), G",
                    "Carbohydrate, by difference, G",
                    "Fatty acids, total saturated, G",
                    "Glucose, G",
                    "Fructose, G",
                    "Fatty acids, total monounsaturated, G",
                    "Fiber, total dietary, G",
                    "Fatty acids, total polyunsaturated, G",
                    "Fatty acids, total trans, G")

dependent_vars <- c("Energy, KCAL", "Portion Size, G")

iv_names_nospace <- c("saturated_fat_g","glucose_g","fructose_g","monounsaturated_fat_g","dietary_fiber_g","protein_g","polyunsaturted_fat_g","fat_g","trans_fat_g","carbs_g")

buildSelectedData <- function(input) {
  cols <- c(input$dependentVariable)
  for (i in 1:length(iv_names_clean)){
    if(input[[iv_names_nospace[i]]]){
      if(input[[paste(iv_names_nospace[i], "transform", sep="_")]] != "None") {
        cols<-append(cols, paste(input[[paste(iv_names_nospace[i], "transform", sep="_")]], iv_names_clean[i]))
      } else {
        cols<-append(cols, iv_names_clean[i])
      }
    }
  }
  data %>%
    select(all_of(cols))
}

ui <- fluidPage(
  titlePanel("Linear Regression Demo"),
  sidebarLayout(
    sidebarPanel(
      h4("Dependent Variable"),
      p("Select the variable you want to predict."),
      selectInput(
        "dependentVariable",
        "DV",
        dependent_vars,
        selected = "Energy, KCAL",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      h4("Independent Variables"),
      p("Select the variables you think will help to predict the dependent variable."),
      uiOutput("indVars")
      
      
    ),
    mainPanel(
      h2("Model Selection"),
      p("To build a linear regression model, we need to select independent variables that will be helpful in predicting our dependent variable. 
      Start by selecting a dependent variable, and a few independent variables in the side panel. Watch how the matrix of plots below changes as you select options.
      In the top row, corr values represent correlations between the dependent variable, and the selected independent variables. Values closer to 1 indicate stronger correlations and better predictors.
      Along the left side, you can see a scatter plot of each IV against the DV, you can look for trends here, but they might not always be obvious.
      Along the diagonal from top left to bottom right, there should be a graph of the distribution for each variable. A line that resembles a bell curve here indicates a normal distribution for that variable. 
      Normal distributions lend themselves better to linear regression. If one of the IV graphs here doesn't look normal, you can try selecting a different transformation for that variable in the side panel."),
      plotOutput(outputId = "corrs"),
      h2("Model Interpretation"),
      p("The table below shows the fitted formula for the linear regression model you've selected. You should see a term for each IV you've selected, and an Intercept term.
        In the second column are the estimated coefficients for each term, which can be used to create a formula for predicting your DV. For example, if your DV is 'Energy, KCAL',
        and you've selected Protein and Carbohydrate as IVs, without any transformation, the formula would be Energy = 41.21 + 8.56*Protein + 3.81*Carbohydrate. The rest of the table 
        helps determine how useful each variable was in making predictions. Most importantly, a lower P-Value indicates a variable was more useful"),
      p("Note: if you see an error below, it means there was an issue transforming some of the values in the model. This sometimes happens with log transformations. Try a different transformation instead."),
      tableOutput('formula'),
      h2("Model Assessment"),
      p("The table below can be used to assess the quality of a model. The 'r.squared' value indicates the proportion of variation in the DV that can be explained by the IVs 
        (interesting note about this data - selecting Energy as the DV, and Protein, Total Fat, and Carbs as IVs produces an R Squared of 1, 
        since these are the three major sources of energy in food). Other interesting values here are the p-value, indicating the likelihood that the IVs have no effect on the DV, 
        and the AIC (Akaike's Information Criterion), which assesses the 'badness of fit' adjusting for simplicity, a lower value here means a better model, and adding unneeded IVs raises the value."),
      tableOutput('fitStats'),
      h2("Validating Assumptions"),
      p("When building a linear regression model, there are certain assumptions we make about the underlying data. The first is that the relationship between the independent and dependent variables is linear.
        We can check that assumption using the Residual plot below: If you were to draw a line through the middle of the points in the plot, from left to right, would it follow the dotted red line? If so, great!
        If the line would have more of a curved shape, we may need to pick a different model."),
      p("The second assumption is equality of variance, Which can also be assessed using this residual plot. If the points seem bunched together in one part of the graph, and spread out in a different part, we may need to select a different model."),
      plotOutput("residualPlot"),
      p("Another assumption we need to check is that the variables we are using are multivariate-normal. One way to assess that is using the histogram of residuals below. Here we hope that the graph is roughly normal."),
      plotOutput("residualHist"),
      p("The last assumption we need to check is that there isn't too much correlation between the IVs. This can be checked using the the matrix at the top of the page. Specifically, look at the Corr values below the top line.
        Values there that are closer to 1 or -1 than 0 are cause for concern.")
    )
  )
)

server <- function(input, output) {
  
  output$indVars <- renderUI({
    lapply(1:length(iv_names_clean), function(i) {
      fluidRow(
        column(6,checkboxInput(iv_names_nospace[i], iv_names_clean[i], FALSE)),
        column(6,selectInput(paste(iv_names_nospace[i], "transform", sep="_"), "Transform", transforms, selected="None"))
      )
    })
  })
  
  output$corrs <- renderPlot({
    buildSelectedData(input) %>% 
      ggpairs()
  })
  
  output$formula <- renderTable({
    lm(as.formula(paste("`",input$dependentVariable, "` ~ .", sep="")), buildSelectedData(input)) %>%
      tidy()
  })
  
  output$fitStats <- renderTable({
    lm(as.formula(paste("`",input$dependentVariable, "` ~ .", sep="")), buildSelectedData(input)) %>%
      glance()
  })
  
  output$residualPlot <- renderPlot({
    lm(as.formula(paste("`",input$dependentVariable, "` ~ .", sep="")), buildSelectedData(input))%>% 
      augment() %>%
      ggplot(aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      xlab("Fitted values") +
      ylab("Residuals")
  })
  
  output$residualHist <- renderPlot({
    lm(as.formula(paste("`",input$dependentVariable, "` ~ .", sep="")), buildSelectedData(input))%>% 
      augment() %>%
      ggplot(aes(x = .resid)) +
      geom_histogram(bins=40) +
      xlab("Residuals")
  })
  
}

shinyApp(ui = ui, server = server)
