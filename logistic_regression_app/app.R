library(shiny)
library(tidyverse)
library(tidymodels)
library(GGally)
library(gridExtra)
library(cowplot)

data <- read_csv("logreg_data.csv")

transforms <- c("None", "Squared", "Square Root", "Log")


category_names <- c("Dairy and Egg Products",
                    "Fruits and Fruit Juices",
                    "Vegetables and Vegetable Products",
                    "Beef Products",
                    "Sausages and Luncheon Meats",
                    "Restaurant Foods",
                    "Poultry Products",
                    "Baked Products",
                    "Finfish and Shellfish Products",
                    "Soups, Sauces, and Gravies",
                    "Spices and Herbs",
                    "Legumes and Legume Products",
                    "Nut and Seed Products",
                    "Pork Products",
                    "Sweets",
                    "Fats and Oils")

vitamin_names <- c("Magnesium, Mg, MG",
                   "Iron, Fe, MG",
                   "Nitrogen, G",
                   "Selenium, Se, UG",
                   "Vitamin B-6, MG",
                   "Copper, Cu, MG",
                   "Phosphorus, P, MG",
                   "Calcium, Ca, MG",
                   "Potassium, K, MG",
                   "Manganese, Mn, MG",
                   "Sodium, Na, MG",
                   "Thiamin, MG",
                   "Zinc, Zn, MG",
                   "Niacin, MG",
                   "Riboflavin, MG")

vitamin_names_no_unit <- c("Magnesium",
                   "Iron",
                   "Nitrogen",
                   "Selenium",
                   "VitaminB-6",
                   "Copper",
                   "Phosphorus",
                   "Calcium",
                   "Potassium",
                   "Manganese",
                   "Sodium",
                   "Thiamin",
                   "Zinc",
                   "Niacin",
                   "Riboflavin")

vitamin_dropdown_options <- c("None", 
                              "Magnesium, Mg, MG",
                              "Iron, Fe, MG",
                              "Nitrogen, G",
                              "Selenium, Se, UG",
                              "Vitamin B-6, MG",
                              "Copper, Cu, MG",
                              "Phosphorus, P, MG",
                              "Calcium, Ca, MG",
                              "Potassium, K, MG",
                              "Manganese, Mn, MG",
                              "Sodium, Na, MG",
                              "Thiamin, MG",
                              "Zinc, Zn, MG",
                              "Niacin, MG",
                              "Riboflavin, MG")


buildSelectedData <- function(category) {
  data %>%
    mutate("{category}" := as.factor(ifelse(category == food_category_name, "Yes", "No")))
}

ui <- fluidPage(
  titlePanel("Logistic Regression Demo"),
  sidebarLayout(
    sidebarPanel(
      h4("Food Categories"),
      p("Select the food category to compare to the rest."),
      selectInput("food_category", "Food Categories", category_names),
      
      h4("Vitamins and Minerals"),
      p("Select the vitamins and minerals you want to use to distinguish between the food categories"),
      selectInput("vitamin1", "#1", vitamin_dropdown_options),
      selectInput("vitamin2", "#2", vitamin_dropdown_options),
      selectInput("vitamin3", "#3", vitamin_dropdown_options)
      
      
    ),
    mainPanel(
      h3("Model Selection"),
      p("This app is designed to help you explore logistic regression. It uses a 
        dataset containing information about the vitamin and mineral content of 
        different foods. To start, select a food category that you'd like to 
        compare against the rest. After that, select up to three vitamins and 
        minerals you would like to explore. As you make selections, density 
        curves will appear below. These density curves show how the vitamin or 
        mineral content for the selected category compares to the rest of the 
        food categories. To determine whether a vitamin is a good option for 
        differentiating between your selected category and the rest of them, 
        compare the 2 lines on each density curve. If it looks like the 
        distributions are significantly different, that's a good sign that the 
        vitamin or mineral is a good choice for a logistic regression model to 
        differentiate the selected food group."),
      plotOutput(outputId="densityCurve1"),
      plotOutput(outputId="densityCurve2"),
      plotOutput(outputId="densityCurve3"),
      h3("Model Interpretation"),
      p("With a food category and vitamins selected, the below table will show 
        a logistic regression formula. The estimates column shows the x 
        intercept and coefficients for each term in the formula. For a given 
        food, if you multiply the coefficient for each term by the amount of 
        that vitamin or mineral in a serving of the food, then add those values 
        together with the intercept, the resulting value will be a LOGIT value.
        The LOGIT is the log of the odds that the food belongs to the category 
        you selected. To convert from the LOGIT value to a probability, type 
        some numeric values into the input boxes below the table."),
      tableOutput(outputId = "modelTable"),
      conditionalPanel(condition = "input.vitamin1 != 'None'", uiOutput("numberBox1")),
      conditionalPanel(condition = "input.vitamin2 != 'None'", uiOutput("numberBox2")),
      conditionalPanel(condition = "input.vitamin3 != 'None'", uiOutput("numberBox3")),
      conditionalPanel(condition = "output.modelTable", uiOutput("probs"))
    )
  )
)

server <- function(input, output) {
  
  output$densityCurve1 <- renderPlot({
    if (input$vitamin1=="None"){return(NULL)}
    reqd_data <- buildSelectedData(input$food_category)
    
    return(ggplot(reqd_data) +
             aes(x=.data[[input$vitamin1]], color=.data[[input$food_category]]) +
             geom_density(adjust=4) + 
             title(main=paste(input$vitamin1, "Distribution for", input$food_category, "vs. Others")))
  })
  
  output$densityCurve2 <- renderPlot({
    if (input$vitamin2=="None"){return(NULL)}
    reqd_data <- buildSelectedData(input$food_category)
    
    return(ggplot(reqd_data) +
             aes(x=.data[[input$vitamin2]], color=.data[[input$food_category]]) +
             geom_density(adjust=4) + 
             title(main=paste(input$vitamin2, "Distribution for", input$food_category, "vs. Others")))
  })
  
  output$densityCurve3 <- renderPlot({
    if (input$vitamin3=="None"){return(NULL)}
    reqd_data <- buildSelectedData(input$food_category)
    
    return(ggplot(reqd_data) +
             aes(x=.data[[input$vitamin3]], color=.data[[input$food_category]]) +
             geom_density(adjust=4) + 
             title(main=paste(input$vitamin3, "Distribution for", input$food_category, "vs. Others")))
  })
  
  tbl <- reactive({
    cols <- c(input$food_category)
    if(input$vitamin1 != "None"){cols <- append(cols, input$vitamin1)}
    if(input$vitamin2 != "None"){cols <- append(cols, input$vitamin2)}
    if(input$vitamin3 != "None"){cols <- append(cols, input$vitamin3)}
    if (length(cols) == 1){return(NULL)}
    reqd_data <- buildSelectedData(input$food_category) %>%
      select(all_of(cols))
    logistic_reg() %>%
      set_engine("glm") %>%
      fit(as.formula(paste("`",input$food_category, "` ~ .", sep="")), data = reqd_data, family = "binomial") %>%
      tidy()
  })
  
  output$modelTable <- renderTable({
    tbl()
  })
  
  output$numberBox1 <- renderUI({
    numericInput("val1", input$vitamin1, 0)
  })
  
  output$numberBox2 <- renderUI({
    numericInput("val2", input$vitamin2, 0)
  })
  
  output$numberBox3 <- renderUI({
    numericInput("val3", input$vitamin3, 0)
  })
  
  output$probs <- renderUI({
    table <- tbl()
    intercept <- as.numeric(table[['estimate']][1])
    logit <- intercept
    idx <- 1
    if(input$vitamin1 != 'None'){
      logit <- logit + (as.numeric(input$val1) * as.numeric(table[['estimate']][1 + idx]))
      idx <- idx + 1
    }
    if(input$vitamin2 != 'None'){
      logit <- logit + (as.numeric(input$val2) * as.numeric(table[['estimate']][1 + idx]))
      idx <- idx + 1
    }
    if(input$vitamin3 != 'None'){
      logit <- logit + (as.numeric(input$val3) * as.numeric(table[['estimate']][1 + idx]))
    }
    odds <- round(exp(logit), digits=6)
    prob <- round((odds / (1 + odds)) * 100, digits=4)
    div(
      p(paste('Logit Value:', logit)),
      p(paste('Odds a Food with the above values is in', input$food_category, ":", odds, "to 1")),
      p(paste('Probability a Food with the above values is in', input$food_category, ":", prob, "%"))
    )
  })
  
}

shinyApp(ui = ui, server = server)
