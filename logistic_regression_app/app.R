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
      plotOutput(outputId="densityCurve1"),
      plotOutput(outputId="densityCurve2"),
      plotOutput(outputId="densityCurve3"),
      tableOutput(outputId = "modelTable")
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
  
  output$modelTable <- renderTable({
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
  
}

shinyApp(ui = ui, server = server)
