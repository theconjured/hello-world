#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

##read in data set
cereal <- read.csv("F:\\theco\\documents\\DMhw1\\cereal.csv", header = TRUE)

#add variables for exploring nutritional density
v1 <- mutate(cereal, calorie_density=(calories/cups),sugar_density=(sugars/cups),carbo_density=(carbo/cups),
             protein_density=(protein/cups), fat_density=(fat/cups), sodium_density=(sodium/cups),
             potassium_density=(potass/cups), fiber_density=(fiber/cups), vitamin_density=(vitamins/cups))



# Define UI for application that draws a histogram
ui <- fluidPage(
        fluidRow(
            column(3, 
                   selectInput("predictor", "predictor:", list(
                       "calories", "protein","fat","potassium","carbs","sugars", "sodium","fiber","vitamins",
                       "calorie density","protein density","fat density","sugar density","carb density",
                       "sodium density","potassium density","vitamin density","fiber density"
                   )),
                   selectInput("response","response:", list(
                       "calories", "protein","fat","potassium","carbs","sugars", "sodium","fiber","vitamins",
                       "calorie density","protein density","fat density","sugar density","carb density",
                       "sodium density","potassium density","vitamin density","fiber density","rating"))),
        
    
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- switch(input$predictor,
                       "calories"=v1$calories,
                       "protein"=v1$protein,
                       "fat"=v1$fat,
                       "potassium"=v1$potass,
                       "carbs"=v1$carbo,
                       "sugars"=v1$sugars,
                       "sodium"=v1$sodium,
                       "fiber"=v1$fiber,
                       "vitamins"=v1$vitamins,
                       "calorie density"=v1$calorie_density,
                       "protein density"=v1$protein_density,
                       "fat density"=v1$fat_density,
                       "sugar density"=v1$sugar_density,
                       "carb density"=v1$carbo_density,
                       "sodium density"=v1$sodium_density,
                       "potassium density"=v1$potassium_density,
                       "vitamin density"=v1$vitamin_density,
                       "fiber density"=v1$fiber_density
        )
        y <- switch(input$response,
                    "calories"=v1$calories,
                    "protein"=v1$protein,
                    "fat"=v1$fat,
                    "potassium"=v1$potass,
                    "carbs"=v1$carbo,
                    "sugars"=v1$sugars,
                    "sodium"=v1$sodium,
                    "fiber"=v1$fiber,
                    "vitamins"=v1$vitamins,
                    "calorie density"=v1$calorie_density,
                    "protein density"=v1$protein_density,
                    "fat density"=v1$fat_density,
                    "sugar density"=v1$sugar_density,
                    "carb density"=v1$carbo_density,
                    "sodium density"=v1$sodium_density,
                    "potassium density"=v1$potassium_density,
                    "vitamin density"=v1$vitamin_density,
                    "fiber density"=v1$fiber_density,
                    "rating"=v1$rating
            
        )
        ggplot(data=NULL, aes(x,y))+geom_point()+geom_smooth()
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
