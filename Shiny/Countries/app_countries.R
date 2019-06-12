library(shiny)

countries <- readRDS("SA1/data/counties.rds")
head(countries)

library(maps)
library(mapproj)
source("SA1/helpers.R")
percent_map(countries$white, "darkgreen", "% White")

ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Creates a demographic maps with information from the 2010 US Census."),
      #br(),
      
      selectInput("var", "Choose a variable to display:",
                  choices= c("Percent White", "Percent Black", "Percent Hispanic", "Percent Asian"),
                  selected = "Percent White"),
      
      sliderInput(inputId = "range",
                  label = "Range of interest:",
                  min = 0,
                  max = 100,
                  value = c(0,100))
    ),
    mainPanel(
      plotOutput("map")
      # textOutput("selected_var"),
      # textOutput("min_max")
      )
    )
)


#server <- function(input, output) {
#  
#  output$map <- renderPlot({
#    data <- switch(input$var, 
#                   "Percent White" = countries$white,
#                   "Percent Black" = countries$black,
#                   "Percent Hispanic" = countries$hispanic,
#                   "Percent Asian" = countries$asian)
#    
#    color <- switch(input$var, 
#                    "Percent White" = "darkgreen",
#                    "Percent Black" = "black",
#                    "Percent Hispanic" = "darkorange",
#                    "Percent Asian" = "darkviolet")
#    
#    legend <- switch(input$var, 
#                     "Percent White" = "% White",
#                     "Percent Black" = "% Black",
#                     "Percent Hispanic" = "% Hispanic",
#                     "Percent Asian" = "% Asian")
#    
#    percent_map(data, color, legend, input$range[1], input$range[2])
#  })
#  
#  # output$selected_var <- renderText({ 
#  #   paste("You have selected", input$var)
#  # })
#  # 
#  # output$min_max <- renderText({
#  #   paste("You have chosen a range that goes from",
#  #         input$range[1], "to", input$range[2])
#  
#  }
#

server <- function(input, output) {
  output$map <- renderPlot({
    args <- switch(input$var,
                   "Percent White" = list(countries$white, "darkgreen", "% White"),
                   "Percent Black" = list(countries$black, "black", "% Black"),
                   "Percent Hispanic" = list(countries$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(countries$asian, "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
}

# RUN APP
shinyApp(ui = ui, server = server)
