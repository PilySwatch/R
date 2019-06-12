library(quantmod)
source("stockVis/helpers.R")

# RUN APP
runApp("stockVis")

# Modified "server" in order to avoid the App re-fetch the results again.
server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
#   output$plot <- renderPlot({
#     data <- dataInput()
#     if (input$adjust) data <- adjust(dataInput()) # Adjust prices for inflation.
#     
#     chartSeries(dataInput(), theme = chartTheme("white"),
#                 type = "line", log.scale = c(input$log[1], input$log[2]),TA = NULL)
#   })
#   
# }
  
  
  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput()) # Return the price values already kept from the first time the App was run.
  })
  
  output$plot <- renderPlot({
    chartSeries(finalInput(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
}


# SHARING THE APPS
runUrl( "<the weblink>") # will lauch the App on a weblink.

runGitHub( "<your repository name>", "<your user name>")
# To share an app through GitHub, create a project repository on GitHub. 
# Then store your app.R file in the repository, along with any supplementary files that the app uses.

runGist("gist number")
# To share your app as a gist:
# Copy and paste your app.R files to the gist web page: gist.github.com. .
# Note the URL that GitHub gives the gist.

