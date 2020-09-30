##########################################################################################
# Welcome!
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
##########################################################################################


##########################################################################################
# This points the Shiny server tool to where all your RStudio Libraries are installed
# that means that any library you install on your Rstudio instance in this project,
# will be available to the shiny server
##########################################################################################
.libPaths( c( .libPaths(), "/srv/R/library") )

##########################################################################################
# Here you can call all the required libraries for your code to run
##########################################################################################
library(shiny)

##########################################################################################
# For deploying tools on MatrixDS, we created this production variable
# when set to true, your shiny app will run on the shiny server tool upon clicking open
# when set to false, your shiny app will run when you hit the "Run App" button on RStudio
##########################################################################################
production <- TRUE

##########################################################################################
# The shiny server tool uses a different path than RStudio.
# this if statement denotes the correct  path for the 2 values of the production variable
##########################################################################################

if(production == FALSE) {
  shiny_path <- "~/shiny-server/ProstateApp/"
  home_path <- "~/"
} else {
  shiny_path <- "/srv/shiny-server/ProstateApp/"
  home_path <- "/srv/"
}

##########################################################################################
# To call a file/artifact on your MatrixDS project use the following line of code
# this example uses the function read.csv
#  my_csv <- read.csv(paste0(home_path,"file_name.csv"))
##########################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
