library(tidyverse)
library(shiny)

# Read data
indices <- read_csv(file="http://www3.mbari.org/science/upper-ocean-systems/biological-oceanography/GlobalModes/climate_indices.csv",comment="%")
nb_time <- length(indices$year)
indices$time <- as.Date(paste(indices$year,indices$month,rep(15,nb_time)),format="%Y %m %d")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Climate indices visualization"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("indexname",label="Parameter",choices=c("MEI","PDO","NPGO","EMI","Nino3","Nino4","AMO","NAO","ATL3")),
         sliderInput("bins",
                     "Years:",
                     min = min(indices$year),
                     max = max(indices$year),
                     value = max(indices$year))
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
      # x    <- indices$year
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
     iok1 <- !is.na(indices[,input$indexname])
     x <- indices$time
     y0 <- rep(0,nb_time)
     plot(indices$time,t(indices[,input$indexname]),type="l",xlab="Time",ylab=input$indexname)
     polygon(c(x[iok1],rev(x[iok1])),c(y0[iok1],rev(t(indices[iok1,input$indexname]))),col="skyblue")
     

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

