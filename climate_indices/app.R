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
  
  # Sidebar with indices selection
  sidebarLayout(
    sidebarPanel(
      selectInput("indexname1",label="Index 1 (shaded)",choices=c("M1","M2","M3","M4","M5","M6")),
      selectInput("indexname2",label="Index 2 (line)",choices=c("MEI","PDO","NPGO","EMI","Nino3","Nino4","AMO","NAO","ATL3"))
    ),
    
    # Show a plot comparing climate indices
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({

    iok1 <- !is.na(Mset[,input$indexname1])
    x <- Mset$time
    y0 <- rep(0,length(Mset$YEAR))
    
    # plot 1: shaded Mset (indexname1)
    par(mar = c(5,5,2,5))
    plot(x,t(Mset[,input$indexname1]),type="l",xlab="Time",ylab=input$indexname1)
    polygon(c(x[iok1],rev(x[iok1])),c(y0[iok1],rev(t(Mset[iok1,input$indexname1]))),col="skyblue")
    
    # plot 2: line climate index (indexname2)
    par(new=T)
    plot(indices$time,t(indices[,input$indexname2]),type="l",axes=F,xlab=NA, ylab=NA,col="red")
    axis(side = 4,col="red")
    mtext(side = 4, line = 3, input$indexname2,col="red")
    
    #legend
    legend("topleft",legend=c(input$indexname1,input$indexname2),lty=c(1,1),col=c("black", "red"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)