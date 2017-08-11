library(tidyverse)
library(plotly)
library(shiny)


# Read Mset
Mset <- read_delim(file="http://www3.mbari.org/science/upper-ocean-systems/biological-oceanography/GlobalModes/Mset.txt",comment="%",delim="\t")
Mset$time <- as.Date(paste(Mset$YEAR,Mset$MONTH,rep(15,length(Mset$YEAR))),format="%Y %m %d")

# Read other climate indices
indices <- read_csv(file="http://www3.mbari.org/science/upper-ocean-systems/biological-oceanography/GlobalModes/climate_indices.csv",comment="%")
indices$time <- as.Date(paste(indices$year,indices$month,rep(15,length(indices$year))),format="%Y %m %d")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Climate indices visualization"),
  
  # Sidebar with indices selection
  sidebarLayout(
    sidebarPanel(
      selectInput("indexname1",label="Index 1 (shaded)",choices=c("M1","M2","M3","M4","M5","M6")),
      selectInput("indexname2",label="Index 2 (line)",choices=c("MEI","PDO","NPGO","EMI","Nino3","Nino4","AMO","NAO","ATL3")),
      width=2
    ),
    
    # Show a plot comparing climate indices
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({

    # max & min will be used to scale the 2nd plot
    max1 <- max(Mset[,input$indexname1],na.rm=TRUE)
    max2 <- max(indices[,input$indexname2],na.rm=TRUE)
    
    # Mset$varpos & $varneg will be used to display anomalies
    Mset[,"varpos"] <- Mset[,input$indexname1]
    Mset[,"varneg"] <- Mset[,input$indexname1]
    Mset$varpos[which(Mset$varpos > 0)] <- rep(0, length(which(Mset$varpos > 0)))
    Mset$varneg[which(Mset$varneg < 0)] <- rep(0, length(which(Mset$varneg < 0)))
    
    # compute correlation that will be displayed in the legend
    commonxM <- Mset$time %in% indices$time
    commonxI <- indices$time %in% Mset$time
    subsetM <- Mset[commonxM,input$indexname1]
    subsetI <- indices[commonxI,input$indexname2]
    iok1 <- !is.na(subsetM) & !is.na(subsetI)
    r=cor(subsetM[iok1,],subsetI[iok1,])
    legname <- paste(input$indexname2," (r = ",toString(floor(r*100)/100),")",sep="")
    
    # define plot
    p <- ggplot() +
      geom_area(data = Mset, aes(x=time, y=varpos), fill="blue") +
      geom_area(data = Mset, aes(x=time, y=varneg), fill="red") +
      geom_line(data = indices, aes(x=time, y=eval(as.name(input$indexname2))/(max2/max1), color=legname),size=0.5) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~.*(max2/max1),name = input$indexname2)) +
      scale_x_date(limits = c(min(Mset$time),max(Mset$time)),expand=c(0,0)) +
      labs(x  ="Time", y = input$indexname1) +
      theme(legend.key.width=unit(3,"line"),legend.background = element_rect(fill="transparent"),legend.text=element_text(size=11)) +
      scale_colour_manual("", 
                          breaks = c(legname),
                          values = c("black"))
    
    # display plot & fix legend location issue (ggplotly would remove the legend position)
    p2 <- plotly_build(p)   
    style(p2) %>% layout( legend = list(x = 0.01, y = 1) )  
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)