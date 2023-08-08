library(shiny)
library(tidyverse)
library(plotly)

# Read Mset
Mset <- read_delim(file="https://data.mbari.org/products/satellite-derived/global-modes/Mset.txt", comment = "%", delim = "\t")
Mset$time <- as.Date(paste(Mset$YEAR, Mset$MONTH, rep(15, length(Mset$YEAR))), format = "%Y %m %d")

# Read other climate indices
indices <- read_csv(file="https://data.mbari.org/products/satellite-derived/global-modes/climate_indices.csv", comment = "%")
indices$time <- as.Date(paste(indices$year, indices$month, rep(15, length(indices$year))), format = "%Y %m %d")
indices$None <- indices$MEI * 0

# Define UI for application that draws timeseries
ui <- fluidPage(
  titlePanel(title = "Climate indices visualization"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput(
        inputId = "indexname1",
        label = "Index 1 (shaded)",
        choices = c("M1", "M2", "M3", "M4", "M5", "M6")
      ),
      selectInput(
        inputId = "indexname2",
        label = "Index 2 (line)",
        choices = c(
          "None",
          "MEI",
          "PDO",
          "NPGO",
          "EMI",
          "Nino3",
          "Nino4",
          "AMO",
          "NAO",
          "ATL3"
        )
      ),
      width = 2
    ),
    mainPanel = mainPanel(plotlyOutput(outputId = "distPlot"))
  )
)

# Define server logic required to draw plots
server <- function(input, output) {

    output$distPlot <- renderPlotly({

    # max & min will be used to scale the 2nd plot
    max1 <- max(Mset[, input$indexname1], na.rm = TRUE)
    max2 <- max(indices[, input$indexname2], na.rm = TRUE)

    # Mset$varpos & $varneg will be used to display anomalies
    Mset[, "varneg"] <- Mset[, input$indexname1]
    Mset[, "varpos"] <- Mset[, input$indexname1]
    Mset$varneg[which(Mset$varneg > 0)] <- rep(0, length(which(Mset$varneg > 0)))
    Mset$varpos[which(Mset$varpos < 0)] <- rep(0, length(which(Mset$varpos < 0)))

    
    if (str_equal(input$indexname2, "None")) {

	# define simple plot (no index)
    p <- ggplot() +
      geom_area(data = Mset, aes(x = time, y = varneg), fill = "blue") +
      geom_area(data = Mset, aes(x = time, y = varpos), fill = "red") +
      scale_x_date(limits = c(min(Mset$time)-15, max(Mset$time)+15), expand=c(0, 0)) +
      labs(x = "Time", y = input$indexname1)

    } else {

	# compute correlation that will be displayed in the legend	
    commonxM <- Mset$time %in% indices$time
    commonxI <- indices$time %in% Mset$time
    subsetM <- Mset[commonxM, input$indexname1]
    subsetI <- indices[commonxI, input$indexname2]
    iok1 <- !is.na(subsetM) & !is.na(subsetI)
    r <- cor(subsetM[iok1, ], subsetI[iok1, ])
    legname <- paste(input$indexname2, " (r = ", toString(floor(r*100)/100), ")", sep = "")

    # define plot
    p <- ggplot() +
      geom_area(data = Mset, aes(x = time, y = varneg), fill = "blue") +
      geom_area(data = Mset, aes(x = time, y = varpos), fill = "red") +
      geom_line(data = indices, aes(x = time, y = eval(as.name(input$indexname2))/(max2/max1), color = legname), linewidth = 0.5) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~.*(max2/max1), name = input$indexname2)) +
      scale_x_date(limits = c(min(Mset$time)-15, max(Mset$time)+15), expand=c(0, 0)) +
      labs(x = "Time", y = input$indexname1) +
      theme(legend.key.width=unit(3,"line"), legend.background = element_rect(fill="transparent"), legend.text = element_text(size = 11)) +
      scale_colour_manual("",
                          breaks = c(legname),
                          values = c("black"))
    }

    # display plot & fix legend location issue (ggplotly would remove the legend position)
    p2 <- plotly_build(p)
    # style(p2) %>% layout(legend = list(x = 0.01, y = 1)) 		# now causes the app to crash.

  })
}

# Run the application
shinyApp(ui = ui, server = server)