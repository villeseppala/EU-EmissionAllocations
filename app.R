library(shiny)
library(bslib)

library(shiny)
library(ggplot2)
# library(pxweb)
library(data.table)
# library(ggplot2)
# library(tibble)
library(tidyr)
library(shinyWidgets)


cumc =hsv(.55,.9,.9)
debt = hsv(.85, .9,.9)
alloc = hsv(.7, .6,.8)
emis = "#4ba180"
#
# Define UI for app that draws a histogram ----
koki<-read.csv(file="data/koki.csv", header=T,  sep=",", stringsAsFactors = F )

koki = as.data.table(koki)

lyear = koki[sector == "metsajapuu" & maara <1000000, max(year)]





koki1 = koki[sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost", "cumu", "cost", "debt", "price"),]
koki2 = koki[sector %in% c("lulucf", "lallocation", "diff", "cost", "cumu"),]


ui <- page_sidebar(
  
  # App title ----
  title = "Hello Shiny!",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    ),
    
    
    # background-color:#e7e7e7!important;   
    div(        
      style = "  margin-right: -0.2vw; padding: .2vw;color:#4ba180;",
      
      #   tags$div(style =  ".irs-bar { background: #0a0; }", 
      #                 sliderInput("lulucf2024", label = "Nettonielu 2024:", 
      # # style = " color:#4ba180; ",
      #                        value=-14, min=-35, max=0,post = " Mt")),
      #            
      #            sliderInput("lulucf2025", label = "Nettonielu 2025:", 
      #                        value=-14, min=-35, max=0,post = " Mt" 
      #                        # style = "background: black;"
      #                        ),
      sliderTextInput(
        "lulucf2024", label = "Nettonielu 2024:", 
        choices = seq(from = 0, to = -35, by = -1),
        selected = -14,
        width = "100%",
        
        post = " Mt"
      ),
      hr(),
      
      sliderTextInput(
        "lulucf2025", label = "Nettonielu 2025:", 
        choices = seq(from = 0, to = -35, by = -1),
        selected = -14,
        width = "100%",
        post = " Mt"
      ),
      hr(),
      
      #    numericInput("pricepre", "Aseta hintataso sektorilla metsämaa 2021-2025", 20,min=0, max=200),
      
      h5(
        tags$b("2021-2023-nielu alustavista tilastoista", 
               # style="color: green;"
               
        ))
      ,
      hr(),
      
      
      
      
      div( style = " color:#bc810d!important;",
           sliderInput("pricepre", label = "Kauden yksiköiden keskihinta", 
                       value=20, min=0, max=100,post = " €/t", animate = T))),
    # textOutput("pul"),
    div(style = "border-color: red; border-style: solid; border-size: .1px; padding: .2vw;",
        uiOutput(style = "margin-left: -0px; border-color: red;","pula")),
    
    
  ),
  
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  rv <- reactiveValues(cums =0)
  rv <- reactiveValues(cul ="Kustannus")
  rv <- reactiveValues(view =2)
  
  
  observeEvent(input$dim, {
    
    if (input$dim[1] >1800) {
      rv$view = 1
    } 
    else  if (input$dim[1] >1150 & input$dim[1] <1800) {
      rv$view = 2
    } else {
      rv$view = 3
    }
  })
  
  
  
  output$pula= renderText({
    paste(
      rv$cul1,
      "\n",
      '<br><span style=\"color:',cumc,'\"><b>',  rv$cums,  
      " Mt ",'</b></span>'," x ",
      '<span style=\"color:', "#bc810d", '\"><b>',  input$pricepre, "€/t ",'</b></span>',"= ",
      '<span style=\"color:', debt, '\"><b>', 
      format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      " milj. €",'</b></span>',
      sep ="")})
  
  
  koke1=reactive({
    koki =copy(koki1)
    
    startpre = koki[year ==lyear & sector =="metsajapuu", maara]
    
    
    koki[year %in% c(2024) & sector =="metsajapuu", maara := input$lulucf2024]
    koki[year %in% c(2025) & sector =="metsajapuu", maara := input$lulucf2025]
    
    
    koki= koki[year %in% c(2021:2025) & sector =="diff", maara := koki[year %in% c(2021:2025) &sector=="metsajapuu", maara, on=c("year")] - koki[year %in% c((2021):2025) & sector=="metsajapuuk", maara, on=c("year")]]
    
    koki =   koki[order(rank(year), year)]
    
    mor =   cumsum(koki[year %in% c(2021:2025) & sector =="metsajapuuk", maara])
    koki = koki[year %in% c(2021:2025)& sector =="metsajapuuk", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2021:2025) & sector =="metsajapuu", maara])
    koki = koki[year %in% c(2021:2025)& sector =="metsajapuu", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2021:2025) & sector =="diff", maara])
    koki = koki[year %in% c(2021:2025)& sector =="diff", maarab :=mor]
    
    koki = koki[year %in% c(2021:2025)& sector =="cumu", maara :=mor]
    
    koki= koki[year %in% c(2021:2025) & sector =="cost", maara := koki[year %in% c(2021:2025) &sector=="diff", maara]*input$pricepre]
    
    mora =   cumsum(koki[year %in% c(2021:2025) & sector =="cost", maara])
    koki = koki[year %in% c(2021:2025)& sector =="cost", maarab :=mora]
    
    koki = koki[year %in% c(2021:2025)& sector =="debt", maara :=mora]
    
    koki = koki[year %in% c(2021:2025)& sector =="price", maara :=input$pricepre]
    
    # koki= koki[year %in% c(2021:2025) & sector =="debt", maara := koki[year %in% c(2021:2025) &sector=="cumu", maara]*input$pricepre]
    
    
    rv$cums= format(round(koki[year %in% 2025 & sector =="cumu", maara],1), nsmall=1, decimal.mark=",")
    rv$cumsi= koki[year %in% 2025 & sector =="cumu", maara]
    
    if (rv$cums > 0) {
      rv$cul1 = "Kustannus yksiköiden hankinnasta:"
    } else {
      rv$cul1 = "Tuotto yksiköiden myynnistä:"
    }
    
    koki
  })
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(
      x,
      breaks = bins,
      col = "#75AADB",
      border = "white",
      xlab = "Waiting time to next eruption (in mins)",
      main = "Histogram of waiting times"
    )
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
