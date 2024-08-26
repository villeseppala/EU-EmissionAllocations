# library(shiny)
# library(bslib)

library(shiny)
library(ggplot2)
# library(pxweb)
library(data.table)
# library(ggplot2)
# library(tibble)
# library(tidyr)
library(shinyWidgets)

#ff
cumc ="#1b87ab"
debt ="#810eaa"
allo = "#262ac5"
emis = "#1baa5b"
price = "#e22c8c"

# --colemis: #24aa61;
#   
#   --colallo: #198baa;
#   --coldiff: #1f25aa;
#   --colprice: #aa1f69;
#   --colcost: #8420aa;
  
#
# Define UI for app that draws a histogram ----
koki<-read.csv(file="data/koki.csv", header=T,  sep=",", stringsAsFactors = F )

koki = as.data.table(koki)

koki[sector =="cumu", col :=cumc]
koki[sector =="diff",col :=cumc]
koki[sector =="metsajapuuk", col :=allo]
koki[sector =="metsajapuu",col :=emis]
koki[sector =="lallocation", col :=hsv(.8, .9,.9)]
koki[sector =="lulucf", col :="#4ba180"]
koki[sector =="cost", col :=debt]
koki[sector =="debt", col :=debt]
koki[sector =="price", col :=price]


lyear = koki[sector == "metsajapuu" & maara <1000000, max(year)]


#https://julkaisut.valtioneuvosto.fi/bitstream/handle/10024/165717/VNTEAS_2024_26.pdf?


#v = Ylitukset: Kiintiötarve = vertailutaso - nielu
# Hoidettu viljelysmaa


koki1 = koki[sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost", "cumu", "cost", "debt", "price"),]
koki2 = koki[sector %in% c("lulucf", "lallocation", "diff", "cost", "cumu"),]


ui <- fluidPage(
  tags$style(".span12 {background-color: black;}"),
  # App title ----
  title = "Hello Shiny!",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "cs.css")
  ),
  
  tags$script(HTML('
                     
       
      var dim = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dim[0] = window.innerWidth;
                                    dim[1] = window.innerHeight;
                                    Shiny.onInputChange("dim", dim);
                                });
                                $(window).resize(function(e) {
                                    dim[0] = window.innerWidth;
                                    dim[1] = window.innerHeight;
                                    Shiny.onInputChange("dim", dim);
                                }); 

                    ')),
  
  
  tags$style(HTML(" 
                                   hr {
  border-top: 1px solid grey; padding: 0p; margin: 0px;
                                   }
.container-fluid { background-color:var(--colback);}
  
                  .irs--shiny .irs-min, .irs--shiny .irs-max {
    
    font-size: 13px;
    padding-bottom: 3.5vw !important;
    background-color: transparent;

                  }


")),
# .irs--shiny .irs-single { font-size: 15px; background: transparent; color:purple}; 

tags$style(HTML("
          .js-irs-0 .irs-bar, .js-irs-0 .irs-single {background: var(--colemis)}
                    .js-irs-1 .irs-bar, .js-irs-1 .irs-single {background: var(--colemis)} 
    .js-irs-2 .irs-bar,  .js-irs-2 .irs-single {background: var(--colprice)}; 
                  .js-irs-0 .irs-max {background-color: transparent !important font-size: 5vw} 
       
                
                ")),
# tags$style(HTML(" .js-irs-2 .irs-bar,  .js-irs-2 .irs-single {background: #bc810d};")),

  # tags$style(HTML(" " )),
  tags$style(HTML(" .js-irs-3 .irs-bar {background:var(--colemis)}
                  .js-irs-3 .irs-single {background: var(--colemis)};" )),
  # tags$style(HTML(" .js-irs-2 .irs-bar,  .js-irs-2 .irs-single {background: #bc810d};")),
  # tags$style(HTML("
  #                 .js-irs-4 .irs-single {background: #bc810d};" )),
  # 
  div(          style = " background-color:#D6D6D6!important;    ",
      
              fluidRow(
                tagList(a
                        (div(style="font-size: 15px !important; left-margin: 15px;",icon("fab fa-github"))
                          # ))
                          , 
                          href="https://github.com/villeseppala/globalcarbonprice/")
                )
                
              ),  
                
                          
                fluidRow( style = " background-color:#D6D6D6!important; padding: -.2vw; margin: -.1vw; ",
                          h5(
                            tags$b("Metsämaan ja puutuotteiden tavoitteet ja päästöt kaudella 2021-2025", 
                                   style="color: blue; padding: 1vw;")
                          ),
  
  # Sidebar panel for inputs ----
  column(3,
    
    # Input: Slider for the number of bins ----
    # sliderInput(
    #   inputId = "bins",
    #   label = "Number of bins:",
    #   min = 1,
    #   max = 50,
    #   value = 30
    # ),
    # 
    
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
      div(        class="slidy", 
        style = " color:var(--colemis);",
      sliderTextInput(
        "lulucf2024", label = "Nettonielu 2024:", 
        choices = seq(from = 0, to = -35, by = -1),
        selected = -14,
        width = "100%",
        # pre = "2024: 
        # ",
        post = " Mt",
        grid =T
      )),
      hr(),
      div(          class="slidy",  
        style = "color:#4ba180;",
        
      sliderTextInput(
        "lulucf2025", label = "Nettonielu 2025:", 
        choices = seq(from = 0, to = -35, by = -1),
        selected = -14,
        width = "100%",
        post = " Mt",
        grid=T
      )),
      hr(),
      
      #    numericInput("pricepre", "Aseta hintataso sektorilla metsämaa 2021-2025", 20,min=0, max=200),
      
      h5(
        tags$b("2021-2023-nielu alustavista tilastoista", 
               # style="color: green;"
               
        )),
      
      h5(
        tags$b("Kauden 2021-2025 nettonielu", 
               # style="color: green;"
               
        ))
      
      )
      ,
      div(style = "border-color: red; border-style: solid; border-size: .1px; padding: .2vw;",
          uiOutput(style = "margin-left: -0px; border-color: red;","difa")),
      
      hr(),
    
      div(  class="slidy",  
        style = "color:var(--colprice)!important;",
           sliderTextInput("pricepre", label = "Kauden yksiköiden keskihinta:", 
                           choices = seq(from = 0, to = 100, by = 1),
                           selected = 20,
                           width = "100%",
                           # pre = "2024: 
                           # ",
                           post = " €/t",
                           grid =T
                       # value=20, min=0, max=100,post = " €/t"
                       )),
    # textOutput("pul"),
    div(style = "border-color: red; border-style: solid; border-size: .1px; padding: .2vw;",
        uiOutput(style = "margin-left: -0px; border-color: red;","pula")),
    hr(),
    hr(),
    hr(),
    
 
  ),
  column(9,
  # Output: Histogram ----
  # plotOutput(outputId = "distPlot"),
  
  div(
    # style="  box-shadow: 15px 15px 14px grey inset; padding: 19px;",
    
    # style = " background-color:grey!important;",
    plotOutput(outputId = "plotmetsajapuu", height = "500px")
  ), 
)
                ),
div(style = "background-color:#D6D6D6!important; border-color: blue; border-style: solid; border-size: .1px; padding: .2vw; margin: .5vw;",
    
    fluidRow(
      
      column(3,
             
             div(   class="slidy",         
               style = "color:var(--colemis);",
               
               sliderTextInput(
                 "maa2025", label = "Maankäyttösektorin muiden tilinpitoluokkien ylitys/alitus kaudelta:", 
                 choices = seq(from = 0, to = 30, by = 1),
                 selected = 14,
                 width = "100%",
                 post = " Mt",
                 grid=T
                 

                 
               )), 
             

             ),

      
      column(9,
             div(style = "border-color: red; border-style: solid; border-size: .1px; padding: .2vw;",
                 uiOutput(style = "margin-left: -0px; border-color: red;","totu")),
             h5(
               tags$b("Huom: Laskelma ei huomioi mahdollisia ns. metsäjoustoja, joita on saatavilla vain mikäli EU kokonaisuutena pääsee tavoitteisiinsa. Tällöin yksiköiden hinta on todennäköisesti hyvin lähellä nollaa tai käytännössä nolla.", 
                      # style="color: green;"
                      
               )), 
      )),
    
    checkboxInput("mores", "Näytä lisätietoja 14 Mt oletukselle", value =F),
    conditionalPanel( condition = "input.mores == true",
                      
                      htmlOutput("moress")
                      
                      
                      )
    
)
))












# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  
  rv <- reactiveValues(cums =0)
  rv <- reactiveValues(cul ="Kustannus")
  rv <- reactiveValues(view =2)
  rv <- reactiveValues(fonts =1)
  
  
  # different setting for different screen sizes
  observeEvent(input$dim, {
    
    if (input$dim[1] >1800) {
      rv$view = 1
    } 
    else  if (input$dim[1] >1300 & input$dim[1] <1800) {
      rv$view = 2
    } else {
      rv$view = 3
      rv$fonts = input$dim[1]/1300
    }
  })
  
  
  
  output$difa= renderText({
    paste(
      rv$cul2,
      "\n",
      '<br><span style=\"color: var(--colallo)','\"><b>',  abs(rv$cumalloi),  
      " Mt ",'</b></span>'," - ",
      
      '<span style=\"color: var(--colemis)','\"><b>',  abs(rv$cumsinki),  
      
      # '<span style=\"color:', "#bc810d", '\"><b>',  input$pricepre, "€/t ",
      " Mt ", '</span>',
      "= ",
      '<span style=\"color: var(--coldiff)', '\"><b>', 
      '<span style=\"color:var(--coldiff)','\"><b>',  rv$cums,  
      
      # format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      " Mt",'</b></span>',
      sep ="")})
  
  
  output$pula= renderText({
    paste(
      rv$cul1,
      "\n",
      '<br><span style=\"color: var(--coldiff)','\"><b>',  rv$cums,  
      " Mt ",'</b></span>'," x ",
      '<span style=\"color: var(--colprice)',  '\"><b>',  input$pricepre, " €/t ",'</b></span>',"= ",
      '<span style=\"color: var(--colcost)', '\"><b>', 
      format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      " milj. €",'</b></span>',
      sep ="")})
  
  
  
  output$moress = renderText({
    paste0(
      " 
          Lähde 14 Mt oletukselle PEIKKO-WEM-Perusskenaariosta s.140: https://julkaisut.valtioneuvosto.fi/bitstream/handle/10024/165717/VNTEAS_2024_26.pdf?",
 '<br/>',
      "Ylitykset eli kiintiötarve = vertailutaso - nielu
         ",
      "\n",
      '<br/>',     "
     Hoidettu viljelysmaa = 29-29=0",
    '<br>',
    "Hoidettu ruohikkoalue = 3,7-3,2=-0,5",
    '<br>',
    "Metsitetty maa = 0-0,5=-0,5",
    '<br>',
    "Metsäkatoalue = 0-14=14",
    '<br>',
    "Ylitykset kokonaisuutena = 14
    
    "
    )
  })
  
  #v = Ylitukset: Kiintiötarve = vertailutaso - nielu
  # Hoidettu viljelysmaa
  
  output$totu= renderText({
    paste(
      rv$cul3,
      "\n",
      '<span style=\"color: var(--colcost)', '\"><b>',
      format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      '</span>',
      " + ",'</b>',
      '<span style=\"color: var(--colprice)',  '\"><b>',  input$maa2025, " Mtt ",'</b></span>',
      " x ",
      '<span style=\"color: var(--colprice)', '\"><b>',  input$pricepre, " €/t ",'</b></span>',"= ",
      
    
      '<span style=\"color: var(--colcost)',  '\"><b>', 
      format(round((input$pricepre*rv$cumsi+input$pricepre*input$maa2025),0), nsmall=0, decimal.mark=","),
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
    koki[year %in% c(2021:2025)& sector =="price", maarab :=maara]
    
    koki$col2 = "white"
    koki[maara < 0, col2 :="green"]
    koki[maara > 0, col2 :="red"]
    koki[maarab < 0, col3 :="green"]
    koki[maarab > 0, col3 :="red"]
    
    # koki= koki[year %in% c(2021:2025) & sector =="debt", maara := koki[year %in% c(2021:2025) &sector=="cumu", maara]*input$pricepre]
    
    rv$cumsink= format(round(koki[year %in% 2025 & sector =="metsajapuu", maarab],1), nsmall=1, decimal.mark=",")
    rv$cumsinki= koki[year %in% 2025 & sector =="metsajapuu", maarab]  
  
    rv$cumallo= format(round(koki[year %in% 2025 & sector =="metsajapuuk", maarab],1), nsmall=1, decimal.mark=",")
    rv$cumalloi= koki[year %in% 2025 & sector =="metsajapuuk", maarab]  
    
    
      rv$cums= format(round(koki[year %in% 2025 & sector =="cumu", maara],1), nsmall=1, decimal.mark=",")
    rv$cumsi= koki[year %in% 2025 & sector =="cumu", maara]
    
    if (rv$cums > 0) {
      rv$cul1 = "Kustannus yksiköiden hankinnasta:"
    } else {
      rv$cul1 = "Tuotto yksiköiden myynnistä:"
    }
    
    if (rv$cums > 0) {
      rv$cul3 = "Kokonaiskustannus yksiköiden hankinnasta maankäyttösektorilla:"
    } else {
      rv$cul3 = "Kokonaistuotto yksiköiden myynnistä maankäyttösektorilla:"
    }
    
    if (rv$cums > 0) {
      rv$cul2 = "Kiintiön ylitys eli tarve yksiköiden hankinnalle:"
    } else {
      rv$cul2 = "Kiintiön alitus eli myytävissä olevat yksiköt:"
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
  # output$distPlot <- renderPlot({
  #   x <- faithful$waiting
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   hist(
  #     x,
  #     breaks = bins,
  #     col = "#75AADB",
  #     border = "white",
  #     xlab = "Waiting time to next eruption (in mins)",
  #     main = "Histogram of waiting times"
  #   )
  # })
  
  
  output$plotmetsajapuu <- renderPlot({
    luk = 2010
    if (rv$view ==1) {
      luk = 2005
    } else if (rv$view==2) {
      luk =2010
    } else {
      luk =2015
    }
    # luk = 2010
    
    f = rv$fonts
    
    koke = koke1()
    koke = as.data.table(koke)
    koke = koke[sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost",  "price") & year %in% C(luk:2025)]
    kokn = koke[sector %in% c( "diff") & year %in% 2021]
    
    mi = as.numeric(unique(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"),min(maara, na.rm=TRUE)]))
    ma = as.numeric(unique(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"),max(maara, na.rm=TRUE)]))
    hi = ma-mi
    # ran =.07
    ba =5.25
    koke[,place := ma + ba + ran*0.064*hi]
    
    bg = "#e7e7e7"
    bg = "#D6D6D6"
    
    #e7e7e7
    
    scaleFactor <- unique(max(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"), maara], na.rm=T))/unique(max(koke[sector %in% c("cost"), maara], na.rm=T))
    
    koke[,hmaara:=maara*scaleFactor]
    
    gup = ggplot(data=koke, aes(x=year, group=sector, fill=col, color=col )) + 
      
      # geom_text(data=koke[year %in% c(2021) & sector %in% c("metsajapuu", "metsajapuuk", "diff", "price",  "cumu", "cost", "debt"),],
      #           aes(x=2020.5, y=place, label=lab), size=5, hjust=1, fontface="bold")+
      # 
      # 
      # geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("metsajapuu", "metsajapuuk", "diff"),],
      #           aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")), size=5, fontface="bold")+
      # 
      # 
      #     
      # geom_text(data=koke[year %in% c(2021:2025) & sector %in% c( "cost", "debt", "price"),],
      #           aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")), size=5, fontface="bold")+
      # 
    # geom_curve(
    #   aes(x = 3, y = 22, xend = 3.5, yend = 15),
    #   arrow = arrow(
    #     length = unit(0.03, "npc"), 
    #     type="closed" # Describes arrow head (open or closed)
    #   ),
    #   colour = "#EC7014",
    #   size = 1.2,
    #   angle = 90 # Anything other than 90 or 0 can look unusual
    # ) +
    # 
    
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("metsajapuuk"),],
                aes(x=year, y=place+.05*hi, label=year), size=5*f, fontface="bold", color ="black")+
     
      geom_rect(data=koke[year %in% c(2025) & sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost", "price"),],
                aes(xmin=luk, xmax=year+1.5+.9, ymax=(place+0.03*hi), ymin=(place-0.03*hi),
                    fill=col), 
                  size=0)+
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("metsajapuu", "metsajapuuk"),],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("diff") ,],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+
      # geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("diff") & maara < 0,],
      #           aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")),
      #           size=5*f, fontface="bold", color="green")+
      # geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("diff") & maara < 0,],
      #           aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")),
      #           size=5*f, fontface="bold", color="green")+
      
      
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c( "price"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
            geom_text(data=koke[year %in% c(2021:2025) & sector %in% c( "cost"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+
      
      
      geom_text(data=koke[year %in% c(2021) & sector %in% c("metsajapuu", "metsajapuuk", "diff", "price",  "cumu", "cost", "debt"),],
                aes(x=2020.2, y=place, label=paste0(lab, ":")), 
                size=5*f, hjust=1, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2021:2024) & sector %in% c("cumu"),],
                aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ",")),
                size=5*f, fontface="bold")+
      geom_label(data=koke[year %in% c(2025) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg, color="red"),
                 size=5*f, fontface="bold", alpha=0,label.size=1)+
      
      geom_label(data=koke[year %in% c(2025) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg), 
                 size=5*f, fontface="bold", alpha=0,label.size=0)+
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("metsajapuu", "metsajapuuk"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,decim), nsmall=decim, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c( "diff"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,decim), nsmall=decim, decimal.mark = ","), color=col3),
                size=5*f, fontface="bold")+
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c( "price"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,0), nsmall=0, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("cost"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,0), nsmall=0, decimal.mark = ","), color=col3),
                size=5*f, fontface="bold")+
      
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
                aes(x=year+1.5, y=place+.05*hi, label="Kertymä"), size=5*f, fontface="bold", color ="black")+
      
      
      geom_rect(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
                aes(xmin=year+1.5-.9, xmax=year+1.5+.9, ymax=(place+1*0.05*hi)+.05*hi, ymin=(place-6*0.06*hi)+.06*hi), 
                color="red", alpha=0, size=1)+
      
      
      geom_curve(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
        aes(x = year+2.6, y = place, xend = year+2.6, yend = place-2*0.064*hi+1*0.014*hi),
        arrow = arrow(
          length = unit(.01, "npc"), 
          type="closed" # Describes arrow head (open or closed)
        ),
        colour = "black",
        size = 1.2,curvature = -0.8,
        angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      geom_curve(data=koke[year %in% c(2025) & sector %in% c("metsajapuu"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-1*0.064*hi+1*0.014*hi),
                 arrow = arrow(
                   length = unit(.015, "npc"), 
                   type="open" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      
      geom_curve(data=koke[year %in% c(2025) & sector %in% c("diff"),],
                 aes(x = year+2.6, y = place-1*0.014*hi, xend = year+2.6, yend = place-2*0.064*hi),
                 arrow = arrow(
                   length = unit(.015, "npc"), 
                   type="open" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      geom_curve(data=koke[year %in% c(2025) & sector %in% c("price"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-1*0.064*hi),
                 arrow = arrow(
                   length = unit(.01, "npc"), 
                   type="closed" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      
      
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("diff"),],
                aes(x=2024.7, y=ma, 
                    label=
                      "
                 Huomioita:
                
                 Ei sisällä muita maankäyttösektorin 
                 alasektoreita ja niiden tavoitteita. 
                
                 Vuodesta 2026 alkaen 
                 maankäyttösektoria käsitellään
                 kokonaisuutena (alempi osio). 
                
                 Nielukiintiö perustuu 
                 Luonnonvarakeskuksen arvioon 
                 ja tulee tarkentumaan.
                 Myös päästötilastot tarkentuvat.
                 
                 "), size=3.5*f,color="black", fontface="bold", hjust=0, vjust=1)+
      
      
      
      geom_col(data=koke[year %in% c(luk:2025) & sector %in% c("metsajapuuk")], 
               aes(x=year, y = maara,fill=col, color=col, width=si), 
               stat='identity', position='stack' )+
      
      geom_col(data=koke[year %in% c(luk:2025) & sector %in% c("metsajapuu"),], 
               aes(x=year, y = maara,fill=col, width=si), 
               stat='identity', position='stack', alpha=.99 )+ 
      
      geom_col(data=koke[year %in% c(luk:2025) & sector %in% c("cumu"),], 
               aes(x=year, y = maara,fill=col, width=si), 
               stat='identity', position='stack', alpha=.99 )+
      
      geom_col(data=koke[year %in% c(luk:2025) & sector %in% c("diff"),], 
               aes(x=year, y = maara,fill=col,  width=si, col=col2), 
               stat='identity', position='stack', linewidth =.6 )+
      
      # geom_area(data=koke[sector %in% c("cost"),], alpha=.3)+
      
      geom_point(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=4, alpha=.6)+
      geom_line(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=1.5)+
      # geom_area(data=koke[sector %in% c("cost"),], aes(y=hmaara),  alpha=.4)+
      
      # geom_line(data=koke[sector %in% c("cost"),], aes(y=maara),  size=2)+
      # 
      # geom_point(data=koke[sector %in% c("cost"),],aes(y=maara*scaleFactor/scaleFactor),   size=2)+
      # geom_line(data=koke[sector %in% c("debt"),],  aes(y=hmaara),  size=3,alpha=.6)+
      
      geom_point(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=4, alpha=.6)+
      geom_line(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=1.5)+      
      
      geom_hline(aes(yintercept=0), size=.4, color="black", linetype="dashed")+
      
      coord_cartesian(xlim=c(luk,  2032), 
                      # ylim=c(mi, max(90, ma)),
                      clip ="off") +
      
      labs(caption =c("Data: Tilastokeskus, Luonnonvarakeskus, omat laskelmat.  ", 
                      "Kuva: villeseppala.github.io/EU-EmissionAllocations")) +
      scale_y_continuous(name= "Päästöt, miljoonaa tCO2-ekvivalenttia",sec.axis=sec_axis(~./scaleFactor, name="Kustannukset, miljoonaa euroa"))   +
      
      scale_x_continuous(breaks =seq(luk, 2030, by=1))   +
      # scale_y_continuous(breaks =seq(mi, ma, by=10))   +
      
      scale_alpha_identity() + 
      
      scale_fill_identity() + 
      scale_color_identity() +
      theme(
        plot.caption=element_text(size =10*f, hjust =c(0,1) , family = "merriweather sans",
                                  lineheight=.85 ,color=c("black","black")),
        
        axis.text.x = element_text(size=15*f), 
        # legend.text=element_text(size =14, color=teksvari),
        # axis.text.y= element_blank(),
        axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue", size=15*f),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red", size=15*f),
         axis.title.x= element_blank(),
        plot.background = element_rect(fill =bg ,color="grey"), 
        panel.background = element_rect(fill = bg, color="grey"), 
        # axis.title.x=element_blank(),
        legend.title=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_line(color="grey"),
        legend.background = element_rect(fill=bg, size=0, color=bg),
        # legend.text=element_text(color=teksvari)
        # plot.margin = unit(c(-10,-25,-15,-65), "mm"),
        # legend.position = c(.53,.535)
      )
    
    # gup =     gup+    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
    
    gup
    # +
    #   geom_point(data=koke[sector %in% c("lulucf"),], aes(y=tse), color="green") + 
    #   geom_point(data=koke[sector %in% c("metsajapuu"),], aes(y=tse), color="green")  +
    # 
    # 
    # #   
    #    geom_point(data=koke[year %in% c(2021:2030) & sector %in% c("lulucf", "metsajapuu"),], aes(y=meno), color="purple")
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
