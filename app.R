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





koki = read
lyear = koki[sector == "metsajapuu" & maara <1000000, max(year)]





koki1 = koki[sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost", "cumu", "cost", "debt", "price"),]
koki2 = koki[sector %in% c("lulucf", "lallocation", "diff", "cost", "cumu"),]

ui <- fluidPage(
  
  
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
                  .js-irs-0 .irs-bar  {background: purple}  .js-irs-0 .irs-single {background: purple}; 
                                    .js-irs-2 .irs-bar {background: purple}; 

                  .js-irs-0 .irs-max {background-color: transparent !important font-size: 5vw} 
                  hr {
  border-top: 1px solid grey; padding: 0p; margin: 0px;
}
                  .irs--shiny .irs-min, .irs--shiny .irs-max {
    
    font-size: 13px;
    
    background-color: transparent;

                  }
.irs--shiny .irs-single { font-size: 15px; background: transparent; color:purple}; 


")),
  tags$style(HTML(" .js-irs-1 .irs-single .irs-bar {background: #4ba180};" )),
  tags$style(HTML(" .js-irs-3 .irs-bar {background: #4ba180} .js-irs-3 .irs-single {background: #bc810d};" )),
  tags$style(HTML(" .js-irs-2 .irs-bar {background: #bc810d} .js-irs-2 .irs-single {background: #bc810d};" )),
  tags$style(HTML(" .js-irs-0 .irs-bar {background: #4ba180} .js-irs-4 .irs-single {background: #bc810d};" )),
  
  # setSliderColor(c("green", "green","orange","green", "green","orange","green", "green","orange"),c(1,2,3,4,5,6,7,8,9)),
  #e7e7e7
  # App title ----
  # title = "Hello Shiny!",
  div(          style = " background-color:#484B4D!important;    ",
                
                fluidRow( style = " background-color:#D6D6D6!important; padding: -.2vw; margin: -.1vw; ",
                          h5(
                            tags$b("Metsämaan ja puutuotteiden tavoitteet ja päästöt kaudella 2021-2025", 
                                   style="color: blue; padding: 1vw;")
                          ),
                          
                          column(3,          
                                 
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
                          column(9, 
                                 div(
                                   # style="  box-shadow: 15px 15px 14px grey inset; padding: 19px;",
                                   
                                   # style = " background-color:grey!important;",
                                   plotOutput(outputId = "plotmetsajapuu", height = "450px")
                                 )
                          )
                          
                ), 
                hr(),
                
                # 
                # fluidRow(
                #   h5(
                #     tags$b("Maankäyttösektorin kiintiöt ja päästöt kaudella 2026-2030", 
                #            style="color: blue;")
                #   ),
                #   column(2, 
                #          # title("Aseta metsämaan nielu"),
                #          shinyWidgets::autonumericInput(
                #            inputId = "lulucf2026", 
                #            label = "Nettopäästö/nielu 2026:", 
                #            value = 0, 
                #            # min=-40, max=20,
                #            currencySymbol = " Mt",
                #             currencySymbolPlacement = "s",
                #            decimalPlaces = 1
                #          ),
                #          # numericInput("lulucf2026", "Aseta maankäyttösektorin nettopäästöt 2026", 0,min=-40, max=20),
                #          # numericInput("lulucf2030", "Nettopäästö/nielu 2026:", -10,min=-40, max=20),
                #          sliderTextInput(
                #            "lulucf2030", label = "Nettonielu 2030:", 
                #            choices = seq(from = 5, to = -25, by = -1),
                #            selected = -14,
                #            width = "100%",
                #           
                #            post = " Mt"
                #          ),
                #          
                #          # sliderInput("lulucf2030", label = "Nettonielu 2030:", 
                #          #             value=-14, min=-35, max=0,post = " Mt" 
                #          #             # style = "background: black;"
                #          # ),
                #          # sliderInput("lulucf2030", "Aseta maankäyttösektorin nettopäästöt 2030", -10,min=-40, max=20),
                #          
                #          h5(
                #            tags$b("2027-2029-nielu edellisten kehityskulkuna", 
                #                   style="color: green;")
                #          ),
                #          
                #          numericInput("pricepost", "Yksiköiden keskihinta 2026-2030:", 20,min=0, max=200),
                #          
                #   ), 
                #   column(10, 
                #          plotOutput(outputId = "plotmaankaytto")
                #          
                #   )
                #   
                # )
                # 
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # read kok
  # 
  
  
  
  
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
  # emis = 
  # 
  
  
  # seq2= seq(input$lulucf2026, input$lulucf2030, length.out = 5)
  # koki[year %in% c(2026:2030) & sector =="lulucf", maara := seq2]
  # 
  # koks= koks[year %in% c(2026:2030), maara := koki[year %in% c(2026:2030) &sector=="lulucf", maara, on=c("year")] - koki[year %in% c(2026:2030) & sector=="lallocation", maara, on=c("year")]]
  # kokl = kokl[year %in% c(2026:2030), maara := cumsum(maara)]
  # 
  
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
    
    
    koke = koke1()
    koke = as.data.table(koke)
    koke = koke[sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost",  "price") & year %in% C(luk:2025)]
    kokn = koke[sector %in% c( "diff") & year %in% 2021]
    
    mi = as.numeric(unique(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"),min(maara, na.rm=TRUE)]))
    ma = as.numeric(unique(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"),max(maara, na.rm=TRUE)]))
    hi = ma-mi
    # ran =.07
    ba =5.25
    koke[,place := ma + ba + ran*0.05*hi]
    
    bg = "#e7e7e7"
    
    scaleFactor <- unique(max(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"), maara], na.rm=T))/unique(max(koke[sector %in% c("cost"), maara], na.rm=T))
    
    koke[,hmaara:=maara*scaleFactor]
    
    gup = ggplot(data=koke, aes(x=year, group=sector, fill=col, color=col )) + 
      
      geom_text(data=koke[year %in% c(2021) & sector %in% c("metsajapuu", "metsajapuuk", "diff", "price",  "cumu", "cost", "debt"),],
                aes(x=2020.5, y=place, label=lab), size=5, hjust=1, fontface="bold")+
      
      
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("metsajapuu", "metsajapuuk", "diff"),],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")), size=5, fontface="bold")+
      
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,decim), nsmall=decim, decimal.mark = ",")), size=5, fontface="bold")+
      
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c( "cost", "debt", "price"),],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")), size=5, fontface="bold")+
      
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("metsajapuuk"),],
                aes(x=year, y=place+.05*hi, label=year), size=5, fontface="bold", color ="black")+
      geom_text(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
                aes(x=year+1.5, y=place+.05*hi, label="Kertymä"), size=5, fontface="bold", color ="black")+
      
      geom_rect(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
                aes(xmin=year+1.5-.9, xmax=year+1.5+.9, ymax=(place+1*0.05*hi)+.05*hi, ymin=(place-6*0.05*hi)+.06*hi), 
                color="red", alpha=0, size=1)+
      
      
      
      geom_text(data=koke[year %in% c(2021:2024) & sector %in% c("cumu"),],
                aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ",")), size=5, fontface="bold")+
      geom_label(data=koke[year %in% c(2025) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg, color="red"), size=5, fontface="bold", alpha=0,label.size=1)+
      geom_label(data=koke[year %in% c(2025) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg), size=5, fontface="bold", alpha=0,label.size=0)+
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("cumu"),],
                aes(x=2024.7, y=ma, 
                    label=
                      "
                 Huomioita:
                
                 Ei sisällä muita maankäyttösektorin 
                 alasektoreita ja niiden tavoitteita. 
                 Niiden vaikutus kuitenkin pieni 
                 kokonaiskustannukseen. 
                
                 Vuodesta 2026 alkaen 
                 maankäyttösektoria käsitellään
                 kokonaisuutena (alempi osio). 
                
                 Nielukiintiö perustuu 
                 Luonnonvarakeskuksen arvioon 
                 ja tulee tarkentumaan.
                 Myös päästötilastot tarkentuvat.
                 
                 "), size=3.5,color="black", fontface="bold", hjust=0, vjust=1)+
      
      
      
      
      
      
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
               aes(x=year, y = maara,fill=col,  width=si), 
               stat='identity', position='stack', linewidth =.6 )+
      
      # geom_area(data=koke[sector %in% c("cost"),], alpha=.3)+
      
      geom_point(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=4, alpha=.6)+
      geom_line(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=1.5)+
      # geom_area(data=koke[sector %in% c("cost"),], aes(y=hmaara),  alpha=.4)+
      
      # geom_line(data=koke[sector %in% c("cost"),], aes(y=maara),  size=2)+
      # 
      # geom_point(data=koke[sector %in% c("cost"),],aes(y=maara*scaleFactor/scaleFactor),   size=2)+
      geom_line(data=koke[sector %in% c("debt"),],  aes(y=hmaara),  size=3,alpha=.6)+
      
      geom_hline(aes(yintercept=0), size=.4, color="black", linetype="dashed")+
      
      coord_cartesian(xlim=c(luk,  2030), 
                      # ylim=c(mi, max(90, ma)),
                      clip ="off") +
      scale_y_continuous(name= "Päästöt, miljoonaa tCO2-ekvivalenttia",sec.axis=sec_axis(~./scaleFactor, name="Kustannukset, miljoonaa euroa"))   +
      
      scale_x_continuous(breaks =seq(luk, 2030, by=1))   +
      # scale_y_continuous(breaks =seq(mi, ma, by=10))   +
      
      scale_alpha_identity() + 
      
      scale_fill_identity() + 
      scale_color_identity() +
      theme(
        axis.text.x = element_text(size=15), 
        # legend.text=element_text(size =14, color=teksvari),
        # axis.text.y= element_blank(),
        axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue", size=15),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red", size=15),
        # axis.title.y= element_blank(),
        plot.background = element_rect(fill =bg ), 
        panel.background = element_rect(fill = bg), 
        # axis.title.x=element_blank(),
        legend.title=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
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
  
  # output$plotmaankaytto <- renderPlot({
  #   koke = koke()
  #   koke = as.data.table(koke)
  #   koke = koke[sector %in% c("lulucf", "lallocation", "diff", "cost", "cumu") ]
  #   
  # koke =   koke[!(sector %in% c("diff", "cost", "cumu") & year %in% c(2021:2025)),] 
  #   
  #   ggplot(data=koke, aes(x=year, y=maara, group=sector, fill=col1, color=col1 )) + 
  #     geom_col(data=koke[sector %in% c("lulucf") & year %in% c(2010:2023, 2026:2030),], 
  #              aes(x=year, y = maara,fill=col1, width=wi), 
  #              stat='identity', position='stack', alpha=.99 )+
  #     # geom_col(data=koke[year %in% c(2010:2025) & sector %in% c("metsajapuu"),], 
  #     #          aes(x=year, y = maara,fill=col1, width=wi), 
  #     #          stat='identity', position='stack', alpha=.99 )+
  #     geom_point(data=koke[sector %in% c("lallocation"),])+ 
  #     
  #     geom_point(data=koke[sector %in% c("diff"),], color="red", size=4)+
  #     # geom_line(data=koke[sector %in% c("cost"),], color="cyan")+
  #     geom_line(data=koke[sector %in% c("cumu"),], color="purple", size=4)+
  #     # geom_line(data=koke[sector %in% c("cost"),], color="cyan")+
  #     
  #     
  #     scale_fill_identity() + 
  #     scale_color_identity() 
  #   # +
  #   #   geom_point(data=koke[sector %in% c("lulucf"),], aes(y=tse), color="green") + 
  #   #   geom_point(data=koke[sector %in% c("metsajapuu"),], aes(y=tse), color="green")  +
  #   # 
  #   # 
  #   # #   
  #   #    geom_point(data=koke[year %in% c(2021:2030) & sector %in% c("lulucf", "metsajapuu"),], aes(y=meno), color="purple")
  # })
  # 
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
