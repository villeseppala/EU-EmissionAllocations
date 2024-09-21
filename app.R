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
debt ="#540772"
allo = "#262ac5"
emis = "#1baa5b"
price = "#e22c8c"

cumc ="#1f7fe1"
cumc ="#0e5faa"

debt ="#6f0c72"
allo = "#3111ab"
emis = "#13aa82"
price = "#c66910"
cu = "#df16e2"


emis = "#8b513b"

allo ="#518161"
cumc ="#6b364a"

debt = "#38553f"

price = "#a6a24c"

re = "#ef4205"
  gr = "#8fff4a"
  dgr = "darkgreen"
  
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
koki[sector =="lallocation", col :=allo]
koki[sector =="lulucf", col :=emis]
koki[sector =="cost", col :=debt]
koki[sector =="debt", col :=debt]
koki[sector =="price", col :=price]
koki[sector =="allocation", col :=allo]
koki[sector =="esd", col :=emis]

lyear = koki[sector == "metsajapuu" & maara <1000000, max(year)]


#https://julkaisut.valtioneuvosto.fi/bitstream/handle/10024/165717/VNTEAS_2024_26.pdf?


#v = Ylitukset: Kiintiötarve = vertailutaso - nielu
# Hoidettu viljelysmaa


koki1 = koki[sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost", "cumu", "cost", "debt", "price"),]
koki2 = koki[sector %in% c("lulucf", "lallocation", "diff", "cost", "cumu","cost", "debt", "price"),]
koki3 = koki[sector %in% c("esd", "allocation", "diff", "cost", "cumu","cost", "debt", "price"),]
koki4 = koki[sector %in% c("esd", "allocation", "diff", "cost", "cumu","cost", "debt", "price"),]


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
  
  
  tags$script(HTML("
                     
document.querySelector('input').addEventListener('input', function(){
  document.querySelector('.box').scrollLeft = this.value;
})
                                

                    ")),
  
  
  
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
  tags$style(HTML(" .js-irs-3 .irs-bar {background:var(--coldiff)}
                  .js-irs-3 .irs-single {background: var(--coldiff)};" )),
tags$style(HTML(" .js-irs-4 .irs-bar {background:var(--colemis)}
                  .js-irs-4 .irs-single {background: var(--colemis)};" )),
tags$style(HTML(" .js-irs-5 .irs-bar {background:var(--colemis)}
                  .js-irs-5 .irs-single {background: var(--colemis)};" )),
tags$style(HTML(" .js-irs-6 .irs-bar {background:var(--colprice)}
                  .js-irs-6 .irs-single {background: var(--colprice)};" )),
tags$style(HTML(" .js-irs-7 .irs-bar {background:var(--colemis)}
                  .js-irs-7 .irs-single {background: var(--colemis)};" )),
tags$style(HTML(" .js-irs-8 .irs-bar {background:var(--colemis)}
                  .js-irs-8 .irs-single {background: var(--colemis)};" )),
tags$style(HTML(" .js-irs-9 .irs-bar {background:var(--colprice)}
                  .js-irs-9 .irs-single {background: var(--colprice)};" )),
tags$style(HTML(" .js-irs-10 .irs-bar {background:var(--colemis)}
                  .js-irs-10 .irs-single {background: var(--colemis)};" )),
tags$style(HTML(" .js-irs-11 .irs-bar {background:var(--colemis)}
                  .js-irs-11 .irs-single {background: var(--colemis)};" )),
tags$style(HTML(" .js-irs-12 .irs-bar {background:var(--colprice)}
                  .js-irs-12 .irs-single {background: var(--colprice)};" )),
  # tags$style(HTML(" .js-irs-2 .irs-bar,  .js-irs-2 .irs-single {background: #bc810d};")),
  # tags$style(HTML("
  #                 .js-irs-4 .irs-single {background: #bc810d};" )),
  # 
# dashboardHeader(title = "My Dashboard"),




uiOutput("css_style2"),









fluidRow(
#   column(1,
#          actionButton(inputId = "eng", label = NULL, style = "width: 2.5rem; height: 1.5rem; margin: 3px;
# background: url('flag/eng.png');  background-size: cover; background-position: center;"), 
#   ) , 

  column(10,
  h3(style="padding-top: .1vw; ",
     tags$b("Suomen EU-päästötavoitteista poikkeamisen kustannusten ja tuottojen hahmottaminen", 
            style="color: white; padding: 1vw;")
  ) ), 
  column(2,
  tagList(a
          (div(style="padding-top: 1.9vw; font-size: 15px !important; left-margin: 5px; color: white;",icon("fab fa-github"))
            # ))
            , 
            href="https://github.com/villeseppala/EU-EmissionAllocations")
  )
  )
), 


fluidRow(  div(
  style = " background-color:#D6D6D6!important;    ",

  style = "padding: .2vw; font-size: 15px !important; margin: 15px;",
  
  checkboxInput("inf", "Mikä tämä sivu on?", value =F),
  conditionalPanel( condition = "input.inf == true",
                    
                    htmlOutput("infoa")),
  
  checkboxInput("more", "Mitä sivu ei toistaiseksi huomioi + muita kehitysideoita", value =F),
  conditionalPanel( condition = "input.more == true",
                                              
                                              htmlOutput("morea"))
  
  )

), 
















div(  style = " background-color:#D6D6D6!important;    ",
      
 
              h3(style="padding-top: .3vw; ",
                tags$b("Maankäyttösektori 2021-2025", 
                       style="color: var(--coltit); padding: 1vw;")
              ), 
                          
                fluidRow( 
                  style = "background-color:#D6D6D6!important; border-color: black; border-style: solid; border-size: .1px; padding: .2vw; margin: .5vw;",
                                            h4(
                            tags$b("Metsämaa ja puutuotteet kaudella 2021-2025", 
                                   style="color: black; padding: 1vw;")
                          ),
  
  column(3, style = "     padding-right: 10px;
    padding-left: 5px;",
         
         
   
    # div(        
    #   style = "  margin-right: -0.2vw; padding: .1vw;",
   
      
    
      uiOutput(class= "boxy",style = " background-color:var(--colallo); ",
outputId = "kiin"
),
hr(),

uiOutput(   class= "boxy",     style = " background-color:var(--colemis);",
                 outputId = "alu"),
hr(),

      div(        class="slidy", 
        style = " background-color:var(--colemis);",
      sliderTextInput(
        "lulucf2024", label = "2024 nettopäästöt:", 
        choices = seq(from = -35, to = 0, by = 1),
        selected = -14,
        width = "100%",
        post = " Mt",
        grid =T
      )),
      hr(),
      div(          class="slidy",  
                    style = " background-color:var(--colemis);",
                    
        # style = "color:#4ba180;",
        
      sliderTextInput(
        "lulucf2025", label = "2025 nettopäästöt:", 
        choices = seq(from = -35, to = 0, by = 1),
        selected = -14,
        width = "100%",
        post = " Mt",
        grid=T
      )),
      hr(),

      # h5(
      #   tags$b("2021-2023-nielu alustavista tilastoista",    )),
      
      # h5(
      #   tags$b("Kauden 2021-2025 nettonielu",   ))  ),
# border-color: var(--colcu);
      # div(style = "background-color:var(--coldiff);",

uiOutput(class= "boxy",style = " background-color:var(--colemis); ",
         outputId = "emisa"
),

          uiOutput(class= "boxy", style = "background-color:var(--coldiff);","difa")
,
      
      hr(),
    
      div(  class="slidy",  
            style = " background-color:var(--colprice)!important;",
            sliderTextInput("pricepre", label = "Yksiköiden keskihinta:", 
                           choices = seq(from = 0, to = 100, by = 1),
                           selected = 30,
                           width = "100%",
                           post = " €/t",
                           grid =T
                       )),
# border-color: var(--colcu);

        uiOutput(class= "boxy",style = "background-color:var(--colcost);","pula"),
    hr(),
    hr(),
  # )
),
  
  

  column(9, class="scrolly",
   #       style = 'overflow-x: scroll;  align-items:left;
   # ',
  # div(
    # style = 'overflow-x: scroll;  direction: rtl;   ',
    # style = "box-shadow: inset -5px -5px 10px 3px #e7e7e7, inset 5px 5px 10px 3px; padding:8px;",
      # div(style = "padding:8px;",
       plotOutput(outputId = "plotmetsajapuu", height = "550px", width = "1100px")
      # ) 
    # )
  )),





# div(
    
    fluidRow(style = "background-color:#D6D6D6!important; border-color: black; border-style: solid; border-size: .1px; padding: .2vw; margin: .5vw;",
      column(3,style = "     padding-right: 10px;
    padding-left: 5px;",
             div(   class="slidy",         
                    style = "background-color:var(--coldiff);",
               sliderTextInput(
                 "maa2025", label = "Maankäyttösektorin muiden tilinpitoluokkien ylitys kaudelta:", 
                 choices = seq(from = 0, to = 30, by = 1),
                 selected = 14,
                 width = "100%",
                 post = " Mt",
                 grid=T
                  )),
             
             checkboxInput("mores", "Näytä lisätietoja 14 Mt oletukselle", value =F),
             conditionalPanel( condition = "input.mores == true",
                               
                               htmlOutput("moress")
                               
             )
               ),

      
      column(9,
             # div(style = "border-color: var(--colcu); border-style: solid; border-size: .1px; padding: .2vw;",
                 uiOutput(class="boxy",style = "background-color:var(--colcost); color: white;","totu")
             # )
      ,
             h5(
               tags$b("Huom: Laskelma ei huomioi Suomen mahdollisia joustoja, joita on saatavilla vain mikäli EU kokonaisuutena pääsee tavoitteisiinsa.", 
                  )), 
      )
      # ),


)
),







div(          style = " background-color:#D6D6D6!important;    ",
              
              
              h3(style="padding-top: .3vw; ",
                tags$b("Maankäyttösektori 2026-2030", 
                       style="color: var(--coltit); padding: 1vw;")
              ), 
              
              fluidRow( 
                style = "padding: .2vw; margin: .5vw;",
                # h5(
                #   tags$b("Maankäyttösektorin tavoitteet ja päästöt kaudella 2026-2030", 
                #          style="color: blue; padding: 1vw;")
                # ),
                
                # Sidebar panel for inputs ----
                column(3,
                       style = "     padding-right: 10px;
    padding-left: 5px;",
                       # div(        
                       #   style = "  margin-right: -0.2vw; padding: .2vw;",
                         
                         uiOutput(class= "boxy",style = " background-color:var(--colallo); ",
                                  outputId = "kiinb"
                         ),
                         
    # A
                         
                         
                         div(        class="slidy", 
                                     style = " background-color:var(--colemis);",
                                     sliderTextInput(
                                       "lulucf2026", label = "2026 nettopäästöt:", 
                                       choices = seq(from = -25 , to =10, by = 1),
                                       selected = -3,
                                       width = "100%",
                                       post = " Mt",
                                       grid =T
                                     )),
                         hr(),
                         div(          class="slidy",  
                                       style = " background-color:var(--colemis);",
                                       
                                       sliderTextInput(
                                         "lulucf2030", label = "2030 nettopäästöt:", 
                                         choices = seq(from = -25 , to = 10, by = 1),
                                         selected = -7,
                                         width = "100%",
                                         post = " Mt",
                                         grid=T
                                       )),
                         hr(),
                         
                         
                         # uiOutput(class= "boxy",style = " background-color:var(--colallo); ",
                         #          outputId = "kiin"
                         # ),
                         
                         # div(
                           p(class= "boxy",style = " background-color:var(--colemis); ","2027-2029 arvot laskettu valintojen lineaarisena kehityksenä",  
                             # )
    ),
                         
    
                         # h5(
                         #   tags$b("Kauden 2021-2025 nettonielu",   )) 
                         # ),
                       # div(                    style = "background-color:var(--coldiff);
                       #                         ",
    
    uiOutput(class= "boxy",style = " background-color:var(--colemis); ",
             outputId = "emisb"
    ),
                           uiOutput( class= "boxy",style = "background-color:var(--coldiff);","difab"),
                       
                       hr(),
                       
                       div(  class="slidy",  
                             style = " background-color:var(--colprice)!important;",
                             sliderTextInput("pricepost", label = "Kauden yksiköiden keskihinta:", 
                                             choices = seq(from = 0, to = 100, by = 1),
                                             selected = 30,
                                             width = "100%",
                                             post = " €/t",
                                             grid =T
                             )),
                       hr(),
                       
                       # div(style = "border-color: var(--colcu); border-style: solid; border-size: .1px; padding: .2vw;",
                           uiOutput(class= "boxy", style = "background-color:var(--colcost); ","pulab")
                           # )
                       ,
                       hr(),
                       hr(),
                ),
                
                
                column(9,class="scrolly",
                       
                       # div(
                         plotOutput(outputId = "plotlulucf", height = "550px", width = "1100px")
                       # ), 
                )
              ),

), 










div(          style = " background-color:#D6D6D6!important;    ",
              
              
              h3(style="padding-top: .3vw; ",
                 tags$b("Taakanjakosektori 2021-2025", 
                        style="color: var(--coltit); padding: 1vw;")
              ), 
              
              fluidRow( 
                style = "padding: .2vw; margin: .5vw;",
                # h5(
                #   tags$b("Maankäyttösektorin tavoitteet ja päästöt kaudella 2026-2030", 
                #          style="color: blue; padding: 1vw;")
                # ),
                
                # Sidebar panel for inputs ----
                column(3,style = "     padding-right: 10px;
    padding-left: 5px;",
                       
                       # div(        
                       #   style = "  margin-right: -0.2vw; padding: .2vw;",
                       #   
                         
                         uiOutput(class= "boxy",style = " background-color:var(--colallo); ",
                                  outputId = "kiinc"
                         ),
                         
                         p(class= "boxy",style = " background-color:var(--colemis); ","2021-2023-päästöt alustavista tilastoista"),  
                           
                         div(        class="slidy", 
                                     style = " background-color:var(--colemis);",
                                     sliderTextInput(
                                       "esd2024", label = "2024 päästöt:", 
                                       choices = seq(from = 10, to = 30, by = 1),
                                       selected = 24,
                                       width = "100%",
                                       post = " Mt",
                                       grid =T
                                     )),
                         hr(),
                         div(          class="slidy",  
                                       style = " background-color:var(--colemis);",
                                       
                                       sliderTextInput(
                                         "esd2025", label = "2025 päästöt:", 
                                         choices = seq(from =  10, to = 30, by = 1),
                                         selected = 23,
                                         width = "100%",
                                         post = " Mt",
                                         grid=T
                                       )),
                         hr(),
                         # h5(
                         #   tags$b("2021-2023-päästöt laskettu lineaarisena kehityksenä",    )),
                         
                         # h5(
                         #   tags$b("Kauden 2021-2025 nettonielu",   )) 
                       # ),
                       uiOutput(class= "boxy",style = " background-color:var(--colemis); ",
                                outputId = "emisc"
                       ),
                       
                       # div(style = "border-color: var(--colcu); border-style: solid; border-size: .1px; padding: .2vw;",
                           uiOutput(class= "boxy",style = "background-color:var(--coldiff);","difac"),
                       
                       hr(),
                       
                       div(  class="slidy",  
                             style = " background-color:var(--colprice)!important;",
                             sliderTextInput("priceesdpre", label = "Kauden yksiköiden keskihinta:", 
                                             choices = seq(from = 0, to = 200, by = 1),
                                             selected = 50,
                                             width = "100%",
                                             post = " €/t",
                                             grid =T
                             )),
                       # div(style = "border-color: var(--colcu); border-style: solid; border-size: .1px; padding: .2vw;",
                           uiOutput(class= "boxy",style = "background-color:var(--colcost);","pulac"),
                       hr(),
                       hr(),
                       hr(),
                ),
                
                
                column(9,class="scrolly",
                       
                       # div(
                         plotOutput(outputId = "plotesdpre", height = "550px", width = "700px")
                       # ), 
                )
              ),
              
)
,









div(          style = " background-color:#D6D6D6!important;    ",
              
              
              h3(style="padding-top: .3vw; ",
                 tags$b("Taakanjakosektori 2026-2030", 
                        style="color: var(--coltit); padding: 1vw;")
              ), 
              
              fluidRow( 
                style = "padding: .2vw; margin: .5vw;",
                # h5(
                #   tags$b("Maankäyttösektorin tavoitteet ja päästöt kaudella 2026-2030", 
                #          style="color: blue; padding: 1vw;")
                # ),
                
                # Sidebar panel for inputs ----
                column(3,style = "     padding-right: 10px;
    padding-left: 5px;",
                       
                       # div(        
                       #   style = "  margin-right: -0.2vw; padding: .2vw;",
                         
                         
                         uiOutput(class= "boxy",style = " background-color:var(--colallo); ",
                                  outputId = "kiind"
                         ),
                         
                         div(        class="slidy", 
                                     style = " background-color:var(--colemis);",
                                     sliderTextInput(
                                       "esd2026", label = "2026 päästöt:", 
                                       choices = seq(from = 10, to = 25, by = 1),
                                       selected = 22,
                                       width = "100%",
                                       post = " Mt",
                                       grid =T
                                     )),
                         hr(),
                         div(          class="slidy",  
                                       style = " background-color:var(--colemis);",
                                       
                                       sliderTextInput(
                                         "esd2030", label = "2030 päästöt:", 
                                         choices = seq(from =  10, to = 25, by = 1),
                                         selected = 19,
                                         width = "100%",
                                         post = " Mt",
                                         grid=T
                                       )),
                         hr(),
                         # h5(class= "boxy",style = " background-color:var(--colemis); color:white",
                            p(class= "boxy",style = " background-color:var(--colemis); ","2027-2029-päästöt laskettu lineaarisena kehityksenä"  ),
                       hr(),
                       
                         # h5(
                         #   tags$b("Kauden 2021-2025 nettonielu",   )) 
                       # ),
                       
                       uiOutput(class= "boxy",style = " background-color:var(--colemis); ",
                                outputId = "emisd"
                       ),
                       hr(),
                       
                       # div(style = "border-color: var(--colcu); border-style: solid; border-size: .1px; padding: .2vw;",
                           uiOutput(class= "boxy",style = "background-color:var(--coldiff);","difad"),
                       
                       hr(),
                       
                       div(  class="slidy",  
                             style = " background-color:var(--colprice)!important;",
                             sliderTextInput("priceesdpost", label = "Kauden yksiköiden keskihinta:", 
                                             choices = seq(from = 0, to = 200, by = 1),
                                             selected = 50,
                                             width = "100%",
                                             post = " €/t",
                                             grid =T
                             )),
                       # div(style = "border-color: var(--colcu); border-style: solid; border-size: .1px; padding: .2vw;",
                           uiOutput(class= "boxy",style = "background-color:var(--colcost);","pulad"),
                       hr(),
                       hr(),
                ),
                
                
                column(9,class="scrolly",
                       
                       # div(
                         plotOutput(outputId = "plotesdpost", height = "550px", width = "800px")
                       # ), 
                )
              ),
              
)

)






# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  
  rv <- reactiveValues(cums =0)
  rv <- reactiveValues(cul ="Kustannus")
  rv <- reactiveValues(view =2)
  rv <- reactiveValues(ale =2)
  
  rv <- reactiveValues(fonts =.95)
  
  
  
  
  
  
  
  
  
  
  
  
  # observeEvent(input$fin,{
  #   rv$lang = c("fin")
  # }    )
  # 
  # observeEvent(input$eng,{
  #   rv$lang = c("eng")
  # }    )
  # 
  
  # serveriin|
    
    style2 <- reactive({
      # if (is.null(input$countr)) { 
      if (rv$ale ==1) {
        # if (input$nok =="EXTRA: Country profiles") { 
        ".scrolly {direction: rtl; overflow-x: scroll; }"
        
      }
      else if (rv$ale ==2) {
        ".scrolly {direction: ltr; overflow-x: scroll; }"    
      }
      
    }
    )
  
  output$css_style2 <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(style2()), collapse = "\n")
        )
      )
    )})
  
  # different setting for different screen sizes
  
  # observeEvent(input$dim, {
  # 
  #   if (input$dim[1] >1800) {
  #     rv$view = 1
  #   }
  #   else  if (input$dim[1] >1300 & input$dim[1] <1800) {
  #     rv$view = 2
  #   } else if (input$dim[1] <1300 & input$dim[1] >770) {
  #     rv$view = 3
  #      rv$fonts = .7
  #     # rv$fonts = input$dim[1]/1300
  #   } else if (input$dim[1] <770 & input$dim[1] >600) {
  #     rv$view = 3
  #     rv$fonts = .8
  #     # rv$fonts = input$dim[1]/1300
  #   }
  #   # else if (input$dim[1] <1300 & input$dim[1] >800) {
  #   #   rv$view = 3
  #   #   rv$fonts = .8
  #   #   # rv$fonts = input$dim[1]/1300
  #   # }
  #   #
  #   else {
  #     rv$view = 3
  #     rv$fonts = .5
  #     # rv$fonts = input$d
  # 
  #   }
  # })

  
  
  observeEvent(input$dim, {
    
    if (input$dim[1] >4800) {
      rv$view = 1
    }
    # else  if (input$dim[1] >1300 & input$dim[1] <1800) {
    #   rv$view = 2
    # } else if (input$dim[1] <1300 & input$dim[1] >770) {
    #   rv$view = 3
    #   rv$fonts = .7
    #   # rv$fonts = input$dim[1]/1300
    # } else if (input$dim[1] <770 & input$dim[1] >600) {
    #   rv$view = 3
    #   rv$fonts = .8
    #   # rv$fonts = input$dim[1]/1300
    # }
    # else if (input$dim[1] <1300 & input$dim[1] >800) {
    #   rv$view = 3
    #   rv$fonts = .8
    #   # rv$fonts = input$dim[1]/1300
    # }
    #
    else {
      rv$view =1
      rv$fonts = .85
      # rv$fonts = input$d
      
    }
    
    if (input$dim[1] <1500) {
      
      rv$ale = 1
    }
    else {
      rv$ale =2
      # rv$fonts = .85
      # rv$fonts = input$d
      
    }
    
  })
  
  
  
  output$difa= renderText({
    paste(
      rv$cul2,
      "\n",
      '<br><span style=\"background-color: var(--colallo); color: white;','\"><b>',  rv$cumallo,  
      " Mt ",'</b></span>'," - ",
      
      '<span style=\"background-color: var(--colemis); color: white;','\"><b>', rv$cumsink,  
      
      # '<span style=\"color:', "#bc810d", '\"><b>',  input$pricepre, "€/t ",
      " Mt ", '</span>',
      "= ",
      '<span style=\"color: white;', '\"><b>', 
      '<span style=\"color: white;','\"><b>',  rv$cums,  
      
      # format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      " Mt",'</b></span>',
      sep ="")})
  
  
  output$pula= renderText({
    paste(
      rv$cul1,
      "\n",
      '<br><span style=\"background-color: var(--coldiff); color: white;','\"><b>', rv$cumsaa,  
      " Mt ",'</b></span>'," x ",
      '<span style=\"background-color: var(--colprice); color: white;',  '\"><b>',  input$pricepre, " €/t ",'</b></span>',"= ",
      '<span style=\"background-color: var(--colcost); color: white;', '\"><b>', 
      format(round(input$pricepre*rv$cumsiaa,0), nsmall=0, decimal.mark=","),
      " milj. €",'</b></span>',
      sep ="")})
  
  
  
  output$difab= renderText({
    paste(
      rv$cul2b,
      "\n",
      '<br><span style=\"color: white; background-color: var(--colallo)','\"><b>',  rv$cumallob,  
      " Mt ",'</b></span>'," - ",
      
      '<span style=\"color: white; background-color: var(--colemis)','\"><b>',  rv$cumsinkb,  
      
      # '<span style=\"color:', "#bc810d", '\"><b>',  input$pricepre, "€/t ",
      " Mt ", '</span>',
      "= ",
      '<span style=\"color: white; background-color: var(--coldiff)', '\"><b>', 
      '<span style=\"color: white; background-color:var(--coldiff)','\"><b>',  rv$cumsb,  
      
      # format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      " Mt",'</b></span>',
      sep ="")})
  
  
  output$pulab= renderText({
    paste(
      rv$cul1b,
      "\n",
      '<br><span style=\"color: white; background-color: var(--coldiff)','\"><b>',  rv$cumsba,  
      " Mt ",'</b></span>'," x ",
      '<span style=\"color: white; background-color: var(--colprice)',  '\"><b>',  input$pricepost, " €/t ",'</b></span>',"= ",
      '<span style=\"color: white; background-color: var(--colcost)', '\"><b>', 
      format(round(input$pricepost*rv$cumsiba,0), nsmall=0, decimal.mark=","),
      " milj. €",'</b></span>',
      sep ="")})
  
  
  
  output$difac= renderText({
    paste(
      rv$cul2c,
      "\n",
      '<br><span style=\"color: white; background-color: var(--colallo)','\"><b>',  rv$cumalloc,  
      " Mt ",'</b></span>'," - ",
      
      '<span style=\"color: white; background-color: var(--colemis)','\"><b>',  rv$cumsinkc,  
      
      # '<span style=\"color:', "#bc810d", '\"><b>',  input$pricepre, "€/t ",
      " Mt ", '</span>',
      "= ",
      '<span style=\"color: white; background-color: var(--coldiff)', '\"><b>', 
      '<span style=\"color: white; background-color:var(--coldiff)','\"><b>',  rv$cumsca,  
      
      # format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      " Mt",'</b></span>',
      sep ="")})
  
  
  output$pulac= renderText({
    paste(
      rv$cul1c,
      "\n",
      '<br><span style=\"color: white; background-color: var(--coldiff)','\"><b>',  rv$cumsca,  
      " Mt ",'</b></span>'," x ",
      '<span style=\"color: white; background-color: var(--colprice)',  '\"><b>',  input$priceesdpre, " €/t ",'</b></span>',"= ",
      '<span style=\"color: white; background-color: var(--colcost)', '\"><b>', 
      format(round(input$priceesdpre*rv$cumsica,0), nsmall=0, decimal.mark=","),
      " milj. €",'</b></span>',
      sep ="")})
  
  output$kiin= renderText({
    paste(
      
      "2021-2025 nielukiintiö -96,4 Mt yhteensä, -19,3 Mt vuodessa"
)})
  
  output$kiinb= renderText({
    paste(
      
      "2026-2030 nielukiintiö ", rv$cumallob, " Mt yhteensä"
    )})
  output$kiinc= renderText({
    paste(
      
      "2021-2025 päästökiintiö ", rv$cumalloc, " Mt yhteensä"
    )})
  output$kiind= renderText({
    paste(
      
      "2026-2030 päästökiintiö ", rv$cumallod, " Mt yhteensä"
    )})
  output$emisa= renderText({
    paste(
      
      "2021-2025 nettopäästöt -", rv$cumsink, " Mt yhteensä"
    )})
  
  output$emisb= renderText({
    paste(
      
      "2026-2030 nettopäästöt -", rv$cumsinkb, " Mt yhteensä"
    )})
  output$emisc= renderText({
    paste(
      
      "2021-2025 päästöt ", rv$cumsinkc, " Mt yhteensä"
    )})
  output$emisd= renderText({
    paste(
      
      "2026-2030 päästöt ", rv$cumsinkd, " Mt yhteensä"
    )})
  
  
  
  output$alu= renderText({
    paste(
      
      "2021-2023 nettopäästöt alustavista tilastoista"
    )})
  
  output$difad= renderText({
    paste(
      rv$cul2d,
      "\n",
      '<br><span style=\"color: white; background-color: var(--colallo)','\"><b>',  rv$cumallod,  
      " Mt ",'</b></span>'," - ",
      
      '<span style=\"color: white; background-color: var(--colemis)','\"><b>',  rv$cumsinkd,  
      
      # '<span style=\"color:', "#bc810d", '\"><b>',  input$pricepre, "€/t ",
      " Mt ", '</span>',
      "= ",
      '<span style=\"color: white; background-color: var(--coldiff)', '\"><b>', 
      '<span style=\"color: white; background-color:var(--coldiff)','\"><b>',  rv$cumsda,  
      
      # format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      " Mt",'</b></span>',
      sep ="")})
  
  
  output$pulad= renderText({
    paste(
      rv$cul1d,
      "\n",
      '<br><span style=\"color: white; background-color: var(--coldiff)','\"><b>',  rv$cumsda,  
      " Mt ",'</b></span>'," x ",
      '<span style=\"color: white; background-color: var(--colprice)',  '\"><b>',  input$priceesdpost, " €/t ",'</b></span>',"= ",
      '<span style=\"color: white; background-color: var(--colcost)', '\"><b>', 
      format(round(input$priceesdpost*rv$cumsida,0), nsmall=0, decimal.mark=","),
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
  output$infoa = renderText({
    paste0(
      "EU:n päästötavoitteet on maankäyttö- ja taakanjakosektoreilla jaoteltu maa- ja vuosikohtaisiksi 
      tavoitteiksi/kiintiöiksi, jotka yhdessä toteuttavat EU:n laajuiset tavoitteet. 
      Mailla on käytössä joustomekanismeja kiintiöisiin pääsemiseksi. Yksi keskeisin on kaupankäynti 
      päästö- tai nieluyksiköistä maiden välillä.",
      '<br>',
      '<br>',
      
      "
      
      Tällä sivulla voi hahmotella, että millaisia kustannuksia tai tuottoja Suomelle 
      tulee kaupankäynnistä, riippuen Suomen päästökehityksestä sekä yksiköiden hinnasta. 
      Yksiköiden kaupalle ei ainakaan toistaiseksi ole keskitettyä markkinapaikkaa ja yksiköillä ei luultavasti ole juurikaan
      käyty kauppaa maiden välillä, joten yksiköiden hintatasoa on vaikea arvioida. Kauden 2021-2025 osalta yksiköiden kaupankäynti
      tulee suorittaa 2027 mennessä ja kauden 2026-2030 osalta 2032 mennessä. 
   
    
    "
    )
  })
  
  output$morea = renderText({
    paste0(
      '<b>',
      " Toistaiseksi ei ole huomioitu: 
         ",
      '</b>',
      '<br/><dd>',
      '<br>',
      
      # '',
      "- Joustoja sektoreiden välillä. Jäsenmailla on mahdollisuus kompensoida sektorin alijäämiään toisen sektorin
      ylijäämillään. Varsinkin mikäli ylijäämäsektorin yksiköiden hinta on halvempi, niin niitä kannattaa käyttää
      alijäämäsektorin kompensointiin sen sijaan että hankkisi yksiköitä muilta jäsenmailta. 
    Jäsenmailla on mahdollisuus myös käyttää rajoitetusti päästökauppasektorin päästöoikeuksien mitätöintiä alijäämän korvaamiseen taakanjakosektorilla. 
    Tällöin ne menettävät mitätöinnin verran tuloja päästöoikeuksien huutokauppaamisesta.
         ",
      # ' ',
      "\n",
      '</dd><br/>',  
    '<br>',
    
          "-Lainausrajoja vuosien välillä sektoreiden sisällä kauden aikana. 
          Kaudella 2021-2025 jäsenmaa voi taakanjakosektorilla siirtää seuraavan vuoden kiintiöstä korkeintaan 
          7,5% edellisen vuoden alijäämän paikkaamiseen. Kaudella 2026-2030 maksimisiirto on 5%. Tämä huomioitava kertymää
          laskettaessa. Kuitenkin, jäsenmaan kaupankäyntimahdollisuuksien myötä tämä ei käytännössä ole välttämättä kovin rajoittava.  
      ",
     # Hoidettu viljelysmaa = 29-29=0",
      '<br>',
    '<br>',
    
      "- Pankitusmahdollisuuksia vuosien ja kausien välillä. Kunkin vuoden ylijäämästä jäsenmaa voi viedä vuoden 2021 osalta korkeintaan 75% vuosikiintiöstä vastaavan summan seuraaville vuosille aina 2030 saakka ja
     2021-2029 ylijäämistä 25% vuosikiintiötä vastaavan summan. Rajoitteet vaikuttavat löysiltä ja tuskin vähentävät pankitusta. Maankäyttösektorilla ei ole kai vastaavia rajoitteita. ",
      '<br>',
    '<br>',
    
      "- Mahdollisia ylimääräisiä yksiköitä, joita jäsenmaat voivat saada mikäli EU pääsee kokonaisuutena tavoitteisiina sektorilla. 
    
     
    "
    )
  })
  #v = Ylitukset: Kiintiötarve = vertailutaso - nielu
  # Hoidettu viljelysmaa
  
  output$totu= renderText({
    paste(
      rv$cul3,
      "\n",
      '<span style=\"color: white; background-color: var(--colcost)', '\"><b>',
      format(round(input$pricepre*rv$cumsi,0), nsmall=0, decimal.mark=","),
      '</span>',
      " + ",'</b>',
      '<span style=\"color: white; background-color: var(--coldiff)',  '\"><b>',  input$maa2025, " Mtt ",'</b></span>',
      " x ",
      '<span style=\"color: white; background-color: var(--colprice)', '\"><b>',  input$pricepre, " €/t ",'</b></span>',"= ",
      
    
      '<span style=\"color: white; background-color: var(--colcost)',  '\"><b>', 
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
    koki[maara < 0, col2 :=gr]
    koki[maara > 0, col2 :=re]
    koki[maarab < 0, col3 :=gr]
    koki[maarab > 0, col3 :=re]
    
    rv$cumsink= format(round(koki[year %in% 2025 & sector =="metsajapuu", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumsinki= koki[year %in% 2025 & sector =="metsajapuu", maarab]  
  
    rv$cumallo= format(round(koki[year %in% 2025 & sector =="metsajapuuk", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumalloi= koki[year %in% 2025 & sector =="metsajapuuk", maarab]  
    
    
      rv$cums= format(round(koki[year %in% 2025 & sector =="cumu", maara],1), nsmall=1, decimal.mark=",")
    rv$cumsi= koki[year %in% 2025 & sector =="cumu", maara]
    
    rv$cumsaa= format(round(koki[year %in% 2025 & sector =="cumu", abs(maara)],1), nsmall=1, decimal.mark=",")
    rv$cumsiaa= koki[year %in% 2025 & sector =="cumu", abs(maara)]
    
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
  
  
  koke2=reactive({
    koki =copy(koki2)
    
    # startpre = koki[year ==lyear & sector =="lulucf", maara]
    
    # 
    # koki[year %in% c(2026) & sector =="lulucf", maara := input$lulucf2026]
    # koki[year %in% c(2030) & sector =="lulucf", maara := input$lulucf2030]
    
    # if (input$manual ==FALSE) {
    
    seqs = seq(input$lulucf2026, input$lulucf2030, length.out =5)
    koki[year %in% c(2026:2030) & sector =="lulucf", maara := seqs]
    
    
    # } else {
    #   
    #   koki[year %in% c(2026) & sector =="lulucf", maara := input$lulucf2026]
    #   koki[year %in% c(2026) & sector =="lulucf", maara := input$lulucf2027]
    #   koki[year %in% c(2026) & sector =="lulucf", maara := input$lulucf2028]
    #   koki[year %in% c(2026) & sector =="lulucf", maara := input$lulucf2029]
    #   koki[year %in% c(2030) & sector =="lulucf", maara := input$lulucf2030]
    #   
    # }
    
    koki= koki[year %in% c(2026:2030) & sector =="diff", maara := koki[year %in% c(2026:2030) &sector=="lulucf", maara, on=c("year")] 
               - koki[year %in% c((2026):2030) & sector=="lallocation", maara, on=c("year")]]
    
    koki =   koki[order(rank(year), year)]
    
    mor =   cumsum(koki[year %in% c(2026:2030) & sector =="lallocation", maara])
    koki = koki[year %in% c(2026:2030)& sector =="lallocation", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2026:2030) & sector =="lulucf", maara])
    koki = koki[year %in% c(2026:2030)& sector =="lulucf", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2026:2030) & sector =="diff", maara])
    koki = koki[year %in% c(2026:2030)& sector =="diff", maarab :=mor]
    
    koki = koki[year %in% c(2026:2030)& sector =="cumu", maara :=mor]
    
    koki= koki[year %in% c(2026:2030) & sector =="cost", maara := koki[year %in% c(2026:2030) &sector=="diff", maara]*input$pricepost]
    
    mora =   cumsum(koki[year %in% c(2026:2030) & sector =="cost", maara])
    koki = koki[year %in% c(2026:2030)& sector =="cost", maarab :=mora]
    
    koki = koki[year %in% c(2026:20305)& sector =="debt", maara :=mora]
    
    koki = koki[year %in% c(2026:2030)& sector =="price", maara :=input$pricepost]
    koki[year %in% c(2026:2030)& sector =="price", maarab :=maara]
    
    koki$col2 = "white"
    koki[maara < 0, col2 :=gr]
    koki[maara > 0, col2 :=re]
    koki[maarab < 0, col3 :=gr]
    koki[maarab > 0, col3 :=re]
    
    
    rv$cumsinkb= format(round(koki[year %in% 2030 & sector =="lulucf", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumsinkib= koki[year %in% 2030 & sector =="lulucf", maarab]  
    
    rv$cumallob= format(round(koki[year %in% 2030 & sector =="lallocation", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumalloib= koki[year %in% 2030 & sector =="lallocation", maarab]  
    
    
    rv$cumsb= format(round(koki[year %in% 2030 & sector =="cumu", maara],1), nsmall=1, decimal.mark=",")
    rv$cumsib= koki[year %in% 2030 & sector =="cumu", maara]
    
    rv$cumsba= format(round(koki[year %in% 2030 & sector =="cumu", abs(maara)],1), nsmall=1, decimal.mark=",")
    rv$cumsiba= koki[year %in% 2030 & sector =="cumu", abs(maara)]
    
    
    
    if (rv$cumsb > 0) {
      rv$cul1b = "Kustannus yksiköiden hankinnasta:"
    } else {
      rv$cul1b = "Tuotto yksiköiden myynnistä:"
    }
    
    if (rv$cumsb > 0) {
      rv$cul3b = "Kokonaiskustannus yksiköiden hankinnasta maankäyttösektorilla:"
    } else {
      rv$cul3b = "Kokonaistuotto yksiköiden myynnistä maankäyttösektorilla:"
    }
    
    if (rv$cumsb > 0) {
      rv$cul2b = "Kiintiön ylitys eli tarve yksiköiden hankinnalle:"
    } else {
      rv$cul2b = "Kiintiön alitus eli myytävissä olevat yksiköt:"
    }
    
    
    
    koki
  })
  
  
  
  
  
  koke3=reactive({
    koki =copy(koki3)
    
 
    
    koki[year %in% c(2024) & sector =="esd", maara := input$esd2024]
    koki[year %in% c(2025) & sector =="esd", maara := input$esd2025]
    
    
    koki= koki[year %in% c(2021:2025) & sector =="diff", maara := koki[year %in% c(2021:2025) &sector=="esd", maara, on=c("year")] 
               - koki[year %in% c(2021:2025) & sector=="allocation", maara, on=c("year")]]
    
    koki =   koki[order(rank(year), year)]
    
    mor =   cumsum(koki[year %in% c(2021:2025) & sector =="allocation", maara])
    koki = koki[year %in% c(2021:2025)& sector =="allocation", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2021:2025) & sector =="esd", maara])
    koki = koki[year %in% c(2021:2025)& sector =="esd", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2021:2025) & sector =="diff", maara])
    koki = koki[year %in% c(2021:2025)& sector =="diff", maarab :=mor]
    
    koki = koki[year %in% c(2021:2025)& sector =="cumu", maara :=mor]
    
    koki= koki[year %in% c(2021:2025) & sector =="cost", maara := koki[year %in% c(2021:2025) &sector=="diff", maara]*input$priceesdpre]
    
    mora =   cumsum(koki[year %in% c(2021:2025) & sector =="cost", maara])
    koki = koki[year %in% c(2021:2025)& sector =="cost", maarab :=mora]
    
    koki = koki[year %in% c(2021:2025)& sector =="debt", maara :=mora]
    
    koki = koki[year %in% c(2021:2025)& sector =="price", maara :=input$priceesdpre]
    koki[year %in% c(2021:2025)& sector =="price", maarab :=maara]
    
    koki$col2 = "white"
    koki[maara < 0, col2 :=gr]
    koki[maara > 0, col2 :=re]
    koki[maarab < 0, col3 :=gr]
    koki[maarab > 0, col3 :=re]
    
    
    rv$cumsinkc= format(round(koki[year %in% 2025 & sector =="esd", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumsinkic= koki[year %in% 2025 & sector =="esd", maarab]  
    
    rv$cumalloc= format(round(koki[year %in% 2025 & sector =="allocation", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumalloic= koki[year %in% 2025 & sector =="allocation", maarab]  
    
    
    rv$cumsc= format(round(koki[year %in% 2025 & sector =="cumu", maara],1), nsmall=1, decimal.mark=",")
    rv$cumsic= koki[year %in% 2025 & sector =="cumu", maara]
    
    rv$cumsca= format(round(koki[year %in% 2025 & sector =="cumu", abs(maara)],1), nsmall=1, decimal.mark=",")
    rv$cumsica= koki[year %in% 2025 & sector =="cumu", abs(maara)]
    
    if (rv$cumsc > 0) {
      rv$cul1c = "Kustannus yksiköiden hankinnasta:"
    } else {
      rv$cul1c = "Tuotto yksiköiden myynnistä:"
    }
    
    if (rv$cumsc > 0) {
      rv$cul3c = "Kokonaiskustannus yksiköiden hankinnasta maankäyttösektorilla:"
    } else {
      rv$cul3c = "Kokonaistuotto yksiköiden myynnistä maankäyttösektorilla:"
    }
    
    if (rv$cumsc > 0) {
      rv$cul2c = "Kiintiön ylitys eli tarve yksiköiden hankinnalle:"
    } else {
      rv$cul2c = "Kiintiön alitus eli myytävissä olevat yksiköt:"
    }
    
    
    
    koki
  })
  
  
  
  
  
  
  
  
  
  
  
  
  koke4=reactive({
    koki =copy(koki4)
    
    
    seqs = seq(input$esd2026, input$esd2030, length.out =5)
    koki[year %in% c(2026:2030) & sector =="esd", maara := seqs]
    # 
    # koki[year %in% c(2024) & sector =="esd", maara := input$esd2024]
    # koki[year %in% c(2030) & sector =="esd", maara := input$esd2030]
    
    
    koki= koki[year %in% c(2026:2030) & sector =="diff", maara := koki[year %in% c(2026:2030) &sector=="esd", maara, on=c("year")] 
               - koki[year %in% c(2026:2030) & sector=="allocation", maara, on=c("year")]]
    
    koki =   koki[order(rank(year), year)]
    
    mor =   cumsum(koki[year %in% c(2026:2030) & sector =="allocation", maara])
    koki = koki[year %in% c(2026:2030)& sector =="allocation", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2026:2030) & sector =="esd", maara])
    koki = koki[year %in% c(2026:2030)& sector =="esd", maarab :=mor]
    
    
    mor =   cumsum(koki[year %in% c(2026:2030) & sector =="diff", maara])
    koki = koki[year %in% c(2026:2030)& sector =="diff", maarab :=mor]
    
    koki = koki[year %in% c(2026:2030)& sector =="cumu", maara :=mor]
    
    koki= koki[year %in% c(2026:2030) & sector =="cost", maara := koki[year %in% c(2026:2030) &sector=="diff", maara]*input$priceesdpost]
    
    mora =   cumsum(koki[year %in% c(2026:2030) & sector =="cost", maara])
    koki = koki[year %in% c(2026:2030)& sector =="cost", maarab :=mora]
    
    koki = koki[year %in% c(2026:2030)& sector =="debt", maara :=mora]
    
    koki = koki[year %in% c(2026:2030)& sector =="price", maara :=input$priceesdpost]
    koki[year %in% c(2026:2030)& sector =="price", maarab :=maara]
    
    koki$col2 = "white"
    koki[maara < 0, col2 :=gr]
    koki[maara > 0, col2 :=re]
    koki[maarab < 0, col3 :=gr]
    koki[maarab > 0, col3 :=re]
    
    
    rv$cumsinkd= format(round(koki[year %in% 2030 & sector =="esd", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumsinkid= koki[year %in% 2030 & sector =="esd", maarab]  
    
    rv$cumallod= format(round(koki[year %in% 2030 & sector =="allocation", abs(maarab)],1), nsmall=1, decimal.mark=",")
    rv$cumalloid= koki[year %in% 2030 & sector =="allocation", maarab]  
    
    
    rv$cumsd= format(round(koki[year %in% 2030 & sector =="cumu", maara],1), nsmall=1, decimal.mark=",")
    rv$cumsid= koki[year %in% 2030 & sector =="cumu", maara]
    
    rv$cumsda= format(round(koki[year %in% 2030 & sector =="cumu", abs(maara)],1), nsmall=1, decimal.mark=",")
    rv$cumsida= koki[year %in% 2030 & sector =="cumu", abs(maara)]
    
    if (rv$cumsd > 0) {
      rv$cul1d = "Kustannus yksiköiden hankinnasta:"
    } else {
      rv$cul1d = "Tuotto yksiköiden myynnistä:"
    }
    
    if (rv$cumsd > 0) {
      rv$cul3d = "Kokonaiskustannus yksiköiden hankinnasta maankäyttösektorilla:"
    } else {
      rv$cul3d = "Kokonaistuotto yksiköiden myynnistä maankäyttösektorilla:"
    }
    
    if (rv$cumsd > 0) {
      rv$cul2d = "Kiintiön ylitys eli tarve yksiköiden hankinnalle:"
    } else {
      rv$cul2d = "Kiintiön alitus eli myytävissä olevat yksiköt:"
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
      luk = 2005
      
    } else {
      luk =2015
      
       luk = 2005
      
    }
    #luk = 2010
    
    f = rv$fonts
    
    koke = koke1()
    koke = as.data.table(koke)
    koke = koke[sector %in% c("metsajapuu", "metsajapuuk", "diff", "cost",  "price") & year %in% C(luk:2025)]

    koks = koke[year %in% c(2025) & sector %in% c("diff"),]
    
    mi = as.numeric(unique(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"),min(maara, na.rm=TRUE)]))
    ma = as.numeric(unique(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"),max(maara, na.rm=TRUE)]))
    hi = ma-mi
    # ran =.07
    ba =5.25
    koke[,place := ma + ba + ran*0.064*hi]
    
    bg = "#D6D6D6"
    bg = "#e7e7e7"
    bg = "#d9d9d9"
    
    #e6e6e6
    #e7e7e7
    
    scaleFactor <- unique(max(koke[sector %in% c("metsajapuu", "metsajapuuk", "diff"), maara], na.rm=T))/unique(max(koke[sector %in% c("cost"), maara], na.rm=T))
    
    koke[,hmaara:=maara*scaleFactor]
    
    gup = ggplot(data=koke, aes(x=year, group=sector, fill=col, color=col )) + 
      geom_vline(aes(xintercept=2023.5), size=.4, color="black", linetype="dashed")+
      
  
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("metsajapuuk"),],
                aes(x=year, y=place+.059*hi, label=year), size=5*f, fontface="bold", color ="black")+
     
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
      
      
      # geom_text(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
      #           aes(x=2023.4, y=place+.159*hi), label = paste0("Päästöt tilastoista <="),
      #           col= "black", fontface="bold" ,  size =5*f, hjust =1, vjust=1, angle=c(0),
      #           alpha=.9, lineheight=.99) +
      # geom_text(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
      #           aes(x=2023.6, y=place+.159*hi), label = paste0("=> Päästöt valinnoista"),
      #           col= "black", fontface="bold" ,  size =5*f, hjust =0, vjust=1, angle=c(0),
      #           alpha=.9, lineheight=.99) +
      # 
      geom_text(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
                aes(x=year+1.5, y=place+.059*hi, label="Kertymä"), size=5*f, fontface="bold", color ="black")+
      
      
      geom_rect(data=koke[year %in% c(2025) & sector %in% c("metsajapuuk"),],
                aes(xmin=year+1.5-.9, xmax=year+1.5+.9, ymax=(place+1*0.05*hi)+.05*hi, ymin=(place-6*0.06*hi)+.06*hi), 
                color=cu, alpha=0, size=1)+
      
      
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
      
      
      
      # geom_text(data=koke[year %in% c(2025) & sector %in% c("diff"),],
      #           aes(x=2024.7, y=ma, 
      #               label=
      #                 "
      #            Huomioita:
      #           
      #            Ei sisällä muita maankäyttösektorin 
      #            alasektoreita ja niiden tavoitteita. 
      #           
      #            Vuodesta 2026 alkaen 
      #            maankäyttösektoria käsitellään
      #            kokonaisuutena (alempi osio). 
      #           
      #            Nielukiintiö perustuu 
      #            Luonnonvarakeskuksen arvioon 
      #            ja tulee tarkentumaan.
      #            Myös päästötilastot tarkentuvat.
      #            
      #            "), size=3.5*f,color="black", fontface="bold", hjust=0, vjust=1)+
      
      
      
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
      
           geom_point(data=koke[sector %in% c("cost"),], aes(y=hmaara, fill=col), 
                      size=4.5, stroke=1, alpha=.9, shape =21)+
      geom_line(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=2)+
        geom_point(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=5, alpha=.9)+
      geom_line(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=1.5)+      
      
      geom_hline(aes(yintercept=0), size=.4, color="black", linetype="dashed")+
      
      coord_cartesian(xlim=c(luk,  2027), 
                      # ylim=c(mi, max(90, ma)),
                      clip ="off") +
      
      labs(caption =c("Data: Tilastokeskus, Luonnonvarakeskus, omat laskelmat.  ", 
                      "Kuva: villeseppala.github.io/EU-EmissionAllocations")) +
      scale_y_continuous(name= "Päästöt, miljoonaa tCO2-ekvivalenttia",sec.axis=sec_axis(~./scaleFactor, name="Kustannukset, miljoonaa euroa", breaks=waiver()))   +
      
        scale_x_continuous(breaks =seq(luk, 2025, by=1), minor_breaks = seq(luk-.5, 2025.5, by=1))   +
       scale_alpha_identity() + 
      
      scale_fill_identity() + 
      scale_color_identity() +
      theme(
        plot.caption=element_text(size =10*f, hjust =c(0,1) , family = "merriweather sans",
                                  lineheight=.85 ,color=c("black","black")),
        
        axis.text.x = element_text(size=15*f), 
          axis.title.y.left=element_text(color=dgr),
        axis.text.y.left=element_text(color=dgr, size=15*f),
        axis.title.y.right=element_text(color=re),
        axis.text.y.right=element_text(color=re, size=15*f),
         axis.title.x= element_blank(),
        plot.background = element_rect(fill =bg ,color="grey"), 
        panel.background = element_rect(fill = bg, color="grey"), 
        legend.title=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_line(color="grey"),
        
        panel.grid.major.x=element_blank(),
        legend.background = element_rect(fill=bg, size=0, color=bg),

      )
    
    # gup =     gup+    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
    
    gup
   })
  
  
  
  
  
  
  
  
  
  output$plotlulucf <- renderPlot({
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
    
    koke = koke2()
    koke = as.data.table(koke)
    koke = koke[sector %in% c("lulucf", "lallocation", "diff", "cost",  "price") & year %in% C(luk:2030)]


    
    bg = "#e7e7e7"
    bg = "#d9d9d9"
    bg = "#D6D6D6"
    
    
    #e7e7e7
    
    # for scaling scales
    if (abs(as.numeric(unique(koke[sector %in% c("cost", "price"),max(maara, na.rm=TRUE)]))) > abs(as.numeric(unique(koke[sector %in% c("cost", "price"),min(maara, na.rm=TRUE)])))) {
      scaleFactor <- unique(max(koke[sector %in% c("lulucf", "lallocation", "diff"), maara], na.rm=T))/unique(max(koke[sector %in% c("cost"), maara], na.rm=T))
      
    } else {
      scaleFactor <- unique(min(koke[sector %in% c("lulucf", "lallocation", "diff"), maara], na.rm=T))/unique(min(koke[sector %in% c("cost"), maara], na.rm=T))
      
    }
    koke[,hmaara:=maara*scaleFactor]
    
    #for scaling graph 
    mi = min(as.numeric(unique(koke[sector %in% c("lulucf", "lallocation", "diff"),min(maara, hmaara, na.rm=TRUE)])), as.numeric(unique(koke[sector %in% c("cost", "price"),min(hmaara, na.rm=TRUE)])))
  
    
      ma = max(as.numeric(unique(koke[sector %in% c("lulucf", "lallocation", "diff"),max(maara, na.rm=TRUE)])), as.numeric(unique(koke[sector %in% c("cost", "price"),max(hmaara, na.rm=TRUE)])))
    hi = ma-mi
    # ran =.07
    ba =5.25
    koke[,place := ma + ba + ran*0.064*hi]
    
    gup = ggplot(data=koke, aes(x=year, group=sector, fill=col, color=col )) + 
      
      geom_vline(aes(xintercept=2023.5), size=.4, color="black", linetype="dashed")+
      
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c("lallocation"),],
                aes(x=year, y=place+.059*hi, label=year), size=5*f, fontface="bold", color ="black")+

      geom_rect(data=koke[year %in% c(2030) & sector %in% c("lulucf", "lallocation", "diff", "cost", "price"),],
                aes(xmin=luk, xmax=year+1.5+.9, ymax=(place+0.03*hi), ymin=(place-0.03*hi),
                    fill=col),
                size=0)+
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c("lulucf", "lallocation"),],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+

      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c("diff") ,],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c( "price"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c( "cost"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+


      geom_text(data=koke[year %in% c(2026) & sector %in% c("lulucf", "lallocation", "diff", "price",  "cumu", "cost", "debt"),],
                aes(x=2025.2, y=place, label=paste0(lab, ":")),
                size=5*f, hjust=1, fontface="bold", color="white")+

      geom_text(data=koke[year %in% c(2026:2029) & sector %in% c("cumu"),],
                aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ",")),
                size=5*f, fontface="bold")+
      geom_label(data=koke[year %in% c(2030) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg, color="red"),
                 size=5*f, fontface="bold", alpha=0,label.size=1)+

      geom_label(data=koke[year %in% c(2030) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg),
                 size=5*f, fontface="bold", alpha=0,label.size=0)+

      geom_text(data=koke[year %in% c(2030) & sector %in% c("lulucf", "lallocation"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,decim), nsmall=decim, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+

      geom_text(data=koke[year %in% c(2030) & sector %in% c( "diff"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,decim), nsmall=decim, decimal.mark = ","), color=col3),
                size=5*f, fontface="bold")+

      geom_text(data=koke[year %in% c(2030) & sector %in% c( "price"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,0), nsmall=0, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+

      geom_text(data=koke[year %in% c(2030) & sector %in% c("cost"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,0), nsmall=0, decimal.mark = ","), color=col3),
                size=5*f, fontface="bold")+


      geom_text(data=koke[year %in% c(2030) & sector %in% c("lallocation"),],
                aes(x=year+1.5, y=place+.059*hi, label="Kertymä"), size=5*f, fontface="bold", color ="black")+


      geom_rect(data=koke[year %in% c(2030) & sector %in% c("lallocation"),],
                aes(xmin=year+1.5-.9, xmax=year+1.5+.9, ymax=(place+1*0.05*hi)+.05*hi, ymin=(place-6*0.06*hi)+.06*hi),
                color=cu, alpha=0, size=1)+


      geom_curve(data=koke[year %in% c(2030) & sector %in% c("lallocation"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-2*0.064*hi+1*0.014*hi),
                 arrow = arrow(
                   length = unit(.01, "npc"),
                   type="closed" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      geom_curve(data=koke[year %in% c(2030) & sector %in% c("lulucf"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-1*0.064*hi+1*0.014*hi),
                 arrow = arrow(
                   length = unit(.015, "npc"),
                   type="open" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +

      geom_curve(data=koke[year %in% c(2030) & sector %in% c("diff"),],
                 aes(x = year+2.6, y = place-1*0.014*hi, xend = year+2.6, yend = place-2*0.064*hi),
                 arrow = arrow(
                   length = unit(.015, "npc"),
                   type="open" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      geom_curve(data=koke[year %in% c(2030) & sector %in% c("price"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-1*0.064*hi),
                 arrow = arrow(
                   length = unit(.01, "npc"),
                   type="closed" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +




      geom_col(data=koke[year %in% c(luk:2030) & sector %in% c("lallocation")],
               aes(x=year, y = maara,fill=col, color=col, width=si),
               stat='identity', position='stack' )+

      geom_col(data=koke[year %in% c(luk:2030) & sector %in% c("lulucf"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+

      geom_col(data=koke[year %in% c(luk:2030) & sector %in% c("cumu"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+

      geom_col(data=koke[year %in% c(luk:2030) & sector %in% c("diff"),],
               aes(x=year, y = maara,fill=col,  width=si, col=col2),
               stat='identity', position='stack', linewidth =.6 )+

      geom_point(data=koke[sector %in% c("cost"),], aes(y=hmaara, fill=col, color=col2), 
                 size=4.5, stroke=1, alpha=.9, shape =21)+
      geom_line(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=2)+
      geom_point(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=5, alpha=.9)+
      geom_line(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=1.5)+      
      
      geom_hline(aes(yintercept=0), size=.4, color="black", linetype="dashed")+
      
      coord_cartesian(xlim=c(luk,  2032), 
                      # ylim=c(mi, max(90, ma)),
                      clip ="off") +
      
      labs(caption =c("Data: Tilastokeskus, Luonnonvarakeskus, omat laskelmat.  ", 
                      "Kuva: villeseppala.github.io/EU-EmissionAllocations")) +
      scale_y_continuous(name= "Päästöt, miljoonaa tCO2-ekvivalenttia",sec.axis=sec_axis(~./scaleFactor, name="Kustannukset, miljoonaa euroa"))   +
      
      scale_x_continuous(breaks =seq(luk, 2030, by=1), minor_breaks = seq(luk-.5, 2030.5, by=1))   +
      scale_alpha_identity() + 
      
      scale_fill_identity() + 
      scale_color_identity() +
      theme(
        plot.caption=element_text(size =10*f, hjust =c(0,1) , family = "merriweather sans",
                                  lineheight=.85 ,color=c("black","black")),
        
        axis.text.x = element_text(size=15*f), 
        axis.title.y.left=element_text(color=dgr),
        axis.text.y.left=element_text(color=dgr, size=15*f),
        axis.title.y.right=element_text(color=re),
        axis.text.y.right=element_text(color=re, size=15*f),
        axis.title.x= element_blank(),
        plot.background = element_rect(fill =bg ,color="grey"), 
        panel.background = element_rect(fill = bg, color="grey"), 
        legend.title=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_line(color="grey"),
        
        panel.grid.major.x=element_blank(),
        legend.background = element_rect(fill=bg, size=0, color=bg),
        
      )
    
    # gup =     gup+    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
    
    gup
  })
  
  
  
  
  output$plotesdpre <- renderPlot({
    luk = 2013
    if (rv$view ==1) {
      luk = 2013
    } else if (rv$view==2) {
      luk =2013
    } else {
      luk =2015
    }
    # luk = 2010
    
    f = rv$fonts
    
    koke = koke3()
    koke = as.data.table(koke)
    koke = koke[sector %in% c("esd", "allocation", "diff", "cost",  "price") & year %in% C(luk:2025)]
    
    
    
    bg = "#e7e7e7"
    bg = "#D6D6D6"
    bg = "#d9d9d9"
    
    
    #e7e7e7
    
    # for scaling scales
    if (abs(as.numeric(unique(koke[sector %in% c("cost", "price"),max(maara, na.rm=TRUE)]))) > abs(as.numeric(unique(koke[sector %in% c("cost", "price"),min(maara, na.rm=TRUE)])))) {
      scaleFactor <- unique(max(koke[sector %in% c("esd", "allocation",  "diff"), maara], na.rm=T))/unique(max(koke[sector %in% c("cost"), maara], na.rm=T))
      
    } else {
      scaleFactor <- unique(min(koke[sector %in% c("esd", "allocation",  "diff"), maara], na.rm=T))/unique(min(koke[sector %in% c("cost"), maara], na.rm=T))
      
    }
    koke[,hmaara:=maara*scaleFactor]
    
    #for scaling graph 
    mi = min(as.numeric(unique(koke[sector %in% c("esd", "allocation", "diff"),min(maara, hmaara, na.rm=TRUE)])), as.numeric(unique(koke[sector %in% c("cost", "price"),min(hmaara, na.rm=TRUE)])))
    
    
    ma = max(as.numeric(unique(koke[sector %in% c("esd", "allocation", "diff"),max(maara, na.rm=TRUE)])), as.numeric(unique(koke[sector %in% c("cost", "price"),max(hmaara, na.rm=TRUE)])))
    hi = ma-mi
    # ran =.07
    ba =5.25
    koke[,place := ma + ba + ran*0.064*hi]
    
    gup = ggplot(data=koke, aes(x=year, group=sector, fill=col, color=col )) + 
      geom_vline(aes(xintercept=2023.5), size=.4, color="black", linetype="dashed")+
      
      
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("allocation"),],
                aes(x=year, y=place+.059*hi, label=year), size=5*f, fontface="bold", color ="black")+
      
      geom_rect(data=koke[year %in% c(2025) & sector %in% c("esd", "allocation",  "diff", "cost", "price"),],
                aes(xmin=luk, xmax=year+1.5+.9, ymax=(place+0.03*hi), ymin=(place-0.03*hi),
                    fill=col),
                size=0)+
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("esd", "allocation"),],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c("diff") ,],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c( "price"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      geom_text(data=koke[year %in% c(2021:2025) & sector %in% c( "cost"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+
      
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("esd", "allocation",  "diff", "price",  "cumu", "cost", "debt"),],
                aes(x=2020.2, y=place, label=paste0(lab, ":")),
                size=5*f, hjust=1, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2026:2029) & sector %in% c("cumu"),],
                aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ",")),
                size=5*f, fontface="bold")+
      geom_label(data=koke[year %in% c(2025) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg, color="red"),
                 size=5*f, fontface="bold", alpha=0,label.size=1)+
      
      geom_label(data=koke[year %in% c(2025) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg),
                 size=5*f, fontface="bold", alpha=0,label.size=0)+
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("esd", "allocation"),],
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
      
      
      geom_text(data=koke[year %in% c(2025) & sector %in% c("allocation"),],
                aes(x=year+1.5, y=place+.059*hi, label="Kertymä"), size=5*f, fontface="bold", color ="black")+
      
      
      geom_rect(data=koke[year %in% c(2025) & sector %in% c("allocation"),],
                aes(xmin=year+1.5-.9, xmax=year+1.5+.9, ymax=(place+1*0.05*hi)+.05*hi, ymin=(place-6*0.06*hi)+.06*hi),
                color=cu, alpha=0, size=1)+
      
      
      geom_curve(data=koke[year %in% c(2025) & sector %in% c("allocation"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-2*0.064*hi+1*0.014*hi),
                 arrow = arrow(
                   length = unit(.01, "npc"),
                   type="closed" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      geom_curve(data=koke[year %in% c(2025) & sector %in% c("esd"),],
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
      
      
      geom_col(data=koke[year %in% c(luk:2020) & sector %in% c("allocation")],
               aes(x=year, y = maara,fill=col, color=col, width=si),
               stat='identity', position='stack',alpha=.0 )+      
      geom_col(data=koke[year %in% c(2021:2025) & sector %in% c("allocation")],
               aes(x=year, y = maara,fill=col, color=col, width=si),
               stat='identity', position='stack' )+
      # geom_col(data=koke[year %in% c(2026:2030) & sector %in% c("allocation")],
      #          aes(x=year, y = maara,fill=col, color=col, width=si),
      #          stat='identity', position='stack', alpha=.0 )+
      
      geom_col(data=koke[year %in% c(2021:2025) & sector %in% c("esd"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+
      geom_col(data=koke[year %in% c(luk:2023) & sector %in% c("esd"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+
      
      geom_col(data=koke[year %in% c(luk:2025) & sector %in% c("cumu"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+
      
      geom_col(data=koke[year %in% c(luk:2025) & sector %in% c("diff"),],
               aes(x=year, y = maara,fill=col,  width=si, col=col2),
               stat='identity', position='stack', linewidth =.6 )+
      
      geom_point(data=koke[sector %in% c("cost"),], aes(y=hmaara, fill=col, color=col2), 
                 size=4.5, stroke=1, alpha=.9, shape =21)+
      geom_line(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=2)+
      geom_point(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=5, alpha=.9)+
      geom_line(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=1.5)+ 
      
      geom_hline(aes(yintercept=0), size=.4, color="black", linetype="dashed")+
      
      coord_cartesian(xlim=c(luk,  2027), 
                      # ylim=c(mi, max(90, ma)),
                      clip ="off") +
      
      labs(caption =c("Data: Tilastokeskus, Luonnonvarakeskus, omat laskelmat.  ", 
                      "Kuva: villeseppala.github.io/EU-EmissionAllocations")) +
      scale_y_continuous(name= "Päästöt, miljoonaa tCO2-ekvivalenttia",sec.axis=sec_axis(~./scaleFactor, name="Kustannukset, miljoonaa euroa"))   +
      
      scale_x_continuous(breaks =seq(luk, 2025, by=1), minor_breaks = seq(luk-.5, 2025.5, by=1))   +
      scale_alpha_identity() + 
      
      scale_fill_identity() + 
      scale_color_identity() +
      theme(
        plot.caption=element_text(size =10*f, hjust =c(0,1) , family = "merriweather sans",
                                  lineheight=.85 ,color=c("black","black")),
        
        axis.text.x = element_text(size=15*f), 
        axis.title.y.left=element_text(color=dgr),
        axis.text.y.left=element_text(color=dgr, size=15*f),
        axis.title.y.right=element_text(color=re),
        axis.text.y.right=element_text(color=re, size=15*f),
        axis.title.x= element_blank(),
        plot.background = element_rect(fill =bg ,color="grey"), 
        panel.background = element_rect(fill = bg, color="grey"), 
        legend.title=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_line(color="grey"),
        
        panel.grid.major.x=element_blank(),
        legend.background = element_rect(fill=bg, size=0, color=bg),
        
      )
    
    # gup =     gup+    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
    
    gup
  })
  
  
  
  
  
  
  
  
  
  output$plotesdpost <- renderPlot({
    luk = 2013
    if (rv$view ==1) {
      luk = 2013
    } else if (rv$view==2) {
      luk =2013
    } else {
      luk =2015
    }
    # luk = 2010
    
    f = rv$fonts
    
    koke = koke4()
    koke = as.data.table(koke)
    koke = koke[sector %in% c("esd", "allocation", "diff", "cost",  "price") & year %in% C(luk:2030)]
    
    
    
    bg = "#e7e7e7"
    bg = "#D6D6D6"
    bg = "#d9d9d9"
    
    
    #e7e7e7
    
    # for scaling scales
    if (abs(as.numeric(unique(koke[sector %in% c("cost", "price"),max(maara, na.rm=TRUE)]))) > abs(as.numeric(unique(koke[sector %in% c("cost", "price"),min(maara, na.rm=TRUE)])))) {
      scaleFactor <- unique(max(koke[sector %in% c("esd", "allocation",  "diff"), maara], na.rm=T))/unique(max(koke[sector %in% c("cost"), maara], na.rm=T))
      
    } else {
      scaleFactor <- unique(min(koke[sector %in% c("esd", "allocation",  "diff"), maara], na.rm=T))/unique(min(koke[sector %in% c("cost"), maara], na.rm=T))
      
    }
    koke[,hmaara:=maara*scaleFactor]
    
    #for scaling graph 
    mi = min(as.numeric(unique(koke[sector %in% c("esd", "allocation", "diff"),min(maara, hmaara, na.rm=TRUE)])), as.numeric(unique(koke[sector %in% c("cost", "price"),min(hmaara, na.rm=TRUE)])))
    
    
    ma = max(as.numeric(unique(koke[sector %in% c("esd", "allocation", "diff"),max(maara, na.rm=TRUE)])), as.numeric(unique(koke[sector %in% c("cost", "price"),max(hmaara, na.rm=TRUE)])))
    hi = ma-mi
    # ran =.07
    ba =5.25
    koke[,place := ma + ba + ran*0.064*hi]
    
    gup = ggplot(data=koke, aes(x=year, group=sector, fill=col, color=col )) + 
      geom_vline(aes(xintercept=2023.5), size=.4, color="black", linetype="dashed")+
      
      
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c("allocation"),],
                aes(x=year, y=place+.059*hi, label=year), size=5*f, fontface="bold", color ="black")+
      
      geom_rect(data=koke[year %in% c(2030) & sector %in% c("esd", "allocation",  "diff", "cost", "price"),],
                aes(xmin=luk, xmax=year+1.5+.9, ymax=(place+0.03*hi), ymin=(place-0.03*hi),
                    fill=col),
                size=0)+
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c("esd", "allocation"),],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c("diff") ,],
                aes(x=year, y=place, label=format(round(maara,decim), nsmall=decim, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c( "price"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      geom_text(data=koke[year %in% c(2026:2030) & sector %in% c( "cost"),],
                aes(x=year, y=place, label=format(round(maara,0), nsmall=0, decimal.mark = ","), color=col2),
                size=5*f, fontface="bold")+
      
      
      geom_text(data=koke[year %in% c(2026) & sector %in% c("esd", "allocation",  "diff", "price",  "cumu", "cost", "debt"),],
                aes(x=2025.2, y=place, label=paste0(lab, ":")),
                size=5*f, hjust=1, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2026:2029) & sector %in% c("cumu"),],
                aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ",")),
                size=5*f, fontface="bold")+
      geom_label(data=koke[year %in% c(2030) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg, color="red"),
                 size=5*f, fontface="bold", alpha=0,label.size=1)+
      
      geom_label(data=koke[year %in% c(2030) & sector %in% c("cumu"),],
                 aes(x=year, y=place, label=format(round(maara,1), nsmall=1, decimal.mark = ","), fill=bg),
                 size=5*f, fontface="bold", alpha=0,label.size=0)+
      
      geom_text(data=koke[year %in% c(2030) & sector %in% c("esd", "allocation"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,decim), nsmall=decim, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2030) & sector %in% c( "diff"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,decim), nsmall=decim, decimal.mark = ","), color=col3),
                size=5*f, fontface="bold")+
      
      geom_text(data=koke[year %in% c(2030) & sector %in% c( "price"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,0), nsmall=0, decimal.mark = ",")),
                size=5*f, fontface="bold", color="white")+
      
      geom_text(data=koke[year %in% c(2030) & sector %in% c("cost"),],
                aes(x=year+1.5, y=place, label=format(round(maarab,0), nsmall=0, decimal.mark = ","), color=col3),
                size=5*f, fontface="bold")+
      
      
      geom_text(data=koke[year %in% c(2030) & sector %in% c("allocation"),],
                aes(x=year+1.5, y=place+.059*hi, label="Kertymä"), size=5*f, fontface="bold", color ="black")+
      
      
      geom_rect(data=koke[year %in% c(2030) & sector %in% c("allocation"),],
                aes(xmin=year+1.5-.9, xmax=year+1.5+.9, ymax=(place+1*0.05*hi)+.05*hi, ymin=(place-6*0.06*hi)+.06*hi),
                color=cu, alpha=0, size=1)+
      
      
      geom_curve(data=koke[year %in% c(2030) & sector %in% c("allocation"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-2*0.064*hi+1*0.014*hi),
                 arrow = arrow(
                   length = unit(.01, "npc"),
                   type="closed" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      geom_curve(data=koke[year %in% c(2030) & sector %in% c("esd"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-1*0.064*hi+1*0.014*hi),
                 arrow = arrow(
                   length = unit(.015, "npc"),
                   type="open" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      
      geom_curve(data=koke[year %in% c(2030) & sector %in% c("diff"),],
                 aes(x = year+2.6, y = place-1*0.014*hi, xend = year+2.6, yend = place-2*0.064*hi),
                 arrow = arrow(
                   length = unit(.015, "npc"),
                   type="open" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      geom_curve(data=koke[year %in% c(2030) & sector %in% c("price"),],
                 aes(x = year+2.6, y = place, xend = year+2.6, yend = place-1*0.064*hi),
                 arrow = arrow(
                   length = unit(.01, "npc"),
                   type="closed" # Describes arrow head (open or closed)
                 ),
                 colour = "black",
                 size = 1.2,curvature = -0.8,
                 angle = 90# Anything other than 90 or 0 can look unusual
      ) +
      
      
  
      geom_col(data=koke[year %in% c(luk:2025) & sector %in% c("allocation")],
               aes(x=year, y = maara,fill=col, color=col, width=si),
               stat='identity', position='stack', alpha=.0)+
      geom_col(data=koke[year %in% c(2026:2030) & sector %in% c("allocation")],
               aes(x=year, y = maara,fill=col, color=col, width=si),
               stat='identity', position='stack' )+
      
      geom_col(data=koke[year %in% c(2026:2030) & sector %in% c("esd"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+
      geom_col(data=koke[year %in% c(luk:2023) & sector %in% c("esd"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+
      
      geom_col(data=koke[year %in% c(luk:2030) & sector %in% c("cumu"),],
               aes(x=year, y = maara,fill=col, width=si),
               stat='identity', position='stack', alpha=.99 )+
      
      geom_col(data=koke[year %in% c(luk:2030) & sector %in% c("diff"),],
               aes(x=year, y = maara,fill=col,  width=si, col=col2),
               stat='identity', position='stack', linewidth =.6 )+
      
      geom_point(data=koke[sector %in% c("cost"),], aes(y=hmaara, fill=col, color=col2), 
                 size=4.5, stroke=1, alpha=.9, shape =21)+
      geom_line(data=koke[sector %in% c("cost"),], aes(y=hmaara),  size=2)+
      geom_point(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=5, alpha=.9)+
      geom_line(data=koke[sector %in% c("price"),], aes(y=hmaara),  size=1.5)+ 
      
      geom_hline(aes(yintercept=0), size=.4, color="black", linetype="dashed")+
      
      coord_cartesian(xlim=c(luk,  2032), 
                      # ylim=c(mi, max(90, ma)),
                      clip ="off") +
      
      labs(caption =c("Data: Tilastokeskus, Luonnonvarakeskus, omat laskelmat.  ", 
                      "Kuva: villeseppala.github.io/EU-EmissionAllocations")) +
      scale_y_continuous(name= "Päästöt, miljoonaa tCO2-ekvivalenttia",sec.axis=sec_axis(~./scaleFactor, name="Kustannukset, miljoonaa euroa"))   +
      
      scale_x_continuous(breaks =seq(luk, 2030, by=1), minor_breaks = seq(luk-.5, 2030.5, by=1))   +
      scale_alpha_identity() + 
      
      scale_fill_identity() + 
      scale_color_identity() +
      theme(
        plot.caption=element_text(size =10*f, hjust =c(0,1) , family = "merriweather sans",
                                  lineheight=.85 ,color=c("black","black")),
        
        axis.text.x = element_text(size=15*f), 
        axis.title.y.left=element_text(color=dgr),
        axis.text.y.left=element_text(color=dgr, size=15*f),
        axis.title.y.right=element_text(color=re),
        axis.text.y.right=element_text(color=re, size=15*f),
        axis.title.x= element_blank(),
        plot.background = element_rect(fill =bg ,color="grey"), 
        panel.background = element_rect(fill = bg, color="grey"), 
        legend.title=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_line(color="grey"),
        
        panel.grid.major.x=element_blank(),
        legend.background = element_rect(fill=bg, size=0, color=bg),
        
      )
    
    # gup =     gup+    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
    
    gup
  })
  
  
  
  
  
  
  
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
