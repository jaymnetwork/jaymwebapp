library(Benchmarking)
library(deaR)
library(dplyr)
library(tidyverse)
library(scales)
library(tibble)
library(plotly)
library(ggrepel)
library(viridis)
library(directlabels)
library(ggplotify)
library(rddtools)
library(DT)
library(rgl)
library(shinyRGL)
library(ggforce)
library(concaveman)
library(MASS)
library(graphics)
library(gridGraphics)
library(interp)

source("functions.R")


hnc_kbl2<-read.csv("data/hnc_kbl2.csv")
drafts<-read.csv("data/draft.csv")
shotloc1<-read.csv("data/shotdata1.csv")
shotloc2<-read.csv("data/shotdata2.csv")

kbl_match_data <- read.csv("data/KBL_Regular_Quarter_Margin3.csv")
kbl_match_data2 <- read.csv("data/KBL_Regular_Quarter_Margin5.csv")
nba_dea <- read.csv("data/DEA3.csv")
shotdata<-bind_rows(shotloc1, shotloc2)
`CRS Efficiencies`<-nba_dea$DEA_crs3
Revenue<-nba_dea$Revenue
`Team Value Increase`<-nba_dea$Diff_value
fld <- with(nba_dea, interp(x = DEA_crs3, y = Revenue, z = Diff_value, duplicate = "mean"))
fld$z<-ifelse(is.na(fld$z)==T,0,fld$z)



ui<-fluidPage(
  shinyUI(
    navbarPage(HTML("Basketball Analytics Archive"),
               tags$style(type="text/css",
                          ".navbar-brand {font-size: 16px;
                            font-weight:100;
                            }"
               ),
               position = "fixed-top",
               inverse=T,
               collapsible = TRUE,
               tabPanel("About",
                        div(
                          tags$style(type = "text/css", 
                                     ".container-fluid {padding-left:0px;
                          font-weight:100;
                          padding-right:0px; padding-bottom:0px;
                          };",
                                     HTML('.navbar-nav > li > a, .navbar-brand {
                                     padding-top:3px ; 
                                     padding-bottom:3px ;
                                     min-height: 25px;
                                     }
                                     .navbar:not(.fixed-bottom):not(.navbar-fixed-bottom):not(.navbar-fixed-bottom){
                                     margin-bottom: 0px;height:55px ;
                                     }
                                     .navbar-inverse .navbar-nav>.active>a, 
                                     .navbar-inverse .navbar-nav>.active>a:focus, 
                                     .navbar-inverse .navbar-nav>.active>a:hover{
                                     background-color: #333333;
                                     }
                                     .navbar-nav>li>a {
                                     padding-top:17px;
                                     }
                                     .navbar-brand{
                                     padding:17px;
                                     padding-left:43px;
                                     }
                                     .navbar .navbar-nav {
                                     float: right;
                                     margin-right:35px;
                                     }
                                     .container-fluid>.navbar-collapse, .container-fluid>.navbar-header, .container>.navbar-collapse, .container>.navbar-header{
                                     margin-right: 5px;
                                     }
                                     .navbar-toggle {
                                     margin-top:10px;
                                     }
                                     .navbar-fixed-bottom .navbar-collapse, .navbar-fixed-top .navbar-collapse {
                                     float: right;
                                     padding-top:0px;
                                     }
                                          ')),
                        ),
                        div(
                          style = "width: 100%; margin: auto; margin-bottom: 0px;padding-bottom:0px ;
                          padding-top:55px",
                          imageOutput("home_img", width = "100%")
                        ),
                        div(
                          style = "width: 90%; margin: auto;margin-bottom: 0px;padding-bottom:0px ;",
                          p(titlePanel("ABOUT THE WEB"),
                            HTML("
                                 <p>  This is a web archive of basketball analytics projects and visualizations. Each project aims to provide meaningful research and actionable insights that could amplify the game of basketball, including athlete performance, team management, and more. </p> 
<p> The data used on the website will not be limited to the NBA but will also include the collegiate level, NCAA, and international leagues such as the KBL (Korean Basketball League).</p>
<p> While the web is designed by JeongJun Moon, each research study consists of one or multiple authors who aim to use data science for good and to push the boundaries of sports analytics. </p>
                                 "))
                        ),
               ),
               navbarMenu(title = "Projects", 
                          tabPanel(title = "Data Envelopment Analysis - part 1",
                                   div(
                                     style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:55px;",
                                     
                                     tags$h3("DEA project - part 1"),
                                     titlePanel(tags$h5("DEA Assessment of Evaluating Possession based Player Efficiency of Korean Professional Basketball Players and Floor Impact Counter (FIC) Performance.")),
                                     div(
                                       style = "width: 90%; margin: auto; text-align:right;",
                                       HTML("<em><b>By PhilSoo Kim, JeongJun Moon</em></b>")
                                     ),
                                     div(
                                       style="margin-top:40px;",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId="caption3",
                                                       label ="3rd Variable",
                                                       choices=c(PTS="ptsplayer",
                                                                 FGA="fgmplayer",
                                                                 REB="trebplayer",
                                                                 AST="astplayer",
                                                                 STL="stlplayer",
                                                                 BLK="bsplayer",
                                                                 FIC="fic")),
                                           radioButtons("radio2", "Chart Type",
                                                        choices = list("Scatter" = 1, "2D Density" = 2), selected = 1),
                                           checkboxGroupInput("checkGroup1", label = ("Identify Data"), 
                                                              choices = list("Foreign players" = 1),
                                                              selected = NULL)
                                         ),
                                         
                                         mainPanel(
                                           plotOutput("dea1"))),
                                       div(
                                         style="margin-top:30px; margin-bottom: 55px",
                                         DEAtext(),
                                         DEAtext2())
                                       
                                     )
                                   ),
                                   div(
                                     style = "width: 90%; margin: auto; text-align:right;
                                       margin-bottom: 55px;",
                                     HTML("<b>The Project Status: PUBLISHED in <em>Korean Society of Sport and Leisure Studies</em></b>")
                                   )),
                          
                          tabPanel(title = "Regression Discontinuity Design",
                                   div(
                                     style = "width: 90%; margin: auto; margin-bottom: 0px;padding-top:55px;",
                                     tags$h3("RDD project"),
                                     titlePanel(tags$h5("When does losing lead to winning? An Empirical Investigation on the Korean Professional Basketball League")),
                                     div(
                                       style = "width: 90%; margin: auto; text-align:right;",
                                       HTML("<em><b>By PhilSoo Kim, SangHyun Lee, JeongJun Moon</em></b>")
                                     )),
                                   div(
                                     style = "width: 90%; margin: auto; margin-bottom: 0px;padding-top:40px; padding-bottom:40px;",
                                     sidebarLayout(sidebarPanel(
                                       # sliderInput("seasons", "Season",min = 2007, max = 2023, value = c(2008,2020),sep = "",
                                       #                                      ticks = T),
                                       selectInput(inputId="quarter",
                                                   label ="Quarter",
                                                   selected="second",
                                                   choices=c(`1Q`="first",
                                                             `2Q`="second",
                                                             `3Q`="third",
                                                             `4Q`="fourth")),
                                       sliderInput(inputId="difference",
                                                   label ="Score Difference",
                                                   value=10,
                                                   min=1,max=28),
                                       selectInput(inputId="month1",
                                                   label ="Starting Month",
                                                   selected="9",
                                                   choices=c(September=9,
                                                             October=10,
                                                             November=11,
                                                             December=12,
                                                             January=1,
                                                             February=2,
                                                             March=3,
                                                             April=4)),
                                       uiOutput("rdd_option"),
                                       sliderInput("range", "Winning Percentage",
                                                   min = 0, max = 100,
                                                   value = c(10,90)),
                                       fluidRow(
                                         column(7,
                                                checkboxGroupInput("checkGroup2", label = ("Round Stage"), 
                                                                   choices = list("Round 1",
                                                                                  "Round 2",
                                                                                  "Round 3",
                                                                                  "Round 4",
                                                                                  "Round 5",
                                                                                  "Round 6"),
                                                                   selected = c("Round 5","Round 6"))),
                                         column(2,checkboxGroupInput("checkGroup3", label = ("Standings"), 
                                                                     choices = list("1st",
                                                                                    "2nd",
                                                                                    "3rd",
                                                                                    "4th",
                                                                                    "5th",
                                                                                    "6th",
                                                                                    "7th",
                                                                                    "8th",
                                                                                    "9th",
                                                                                    "10th"),
                                                                     selected = c("1st","2nd","3rd","4th")))
                                         
                                       ),
                                     ),
                                     mainPanel(tabsetPanel(
                                       tabPanel(HTML("<b>Plot</b>"), plotOutput('rdd1'),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px;",
                                                  HTML("<b>Model Summary:</b> <br> "),
                                                  verbatimTextOutput("modelSummary"),
                                                  HTML("<em><b>Note:</em></b> <br> "
                                                  ),
                                                  tags$div("For interpretation of the summary, please refer to the following link:",
                                                           tags$a(href="https://rpubs.com/phle/r_tutorial_regression_discontinuity_design", 
                                                                  "RDD interpretation link")))),
                                       tabPanel(HTML("<b>Data 1</b>"),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  DT::dataTableOutput('table')
                                                ) 
                                       ),
                                       tabPanel(HTML("<b>Data 2</b>"),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  DT::dataTableOutput('table2')
                                                ) 
                                       ),
                                       tabPanel(HTML("<b>3D plot</b>"),
                                                div(
                                                  style = "width: 100%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  
                                                  rglwidgetOutput('rglreg')
                                                ) 
                                       ))
                                     )
                                     )),
                                   div(
                                     style="width: 85%; margin: auto; padding-top:0px; padding-bottom:50px;",
                                     RDDtext()),
                                   div(
                                     style = "width: 90%; margin: auto; text-align:right;
                                       margin-bottom: 55px;",
                                     HTML("<b>The Project Status: PUBLISHED in <em>Korea Journal of Sports Science </em></b>")
                                   )
                          ),
                          
                          tabPanel(title = "Data Envelopment Analysis - part 2",
                                   
                                   div(
                                     style = "width: 90%; margin: auto; margin-bottom: 0px;padding-top:55px;",
                                     tags$h3("DEA project - part 2"),
                                     titlePanel(tags$h5("Evaluating Operational Efficiency of NBA Professional Teams on Organizational Performance: Assessment of Data Envelopment Analysis")),
                                     div(
                                       style = "width: 90%; margin: auto; text-align:right;",
                                       HTML("<em><b>By SangHyun Lee, JeongJun Moon, PhilSoo Kim </em></b>")
                                     )),
                                   div(
                                     style = "width: 90%; margin: auto; margin-bottom: 0px;padding-top:55px;",
                                     tabsetPanel(
                                       tabPanel(HTML("<b>Scatterplot</b>"),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  plotOutput("deaplotgg")
                                                ) 
                                       ),
                                       tabPanel(HTML("<b> 3D surface plot</b>"),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  plotlyOutput("deaplotly"),
                                                  
                                                ) 
                                       ),
                                       tabPanel(HTML("<b>3D Scatterplot</b>"),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  rglwidgetOutput('rglreg2')
                                                ) 
                                       ),
                                       tabPanel(HTML("<b>Contour Plot</b>"),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  plotOutput("deacontour")
                                                  #imageOutput("contour1", width = "100%")
                                                ) 
                                       ),
                                       tabPanel(HTML("<b>Data Caveat</b>"),
                                                div(
                                                  style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                                  plotlyOutput("deacaveat"),
                                                  HTML("<em><b>Note:</em></b> <br> We have to be aware that the NBA provides extremely low numbers for some tracking stats in the 2016 season compared to other seasons. This may have caused the result to have a relatively high number of teams in 2016. Nevertheless, the teams mentioned in the results still arguably had successful management in many aspects.")
                                                  
                                                ) 
                                       )
                                     ),
                                   ),
                                   div(
                                     style = "width: 85%; margin: auto; margin-bottom: 55px;padding-top:40px; padding-bottom:0px;",
                                     DEAtext3()
                                   ),
                                   div(
                                     style = "width: 90%; margin: auto; text-align:right;
                                       margin-bottom: 55px;",
                                     HTML("<b>The Project Status: FORTHCOMING in <em>PLOS ONE </em></b>")
                                   )
                          )
               ),
               
               navbarMenu(title = "Interactives", 
                          tabPanel(title = "Shot Locations",
                                   div(
                                     style = "width: 90%; margin: auto; margin-bottom: 0px;padding-bottom:0px;
                                     padding-top:75px;",
                                     sidebarLayout(sidebarPanel(titlePanel(tags$h4("Shot Chart")),
                                                                selectInput(inputId="caption1",
                                                                            label ="Team Select",
                                                                            choices=c(ATL="Atlanta Hawks",
                                                                                      BOS="Boston Celtics",
                                                                                      BKN="Brooklyn Nets",
                                                                                      CHA="Charlotte Hornets",
                                                                                      CHI="Chicago Bulls",
                                                                                      CLE="Cleveland Cavaliers",
                                                                                      DAL="Dallas Mavericks",
                                                                                      DEN="Denver Nuggets",
                                                                                      DET="Detroit Pistons",
                                                                                      GSW="Golden State Warriors",
                                                                                      HOU="Houston Rockets",
                                                                                      IND="Indiana Pacers",
                                                                                      LAC="Los Angeles Clippers",
                                                                                      LAL="Los Angeles Lakers",
                                                                                      MEM="Memphis Grizzlies",
                                                                                      MIA="Miami Heat",
                                                                                      MIL="Milwaukee Bucks",
                                                                                      MIN="Minnesota Timberwolves",
                                                                                      NOP="New Orleans Pelicans",
                                                                                      NYK="New York Knicks",
                                                                                      OKC="Oklahoma City Thunder",
                                                                                      ORL="Orlando Magic",
                                                                                      PHI="Philadelphia 76ers",
                                                                                      PHX="Phoenix Suns",
                                                                                      POR="Portland Trail Blazers",
                                                                                      SAC="Sacramento Kings",
                                                                                      SAS="San Antonio Spurs",
                                                                                      TOR="Toronto Raptors",
                                                                                      UTA="Utah Jazz",
                                                                                      WAS="Washington Wizards")),
                                                                selectInput(inputId="obs",
                                                                            label ="Season",
                                                                            selected = 2023,
                                                                            choices=seq(2020,2023)),
                                                                uiOutput("player_names"),
                                                                radioButtons("radio", "Chart Type",
                                                                             choices = list("Scatter" = 1, "Heat Map" = 2), selected = 2)
                                                                
                                                                
                                     ),
                                     mainPanel(
                                       plotOutput("shot"),
                                       div(
                                         style = "width: 94%; margin: auto; text-align:right;"
                                       )
                                     )),
                                     HTML("<em><b>Note:</em></b> <br> Shot data is available from 1997. 
                                          However, only the shot data from the 20s will 
                                          be visualized due to the limited instance size of the web.")
                                   )
                                   
                                   
                          ),
                          tabPanel(title = "Draft",
                                   div(
                                     style = "width: 90%; margin: auto; margin-bottom: 0px;padding-top:100px;",
                                     sidebarLayout(sidebarPanel(titlePanel(tags$h4("NBA Draft")),
                                                                selectInput(inputId="picks",
                                                                            label ="Picks drafted from top",
                                                                            selected=30,
                                                                            choices=seq(1,60)),
                                                                sliderInput("year", "Year",min = 1949, max = 2022, value = c(1980,2020),sep = "",
                                                                            ticks = T)),
                                                   mainPanel(plotOutput("drafts")
                                                   )
                                     )),
                                   div(
                                     style = "width: 85%; margin: auto; margin-bottom: 0px;padding-top:20px; padding-bottom:0px; overflow-x: scroll;",
                                     DT::dataTableOutput('draft')
                                   ) 
                                   
                          ),
               ),
               tabPanel(title = "Contact",
                        div(
                          style = "width: 100%; margin: auto; margin-bottom: 0px;padding-bottom:0px ;
                          padding-top:55px",
                          imageOutput("contact_img", width = "100%"),
                        ),
                        div(
                          style = "width: 90%; margin: auto; margin-bottom: 0px;padding-bottom:0px ;
                          padding-top:0px",
                          titlePanel("CONTACT"),
                          HTML("&nbsp;jaymnetwork@gmail.com"),
                          
                        )
               ),
               tags$style(HTML("
               .dropdown-item:active, .dropdown-menu>li>a:active{
               background-color: darkred;
               }
               .dropdown-menu{
               --bs-dropdown-link-active-bg:darkred;
               }
               .dropdown-menu>.active>a, .dropdown-menu>.active>a:focus, .dropdown-menu>.active>a:hover {
               background-color: darkred;
               }
               .navbar-default .navbar-form {
               background-color: #f0f0f0;
               }
               .navbar-inverse .navbar-collapse, .navbar-inverse .navbar-form {
               background-color: #222322;
               }
               .navbar-inverse .navbar-nav>.active>a, .navbar-inverse .navbar-nav>.active>a:focus, 
               .navbar-inverse .navbar-nav>.active>a:hover{
               background-color: #222322;
               }
               .navbar-inverse .navbar-nav>.open>a, .navbar-inverse .navbar-nav>.open>a:focus, 
               .navbar-inverse .navbar-nav>.open>a:hover{
               background-color: #222322;
               }
               .h1, .h2, .h3, h1, h2, h3 {
               margin-top:20px;
               }
               .well{
               padding:13px;
               }
               .irs--shiny .irs-bar {
               background: darkred;
               border: darkred;
               }
               .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single{
               background:darkred;
               }
               a {
    color: darkgray;
    text-decoration: none;
               }
               input{
    color: darkred;
    font: inherit;
    margin: 0;
               }

                               "))
               
    )
    
  )
)


######################## SERVER ########################


server <- function(input, output){
  
  output$selected_var <- renderText({ 
    data()$typeAction
  })
  
  output$home_img <- renderImage({
    
    list(src = "www/01292019_Huskies53_184230.webp",
         width = "100%",
         height = 405)
    
  }, deleteFile = F)
  
  output$contact_img <- renderImage({
    
    list(src = "www/1000.jpeg",
         width = "100%",
         height = 405)
    
  }, deleteFile = F)
  
  
  data <- reactive({
    shotdata[shotdata$nameTeam==input$caption1 & shotdata$yearSeason==input$obs,]
  })
  
  output$player_names <- renderUI({
    selectInput(
      inputId = "caption2",
      label = "Playername", 
      choices = sort(data()$namePlayer %>% unique())
    )
  })
  
  data2 <- reactive({
    data() %>% filter(namePlayer==input$caption2) %>%
      mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))
  })
  
  output$shot <- renderPlot({
    if(input$radio==1){
      shotloc(data2())
    }
    else if(input$radio==2){
      shotloc_d(data2())
    }
  })
  
  
  output$dea1 <- renderPlot({
    if(input$radio2==1){
      ggplot(hnc_kbl2, aes(x= possplayer, y= DEA_vrs,family = "NanumGothic",
                           label=ifelse(possplayer>21.1 &DEA_vrs==1,paste(as.character(PLAYER),SEASON),                                  ifelse(possplayer>5.861 & possplayer<9.1 & DEA_vrs==1,paste(as.character(PLAYER),SEASON),""))))+
        labs(x="Possesion",
             y="Efficiency (VRS)") +
        geom_point(aes(colour=.data[[input$caption3]]))+{
          if(!is.null(input$checkGroup1)) geom_point(data=subset(hnc_kbl2,
                                                                 foreign==1),
                                                     colour="white",shape=1,stroke=1)
        }+
        scale_color_viridis(option = "plasma", name="Selected variable") +
        geom_text_repel(point.padding = 0.6,
                        nudge_x = .5,
                        segment.size=0.2,
                        segment.color="grey50",
                        arrow = arrow(length = unit(0.015, "npc")),size=2.5,max.overlaps=Inf,
                        colour="black")+expand_limits(y = c(0, 1.19), x=c(0,20))+
        geom_mark_hull(data = hnc_kbl2[hnc_kbl2$possplayer>21.1 & hnc_kbl2$DEA_vrs == 1,], 
                       aes(x= possplayer, y= DEA_vrs, label="Extreme Productivity"),
                       label.fill=NA,
                       label.family = "NanumGothic",
                       label.fontsize = 7,
                       con.cap =1,
                       con.type="straight",
                       color="darkgreen",
                       fill = "darkgreen",
                       expand=0.000001,linetype = 2)+
        geom_mark_hull(data = hnc_kbl2[hnc_kbl2$possplayer>5.861 &
                                         hnc_kbl2$possplayer<9.1& hnc_kbl2$DEA_vrs == 1,], 
                       aes(x= possplayer, y= DEA_vrs, label="Cost Effective"),
                       label.fill=NA,
                       label.family = "NanumGothic",
                       label.fontsize = 7,
                       con.cap =1,
                       con.type="straight",
                       color="darkgreen",
                       fill ="darkgreen",
                       expand=0.00001,linetype = 2)+
        expand_limits(y = c(-0.2, 1.35), x=c(0,38))+
        scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))+
        scale_x_continuous(breaks = c(0,3.86,10,20,30))+
        geom_vline(xintercept=3.86, linetype=3)+
        theme(plot.title=element_text(face="bold"),
              legend.title=element_text(size=8),
              legend.text=element_text(size=7))
    }
    else if(input$radio2==2){
      ggplot(hnc_kbl2,aes(x=possplayer,y=DEA_vrs, family = "NanumGothic",
                          label=ifelse(possplayer>21 &DEA_vrs==1,paste(as.character(PLAYER),SEASON),
                                       ifelse(possplayer>5.861 & possplayer<9.1 & DEA_vrs==1,
                                              paste(as.character(PLAYER),SEASON),"")))) +
        stat_density_2d(
          geom = "raster",
          aes(fill = after_stat(density)),
          contour = FALSE) +
        scale_fill_viridis_c(name="Density") +
        labs(x="Possesion",
             y="Efficiency (VRS)") +
        {
          if(!is.null(input$checkGroup1)) geom_point(data=subset(hnc_kbl2,
                                                                 foreign==1),
                                                     colour="turquoise",shape=1,stroke=1)
        }+
        geom_point(data=hnc_kbl2[hnc_kbl2$DEA_vrs==1 & hnc_kbl2$possplayer>21|
                                   hnc_kbl2$DEA_vrs==1 & hnc_kbl2$possplayer>5.861 & hnc_kbl2$possplayer<9.1,],
                   colour = "red",alpha=0.6)+
        geom_text_repel(point.padding = 0.6,
                        nudge_x = .5,
                        segment.size=0.2,
                        segment.color="white",
                        arrow = arrow(length = unit(0.015, "npc")),size=2,max.overlaps=Inf,
                        colour="white")+
        expand_limits(y = c(-0.2, 1.3), x=c(0,35))+
        scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0))+
        theme_minimal(base_family = "NanumGothic")+
        theme(plot.title=element_text(face="bold"),
              legend.title=element_text(size=8),
              legend.text=element_text(size=7))
    }
  })
  
  output$dea2 <- renderPlot({
    DEA_plot2(hnc_kbl2)
  })
  
  output$dea3 <- renderPlot({
    DEA_plot3(hnc_kbl2)
  })
  
  draftd <- reactive({
    drafts %>% filter(yearDraft>=input$year[1] & yearDraft<=input$year[2]) %>%
      filter(numberPickOverall <= input$picks) %>%
      count(OrganizationFrom, sort = T) %>% top_n(15)
  })
  
  output$drafts <- renderPlot({
    ggplot(draftd(), aes(x=reorder(OrganizationFrom, -n), y=n)) +
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 45,hjust=1))+
      labs(x="College/Organization",
           y="Number of players drafted")
  })
  
  
  rdd_data1 <- reactive({
    dtf(kbl_match_data,as.numeric(input$month1),
        as.numeric(input$month2),input$range[1],input$range[2],input$checkGroup2,input$checkGroup3)
  })
  
  rdd_data <- reactive({
    kbl_match(rdd_data1(), input$quarter, as.integer(input$difference))
  })
  
  output$rdd1 <- renderPlot({
    rdd(rdd_data())
  })
  
  option<-c(September=9,October=10,November=11,December=12,January=1,February=2,March=3,April=4)
  
  output$rdd_option <- renderUI({
    selectInput(inputId="month2",
                label ="Ending Month",
                selected="4",
                choices=option[which(option==input$month1):8])
  })
  output$rdd_option2 <- renderUI({
    selectInput(inputId="month4",
                label ="Ending Month",
                selected="4",
                choices=option[which(option==input$month3):8])
  })
  
  
  output$modelSummary <- renderPrint({
    cat("Total Games:",sum(rdd_data()$games),"\n")
    cat("Score Difference:",input$difference,"\n")
    cat("Season Span:",input$month1,"to",input$month2,"\n")
    cat("Teams' Winning Percentage:",input$range[1],"to",input$range[2],"%","\n")
    summary(rdd_reg_lm(rdd_object(rdd_data()), slope = "separate", order = 2))
  })
  output$deaplotly <- renderPlotly({
    deaplotly(fld,fld$z)
  })
  output$deaplotgg <- renderPlot({
    deaplotgg(nba_dea)
  })
  output$deacaveat <- renderPlotly({
    deacaveat(nba_dea)
  })
  output$deacontour <- renderPlot({
    filled.contour(x = fld$x,
                   y = fld$y,
                   z = fld$z,
                   plot.axes={
                     axis(1,cex.axis=0.7)
                     axis(2,cex.axis=0.7)
                   },
                   key.axes = axis(4,cex.axis=0.6),
                   color.palette =
                     colorRampPalette(c("darkblue", "skyblue", "white","red","darkred")),
                   nlevels = 20)
  })
  
  
  output$rglreg <- renderRglwidget({
    rgl.open(useNULL=T)
    threeDim(rdd_data())
    rglwidget()
  })
  
  output$rglreg2 <- renderRglwidget({
    open3d(useNULL=T)
    scatter3dim(x=`CRS Efficiencies`, y=`Team Value Increase`, z=Revenue, fit="quadratic",axis.col = c("black", "black", "black"),surface.col = "steelblue",sphere.size = 1.2,
                tick.marks=T,lable.tick.marks=T,grid=T,
                highlight.3d=T,point.col=ifelse(nba_dea$resid>0,"blue","red"),
                id.n = ifelse(nba_dea$DEA_crs3==1,"1",""))
    view3d( theta = -100, phi = 10, fov = -20, zoom = 1, 
            scale = par3d("scale"), interactive = TRUE)
    rglwidget()
  })
  
  
  
  output$table <- DT::renderDataTable(kbl_match_data2)
  output$table2 <- DT::renderDataTable(rdd_data())
  output$draft <- DT::renderDataTable(drafts)
  
}

shinyApp(ui = ui, server = server)


