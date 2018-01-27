library(shiny) 
library(shinydashboard) 

library(DT)
library(xts)
library(stringr)
library(ISOweek)
library(dygraphs)
library(googlesheets)
library(mosaic)

library(ROAuth)
library(streamR)
library(tidyverse)
library(leaflet)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(maps)
library(rnoaa)
library(ggplot2)
library(zoo)
library(gtrendsR)

# gs_auth(token="googlesheets_token.rds")
# sheets <- gs_ls()
# datGS <- sheets$sheet_title
# datGS <- gsub('.{18}$', '', datGS[1:length(datGS)])


# UI ----------------------------------------------------------------------

ui <- dashboardPage( 
  dashboardHeader(
    title="EWARS v.0.2a"
  ), 
  
  dashboardSidebar(
    sidebarMenu(menuItem("Retropective", tabName="retro", icon=icon("line-chart")),
                #menuItem("Datasets", tabName = "dataset", icon = icon("database")),
                menuItem("Help", tabName="help", icon=icon("info"))
    ),
    div(style="padding-left: 15px; padding-top: 40px; padding-right: 15px; ",
        p(class="small", "TDR | World Health Organization", tags$br(),
          "")
    )
  ), 
  
  dashboardBody(
    tabItems(
      tabItem(tabName="retro", 
              #h2("Prediction"),
              fluidRow(tags$head(includeScript("google-analytics.js")),
                       ## left
                       box(width=3, title="## parameters",
                           # tags$b("area:"), textOutput("par_area"),tags$br(),
                           # tags$b("year:"), textOutput("par_year"),tags$br(),
                           # tags$b("population:"), textOutput("par_population"),tags$br(),
                           tags$b("alarm indicator 1:"), textOutput("par_alarm_1"),tags$br(),
                           tags$b("alarm indicator 2:"), textOutput("par_alarm_2"),tags$br(),
                           tags$b("Z outbreak:"), textOutput("par_Z_outbreak"),tags$br(),
                           tags$b("prediction distance:"), textOutput("par_pred_distance"),tags$br(),
                           tags$b("outbreak window size:"), textOutput("par_out_window_size"),tags$br(),
                           tags$b("alarm window size:"), textOutput("par_alarm_window_size"),tags$br(),
                           tags$b("outbreak threshold:"), textOutput("par_out_threshold"),tags$br(),
                           tags$b("alarm threshold:"), textOutput("par_alarm_threshold"),tags$br(),
                           tags$b("outbreak week length:"), textOutput("par_out_week_length"),tags$br()),
                       
                       ## right
                       box(width=9,
                           tabsetPanel(
                             tabPanel(title="surveillance workbook",
                                      tags$br(),
                                      fluidRow(
                                        column(width=2,
                                               selectInput('country_code', 'country code', 
                                                           c("MX","XX"),
                                                           selected="XX")),
                                        column(width=3,
                                               textInput("ID_area", "district/ municipality code", "X")),
                                        column(width=3,
                                               textInput("psswd", "password", "password"))),
                                      tags$hr(),
                                      textOutput("area"),
                                      checkboxInput("tblWB_reload", "RE-load data", value=FALSE),
                                      tags$br(),tags$br(),
                                      DT::dataTableOutput('tblWB')
                             ),
                             tabPanel("input data",
                                      tags$br(),
                                      fluidRow(
                                        column(width=3,
                                               selectInput('gs_year', 'year', 
                                                           c(2011:2020),selected=2015),
                                               selectInput('gs_EpiWeek', 'epidemiological week', 
                                                           c(2:53),selected=53),
                                               actionButton("go1", label="send"),
                                               tags$br(),tags$br()),
                                        column(width=3,
                                               tags$br(),tags$br(),
                                               numericInput("gs_population","population", value=0),
                                               numericInput("gs_cases","hospitalized cases", value=0)),
                                        column(width=2,
                                               tags$br(),tags$br(),
                                               numericInput("gs_alarm1","alarm indicator 1", value=0),
                                               numericInput("gs_alarm2","alarm indicator 2", value=0)))),
                             tabPanel("outbreak",
                                      tags$br(),
                                      #dygraphOutput("graph_outbreak"),
                                      plotOutput("graph_outbreak2"),
                                      tags$br(),tags$br()),
                             tabPanel("probability", 
                                      tags$br(),
                                      #dygraphOutput("graph_probability"),
                                      plotOutput("graph_probability2"),
                                      tags$br(),tags$br()),
                             tabPanel("alarm plus outbreak", 
                                      tags$br(),
                                      #dygraphOutput("graph_alarm"),
                                      plotOutput("graph_alarm2"),
                                      tags$br(),tags$br()),
                             tabPanel("response", 
                                      tags$br(),
                                      #dygraphOutput("graph_response"),
                                      plotOutput("graph_response2"),
                                      tags$br(),tags$br())
                           ))
              )),
      
      tabItem(tabName = "dataset", 
              #h2("Available Datasets"),
              fluidRow(tabBox(width = 12, 
                              tabPanel(title = "weather",
                                       tags$h4("study area"),
                                       fluidRow(
                                         column(width=3,
                                                numericInput("area_lat", label="latitude", value =  3.139)),
                                         column(width=3,
                                                numericInput("area_lon", label="longitude", value = 101.6869)),
                                         column(width=3,
                                                numericInput("area_rad", label="radius (in km)", value = 100))),
                                       actionButton("go_wStatL", label = "click to search weather stations..."),
                                       tags$br(),tags$br(),tags$hr(),
                                       leafletOutput("map_wStat"),
                                       tags$hr(),
                                       tags$h4("observation period"),
                                       textInput("wStat_min", label="the earliest date (yyyy-mm-dd)", value = "2017-01-01"),
                                       textInput("wStat_max", label="the latest date (yyyy-mm-dd)", value = "2017-12-31"),
                                       actionButton("go_wStatT", label = "click to proceed..."),
                                       tags$br(),tags$br(),tags$hr(),
                                       plotOutput("Wplot"),
                                       tags$hr(),
                                       tags$h4("select the observation station"),
                                       textInput("WstatID", label="weather station ID"),
                                       actionButton("go_wStatS", label = "click to select..."),
                                       tags$br(),tags$br(),tags$hr(),
                                       DT::dataTableOutput('tblwStatS'),
                                       tags$hr()),
                              tabPanel(title = "Twitter",
                                       tags$b("the last 15 minutes data retrieve from the Twitter public streaming API"),
                                       fluidRow(
                                         valueBoxOutput("n_tweets", width = 2),
                                         valueBoxOutput("n_users", width = 2),
                                         valueBoxOutput("n_usersLoc", width = 6)
                                       ),
                                       fluidRow(
                                         ## viewer
                                         box(width = 12, title = "",
                                             #verbatimTextOutput("value_form"),
                                             leafletOutput("mymap"))),
                                       tags$hr()),
                              tabPanel(title = "Wikipedia",
                                       tags$b("Pageviews Analysis"),
                                       tags$br(),tags$br(),tags$br(),
                                       dygraphOutput("graph_wiki")
                              ),
                              tabPanel(title = "Google search-term",
                                       textInput("GST", label="search-term", value="dengue, zika, chikungunya"),
                                       plotOutput("graph_GST")),
                              tabPanel(title = "++"))
              )),
      
      tabItem(tabName="help", 
              #h2("Help"),
              fluidRow(## left
                box(width=3, #title="about us",
                    tags$p("TDR, the Special Programme for Research and Training in Tropical Diseases, 
                           is a global programme of scientific collaboration that helps facilitate, 
                           support and influence efforts to combat diseases of poverty. 
                           It is hosted at the World Health Organization (WHO), 
                           and is sponsored by the United Nations Children’s Fund (UNICEF), 
                           the United Nations Development Programme (UNDP), the World Bank and WHO."),
                    tags$br(),tags$br(),tags$br(),tags$br()),
                
                ## right
                box(width=9,
                    tabsetPanel(
                      # tabPanel("glossary",
                      #          tags$br(),tags$br(),tags$br()),
                      # tabPanel("operational guide",
                      #          tags$br(),tags$br(),tags$br()),
                      # tabPanel("references", 
                      #          tags$br(),tags$br(),tags$br()),
                      tabPanel("++ early release ++", 
                               tags$br(),tags$br(),
                               tags$div(
                                 HTML(paste(tags$strong("Early Warning and Response System for Dengue Outbreaks: Operational Guide using the web-based Dashboard"), 
                                            tags$a(href="http://globalminers8973.cloudapp.net/EWARS-R_dashboard.pdf", target="_blank", tags$br()," [download the pdf file here]"),
                                            sep = ""))
                               ),
                               tags$br(),tags$br(),
                               htmlOutput("video"),
                               tags$br(),tags$br(),tags$br())
                    )
                    )
                ))
              )
      ))


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  gs_country_code <- reactive({input$country_code})
  gs_ID_area <- reactive({input$ID_area})
  gs_psswd <- reactive({input$psswd})
  
  tblWB_reload <- reactive({input$tblWB_reload})
  
  gs_year <- reactive({input$gs_year})
  gs_EpiWeek <- reactive({input$gs_EpiWeek})
  
  gs_cases <- reactive({input$gs_cases})
  gs_population <- reactive({input$gs_population})
  gs_alarm1 <- reactive({input$gs_alarm1})
  gs_alarm2 <- reactive({input$gs_alarm2})
  
  output$area <- renderText({ 
    paste("you have selected surveillance workbook for country code: ",gs_country_code(),
          ", and district/ municipality code: ",gs_ID_area(),sep='')
  })
  
  datLoad <- reactive({
    
    validate(
      need(nchar(gs_psswd()) == 8, "the password should be 8 digit")
    )
    
    gs_token <- paste("data/GST_",gs_country_code(),".rds",sep="")
    gs_auth(token=gs_token)
    
    wb_name <- paste("Surveillance workbook_R",gs_ID_area(),".xlsxyour_",
                     gs_psswd(),"_",gs_country_code(),gs_country_code(),sep='')
    wb_list <- gs_ls()[1]
    
    validate(
      need(wb_name %in% wb_list$sheet_title == TRUE, "you specified an invalid value for the [area code] AND/OR the [password]")
    )
    
    if(tblWB_reload()==FALSE){
      
      dat_gsheet <- gs_title(wb_name)
      
      datLoad <- gs_read(dat_gsheet)
      datLoad <- datLoad[c(11:62),c(1:12)]
      names(datLoad) <- datLoad[1,]
      datLoad <- datLoad[-1,]
    }
    
    if(tblWB_reload()==TRUE){
      
      dat_gsheet <- gs_title(wb_name)
      
      datLoad <- gs_read(dat_gsheet)
      datLoad <- datLoad[c(11:62),c(1:12)]
      names(datLoad) <- datLoad[1,]
      datLoad <- datLoad[-1,]
    }
    
    return(datLoad)
  })
  
  datPar <- reactive({
    
    gs_token <- paste("data/GST_",gs_country_code(),".rds",sep="")
    gs_auth(token=gs_token)
    
    wb_name <- paste("Surveillance workbook_R",gs_ID_area(),".xlsxyour_",
                     gs_psswd(),"_",gs_country_code(),gs_country_code(),sep='')
    wb_list <- gs_ls()[1]
    
    validate(
      need(wb_name %in% wb_list$sheet_title == TRUE, "")
    )
    
    dat_gsheet <- gs_title(wb_name)
    
    datPar <- gs_read(dat_gsheet)
    datPar <- datPar[c(1:8),c(1:2)]
    return(datPar)
  })
  
  output$tblWB <- DT::renderDataTable({
    datINPUT <- datLoad()
    datINPUT <- datINPUT[,c(2:5,7:9,11,12)]
    datINPUT[] <- lapply(datINPUT, function(x) as.numeric(as.character(x)))
    
    colnames(datINPUT)[1] <- "epi_week"
    colnames(datINPUT)[2] <- "out_indic"
    colnames(datINPUT)[3] <- "end_channel"
    colnames(datINPUT)[4] <- "out_prob"
    colnames(datINPUT)[5] <- "out_period"
    colnames(datINPUT)[6] <- "alarm_sign"
    colnames(datINPUT)[7] <- "cases"
    colnames(datINPUT)[8] <- "alarm1"
    colnames(datINPUT)[9] <- "alarm2"
    datINPUT$out_indic <- round(datINPUT$out_indic,7)
    datINPUT$end_channel <- round(datINPUT$end_channel,7)
    datINPUT$out_prob <- round(datINPUT$out_prob,7)
    datINPUT$alarm1 <- round(datINPUT$alarm1,2)
    datINPUT$alarm2 <- round(datINPUT$alarm2,2)
    
    datINPUT$out_indic <- NULL
    datINPUT$out_period <- NULL
    datINPUT$alarm_sign <- NULL
    
    datatable(datINPUT, rownames=FALSE)
  })
  
  
  # output$par_area <- renderText({ 
  #   datPar <- datPar()
  #   x <- paste(as.character(names(dat_par)[2]))
  #   paste(x)
  # })
  
  # output$par_year <- renderText({
  #   datPar <- data()
  #   paste(as.character(unique(datPar$Year)))
  # })
  
  # output$par_population <- renderText({
  #   datPar <- data()
  #   paste(as.character(unique(datPar$population)))
  # })
  
  output$par_alarm_1 <- renderText({
    datPar <- datLoad()
    colnames(datPar)[(ncol(datPar)-1)]
  })
  
  output$par_alarm_2 <- renderText({
    datPar <- datLoad()
    colnames(datPar)[(ncol(datPar))]
  })
  
  output$par_Z_outbreak <- renderText({
    datPar <- datPar()
    paste(as.character(datPar[2,2]))
  })
  
  output$par_pred_distance <- renderText({
    datPar <- datPar()
    paste(as.character(datPar[3,2]))
  })
  
  output$par_out_window_size <- renderText({
    datPar <- datPar()
    paste(as.character(datPar[4,2]))
  })
  
  output$par_alarm_window_size <- renderText({
    datPar <- datPar()
    paste(as.character(datPar[5,2]))
  })
  
  output$par_out_threshold <- renderText({
    datPar <- datPar()
    paste(as.character(datPar[6,2]))
  })
  
  output$par_alarm_threshold <- renderText({
    datPar <- datPar()
    paste(as.character(datPar[7,2]))
  })
  
  output$par_out_week_length <- renderText({
    datPar <- datPar()
    paste(as.character(datPar[8,2]))
  })
  
  observeEvent(input$go1, {
    
    gs_token <- paste("data/GST_",gs_country_code(),".rds",sep="")
    gs_auth(token=gs_token)
    
    wb_name <- paste("Surveillance workbook_R",gs_ID_area(),".xlsxyour_",
                     gs_psswd(),"_",gs_country_code(),gs_country_code(),sep='')
    
    dat_gsheet <- gs_title(wb_name)
    
    i <- 11+as.numeric(as.character(gs_EpiWeek()))
    # cases
    gs_edit_cells(dat_gsheet, ws=1, input=gs_cases(), anchor=paste("I",i,sep=""), trim=FALSE)
    # population
    gs_edit_cells(dat_gsheet, ws=1, input=gs_population(), anchor=paste("J",i,sep=""), trim=FALSE)
    # alarm1
    gs_edit_cells(dat_gsheet, ws=1, input=gs_alarm1(), anchor=paste("K",i,sep=""), trim=FALSE)
    # alarm2
    gs_edit_cells(dat_gsheet, ws=1, input=gs_alarm2(), anchor=paste("L",i,sep=""), trim=FALSE)
    
  })
  
  datWB <- reactive({
    
    datWB <- datLoad()
    year_num <- gs_year()
    
    datWB$Year <- rep(year_num,nrow(datWB))
    datWB[] <- lapply(datWB, function(x) as.numeric(as.character(x)))
    
    datWB$WoY <- paste("0",datWB$Week,sep="")
    datWB$WoY <- str_sub(datWB$WoY,start=-2)
    datWB$WoY <- paste(datWB$Year,"-W",datWB$WoY,"-7",sep="")
    datWB$WoYdate <- ISOweek2date(datWB$WoY)
    datWB$WoY <- ISOweek(datWB$WoYdate)
    
    ##
    names(datWB)[3] <- "outbreak_indicator"
    names(datWB)[4] <- "endemic_channel"
    names(datWB)[5] <- "outbreak_probability"
    names(datWB)[6] <- "alarm_threshold"
    names(datWB)[7] <- "outbreak_period"
    names(datWB)[8] <- "alarm_signal"
    
    ##
    shift<-function(x,shift_by){
      stopifnot(is.numeric(shift_by))
      stopifnot(is.numeric(x))
      
      if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
      
      out<-NULL
      abs_shift_by=abs(shift_by)
      if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
      else if (shift_by < 0 )
        out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
      else
        out<-x
      out
    }
    
    ##
    
    datWB$resA <- NA
    datWB <- 
      mutate(datWB, 
             resA = derivedFactor(
               "1" = (outbreak_probability>alarm_threshold),
               .method = "first",
               .default = 0
             ))
    
    datWB$resA <- as.numeric(as.character(datWB$resA))
    datWB$resA_L1 <-shift(datWB$resA,-1)
    datWB$resA_L2 <-shift(datWB$resA,-2)
    
    datWB$resA[is.na(datWB$resA)] <- 0
    datWB$resA_L1[is.na(datWB$resA_L1)] <- 0
    datWB$resA_L2[is.na(datWB$resA_L2)] <- 0
    
    ##
    datWB$resB <- NA
    datWB <- 
      mutate(datWB, 
             resB = derivedFactor(
               "1" = (outbreak_indicator>endemic_channel),
               .method = "first",
               .default = 0
             ))
    
    datWB$resB <- as.numeric(as.character(datWB$resB))
    datWB$resB_L1 <-shift(datWB$resB,-1)
    datWB$resB_L2 <-shift(datWB$resB,-2)
    
    datWB$resB[is.na(datWB$resB)] <- 0
    datWB$resB_L1[is.na(datWB$resB_L1)] <- 0
    datWB$resB_L2[is.na(datWB$resB_L2)] <- 0
    
    ## trigger
    datWB$res_no <- datWB$resA
    datWB$res_no2w <- datWB$resA + datWB$resA_L1
    datWB$res_no3w <- datWB$resA + datWB$resA_L1 + datWB$resA_L2
    
    datWB$res_late3w <- datWB$resB + datWB$resB_L1 + datWB$resB_L2
    
    ## trigger
    datWB$trigger <- NA
    datWB <- 
      mutate(datWB, 
             trigger = derivedFactor(
               "4" = (res_late3w==3),
               "3" = (res_no==1&res_no3w==3),
               "2" = (res_no==1&res_no2w==2),
               "1" = (res_no==1&res_no3w<3&res_no2w<2),
               .method = "first",
               .default = NA
             ))
    
    datWB$trigger[is.na(datWB$weekly_hospitalised_cases)] <- NA
    datWB$trigger <- as.numeric(as.character(datWB$trigger))
    datWB <- datWB[,c(1:14,25)]
    
    return(datWB)
  })
  
  # output$graph_outbreak <- renderDygraph({
  #   
  #   datPro <- datWB()
  #   
  #   datProOut <- select(datPro, WoYdate,outbreak_indicator,endemic_channel)
  #   datProOut_xts <- xts(datProOut[,-1], order.by=datProOut$WoYdate)
  #   
  #   dygraph(datProOut_xts) %>%
  #     dySeries("outbreak_indicator", label="confirmed cases", color="red") %>%
  #     dySeries("endemic_channel", label="endemic channel", fillGraph=TRUE, color="grey") %>%
  #     dyLegend(width=700) %>%
  #     dyOptions(drawGrid=F)
  # })
  
  output$graph_outbreak2 <- renderPlot({
    dat <- datWB()
    dat <- select(dat, Week,endemic_channel,outbreak_indicator)
    
    line_x <- c(1,dat$Week,53)
    line_y <- c(0,dat$endemic_channel,0)
    
    plot(1, type="n", xlab="epidemiological week", ylab="", axes=F, xlim=c(2,52), ylim=c(0,0.8))
    axis(1, at=c(2:52),labels=c(2:52))
    axis(2, at=c(0,0.1,0.2,0.3,0.4,0.5),labels=c(0,0.1,0.2,0.3,0.4,0.5))
    
    polygon(line_x, line_y, col='grey75', border='grey75')
    points(dat$Week, dat$outbreak_indicator, type="o", col="red", lwd=2)
    
    legend("topleft", legend=c("confirmed cases", "endemic channel"),
           col=c("red", "grey75"), lty=1:1, cex=1.0,
           box.lty=0)
  })
  
  # output$graph_probability <- renderDygraph({
  #   
  #   datPro <- datWB()
  #   
  #   datProProb <- select(datPro, WoYdate,outbreak_probability,alarm_threshold)
  #   datProProb$Outbreak.probability[is.na(datPro$weekly_hospitalised_cases)] <- NA
  #   
  #   datProProb_xts <- xts(datProProb[,-1], order.by=datProProb$WoYdate)
  #   
  #   dygraph(datProProb_xts) %>%
  #     dySeries("outbreak_probability", label="outbreak probability", color="darkgreen") %>%
  #     dySeries("alarm_threshold", label="alarm threshold", color="blue",
  #              strokeWidth=2, strokePattern="dashed") %>%
  #     dyLegend(width=700) %>%
  #     dyOptions(drawGrid=F)
  # })
  
  output$graph_probability2 <- renderPlot({
    dat <- datWB()
    dat <- select(dat, Week,outbreak_probability,alarm_threshold,weekly_hospitalised_cases)
    
    dat$outbreak_probability[is.na(dat$weekly_hospitalised_cases)] <- NA
    
    plot(1, type="n", xlab="epidemiological week", ylab="", axes=F, xlim=c(2,52), ylim=c(0,0.8))
    axis(1, at=c(2:52),labels=c(2:52))
    axis(2, at=c(0,0.1,0.2,0.3,0.4,0.5),labels=c(0,0.1,0.2,0.3,0.4,0.5))
    
    points(dat$Week, dat$alarm_threshold, type="l", col="darkgreen", lwd=2, lty=2)
    points(dat$Week, dat$outbreak_probability, type="o", col="darkblue", lwd=2)
    
    legend("topleft", legend=c("alarm threshold", "outbreak probability"),
           col=c("darkgreen", "darkblue"), lty=1:1, cex=1.0,
           box.lty=0)
  })
  
  # output$graph_alarm <- renderDygraph({
  #   
  #   datPro <- datWB()
  #   
  #   datProALARM <- select(datPro, WoYdate,outbreak_indicator,endemic_channel,outbreak_probability,alarm_threshold)
  #   datProALARM$outbreak_indicator[is.na(datPro$weekly_hospitalised_cases)] <- NA
  #   datProALARM$endemic_channel[is.na(datPro$weekly_hospitalised_cases)] <- NA
  #   datProALARM$outbreak_probability[is.na(datPro$weekly_hospitalised_cases)] <- NA
  #   
  #   datProALARM_xts <- xts(datProALARM[,-1], order.by=datProALARM$WoYdate)
  #   
  #   dygraph(datProALARM_xts) %>%
  #     dySeries("outbreak_indicator", label="confirmed cases", color="red", axis='y2') %>%
  #     dySeries("endemic_channel", label="endemic channel", fillGraph=TRUE, color="grey", axis='y2') %>%
  #     dySeries("outbreak_probability", label="outbreak probability", color="darkgreen") %>%
  #     dySeries("alarm_threshold", label="alarm threshold", color="blue",
  #              strokeWidth=2, strokePattern="dashed") %>%
  #     dyLegend(width=700) %>%
  #     dyOptions(drawGrid=F)
  # })
  
  output$graph_alarm2 <- renderPlot({
    dat <- datWB()
    dat <- select(dat, Week,endemic_channel,outbreak_indicator,outbreak_probability,alarm_threshold,weekly_hospitalised_cases)
    
    dat$outbreak_probability[is.na(dat$weekly_hospitalised_cases)] <- NA
    
    line_x <- c(1,dat$Week,53)
    line_y <- c(0,dat$endemic_channel,0)
    
    plot(1, type="n", xlab="epidemiological week", ylab="", axes=F, xlim=c(2,52), ylim=c(0,0.8))
    axis(1, at=c(2:52),labels=c(2:52))
    axis(2, at=c(0,0.1,0.2,0.3,0.4,0.5),labels=c(0,0.1,0.2,0.3,0.4,0.5))
    
    polygon(line_x, line_y, col='grey75', border='grey75')
    points(dat$Week, dat$outbreak_indicator, type="o", col="red", lwd=2)
    
    points(dat$Week, dat$alarm_threshold, type="l", col="darkgreen", lwd=2, lty=2)
    points(dat$Week, dat$outbreak_probability, type="o", col="darkblue", lwd=2)
    
    legend("topleft", legend=c("confirmed cases", "endemic channel", "alarm threshold", "outbreak probability"),
           col=c("red", "grey75", "darkgreen", "darkblue"), lty=1:1, cex=1.0,
           box.lty=0)
  })
  
  # output$graph_response <- renderDygraph({
  #   
  #   datPro <- datWB()
  #   
  #   datTRIGGER <- select(datPro, WoYdate,trigger)
  #   datTRIGGER$trigger[is.na(datPro$weekly_hospitalised_cases)] <- NA
  #   datTRIGGER_xts <- xts(datTRIGGER[,-1], order.by=datTRIGGER$WoYdate)
  #   
  #   #library(dygraphs)
  #   
  #   dygraph(datTRIGGER_xts) %>%
  #     dyAxis("y", valueRange=c(0.5,4.5)) %>% 
  #     dySeries("trigger", label="response", color="red", stepPlot=TRUE) %>% 
  #     #dySeries("weekly_hospitalised_cases", label="cases", color="darkblue") %>%
  #     dyLegend(width=700) %>%
  #     dyOptions(drawGrid=F, drawPoints=TRUE, pointSize=4)
  # })
  
  output$graph_response2 <- renderPlot({
    dat <- datWB()
    dat <- select(dat, Week,trigger)
    
    # plot(1, type="n", xlab="epidemiological week", ylab="", axes=F, xlim=c(2,52), ylim=c(0,6))
    # axis(1, at=c(2:52),labels=c(2:52))
    # #axis(2, at=c(1:4),labels=c(1:4))
    # 
    # abline(h=c(1:4), col=c("darkgreen","darkorange","red","brown"), lty=1)
    # text(1,1, "no response", col="darkgreen", adj=c(0,-1))
    # text(1,2, "initial response", col="darkorange", adj=c(0,-1))
    # text(1,3, "early response", col="red", adj=c(0,-1))
    # text(1,4, "late/emergency response", col="brown", adj=c(0,-1))
    # 
    # points(dat$Week, dat$trigger, type="h", lwd=2)
    
    #plot(1, type="n", xlab="epidemiological week", ylab="", axes=F, xlim=c(2,52), ylim=c(0,6))
    plot(1, type="n", xlab="epidemiological week", ylab="", axes=F, xlim=c(-15,52), ylim=c(0,6))
    axis(1, at=c(2:52),labels=c(2:52))
    
    abline(h=c(1:4), col=c("darkgreen","darkorange","red","brown"), lty=1)
    text(-15,1, "no response", col="darkgreen", adj=c(0,-1))
    text(-15,2, "initial response", col="darkorange", adj=c(0,-1))
    text(-15,3, "early response", col="red", adj=c(0,-1))
    text(-15,4, "late/emergency response", col="brown", adj=c(0,-1))
    
    points(dat$Week, dat$trigger, type="p", lwd=2)
    
  })
  
  dat_wStatL <- eventReactive(input$go_wStatL, {
    load("datStation.RData")
    lat_lon_df <- data.frame(id="study_area", name="study_area", 
                             latitude=input$area_lat, longitude=input$area_lon) #-7.797068 #110.370529
    
    # Get all stations within 150 kilometers
    meteo_stat <- meteo_nearby_stations(lat_lon_df = lat_lon_df, station_data = station_data,
                                        radius = input$area_rad, var = c("PRCP","TMIN","TMAX"))
    
    meteo_stat <- meteo_stat$study_area
    meteo_stat <- rbind(lat_lon_df,meteo_stat[,c(1:4)])
    return(meteo_stat)
  })
  
  output$map_wStat <- renderLeaflet({
    datMap <- dat_wStatL()
    lat_lon_df <- datMap[1,]
    meteo_stat <- datMap[-1,]
    
    leaflet() %>% addTiles() %>%
      addCircleMarkers(data=lat_lon_df, ~longitude, ~latitude, radius = 5, color = "red", label = ~as.character(id)) %>%
      addCircleMarkers(data=meteo_stat, ~longitude, ~latitude, radius = 5, color = "blue", popup = ~as.character(id), label = ~as.character(name))
    
  })
  
  dat_wStatT <- eventReactive(input$go_wStatT, {
    datMap <- dat_wStatL()
    meteo_stat <- datMap[-1,]
    
    meteo_dat <- meteo_pull_monitors(meteo_stat$id,
                                     date_min = input$wStat_min,
                                     date_max = input$wStat_max) %>%
      rename(day = date, location = id)
    return(meteo_dat)
  })
  
  dat_wStatS <- eventReactive(input$go_wStatS, {
    meteo_datS <- meteo_pull_monitors(input$WstatID,
                                      date_min = input$wStat_min,
                                      date_max = input$wStat_max) %>%
      rename(day = date, location = id)
    
    meteo_datS <- select(meteo_datS, day,prcp,tavg) %>% mutate(tavg=(tavg/10))
    meteo_datS$YM <- as.yearmon(meteo_datS$day)
    
    meteo_datS <- select(meteo_datS, YM,prcp,tavg) %>% group_by(YM) %>%
      summarise(tavg=round(mean(tavg, na.rm=T),2),
                prcp=(sum(prcp, na.rm=T)))
    meteo_datS$YM <- as.character(meteo_datS$YM)
    return(meteo_datS)
  })
  
  output$Wplot <- renderPlot({
    meteo_dat <- dat_wStatT()
    
    meteo_plot <- meteo_dat %>%
      select(-tmax, -tmin) %>%
      tidyr::gather(parameter, value, tavg:prcp)
    
    ggplot(meteo_plot) +
      geom_line(aes(x = day, y = value, col = location)) +
      facet_grid(parameter ~ ., scales = "free_y") + theme_bw()
  })
  ##
  output$tblwStatS <- DT::renderDataTable({
    tblwStatS <- dat_wStatS()
    datatable(tblwStatS, options = list(pageLength=12))
  })
  
  datTweets <- reactive({
    download.file("http://globalminers8973.cloudapp.net/gl_twts.csv.gz", "data/gl_twts.csv.gz")
    datTweets <- read_csv("data/gl_twts.csv.gz")
    return(datTweets)
  })
  
  datTweetsGeo <- reactive({
    datTweetsGeo <- datTweets()
    datTweetsGeo <- drop_na(datTweetsGeo, c(lon,lat))
    return(datTweetsGeo)
  })
  
  output$mymap <- renderLeaflet({
    datTweetsGeo <- datTweetsGeo()
    leaflet() %>% addTiles() %>%
      addCircleMarkers(data=datTweetsGeo, ~lon, ~lat, radius=1, color="red", label=~as.character(location))
  })
  
  output$n_tweets <- renderValueBox({
    datTweets <- datTweets()
    valueBox("Tweets", nrow(datTweets), icon = icon("twitter"), color = "purple" )
  })
  
  output$n_users <- renderValueBox({
    datTweets <- datTweets()
    valueBox("Users", length((unique(datTweets$user_id_str))), icon = icon("user"), color = "yellow" )
  })
  
  output$n_usersLoc <- renderValueBox({
    datTweets <- datTweetsGeo()
    valueBox("Users with location data", length((unique(datTweets$user_id_str))), icon = icon("user"), color = "green" )
  })
  
  datGST <- reactive({
    
    validate(
      need(gs_country_code() != "XX", "the country code is not valid")
    )
    
    datGST <- gtrends(c("dengue","zika","chikungunya"), geo=c(gs_country_code()))
    
    return(datGST)
    
  })
  
  output$graph_GST <- renderPlot({
    
    datGST <- datGST()
    plot(datGST)
    
  })
  
  output$graph_wiki <- renderDygraph({

    wikiDen <- read.csv(url("http://globalminers8973.cloudapp.net/pageviews.csv"))
    wikiDen$Date <- as.Date(wikiDen$Date, format="%Y-%m-%d")
    wikiDen_xts <- xts(wikiDen[,-1], order.by=wikiDen$Date)
    
    dygraph(wikiDen_xts) %>%
      dySeries("Dengue.fever", label="en.wikipedia.org/wiki/Dengue_fever", color="red") %>%
      dySeries("Dengue.virus", label="en.wikipedia.org/wiki/Dengue_virus", color="blue") %>%
      dyLegend(width=700) %>%
      dyOptions(drawGrid=F, drawPoints=TRUE) %>%
      dyRangeSelector()
  })
  
  output$video <- renderUI({
    my_vid <- tags$iframe(src="https://www.youtube.com/embed/japeI6ul9uk", height=409.5, width=728, frameborder="0")
    print(my_vid)
    my_vid
  })
  
}


# APP ---------------------------------------------------------------------

shinyApp(ui, server)