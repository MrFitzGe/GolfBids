# Shiny App for NNet Log Data Analysis

## Load Packages -------------
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggridges)
library(lubridate)
# library(cowplot)
# library(corrr)
# require(brotools)
# require(data.table)

## Grab Data and preprocess -----------


# Helper functions
source("../GLserverCourseList.R")
source("../archiveReader.R")

# Log file directory
datDir ="../GolfLineLogData/"
archDir = "../BidLogArchives_Spring2019"

## Gather Data -------

listedFiles <-  list.files(path = datDir, pattern = "*.csv")

df <- listedFiles %>% 
  map(function(x) {
    read_csv(paste0(datDir, x)) 
  }) %>%
  reduce(rbind)

# add archived bids
archivedBids_df <- readMultiDirectories(archDir)

if (exists("archivedBids_df")) {
  df <- bind_rows(df, archivedBids_df)
}

## Clean Data ---------
CleanLog_df <- df %>% 
  select(-scaled_input_data_array) %>% 
  mutate(timeOfBidResolve = ymd_hms(timeOfBidResolve)) %>% 
  mutate(timeOfBidResolve = timeOfBidResolve- hours(4)) %>% 
  mutate(requestedStartTime = mdy_hm(requestedStartTime)) %>% 
  mutate(requestedEndTime = mdy_hm(requestedEndTime)) %>% 
  # filter(timeOfBidResolve >  ymd_hms("2018-07-07 00:00:00")) %>%
  mutate(isAutoBid = as.logical(isAutoBid)) %>% 
  mutate(successfulBid = ifelse(bidAmount >= output_MAB_perRound & bidAmount >= floorPrice, 1, 0)) %>% 
  mutate(bidAboveMAB = ifelse(bidAmount >= output_MAB_perRound, 1, 0)) %>% 
  mutate(relativeMAB = output_MAB_perRound/onlineRate) %>% 
  mutate(ttHour = factor(hour(requestedStartTime)))  %>%
  mutate(ttDay = factor(wday(requestedStartTime, label = T, week_start = getOption("lubridate.week.start", 1))))  %>% 
  mutate(ttDate = factor(date(requestedStartTime))) %>% 
  mutate(courseID = factor(GLserverCourseList[courseID]),
         customerID = factor(customerID),
         isWalking = as.logical(isWalking),
         numHoles = factor(numHoles),
         strategy = factor(strategy, levels = c("1", "2", "3", "4", "5"), labels = c("Conservative", "Moderate", "Aggressive", "Early-Fill", "Custom"))) %>% 
  filter(!is.null(timeOfBidResolve)) %>% 
  arrange(., desc(timeOfBidResolve))

CleanLog_df$Session <- CleanLog_df %>%  
  group_by(customerID, ttDate) %>% 
  group_indices() 

CleanLog_df <- CleanLog_df %>% 
  mutate(Session = factor(Session))

# HumanBid_df <- CleanLog_df %>%
#   filter(isAutoBid == "False") %>%
#   arrange(desc(timeOfBidResolve))


## Supplementary Functions -------------

# Function for defining the mode
trueMode <- function(x) {
  ux <- unique(x)
  
  modey <- ux[which.max(tabulate(match(x, ux)))]
  return(modey)
}

# Functions for defining BAS
bid_strat_discount <- function(strategy, hours2tt){
  switch(strategy,
         Moderate = 1*(1-(1/(1+exp(-6.5*(((168-hours2tt)/168) - 0.3))) - 1/(1+exp(6.5*0.3)))*(1+1/exp(6.5*0.3))),
         Aggressive = 1*(1-(1/(1+exp(-10*(((168-hours2tt)/168) - 0.1))) - 1/(1+exp(10* 0.1)))*(1+1/exp(10* 0.1))), 
         Conservative = 1*(1-(1/(1+exp(-13*(((168-hours2tt)/168) - 0.7))) - 1/(1+exp(13*0.7)))*(1+1/exp(13*0.7)))
  )
}

generateMAB <- function(BASratio, EV, floorPrice, onlineRate){
  MAB = (EV + floorPrice + ((onlineRate - floorPrice)*BASratio))/2
  return(MAB)
}


## Define UI -------------------

### Define Header bar ------
header <- dashboardHeader(title = "Bid Analysis App")

### Define SideBar for Navigation and Global inputs------
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Bids", tabName = "Bids", icon = icon("calendar")),
  menuItem("Summaries", tabName = "Summaries", icon = icon("list")),
  menuItem("KPIs", tabName = "KPIs", icon = icon("dashboard")),
  menuItem("Sessions", tabName = "Sessions", icon = icon("list-alt")),
  menuItem("MAB Distribution", tabName = "MABSpread", icon = icon("area-chart")),
  menuItem("MAB Lead-Up", tabName = "MABramp", icon = icon("line-chart")),
  menuItem("Course Utilization", tabName = "CourseUtil", icon = icon("bar-chart")),
  menuItem("Bid Accept Strats Analysis", tabName = "BAS", icon = icon("refresh")),
  menuItem("Floor Price Finder", tabName = "FloorPriceFinder", icon = icon("binoculars")),
  menuItem("Model Input Correlations", tabName = "modCorr", icon = icon("align-left")),
  
  dateRangeInput("filterDates", "Date Range:",
                 start = "2018-07-10",
                 end = Sys.Date()+days(1),
                 format = "yyyy-mm-dd"),
  checkboxInput("humanBidsOnly","Only Show Human Bids", FALSE),
  checkboxInput("excludeGLEmployees","Remove GL Employees and Test accounts", TRUE),
  selectInput("courseFilter", "Course Scope:", c("All", GLserverCourseList), selectize = T, multiple = T, selected = "All"),
  downloadButton('downloadData',"Download the data")
))

### Define Body (each page of output) --------
body <- dashboardBody(
  tabItems(
    # Bids tab content
    tabItem(tabName = "Bids",
            fluidRow(
              box(dataTableOutput("SuccessfulBidsBreakdown"), width = 12, title = "Bids Accepted")
            ),
            
            fluidRow(
              box(dataTableOutput("BidsTable"), width = 12, title = "Bids (Most Recent)")
            )
    ),
    
    # Summaries tab content
    tabItem(tabName = "Summaries",
            fluidRow(
              box(dataTableOutput("DataSummaryTable"), width = 12, title = "Data Summary")
            )
    ),
    
    # KPI tab content
    tabItem(tabName = "KPIs",
            fluidRow(
              tabBox(title = "KPIs", id = "KPItabs", width = 12,
                     tabPanel("Course", dataTableOutput("CourseKPItable")),
                     tabPanel("Customer", dataTableOutput("CustomerKPItable")))
            )
    ),
    
    # Sessions tab content
    tabItem(tabName = "Sessions",
            fluidRow(
              box(dataTableOutput("SessionsSummaryTable"),  width = "70%", height = "auto", title = "Sessions Summary")
            ),
            
            fluidRow(
              box(dataTableOutput("SessionsTable"),  width = "70%", height = "auto", title = "Sessions")
            )
    ),
    
    # MAB distribution tab content
    tabItem(tabName = "MABSpread",
            fluidRow(
              # column(width = 4, offset = 2,
              box(selectInput("y_var", label = "Select the variable you want to examine:", 
                              choices = c("Neural Net Predicted Utilization" = "output_NNpredUtilization",
                                          "Minimum Acceptable Bid ($MAB)" = "output_MAB_perRound",
                                          "MAB ratio to the Online Rate" = "relativeMAB"))),
              box(selectInput("plot_grouping_vars", label = "Select Course Feature to Segment:", 
                              choices = c("No Segmentation" = " ",
                                          "By Course" = "courseID",
                                          "By Bid Acceptance Strategy" = "strategy",
                                          "By Tee Time Hour" = "ttHour",
                                          "By Day of the Week" = "ttDay",
                                          "Walking vs Riding" = "isWalking",
                                          "Number of Holes" = "numHoles")))
              
            ),
            
            fluidRow(
              
              box(plotOutput("MABplot"),  width = "70%", height = "auto", title = "Distribution Graph" )
            )
    ),

    # BAS Analysis
    tabItem(tabName = "BAS",
            fluidRow(
              tabBox(title = "Results of Alternative Bid Acceptance Strategy Analysis", id = "BAStabs", width = 12,
                     tabPanel("Course", dataTableOutput("BAStable_Courses")),
                     tabPanel("Session", dataTableOutput("BAStable_Sessions")))
                    )
            ),
    
    # Floor Price Finder
    
    tabItem(tabName = "FloorPriceFinder",
            fluidRow(
              box(sliderInput("altFloorPrice", label = "Modify Floor Price by X%:", min = -25, max = 25, value = 0, step = 5),  width = "70%", height = "auto", title = "Input Additive Floor Price Adjustment")
            ),
            
            fluidRow(
              box(dataTableOutput("AlternateFloorRevenue"),  width = "70%", height = "auto", title = "Floor Price Comparison")
            )
    ),
    
    # MAB Ramp
    tabItem(tabName = "MABramp",
            fluidRow(
              # column(width = 4, offset = 2,
              box(selectInput("courseRampFilter", label = "Select Course:", 
                              choices = GLserverCourseList))
              
            ),
            
            fluidRow(
              
              box(plotOutput("MABramp"), width = "70%", height = "auto", title = "MAB values leading up to a tee time")
            )
    ),
    
    # Course Utilization Ramp
    tabItem(tabName = "CourseUtil",
            fluidRow(
              # column(width = 4, offset = 2,
              box(selectInput("courseUtilFilter", label = "Select Course:", 
                              choices = GLserverCourseList))
              
            ),
            fluidRow(
              
              box(plotOutput("UtilizationTimeSeries"), width = "70%", height = "auto", title = "Weekly Utilization")
            ),
            
            fluidRow(
              
              box(plotOutput("UtilizationHeatmap"), width = "70%", height = "auto", title = "Breakdown by Tee Time Hours")
            )
    ),
    
    # MAB Input Correlations
    tabItem(tabName = "modCorr",
            
            fluidRow(
              # box(dataTableOutput("FeatureCorrFrame"),  width = "70%", height = "auto", title = "Linear Correlations between Model Input and Desired Output" )
              
              box(plotOutput("FeatureCorrPlot"),  width = "70%", height = "auto", title = "Linear Correlations between Model Input and Desired Output" )
            )
    )
  )
)



ui <- dashboardPage(skin = "green", header, sidebar, body)


## Define server logic required to draw all output -------
server <- function(input, output, session) {
  
  # currentTimeStamp
  currentTimeStamp <- Sys.time()
  
  # currentTimeStampUTC
  currentTimeStampUTC <- with_tz(currentTimeStamp, tzone = "UTC")
  
  ## Potential Filters for Data Set and Download ---------
  #Filter by time inputs
  logSubset_Time <- reactive({
    CleanLog_df %>%
    filter(timeOfBidResolve <= (input$filterDates[2])) %>%
    filter(timeOfBidResolve >= (input$filterDates[1]))
  })
  
  #Filter by Course
  logSubset_Course <- reactive({
    if (input$courseFilter == "All") {
      logSubset_Time()
      
    }else{
      logSubset_Time() %>%
        filter(courseID %in% input$courseFilter)
    }
    
  })
 
  #Filter out GL Employees
  
  logSubset_CustomOnly <- reactive({
    
    if(input$excludeGLEmployees == T){
      logSubset_Course() %>%
        filter(!customerID %in% employeeCIDsList)
    }else{
      logSubset_Course()
    }
    
  })
  
  #Filter Human Bids only 
  logSubset_Human <- reactive({
    
    if(input$humanBidsOnly == T){
      logSubset_CustomOnly() %>%
            filter(isAutoBid == F)
      }else{
        logSubset_CustomOnly()
    }
    
  })
  
  ## Create output Bid table --------------
  output$BidsTable <- renderDataTable({
    logSubset_Human() %>% 
      mutate(`Accepted Bid` = ifelse(bidAmount >= output_MAB_perRound & bidAmount >= floorPrice, 1, 0)) %>% 
      mutate(`Bid Received EST` = timeOfBidResolve,
             `Customer ID` = customerID,
             `Bid Amount` = scales::dollar(bidAmount),
             `Online Rate` = scales::dollar(onlineRate),
             `MAB` = scales::dollar(output_MAB_perRound),
             `Floor Price` = scales::dollar(floorPrice),
             `Bid Successful` = successfulBid,
             `Course` = courseID,
             `Hours prior to Tee Time` = hoursToTeeTime,
             `Number of Golfers` = numGolfers,
             `Requested Start Hour` = requestedStartTime) %>% 
      mutate(`Pct of Rounds Available for Requested Hour` = scales::percent(ttRemainingPerc)) %>% 
      select(`Bid Received EST`,`Customer ID`, `Course`, `Bid Successful`, `Bid Amount`,`Online Rate`, `MAB`, `Floor Price`, `Hours prior to Tee Time`, `Number of Golfers`, `Requested Start Hour`, `Pct of Rounds Available for Requested Hour`) %>% 
      arrange(desc(`Bid Received EST`))  %>% 
      mutate_if(is.numeric, round, 2)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("bids_", format(Sys.Date(), "%m %d %y"), ".csv", sep="")
    },
    content = function(file) {
      write_csv(logSubset_Human(), file)
    })
  
  ## Breakdown stats for number of bids and Bidders -----
  output$SuccessfulBidsBreakdown <- renderDataTable({
    logSubset_Human() %>% 
      # group_by(successfulBid) %>% 
      filter(successfulBid == 1) %>% 
      summarise(`Number of Successful Bids` = sum(`successfulBid`),
                `Revenue Generated` = scales::dollar(sum(as.numeric(numGolfers)*(bidAmount))),
                `Average $ Savings from Online Rate per transaction` = scales::dollar(mean((onlineRate - bidAmount))), 
                `Average % Savings off Online Rate per transaction` = scales::percent(mean(((onlineRate - bidAmount)/onlineRate)))) %>% 
      mutate_if(is.numeric, round, 2) 
      
  })
  
  
  # Create output summary tables for Sumamries Tab--------------
  output$DataSummaryTable <- renderDataTable({
    logSubset_Human() %>% 
      mutate_if(is.numeric, round, 3) %>% 
      brotools::describe(.) %>% 
      mutate_if(is.numeric, round, 3)
      
  })
  
  
  # Create Output Distribution plots for MAB Tab----------------------
  output$MABplot <- renderPlot({
    
    if(input$y_var == "output_MAB_perRound"){
      
      hold_max_X  <-  logSubset_Human() %>% summarise(mean(floorPrice))
      meanFloor <- hold_max_X[[1]]
    }else{
      
      hold_max_X  <-  logSubset_Human() %>% summarise(mean(floorPrice/onlineRate))
      meanFloor <- hold_max_X[[1]]
      
    }
    
    
    
    if(input$plot_grouping_vars == " "){
    
    logSubset_Human() %>% 
    ggplot(aes_string(x=input$y_var)) + 
        geom_density(color = "orange", fill = "darkgreen", alpha = 0.8) +
        geom_vline(xintercept = meanFloor, linetype=2, size = 2, color = "red") +
        labs(title = "Distribution of selected variable", subtitle = "Red Line shows average floor price", xlab = input$y_var) + 
        guides(fill = F, alpha = F) +
        xlim(0, NA) +
        theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18,face="bold"))
      
    }else{
      
      logSubset_Human() %>% 
        ggplot(aes_string(x=input$y_var, y = input$plot_grouping_vars, fill = input$plot_grouping_vars)) +
        geom_density_ridges(scale = 4, alpha = 0.8) + scale_y_discrete(expand = c(0.01, 0)) +  scale_x_continuous(expand = c(0, 0)) +
        labs(title = "Distribution of selected variable by segment", xlab = input$y_var) +
        theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18,face="bold"))
      
    }
  }, height = function() {
    session$clientData$output_MABplot_width*0.45}
  )

  ## Create KPI stats tables for KPIs Tab ----------------------
  
  # Customer KPIs
  output$CustomerKPItable <- renderDataTable({
    logSubset_Human() %>% 
    group_by(customerID) %>%
    summarise(`Number of Bids` = n(),
              `Number of Sessions` = n_distinct(Session),
              `Successful Bid Rate` = mean(successfulBid),
              `Mean Bid $Amount` = scales::dollar(mean(bidAmount)),
              `Mean Bid $Amount - Floor` = scales::dollar(mean(bidAmount - floorPrice)),
              `Mean Bid percent of Online Rate` = scales::percent(mean(bidAmount/onlineRate)),
              `Most Common Course` = trueMode(courseID),
              `Most common Group Size` = trueMode(numGolfers),
              `Most common Num Holes Played` = trueMode(numHoles),
              `Mean Hours To Tee Time` = mean(hoursToTeeTime))%>% 
      mutate_if(is.numeric, round, 2)          
  
  })
  
  # Course KPIs
  output$CourseKPItable <- renderDataTable({
    logSubset_Human() %>% 
    group_by(courseID, successfulBid) %>%
    summarise(`Number of Bids` = n(),
              `Number of Sessions` = n_distinct(Session),
              `Mean Bid $Amount` = scales::dollar(mean(bidAmount)),
              `Mean Floor Discount %` = scales::percent(mean(1-(floorPrice/onlineRate))),
              `Mean Bid $Amount - Floor` = scales::dollar(mean(bidAmount - floorPrice)),
              `Mean Online Rate - Bid $Amount` = scales::dollar(mean(onlineRate-bidAmount)),
              `Mean Utilization at Time of Bid` = scales::percent(mean((1-ttRemainingPerc))),
              `Mean HoursToTeeTime` = mean(hoursToTeeTime),
              `Most common Group Size` = trueMode(numGolfers),
              `Most common Num Holes Played`  = trueMode(numHoles))%>% 
      mutate_if(is.numeric, round, 2)
  })
  
  # Session KPIs
  output$SessionKPItable <- renderDataTable({
    logSubset_Human() %>% 
      group_by(Session) %>%
      mutate(`Successful Session` = max(successfulBid)) %>%
      mutate(RevenuePotential = ifelse(`Successful Session` == 1,
numGolfers[which.min(bidAmount[successfulBid == 1])]*min(bidAmount[successfulBid == 1]),
numGolfers[which.max(bidAmount)]*max(bidAmount))) %>% 
      summarise()
        
  })
  
  # Create Sessions stats tables for Sessions Tab----------------------
  
  ## make summary of each session for bottom box
  output$SessionsTable <- renderDataTable({
    logSubset_Human() %>% 
      group_by(Session, customerID, ttDate) %>%
      summarise(`Number of Bids` = n_distinct(timeOfBidResolve),
                `Successful Bid Rate` = mean(successfulBid),
                `Mean Bid $Amount` = scales::dollar(mean(bidAmount)),
                `Mean Bid $Amount - Floor` = scales::dollar(mean(bidAmount - floorPrice)),
                `Mean Bid percent of Online Rate` = scales::percent(mean(bidAmount/onlineRate)),
                `Most Common Course` = trueMode(courseID),
                `Most common Group Size` = trueMode(numGolfers),
                `Most common Num Holes Played` = trueMode(numHoles),
                `Mean HoursToTeeTime` = mean(hoursToTeeTime))%>% 
      mutate_if(is.numeric, round, 2)          
  })
  
  
  
  ## make summary for top box/sessions overview
  viableCutoff = 0.5
  
  output$SessionsSummaryTable <- renderDataTable({
    logSubset_Human() %>% 
      mutate(bidAboveFloor = ifelse(bidAmount >= floorPrice, 1, 0)) %>%
      mutate(bidViable = ifelse(bidAmount/onlineRate >= viableCutoff, 1, 0)) %>%
      group_by(Session) %>%
      mutate(`Session Revenue Potential` = ifelse(max(successfulBid) == 1,
                                          numGolfers[which.min(bidAmount[successfulBid == 1])]*min(bidAmount[successfulBid == 1]),
                                          numGolfers[which.max(bidAmount)]*max(bidAmount) )) %>% 
      summarise(`Number of Bids` = n_distinct(timeOfBidResolve),
                `One Bid Above MAB` = max(bidAboveMAB),
                `Real Revenue` = sum(successfulBid*numGolfers*bidAmount),
                `Revenue Potential` = max(`Session Revenue Potential`),
                `Revenue without Floor` =  ifelse(is.infinite(min(bidAmount[bidAboveMAB == 1])), 0, numGolfers[which.min(bidAmount[bidAboveMAB == 1])]*min(bidAmount[bidAboveMAB == 1])),
                `One Bid Above Floor` = max(bidAboveFloor),
                `One Viable Bid` = max(bidViable),
                `Successful Session` = max(successfulBid))%>% 
      summarise(`Mean Number of Bids per Session` = mean(`Number of Bids`),
                `Sessions with a bid above MAB` = sum(`One Bid Above MAB`),
                `Total Revenue` = scales::dollar(sum(`Real Revenue`)),
                `Total Revenue without Floor` = scales::dollar(sum(`Revenue without Floor`)),
                `Total Revenue + HiBid of Rejected Sessions` = scales::dollar(sum(`Revenue Potential`)),
                `Sessions with a bid above Floor` = sum(`One Bid Above Floor`),
                `Sessions with a Successful Bid` = sum(`Successful Session`),
                `Sessions with 1 Viable Bid` = sum(`One Viable Bid`)) %>% 
      mutate_if(is.numeric, round, 2)      
  })
  
  
  ## Create stats for comparison of alternative bid acceptance strategies -------------
  
  output$BAStable_Sessions <- renderDataTable({
    logSubset_Human() %>% 
      select(Session, customerID, courseID, customerID, bidAmount, floorPrice, onlineRate, hoursToTeeTime, strategy, output_EV, timeOfBidResolve, numGolfers) %>% 
      rename(`Assigned Strategy` = strategy) %>% 
      mutate(bidAboveFloor = ifelse(bidAmount >= floorPrice, 1, 0)) %>%
      mutate(bidViable = ifelse(bidAmount/onlineRate >= viableCutoff, 1, 0)) %>%
      mutate(BASadjust_Con = bid_strat_discount("Conservative", hoursToTeeTime),
             BASadjust_Mod = bid_strat_discount("Moderate", hoursToTeeTime),
             BASadjust_Agg = bid_strat_discount("Aggressive", hoursToTeeTime)) %>% 
      mutate(MAB_Con = generateMAB(BASadjust_Con, output_EV, floorPrice, onlineRate),
             MAB_Mod = generateMAB(BASadjust_Mod, output_EV, floorPrice, onlineRate),
             MAB_Agg = generateMAB(BASadjust_Agg, output_EV, floorPrice, onlineRate)) %>% 
      mutate(bidAbove_ConMAB = ifelse(bidAmount >= MAB_Con, 1, 0),
             bidAbove_ModMAB = ifelse(bidAmount >= MAB_Mod, 1, 0),
             bidAbove_AggMAB = ifelse(bidAmount >= MAB_Agg, 1, 0)) %>% 
      group_by(Session, customerID) %>%
      summarise(`Number of Bids` = n(),
                `Revenue: Conservative` = ifelse(is.infinite(min(bidAmount[bidAbove_ConMAB == 1])), 0, numGolfers[which.min(bidAmount[bidAbove_ConMAB == 1])]*min(bidAmount[bidAbove_ConMAB == 1])),
                `Revenue: Moderate` = ifelse(is.infinite(min(bidAmount[bidAbove_ModMAB == 1])), 0, numGolfers[which.min(bidAmount[bidAbove_ModMAB == 1])]*min(bidAmount[bidAbove_ModMAB == 1])),
                `Revenue: Aggressive` = ifelse(is.infinite(min(bidAmount[bidAbove_AggMAB == 1])), 0, numGolfers[which.min(bidAmount[bidAbove_AggMAB == 1])]*min(bidAmount[bidAbove_AggMAB == 1])),
                `One Bid Above ConMAB` = max(bidAbove_ConMAB),
                `One Bid Above ModMAB` = max(bidAbove_ModMAB),
                `One Bid Above AggMAB` = max(bidAbove_AggMAB),
                `One Bid Above Floor` = max(bidAboveFloor),
                `One Viable Bid` = max(bidViable)) %>% 
      mutate(`Revenue: Conservative` = scales::dollar(`Revenue: Conservative`),
             `Revenue: Moderate` = scales::dollar(`Revenue: Moderate`),
             `Revenue: Aggressive` = scales::dollar(`Revenue: Aggressive`))
  })
  
  output$BAStable_Courses <- renderDataTable({
    logSubset_Human() %>% 
      select(Session, courseID, customerID, bidAmount, floorPrice, onlineRate, hoursToTeeTime, strategy, output_EV, timeOfBidResolve, numGolfers) %>% 
      rename(`Assigned Strategy` = strategy) %>% 
      mutate(bidAboveFloor = ifelse(bidAmount >= floorPrice, 1, 0)) %>%
      mutate(bidViable = ifelse(bidAmount/onlineRate >= viableCutoff, 1, 0)) %>%
      mutate(BASadjust_Con = bid_strat_discount("Conservative", hoursToTeeTime),
             BASadjust_Mod = bid_strat_discount("Moderate", hoursToTeeTime),
             BASadjust_Agg = bid_strat_discount("Aggressive", hoursToTeeTime)) %>% 
      mutate(MAB_Con = generateMAB(BASadjust_Con, output_EV, floorPrice, onlineRate),
             MAB_Mod = generateMAB(BASadjust_Mod, output_EV, floorPrice, onlineRate),
             MAB_Agg = generateMAB(BASadjust_Agg, output_EV, floorPrice, onlineRate)) %>% 
      mutate(bidAbove_ConMAB = ifelse(bidAmount >= MAB_Con, 1, 0),
             bidAbove_ModMAB = ifelse(bidAmount >= MAB_Mod, 1, 0),
             bidAbove_AggMAB = ifelse(bidAmount >= MAB_Agg, 1, 0)) %>% 
      group_by(courseID, Session) %>%
      summarise(`Number of Bids` = n(),
                `Revenue: Conservative` = ifelse(is.infinite(min(bidAmount[bidAbove_ConMAB == 1])), 0, numGolfers[which.min(bidAmount[bidAbove_ConMAB == 1])]*min(bidAmount[bidAbove_ConMAB == 1])),
                `Revenue: Moderate` = ifelse(is.infinite(min(bidAmount[bidAbove_ModMAB == 1])), 0, numGolfers[which.min(bidAmount[bidAbove_ModMAB == 1])]*min(bidAmount[bidAbove_ModMAB == 1])),
                `Revenue: Aggressive` = ifelse(is.infinite(min(bidAmount[bidAbove_AggMAB == 1])), 0, numGolfers[which.min(bidAmount[bidAbove_AggMAB == 1])]*min(bidAmount[bidAbove_AggMAB == 1])),
                `One Bid Above ConMAB` = max(bidAbove_ConMAB),
                `One Bid Above ModMAB` = max(bidAbove_ModMAB),
                `One Bid Above AggMAB` = max(bidAbove_AggMAB),
                `One Bid Above Floor` = max(bidAboveFloor),
                `One Viable Bid` = max(bidViable)) %>% 
      ungroup() %>%
      group_by(courseID) %>%
      summarise(`Mean Number of Bids per Session` = mean(`Number of Bids`),
                `Sessions with 1 bid above Conserv.MAB` = sum(`One Bid Above ConMAB`),
                `Sessions with 1 bid above Moderate.MAB` = sum(`One Bid Above ModMAB`),
                `Sessions with 1 bid above Aggro.MAB` = sum(`One Bid Above AggMAB`),
                `Sessions with 1 bid above Floor` = sum(`One Bid Above Floor`),
                `Sessions with 1 Viable Bid` = sum(`One Viable Bid`),
                `Revenue with Conservative` = scales::dollar(sum(`Revenue: Conservative`)),
                `Revenue with Moderate` = scales::dollar(sum(`Revenue: Moderate`)),
                `Revenue with Aggressive` = scales::dollar(sum(`Revenue: Aggressive`))) %>%
            mutate_if(is.numeric, round, 2)
  })
  
  
  ## MAB correlation with input features -------------------
  
  output$FeatureCorrPlot <- renderPlot({
  logSubset_Human() %>%
    ungroup() %>% 
    mutate(numGolfers = as.numeric(numGolfers)) %>%
    mutate(ttHour = as.numeric(ttHour)) %>%
    mutate(ttDay = as.numeric(ttDay)) %>%
    select(output_NNpredUtilization, output_MAB_perRound,
          bidAmount, onlineRate, numGolfers,
          bidStratAdjustmentRatio, hoursToTeeTime, 
          ttHour, ttDay,
          ttRemainingPerc, prophetPrediction,
          weather_tempF, weather_residualTemp, weather_chanceOfRain, weather_precipInches, weather_windSpeed, 
          successfulBid, bidAboveMAB) %>% 
    # mutate_(Y = !! input$desiredYcorrelate) %>%
    correlate() %>%
    focus(output_MAB_perRound) %>%
    rename(feature = rowname) %>%
    arrange(abs(output_MAB_perRound)) %>%
    mutate(feature = as_factor(feature))  %>%
    # return data frame here if needed
    ggplot(aes(x = output_MAB_perRound, y = fct_reorder(feature, desc(output_MAB_perRound)))) +
    geom_point(size = 2) +
    # Positive Correlations 
    geom_segment(aes(xend = 0, yend = feature),
                 color = "dark green",
                 data = . %>% filter(output_MAB_perRound > 0)) +
    geom_point(color = "dark green", size = 2,
               data = . %>% filter(output_MAB_perRound > 0)) +
    # # Negative Correlations 
    geom_segment(aes(xend = 0, yend = feature),
                 color = "red",
                 data = . %>% filter(output_MAB_perRound < 0)) +
    geom_point(color = "red", size = 2,
               data = . %>% filter(output_MAB_perRound < 0)) +
    # Vertical lines
    geom_vline(xintercept = 0, color = "blue", size = 1, linetype = 2) +
    geom_vline(xintercept = -0.25, color = "blue", size = 1, linetype = 2) +
    geom_vline(xintercept = 0.25, color = "blue", size = 1, linetype = 2) +
    # Aesthetics
    theme_bw(base_size = 18) +
    labs(title = "Model Features Correlation Analysis",
         subtitle = "Positive Correlations (increase MAB), Negative Correlations (decrease MAB)",
         y = "Feature Importance")
    })
 
  
  
  ## Show MAB ramp up graphs for each day of the week for one course ------------

  output$MABramp <- renderPlot({
    logSubset_Human() %>% 
      mutate(hoursToTeeTime = hoursToTeeTime*-1) %>%
      filter(courseID == input$courseRampFilter) %>% 
      ggplot(aes(x = hoursToTeeTime, y = output_MAB_perRound/onlineRate, group = ttHour, color = ttHour, alpha = 0.9)) + 
      facet_grid(.~ttDay) +
      geom_point(aes(size = exp(1-ttRemainingPerc)), position = "jitter") +
      geom_line() +
      labs(x = "Hours remaining to Tee Off",
           y = "MAB as % of Online Rate") +
      ylim(0.0, 1.0) +
      guides(colour = guide_legend("Tee Time Hour"),
             alpha = "none",
             size = "none") +
      theme_bw()
  }
  # , height = function() {
    # session$clientData$output$MABramp_width*0.45}
  )

  ## Show Utilization Heatmap and time series stats --------------
  
  output$UtilizationHeatmap <- renderPlot({
    
    logSubset_Human() %>%  
    filter(courseID == input$courseUtilFilter) %>% 
    group_by(courseID, ttHour, ttDay) %>% 
    transmute(Utilization = round(mean(1-ttRemainingPerc),2)) %>%
    ggplot(., aes(x= ttHour, y=ttDay)) + 
    facet_wrap(vars(courseID)) + 
    geom_raster(aes(fill = Utilization)) + 
    scale_fill_gradient2(low = "#edf8e9", high = "#006d2c", na.value = "grey50", limits = c(0,1)) +
    labs(title = "Heatmap of Utilization",
         x = "Hour of Tee Times",
         y = "Day of Tee Times")
  
  })
  
  output$UtilizationTimeSeries <- renderPlot({
    
    logSubset_Human() %>%  
      filter(courseID == input$courseUtilFilter) %>% 
      mutate(ttWeek = as.factor(week(requestedStartTime))) %>%
      group_by(courseID, ttWeek) %>% 
      mutate(Utilization = round(mean(1-ttRemainingPerc),2)) %>%
      ggplot(., aes(x= week(requestedStartTime), y=Utilization, fill = Utilization)) + 
      facet_wrap(vars(courseID)) + 
      stat_summary(fun.y = mean, geom= "bar") + 
      scale_fill_gradient(low = "#edf8e9", high = "#006d2c", na.value = "grey50", limits = c(0,1)) +
      labs(title = "Time Series of Weekly Utilization",
           x = "Week",
           y = "Utilization")
    
  })
  
  ## Floor Price Finder ----------------
  
  output$AlternateFloorRevenue <- renderDataTable({
    
    logSubset_Human() %>% 
      filter(bidAboveMAB == 1) %>% 
      mutate(alternateFloorPrice =onlineRate*(1-(((onlineRate-floorPrice)/onlineRate)-(input$altFloorPrice*0.01)))) %>% 
      select(Session, courseID, customerID, bidAmount, floorPrice, alternateFloorPrice, onlineRate, output_MAB_perRound, bidAboveMAB, relativeMAB, numGolfers) %>% 
      mutate(bidAboveFloor = ifelse(bidAmount >= floorPrice, 1, 0)) %>%
      mutate(bidAboveAltFloor = ifelse(bidAmount >= alternateFloorPrice, 1, 0)) %>%
      group_by(courseID, Session) %>%
      summarise(`Number of Bids` = n(),
                `Revenue: Floor` = ifelse(is.infinite(min(bidAmount[bidAboveFloor == 1])), 0, numGolfers[which.min(bidAmount[bidAboveFloor == 1])]*min(bidAmount[bidAboveFloor == 1])),
                `Revenue: Alternate Floor` = ifelse(is.infinite(min(bidAmount[bidAboveAltFloor == 1])), 0, numGolfers[which.min(bidAmount[bidAboveAltFloor == 1])]*min(bidAmount[bidAboveAltFloor == 1])),
                `One Bid Above Floor` = max(bidAboveFloor),
                `One Bid Above AltFloor` = max(bidAboveAltFloor),
                `Session Floor Price` = mean(floorPrice),
                `Session Alt. Floor Price` = mean(alternateFloorPrice)) %>% 
      ungroup() %>%
      group_by(courseID) %>%
      summarise(`Mean Number of Bids per Session` = mean(`Number of Bids`),
                `Num. Sessions with 1 bid above Floor` = sum(`One Bid Above Floor`),
                `Num. Sessions with 1 bid above Alt Floor` = sum(`One Bid Above AltFloor`),
                `Mean Floor Price` = scales::dollar(mean(`Session Floor Price`)),
                `Mean Alternate Floor Price` = scales::dollar(mean(`Session Alt. Floor Price`)),
                `Revenue with Baseline Floor Price` = scales::dollar(sum(`Revenue: Floor`)),
                `Revenue with Alternate Floor` = scales::dollar(sum(`Revenue: Alternate Floor`))) %>%
      mutate_if(is.numeric, round, 2)
    
  })
  
}

## Run the application ---------
shinyApp(ui = ui, server = server)
