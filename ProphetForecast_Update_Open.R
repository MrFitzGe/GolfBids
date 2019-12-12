# Prophet Forecast Update

library(tidyverse)
library(prophet)
library(lubridate)
source("~/anaconda3/envs/r-tensorflow/GL_NNET/Scripts/HolidayList.R")
targetDate <- ymd_hms("2020-11-01 00:00:00")

# Load Data 

groupDF <- read_csv(".../Data.csv")

# Preprocessing ------
ttDF <- groupDF %>% 
  ungroup() %>% 
  group_by(`Course Name`, ds) %>%
  mutate(utilization = sum(numGolfers)/(Openings)) %>%
  ungroup() %>%
  filter(utilization <= 1) %>%
  filter(utilization >= 0)

readyForProphetModeling <- ttDF %>% 
  select(`Course Name`, ds, utilization)

readyForProphetModeling %>% 
  group_by(`Course Name`) %>% 
  summarise(minDS = min(ds),
            maxDS = max(ds), 
            mean(utilization)) -> courseDateRange_df

# OutlierReplace 
naReplace = 0

readyForProphetModeling %>% 
  mutate(cap = 1) %>% 
  mutate(floor = 0) %>% 
  rename(y = utilization) %>% 
  mutate(y = ifelse(is.na(y), naReplace, y)) -> readyForProphetModeling


# Prep Modeling objects

predlist = NULL


for(courseIdx in seq(1, length(unique(readyForProphetModeling$`Course Name`)))){
  
  print(paste0("Course Number: ", courseIdx))
  
  print("Clearing temp dataframes")
  
  prophetPred  = NULL 
  histUtilData = NULL
  ProphetModel = NULL
  future = NULL
  future2 = NULL
  fcst = NULL
  prophetPred = NULL
  
  
  # isolate one course at a time for modeling
  print("Filtering Specific Course's Data")
  histUtilData <- readyForProphetModeling %>% 
    filter(`Course Name` == courseDateRange_df$`Course Name`[courseIdx])
  
  # Model the historical utiliztion
  print("fitting prophet model...")
  ProphetModel = prophet(histUtilData , holidays = custom_holidays, yearly.seasonality=TRUE)
  
  # predict the future to the target date
  periodHorizonInHours <- ceiling(as.numeric(difftime(targetDate, courseDateRange_df$maxDS[courseIdx], units = "hours")))
  print(paste0("Will now forecast  ", periodHorizonInHours, "  hours between ", courseDateRange_df$maxDS[courseIdx], " and ", targetDate))
  
  # periods:: hours per day*days into the future, freq::  by hour = 3600 
  future <- make_future_dataframe(ProphetModel, periods = periodHorizonInHours, freq = 3600, include_history = F)
  future2 <-  future %>% 
    filter(as.numeric(format(ds, "%H")) >= 5) %>% 
    filter(as.numeric(format(ds, "%H")) < 20)
  
  future2$cap <- 1
  future2$floor <- 0
  
  print("Predicting...")
  fcst <- predict(ProphetModel, future2)
  
  print("Plot model outputs")
  plot(ProphetModel, fcst, uncertainty = F)
  prophet_plot_components(ProphetModel, fcst)
  
  print("saving results in production server-ready format")
  prophetPred <- fcst %>% 
    select(ds, yhat) %>% 
    mutate(ProphetPred = round(yhat, 3)) %>% 
    mutate(TimeHour = format(ds, "%Y-%m-%dT%H:%M:%OS")) %>% 
    mutate(TimeHour = as.character(TimeHour)) %>% 
    mutate(`Course Name` = courseDateRange_df$`Course Name`[courseIdx]) %>% 
    select(`Course Name`, TimeHour, ProphetPred)
  
  predlist = bind_rows(predlist, prophetPred)
  print("data saved to predlist. Onto the next course!")
}

predlist