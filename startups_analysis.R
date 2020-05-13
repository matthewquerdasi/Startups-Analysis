# Matt Querdasi
# Final Project Code 
# Dataset download: https://www.kaggle.com/arindam235/startup-investments-crunchbase
# Geojson download: https://eric.clst.org/tech/usgeojson/  (just downloaded US states 5m)

require(ggplot2)
require(scales) 
require(stringr)
require(reshape2)
require(dplyr)
require(shiny)
require(leaflet)
require(rgdal)
require(geojsonio)
require(readr)
require(data.table)
require(RColorBrewer)
require(countrycode) # package to convert country codes to full names
require(openintro) # package to go from US state abbr to full name

# local import (please adjust based on your file location)
investments_VC <- read.csv("~/Documents/Junior Year/Spring/Data Visualization/project/investments_VC.csv")
# View(investments_VC)

# turning off scientific notation for the axis'
options(scipen=999)

# Data manipulation 
investments_VC %>% 
  mutate(funding_total_usd = as.numeric(gsub(",", "", funding_total_usd)), # removing commas and make numeric
         
         # adding a country_name column for full country names
         country_name = countrycode(as.character(country_code), "iso3c", "country.name"),
         country_name = as.factor(country_name),
         
         # converting all date columns to date format 
         founded_at = as.Date(founded_at),
         first_funding_at = as.Date(first_funding_at),
         last_funding_at = as.Date(last_funding_at),
         
         # replacing empty values of status with NA values 
         status = as.character(status),
         status = ifelse(status=="", NA, status),
         status = as.factor(status),
         
         # adding 'Uncategorized' value to all empty values of market column 
         market = as.character(market),
         market = ifelse(market=="", " Uncategorized ", market), 
         # removing space before and after market
         market = str_sub(market, 2, -2),
         market = as.factor(market),
         
         # creating a quarter variable for just the quarter (no year)
         quarter = str_sub(founded_quarter, -2, -1),
         quarter = ifelse(quarter=="", NA, quarter),
         quarter = as.factor(quarter))-> investments_VC


# Pre shiny variables and manipulation-
# setting list of top 5 most common industries in US (for chloropleth)
top10_industries_USA <- c("Software", "Biotechnology", "Mobile", "Curated Web", "Health Care",
                          "Enterprise Software", "E-Commerce", "Health and Wellness", 
                          "Clean Technology", "Hardware + Software")

# list of top 10 most common industries for all startups (all countries)
top10_industries <- c("Software", "Biotechnology", "Mobile", "E-Commerce", 
                      "Curated Web", "Enterprise Software", "Health Care", "Clean Technology", 
                      "Games")

# making list of top 20 countries with the most startups
investments_VC %>% 
  filter(is.na(country_name) == FALSE) %>% 
  group_by(country_name) %>% 
  count() %>% 
  arrange(desc(n)) -> temp

temp <- data.frame("country"=temp$country_name, "n"=temp$n) # making into a dataframe because top_n won't work on existing

temp %>% 
  arrange(desc(n)) %>% 
  top_n(20) -> temp # selecting only the top 20 values based off n

top20_countries <- temp$country # final top 20 countries value

# Chloropleth
# Import geojson file (just went on web and got US states file)
# geojson_read will read in a ".json" file
# and create a Large Spatial Polygons DataFrame
state_shapes<-geojson_read("~/Documents/Junior Year/Spring/Data Visualization/us_states.json",
                           what="sp")



# BEGIN SHINY
ui<-fluidPage(
  # title
  h1("Startups Dataset Analysis"),
  # general funding round and start year distributions 
  h3("General Distributions of Year Founded and Number of Funding Rounds"),
  selectInput(inputId="marketGeneral",
              label="Choose a market industry to subset",
              choices=unique(top10_industries)),
  radioButtons(inputId="plotByVariable",
               label="Pick a variable to breakdown: ",
               choices=c("Year founded"="founded_year",
                         "Number of funding rounds"="funding_rounds")),
  sliderInput(inputId="yearBasicRange",            
              label="Choose range of years to examine", 
              value=c(1990,2014),
              min=1902,max=2014,   
              step=1),
  plotOutput(outputId="general_distributions"),
  
  # number of startups by market and country 
  h3("Top 10 Most Common Startup Industries by Country"),
  selectInput(inputId="country",
              label="Choose a country",
              choices=ordered(top20_countries)
  ),
  plotOutput(outputId="startups_by_country"),
  
  # leaflet chloropleth
  h3("U.S. State Breakdown by Top 10 Market Industry"),
  selectInput(inputId="market",
              label="Choose a market",
              choices=unique(top10_industries_USA)),
  leafletOutput(outputId="industry_by_state"),
  
  # average funding rounds over time line plot 
  h3("Average Funding and Funding Rounds Breakdown by Industry Over Time"),
  sliderInput(inputId="yearRange",            
              label="Choose range of years to examine", 
              value=c(2000,2014),
              min=1902,max=2014,   
              step=1),
  radioButtons(inputId="yearVariable",
               label="Pick an explanatory variable to plot on the y axis:",
               choices=c("Total average funding"="funding_total_usd",
                         "Average funding rounds"="funding_rounds")),
  checkboxGroupInput("choiceMarket", "Choose market:",
                     choiceNames =
                       list("Software", "Biotechnology", "Health Care", "Mobile",
                            "E-Commerce", "Curated Web", "Enterprise Software",
                            "Clean Technology", "Games"),
                     choiceValues =
                       list("Software", "Biotechnology", "Health Care", "Mobile",
                            "E-Commerce", "Curated Web", "Enterprise Software",
                            "Clean Technology", "Games")
  ),
  textOutput("choiceTxt"),
  plotOutput(outputId="funding_rounds"),
  
  # type of funding by current status
  h3("Funding Breakdown by Startup Operating Status"),
  selectInput(inputId="fundingType",
              label="Choose a type of funding average to breakdown",
              choices=c("Total funding"="avgTotalFunding",
                        "Seed"="avgSeed",
                        "Venture"="avgVenture",
                        "Crowd funding"="avgCrowdFunding",
                        "Angel investment"="avgAngel",
                        "Grant"="avgGrant",
                        "Private equity"="avgPrivateEquity")),
  plotOutput(outputId="fundingStatus")
  
)


server<-function(input, output){
  # general barplot plot for founded yeaer and funding round breakdowns
  output$general_distributions<-renderPlot({
    investments_VC %>% 
      filter(market == input$marketGeneral,
             founded_year >= input$yearBasicRange[1],
             founded_year <= input$yearBasicRange[2]) -> general
    
    ggplot(data=general)+
      geom_bar(aes(x=eval(as.symbol(input$plotByVariable))), fill="cyan3")+
      xlab(eval(as.character(input$plotByVariable)))+
      ylab("Number of startups")
    
  })
  
  # TOP INDUSTRIES BY COUNTRY
  output$startups_by_country<-renderPlot({
    
    # filtering to include only data for selected country, and counting number 
    investments_VC %>%
      filter(country_name %in% input$country,
             market != "Uncategorized") %>% 
      group_by(market) %>% 
      count() %>% 
      #mutate(market = ifelse(market=="", "Other", market)) %>% 
      arrange(desc(n)) -> market_count
    
    # creating fresh dataframe to order market 
    market_count <- data.frame("market"=market_count$market, "n"=market_count$n)
    market_count %>% 
      mutate(market = factor(market, levels = market, ordered = TRUE)) -> market_count
    
    #selecting only the top categories from the table to display
    market_count <- market_count[1:9,] 
    
    # bar graph 
    ggplot(data=market_count)+
      stat_summary(aes(x=market, y=n, fill=market), fun.y="mean", geom="bar")+
      ggtitle("Most Common Startup Industries by Country")+
      ylab("Number of Startups")+
      xlab("Market Category")+
      theme(axis.text=element_text(angle=15))+
      scale_fill_brewer("Market Category", palette="Paired", direction=-1)
  })
  
  # USA MARKET CHLOROPLETH
  output$industry_by_state<-renderLeaflet({
    # subsetting to just US startups, making full state NAMES column, and counting total 
    # startups in that state
    investments_VC %>% 
      filter(country_code=="USA",
             market==input$market) %>% 
      mutate(NAME=abbr2state(state_code)) %>% 
      group_by(NAME) %>% 
      count() -> USA_startups
    
    # Save the data row.names into an explicit variable (ALWAYS NECESSARY)
    state_shapes@data$rn <- row.names(state_shapes)
    
    # Create temporary data tables to work on the attributes (ALWAYS NECESSARY)
    temp.states<-data.table(state_shapes@data)
    
    out.states<-merge(temp.states, USA_startups, by="NAME", all.x=TRUE)
    
    # In the above, all.x=TRUE
    # keeps all information from the first dataset listed
    out.states<-data.table(out.states)
    
    # Then let's re-attach the table to the original SpatialPolygons DataFrame
    #(preserving the original order of the row.names)
    setkey(out.states, rn)
    state_shapes@data <- out.states[row.names(state_shapes)]
    
    # setting palette
    pal <- colorQuantile("RdPu", domain=NULL, n=5)  # 5 is specifying 5 different levels 
    
    # final leaflet with popup with state name and number of startups 
    leaflet(data = state_shapes) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(n),
                  fillOpacity = 0.8,
                  color = "black",
                  weight = 1,
                  popup=~paste(NAME, "<br>", input$market, "startups:", n) ) %>%
      addLegend("bottomleft",
                colors=brewer.pal(5,"RdPu"),
                labels=c("low","","","","high"),
                title="Number of startups") %>% 
      setView(-95, 41, zoom = 3)
      
  })
  
  # FUNDING ROUNDS
  output$funding_rounds<-renderPlot({
    # creating list of chosen groups
    choices <- c(input$choiceMarket)
    
    # filtering to have output without selected year range, and calculating the average off of the selection
    investments_VC %>% 
      filter(market %in% choices,
             founded_year>=input$yearRange[1],
             founded_year<=input$yearRange[2]) %>% 
      group_by(founded_year, market) %>% 
      summarise(avgVariable=mean(eval(as.symbol(input$yearVariable)), na.rm=TRUE)) -> temp
    
    # line plot either displaying total funding or funding rounds over the selected years
    ggplot(data=temp)+
      geom_line(aes(x=founded_year, y=avgVariable, color=market))+
      scale_color_brewer("Markets", palette="Set1")+
      scale_y_continuous(label=comma)+
      xlab("Year")+
      ylab(eval(as.character(input$yearVariable)))
  })
  
  # Funding totals by operating status
  output$fundingStatus <- renderPlot({
    # subsetting to not include uncategorized status, also calculating averages for each investment category
    investments_VC %>%  
      filter(status %in% c("acquired", "closed", "operating")) %>% 
      group_by(status) %>% 
      summarise(avgTotalFunding=mean(funding_total_usd, na.rm=TRUE),
                avgSeed=mean(seed, na.rm=TRUE),
                avgVenture=mean(venture, na.rm=TRUE),
                avgCrowdFunding=mean(equity_crowdfunding, na.rm=TRUE),
                avgAngel=mean(angel, na.rm=TRUE),
                avgGrant=mean(grant, na.rm=TRUE),
                avgPrivateEquity=mean(private_equity, na.rm=TRUE)) -> tempStatus
    
    # bar plot broken down by operating status displaying selected investment type
    ggplot(data=tempStatus)+
      stat_summary(aes(x=status, y=eval(as.symbol(input$fundingType)), fill=status), 
                   fun.y="identity", geom="bar")+
      scale_y_continuous(label=comma)+
      xlab("Operating Status")+
      ylab("Investment/Funding Average")+
      labs(fill = "Operating Status")
  })
  }

shinyApp(ui=ui, server=server)



  


