# Project 1, Nicholas Abbasi

#Needed Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

#read in .csv file
litdata2 <- read.table(file = "litdata.csv", sep = ",", header = TRUE)

#=====================================================================
#Adjusting location information

# convert lat and lon from strings to numbers
litdata2$lat <- as.numeric(as.character(litdata2$lat))
litdata2$lon <- as.numeric(as.character(litdata2$lon))

# remove coordinates outside of forrest park
litdata <- subset(litdata2, (lat > 41.849783 & lat < 41.888720) & (lon < -87.803481 & lon > -87.833827) )

# insert untagged for untagged pick ups
litdata$tags <- sub("^$", "untagged", litdata$tags)

#=====================================================================
#Adjusting date/time info

# convert dates to lubridate chicago time format
litdata$litterTimestamp <-ymd_hms(litdata$litterTimestamp, tz = "GMT")
litdata$litterTimestamp <-with_tz(litdata$litterTimestamp, tz = "America/Chicago")
#litdata$litterTimestamp <- newdates

# adding colomns to data frame bar chart inquiries
litdata$dayoftheweek <- wday(litdata$litterTimestamp, label = TRUE) #day of the week

litdata$date <- date(litdata$litterTimestamp) #date (alone)
litdata$hour <- hour(litdata$litterTimestamp) #hour (alone)

#=====================================================================
#Generating table data:

#Total trash picked up for text segment
totalTrash <- NROW(litdata)

#generate table of most frequent days trash was picked up!
litdata.table_datefreq <- as.data.frame(table(litdata$date))
litdata.table_datefreq <- litdata.table_datefreq[order(litdata.table_datefreq$Freq,  decreasing=TRUE), ]
#var1 = Date, Freq = Trash Collected

#generate table of top users who picked up most trash!
litdata.table_topusers <- as.data.frame(table(litdata$username))
litdata.table_topusers <- litdata.table_topusers[order(litdata.table_topusers$Freq,  decreasing=TRUE), ]
#var1 = Usernames, Freq = Trash Collected

#generate table of days of the week most trash was picked up!
litdata.table_topdays <- as.data.frame(table(litdata$dayoftheweek))
litdata.table_topdays <- litdata.table_topdays[order(litdata.table_topdays$Freq,  decreasing=TRUE), ]
#var1 = Day of the Week, Freq = Trash Collected

#generate table of hours of the day most trash was picked up!
litdata.table_tophours <- as.data.frame(table(litdata$hour))
litdata.table_tophours <- litdata.table_tophours[order(litdata.table_tophours$Freq,  decreasing=TRUE), ]
#var1 = Hour of Day (in centeral time), Freq = Trash Collected

#generate table for most frequent tags!
#Requires more steps due to parsing tags into a more managable form
unique_tag_list <- as.list(unique(litdata$tags)) #extract all unique tags, pre-parse
unique_tag_string <- gsub(" ", ",", paste(unique_tag_list, collapse=' ')) #long string parsable now by commas
unique_tag_vec <- strsplit(unique_tag_string, ",") #now all in 1 vector of indivisual strings, 6907 long

#generate table for most frequent tags! continued
tagstable.table <- as.data.frame(table(unique_tag_vec))
tagstable.table <- tagstable.table[order(tagstable.table$unique_tag_vec, decreasing=TRUE), ]
#unique_tag_vec = Tags, Freq = Tag Count
 
#===================================================================== 

#Days of the Week bar chart
ggplot(litdata.table_topdays, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
  labs(x="Day of the Week", y = "Trash Collected")

#24hr Bar Chart
ggplot(litdata.table_tophours, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
  labs(x="Hour of Day", y = "Trash Collected")

#Trash collected per day
ggplot(litdata.table_datefreq, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
  labs(x="Date", y = "Trash Collected")

#Top Tags Bar Chart
ggplot(tagstable.table, aes(x = reorder(unique_tag_vec, -Freq), y = Freq)) + geom_bar(stat = "identity", fill="steelblue") +
  labs(x="Tags", y = "Tag Count") + scale_x_discrete(limit = c("plastic", "paper", "wrapper", "cup", "bottle","bag", "cigarette", "candy","can", "aluminum"))

#add more info to markers!
leaflet(litdata) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

#===================================================================== 

ui <- dashboardPage(
  dashboardHeader(title = "Cs424 Project 1"),
  dashboardSidebar(
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   
                   selectInput("Username", "Select A Username", litdata.table_topusers),
                   selectInput("Tags", "Select a Tag", tagstable.table)
                   )
                   ),
                   
      dashboardBody(
    fluidRow(
      column(7,
             
             fluidRow(
               box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                   leafletOutput("leaf", height = 700)
               )
             )
      ),
      column(2,
             
             fluidRow(
               box(title = "Top Tags Bar Chart", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("tagBar", height = 200)
               )
             ),
             fluidRow(
               box(title = "Trash Collected Per Day Bar Chart", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("daysBar", height = 200)
               )
             ),
             fluidRow(
               box(title = "Trash Collected Per Day Of The Week Bar Chart", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("dayOfWeekBar", height = 200)
               )
             )
             ),
      column(2,
             
             fluidRow(
               box(title = "Trash Collected Per Spesific Hour Bar Chart", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hourBar", height = 200)
               )
             ),
             fluidRow(
               box(title = "www.litterati.org Trash Collected Data", solidHeader = TRUE, status = "primary", width = 12,
                   paste("Total Trash Collected:", totalTrash)
               )
             ),
             fluidRow(
               box(title = "Top Users Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("userTab", height = 200)
               )
             )#,
             #fluidRow(
              #  box(title = "All Information Table", solidHeader = TRUE, status = "primary", width = 12,
                #    dataTableOutput("allInfoTab", height = 100)
              #)
             #)
             
      )
    )
      ))
  
  #====================


server <- function(input, output) { 
  
  theme_set(theme_grey(base_size = 18)) 

  output$leaf <- renderLeaflet(
    {
      leaflet(litdata) %>% addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions(),
        lng=litdata$lon,
        lat=litdata$lat,
        popup=paste("Username: ", litdata$username, "<br>",
                    "ID: ", litdata$user_id, "<br>",
                    "Date/Time: ", litdata$litterTimestamp, "<br>",
                    "Litter Type: ", litdata$tags, "<br>",
                    "[Lat, Lon]: [", litdata$lat, ", ", litdata$lon, "]")
      )

    }
      )
  
  output$allInfoTab <- DT::renderDataTable(
    DT::datatable(
      {
        litdata
      },
      options = list(searching=FALSE, pageLength=10, lengthChange=FALSE, rownames=FALSE)
    )
  )
  
  output$userTab <- DT::renderDataTable(
    DT::datatable(
      {
        litdata.table_topusers
      },
      options = list(searching=FALSE, pageLength=10, lengthChange=FALSE,  rownames=FALSE)
    )
  )
  
  output$daysBar <- renderPlot({
    ggplot(litdata.table_datefreq, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
      labs(x="Date", y = "Trash Collected") 
  })
  output$dayOfWeekBar <- renderPlot({
    ggplot(litdata.table_topdays, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
      labs(x="Day of the Week", y = "Trash Collected") 
  })
  output$hourBar <- renderPlot({
    ggplot(litdata.table_tophours, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
      labs(x="Hour of Day", y = "Trash Collected") 
  })
  output$tagBar <- renderPlot({
    ggplot(tagstable.table, aes(x = reorder(unique_tag_vec, -Freq), y = Freq)) + geom_bar(stat = "identity", fill="steelblue") +
      labs(x="Tags", y = "Tag Count") + scale_x_discrete(limit = c("plastic", "paper", "wrapper", "cup", "bottle","bag", "cigarette", "candy","can", "aluminum"))
    
  })
  
}

shinyApp(ui, server)