# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)

#head(recommendation)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Covid Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.github.com")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Confirmed Cases Across Various States"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("ConfirmedbyStates", height = "300px")
  )
  
  ,box(
    title = "Active Cases Across Various Staets"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("ActivebyStates", height = "300px")
  ) 
  
  ,box(
    title = "Total Deaths Across Various States"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("DeathbyStates", height = "300px")
  ) 
  
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  total.revenue <- sum(recommendation$Confirmed)
  total.death <- sum(recommendation$Death)
  sales.account <- recommendation %>% group_by(State) %>% summarise(value = sum(Confirmed)) %>% filter(value==max(value))
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Most Confirmed :',sales.account$Account)
      
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })

  
  
  
  #creating the plotOutput content
  
  output$ConfirmedbyStates <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=State, y=Confirmed,fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("No of Confirmed Cases") + 
      xlab("States") + theme(legend.position="top"
                              ,plot.title = element_text(size=20, face="bold")) + ggtitle("States") + labs(fill = "Region")
  })
  
  
  output$ActivebyStates <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=State, y=Active, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("No of Active Cases") + 
      xlab("States") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("States") + labs(fill = "Region")
  })
  
  
  
  output$DeathbyStates <- renderPlot({
    ggplot(data=recommendation,
           aes(x=State, y=Death, fill=factor(Region))) +
      geom_bar(position = "dodge", stat = "identity") + ylab("No of Deceased Cases") + 
      xlab("States") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("States") + labs(fill = "Region")
  })
  
  
  
}


shinyApp(ui, server)