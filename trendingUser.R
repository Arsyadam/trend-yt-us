## app.R ##
options(scipen = 123)
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(glue)
library(plotly)

youtube.trending <- read.csv("youtubetrends.csv")
str(youtube.trending)

yt <- youtube.trending %>% 
  mutate(category_id = as.factor(category_id),
         publish_wday = as.factor(publish_wday),
         trending_date = ymd(trending_date),
         publish_time = ymd_hms(publish_time)
         )

ui <- 
  dashboardPage(
  dashboardHeader(title = "Youtube Trending"),
  dashboardSidebar(
    sidebarMenu(
    menuItem(text = "Home",tabName = "home",icon = icon("home")) ), collapsed = TRUE),
  dashboardBody(
    
    tabItems(
     tabItem(tabName = "home",
      a(href="https://github.com/Arsyadam/trend-yt-us",icon('file-code-o')," View File ", style="background-color:#5B90BF; color:white; padding-left:30px; padding-right:30px; padding-top:10px;padding-bottom:10px;border-radius:5px;"),
       div(style = "margin-bottom:3rem;",h1("Youtube Trending US Nov 2017 -Jan 2018")),
       fluidRow(
         valueBox(value = yt %>%
                    count(category_id) %>%
                    arrange(-n) %>% 
                    filter(row_number()==1) %>% 
                    select(category_id) %>% 
                    ungroup(), 
                  icon = icon("fire"),
                  subtitle = "Kategori video yang paling sering trending ",
                  color = "blue",
                  width = 6),
         
         valueBox(value = topChannel <- yt %>%
                    count(channel_title) %>%
                    arrange(-n) %>% 
                    filter(row_number()==1) %>% 
                    select(channel_title) %>% 
                    ungroup(), 
                  
                  href = glue("https://youtube.com/{topChannel}"),
                  icon = icon("tv"),
                  subtitle = "Channel youtube dengan video trending paling sering",
                  color = "red",
                  width = 6),
         
         
       ),
       fluidRow(
        div(style = "margin-bottom:3rem; margin-left:3rem;margin-right:3rem;",h3("Category dengan Rata-Rata Views tertinggi"),
          box(width = 12, 
              plotlyOutput("plot1"))
        )
       )
     )
    )
  )
  
)

#berapa kali category tersebut menampilkan video 
#2. filter category
#
server <- function(input, output) {
  output$plot1 <- renderPlotly ({
    meanViewsPlot <- 
      yt %>% 
      filter(views>mean(views)) %>% 
      group_by(category_id) %>% 
      summarise(meanViews=mean(views)) %>% 
      ungroup()
    
    plot_bar <- meanViewsPlot %>% 
      
      ggplot(mapping = aes(x=meanViews,
                           y=reorder(category_id,meanViews),
                           text=glue("Averege Views: {scales::comma(meanViews, 1)}"))) +
      geom_col() +
      scale_x_continuous(labels = scales::comma) +
      labs(title = , 
           x = NULL, 
           y = NULL, 
           fill = NULL) 
      
    ggplotly(plot_bar,tooltip = "text")
    
  })
}



shinyApp(ui, server)




















