
college <- read.csv("college.csv")
#country <- read.csv("country.csv")
intlalls<-read.csv("country.csv", stringsAsFactors = FALSE)
gender<-read.csv("Gender.csv")
ethnicity<-read.csv("by_ethnicity.csv")
state<-read.csv("state_19.csv")
intlall<-read.csv("country.csv", stringsAsFactors = FALSE)


# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(IRdisplay)
library(gganimate)
library(googlesheets4)
library(tidyverse)
library(gifski)
library(plotly)
library(ggmap)
library(maps)
library(mapproj)


title<-
  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
                  navbarPage("GVSU Graduate Student",
                             #theme = "cerulean",  # <--- To use a theme,
                             tabPanel("Home",
                                      sidebarPanel(
                                        tags$img(src = "logo.png")
                                      ),
                                      
                                      mainPanel(
                                        h3("Visualization of data of Graduate Students in Grand Valley State University",align = "center"),
                                        p("This project has the objective to analyze and describe the data of Graduate Students in Grand Valley State University. The data was obtained from the GVSU office of Institutional analysis. This project focuses on the years from 2015-2016 to 2020-2021. Grand Valley State University had 7 colleges in the graduate program until 2018-2019. The first seven colleges were: College of Community and public service, college of education, college of health professions, college of liberal arts and sciences, Kirkhof college of Nursing, Padnos college of Engineering and computing, and Seidman college of Business. In 2018-2019 Grand Valley added a new college, which was Brooks college of Interdisciplinary studies. 
                               We created a Shiny App to show different visualizations about the following data: College and Departments, gender in the different colleges, ethnicity, and countries for the last 6 years."
                                          ,align = "center"),
                                        #imageOutput("college_data")
                                        tags$img(src = "outfilec.gif")
                                      ) # mainPanel
                                      
                             ),
                             tabPanel("College and Departments",
                                      sidebarPanel(
                                        tags$h3("Input:"),
                                        selectInput("year", "Choose Year", choices = unique(college$Year)),
                                        h4("Table of Department vs Student Count of each choosen year: "),
                                        tableOutput("duration_table"),
                                      ), # sidebarPanel
                                      mainPanel(
                                        h4("Distribution of students of different departments for each choosen year between 2015-16 to 2020-21: "),
                                        
                                        plotOutput("barplot_input"),
                                        
                                        p("In 2015-2016 the college that had the most students was College of Health Professions with 405 students. 
                               In 2016-2017, the college that had the most students was College of Health Professions with 396 students. 
                               In 2017-2018, the college that had the most students was College of Health Professions with 371 students. 
                               In 2018-2019, the college that had the most students was College of Education with 405 students. 
                               In 2019-2020, the college that had the most students was College of Education with 364 students. 
                               In 2020-2021, the college that had the most students was College of Education with 399 students."
                                          ,align = "center"),
                                        h4("Horizontal Barchart of Total Student for each Department from 2015-16 to 2020-21: "),
                                        plotOutput("barplot_collegesum")
                                        #h4("Histogram: "),
                                        #plotOutput("hist"),
                                      ) # mainPanel
                                      
                             ), # Navbar 1, tabPanel 1
                             tabPanel("Gender", 
                                      h4("Gender Visualizations of data from 2015-16 to 2020-21"),
                                      sidebarPanel(
                                        h3("Doughnut of Gender Distribution of all students from 2015-16 to 2020-21 "),
                                        # tags$h3("Choose Year:"),
                                        # selectInput("g_year", "Choose Year", choices = unique(gender$Year)),
                                        # selectInput("year", "Choose Year", choices = unique(college$Year)),
                                        plotOutput("gender_pie")
                                        
                                        
                                      ),
                                      
                                      mainPanel(
                                        h3("Stacked Barplot of Gender Distribution of all students for Six years "),
                                        plotOutput("gender_stack"),
                                        p("When analyzing the data for Gender, we created a doughnut for the gender distribution of all students for the six years. We can see that almost three quarters of the students were female. The year that had more females was 2020-2021 while the year that had less females was 2017-2018. In the male side, the year that had the most male students was 2015-2016, while the year that had less male students was 2019-2020."
                                          ,align = "center")
                                        
                                        #imageOutput("gender_data")
                                      )
                             ),
                             tabPanel("Ethnicity",
                                      h4("Ehnicity Visualizations of all years"),
                                      sidebarPanel(
                                        tags$h3("Input:"),
                                        selectInput("year_eth", "Choose Year", choices = unique(ethnicity$Year)),
                                        
                                        h4("Pie Chart of all ethnicities for each input year"),
                                        plotOutput("ethnicity_pie")
                                      ),
                                      
                                      mainPanel(
                                        h4("Line Chart of all ethnicities for each year"),
                                        # plotOutput("ethnicity"),
                                       # imageOutput("ethnicity_data"),
                                       tags$img(src = "outfilet.gif"),
                                        p("When analyzing the data for ethnicity, we conducted a pie chart of all ethnicities for all the years, and a line chart for each year. As we can see in the pie chart, the ethnicity that predominates the most is “White”. Other ethnicities that also have students are: African American, American Indian or Alaskan Native, Asian, Hispanic or Latino, Native Hawaiian or other pacific island, non-resident international, and others. 
In the line chart we can see that white is the one that has the highest number of students, while the other categories have similar numbers of students. 
",align = "center")
                                        
                                        
                                      )
                             )
                             ,
                             tabPanel("Countries",
                                      sidebarPanel(
                                        tags$h3("Input:"),
                                        selectInput("year_country", "Choose Year", choices = unique(intlalls$Year)),
                                        tableOutput("country_table")
                                        
                                      ), # sidebarPanel
                                      mainPanel(
                                         h4("2d Visualizations of students from different countries from 2015-16 to 2020-21"),
                                        
                                        plotOutput("country_2d"),
                                        h4("3d Visualizations of students from different countries from 2015-16 to 2020-21"),
                                        plotOutput("country_3d"),
                                        p("We created a map that shows the number of students for each year and each country in the world. 
In 2020-2021, the country that had more students was India, with a total of 48 students. The total number of international students was 58. 
In 2019-2020, the country that had more students was India, with a total of 23 students. 
The total number of international students was 66. 
In 2018-2019, the country that had more students was India, with a total of 20 students.
The total number of international students was 69.
In 2017-2018, the country that had more students was Nepal, with a total of 18 students. The total number of international students was 87. 
In 2016-2017, the country that had more students was India, with a total of 26 students.
The total number of international students was 84. 
In 2015-2016, the country that had more students was India, with a total of 43 students.
The total number of international students was 90.
",align="center")
                                        
                                      )
                             )
                             
                             #   tabPanel("States", 
                             #           sidebarPanel(
                             #            h4("State Visualizations")
                             #         ),
                             
                             #        mainPanel(
                             #         plotOutput("state_data")
                             
                             
                             #
                             # ))
                  ) # navbarPage
  ) # fluidPage


# Define server function  
server <- function(input, output) {
  
  #Departments-------------------------------------------------------------
  
  output$barplot_input <- renderPlot({
    
    
    df <- college %>% filter(Year==input$year)
    g <- ggplot(df, aes( y = data, x = college,fill=college))
    g + geom_bar(stat = "sum")+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
    
    
  })
  
  
  
  output$college_data <-renderImage({
    outfile <- tempfile(fileext='.gif')
    p= ggplot(college, aes( y = data, x = college, fill=college))+geom_bar(stat='identity') +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+transition_states(Year, transition_length = 2,state_length = 1) +labs(title = 'Year: {closest_state}',subtitle  = "Departments Visualization of Graduate Student from 2015 to 2020")
    
    anim_save("outfilec.gif", animate(p,height=350,width=900,fps=20,duration=20,end_pause=60,res=120))
    list(src = "outfilec.gif",
         contentType = 'image/gif',
         width = 900,
         height = 400
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  
  
  #output$barplot_allyears <-renderPlot({barplot(college$data)})
  
  output$barplot_collegesum <- renderPlot({
    
    
    df <- college %>% group_by(college) %>% summarize(data = sum(data))
    g <- ggplot(df, aes( y = data, x = college,fill=college))
    p<- g + geom_bar(stat = "sum")
    p + coord_flip()
    
  })
  
  #output$hist <-renderPlot({
  #  x    <- college$data
  #  x    <- na.omit(x)
  #  hist(x, breaks = 10, col = "#75AADB", border = "black",
  #      xlab = "Data",
  #     main = "Histogram of college")
  #})
  
  output$duration_table <- renderTable({
    
    df<- college %>% filter(Year==input$year)
    df 
  })
  
  
  #Country-----------------------------------------------------------------
  
  output$country_table <- renderTable({
    
    ab<- intlall %>% filter(Year==input$year_country,Total>=1)
    ab$Total=as.numeric(ab$Total)
    ab<-ab[order(-ab$Total),]
    ab
    
  })
  
  
  output$country_2d <-renderPlot({
    library(ggmap)
    library(maps)
    library(mapproj)
    
    library(ggplot2)
    intlall<-read.csv("country.csv", stringsAsFactors = FALSE)
    intlall<- intlall %>% filter(Year==input$year_country) #%>% group_by(Country) %>% summarize(Total = sum(Total))
    world_map=map_data("world")
    world_map=merge(world_map,intlall,by.x="region",by.y="Country")
    world_map=world_map[order(world_map$group, world_map$order),]
    ggplot(world_map, aes(x=long, y=lat, group=group)) +
      geom_polygon(aes(fill=Total), color="red")+coord_map(xlim=c(-180,180), ylim=c(-60, 90))
    
  })
  
  output$country_3d <-renderPlot({
    
   
    intlall<-read.csv("country.csv", stringsAsFactors = FALSE)
    intlall<- intlall %>% filter(Year==input$year_country) #%>% group_by(Country) %>% summarize(Total = sum(Total))
    world_map=map_data("world")
    world_map=merge(world_map,intlall,by.x="region",by.y="Country")
    world_map=world_map[order(world_map$group, world_map$order),]
    ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(aes(fill=Total), color="black")+coord_map("ortho", orientation = c(20,50,10))
  })
  
  
  
  
  #ethnicity-----------------------------------------------------------------------------
  
  output$ethnicity_pie<-renderPlot({
    ethnicity <- ethnicity %>% filter(Year==input$year_eth)
    library(ggplot2)
    ggplot(ethnicity, aes(x="", y=Total, fill=Ethnicity)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void()
    
    
    
    
  })
  
  
  output$ethnicity <-renderPlot({
    
    g <- ggplot(ethnicity, aes( y = Total, x = Year,color=Ethnicity))
    g + geom_line()+theme_bw()
    
  })
  
  output$ethnicity_data <-renderImage({
    outfile <- tempfile(fileext='.gif')
    p= ggplot(ethnicity, aes( y = Total, x = Year, fill=Ethnicity,color=Ethnicity))+
      geom_line(stat='identity')+ theme_bw() + transition_reveal(Year)
    #transition_states(Year, transition_length = 2,state_length = 1) +
    #labs(title = 'Year: {closest_state}',subtitle  = "Gender Visualization of Graduate Student from 2015 to 2020")
    
    anim_save("outfilet.gif", animate(p,height=400,width=800,fps=20,duration=20,end_pause=60,res=120))
    list(src = "outfilet.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  
  
  #Gender-----------------------------------------------------
  
  # output$gender_pie<-renderPlot({
  
  #  df<- gender %>% filter(Year==input$year)
  # hsize <- 4
  #df <- df %>% 
  # mutate(x = hsize)
  
  #ggplot(df, aes(x = hsize, y = Total, fill = Sex)) +geom_col() + coord_polar(theta = "y") +      xlim(c(0.2, hsize + 0.5))
  
  # })
  
  
  output$gender_pie<-renderPlot({
    library(ggplot2)
    hsize <- 8
    gender <- gender %>% mutate(x = hsize)
    ggplot(gender, aes(x = hsize, y = Total, fill = Sex)) +geom_col() + coord_polar(theta = "y") +      xlim(c(0.2, hsize + 0.5))
    
  })
  
  
  
  
  output$gender_stack <-renderPlot({
    ggplot(gender, aes( y = Total, x = Sex, fill=Sex))+geom_bar(stat='identity')
    ggplot(gender, aes(fill=Sex, y=Total, x=Year)) + 
      geom_bar(position="dodge", stat="identity")
  })
  
  
  #output$gender_data <-renderImage({
  # outfile <- tempfile(fileext='.gif')
  #p= ggplot(gender, aes( y = Total, x = Sex, fill=Sex))+geom_bar(stat='identity')+ theme_bw() +transition_states(Year, transition_length = 2,state_length = 1) +labs(title = 'Year: {closest_state}',subtitle  = "Gender Visualization of Graduate Student from 2015 to 2020")
  
  #anim_save("outfile.gif", animate(p))
  #list(src = "outfile.gif",
  #    contentType = 'image/gif'
  # width = 400,
  # height = 300,
  # alt = "This is alternate text"
  #)}, deleteFile = TRUE)
  
  
  
}

# Create Shiny object
shinyApp(ui, server)