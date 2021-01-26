library(shiny)
library(tidyverse)
library(jsonlite)
library(plotly)
library(ggplot2)
library(leaflet)

source("heroMap.R")

basicdata = dplyr::select(fromJSON("https://akabab.github.io/superhero-api/api/all.json",flatten = TRUE),id,name)

ui <- navbarPage("Super Hero", id="superheroPage",
                 tabPanel("Introduction",
                          titlePanel("Hero introduction"),
                          # selection in sidebar
                          sidebarPanel(
                              # select the interested hero
                              selectInput("hero", label = "Choose a Character",
                                          choices = basicdata$name),
                              plotlyOutput("singleSpiderPlot")
                             
                          ),
                          # Create a main panel to show the histogram
                          wellPanel(verbatimTextOutput("content"),
                                    uiOutput("heroimage",align="center"),
                                    tableOutput("herotable")
                                    )),
                 tabPanel(
                     "Comparision",
                     # Add a titlePanel to your tab
                     titlePanel("Comparing Superhero Power Statistics"),
                     # Create a sidebarPanel for your controls
                     sidebarPanel(
                         # Make a textInput widget for searching for a state in the scatter
                         selectInput("hero1", label = "Choose a Character",
                                     choices = basicdata$name),
                         selectInput("hero2", label = "Choose Second Character",
                                     choices = basicdata$name, selected = basicdata$name[2])
                     ),
                     # Create a main panel to show the histogram
                     mainPanel(
                         #plotOutput("hist"),
                         plotlyOutput("SpiderPlot")
                     )
                 ),
                 tabPanel(
                     "Hero Map",
                     # Add a titlePanel to your tab
                     titlePanel("Birthplaces of Superheros in the United States"),
                     # Create a sidebarPanel for your controls,
                     sidebarPanel(
                         selectInput("publisher", label = "Choose a publisher:",
                                     choices = list("Marvel Comics","DC Comics", "Thunderbird II", "She-Thing")
                         )
                     ),
                     # Create a main panel to show the histogram
                     mainPanel(
                         leafletOutput("map")
                     )
                 )
                
                 
)

server <- function(input, output, session) {
    #2nd page
    id = reactive({basicdata$id[basicdata$name == input$hero][1]})
    url = reactive({str_glue("https://akabab.github.io/superhero-api/api/id/{i}.json",i=id())})
    info = reactive({fromJSON(url())})
    image = reactive({info()$images$md})
    output$content = renderPrint(c(info()$appearance$gender,info()$appearance$race,info()$appearance$height,info()$appearance$weight))
    output$heroimage = renderUI({
        fp = image()
        tags$img(src=fp)})
    output$singleSpiderPlot = renderPlotly ({
        SpiderPlot = plot_ly(
            type = 'scatterpolar',
            fill = 'toself',
            mode = 'markers'
        ) 
        SpiderPlot <- SpiderPlot %>%
            add_trace(
                r = c(info()$powerstats$intelligence, 
                      info()$powerstats$strength, 
                      info()$powerstats$speed, 
                      info()$powerstats$durability, 
                      info()$powerstats$power, 
                      info()$powerstats$combat),
                theta = c("intelligence","strength","speed","durability","power","combat"),
                name = input$hero
            )
        SpiderPlot <- SpiderPlot%>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,100)
                    )
                )
            )
    })
    output$herotable <-renderTable({
        df = tibble(name=info()$biography$fullName,place_of_birth=info()$biography$placeOfBirth,occupation=info()$work$occupation,relatives=info()$connections$relatives)
        })
    
    # 3rd page
    id1 = reactive({basicdata$id[basicdata$name == input$hero1][1]})
    id2 = reactive({basicdata$id[basicdata$name == input$hero2][1]})
    url1 = reactive({str_glue("https://akabab.github.io/superhero-api/api/id/{i}.json",i=id1())})
    url2 = reactive({str_glue("https://akabab.github.io/superhero-api/api/id/{i}.json",i=id2())})
    info1 = reactive({fromJSON(url1())$powerstats})
    info2 = reactive({fromJSON(url2())$powerstats})
    # barplot
    #df = reactive({tibble(name = c(input$hero1,input$hero2),
    #                     intelligence = c(info1()$intelligence,info2()$intelligence), 
    #                      strength = c(info1()$strength, info2()$strength), 
    #                      speed = c(info1()$speed, info2()$speed),
    #                      durability = c(info1()$durability, info2()$durability),
    #                      power = c(info1()$power, info2()$power),
    #                      combat = c(info1()$combat,info2()$combat))})
    #data = reactive({gather(df(), type, value, -name)})
    #output$hist <- renderPlot({
        #ggplot(data(), aes(type, value)) +
        #    geom_bar(aes(fill = name), stat = "identity", position = "dodge")
        
    #})
    #Spiderchart
    output$SpiderPlot = renderPlotly ({
        SpiderPlot = plot_ly(
            type = 'scatterpolar',
            fill = 'toself',
            mode = 'markers'
        ) 
        SpiderPlot <- SpiderPlot %>%
            add_trace(
                r = c(info1()$intelligence, 
                      info1()$strength, 
                      info1()$speed, 
                      info1()$durability, 
                      info1()$power, 
                      info1()$combat),
                theta = c("intelligence","strength","speed","durability","power","combat"),
                name = input$hero1
            )
        SpiderPlot <- SpiderPlot %>%
            add_trace(
                r = c(info2()$intelligence, 
                      info2()$strength, 
                      info2()$speed, 
                      info2()$durability, 
                      info2()$power, 
                      info2()$combat),
                theta = c("intelligence","strength","speed","durability","power","combat"),
                name = input$hero2
            )
        SpiderPlot <- SpiderPlot%>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,100)
                    )
                )
            )
    })
   
    # Fourth page output, changes publisher
    output$map <- renderLeaflet({
        return(make_super_map(input$publisher))
    })
    
   
}

shinyApp(ui, server)
