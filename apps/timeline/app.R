library(shiny)
library(vistime)
library(plotly)

d_timeline <- read.csv("data/timeline_data.csv")

# set names to keyids to display titles in selectInput
title_id <- setNames(d_timeline$keyid, d_timeline$title)

# special row for empty selection
d_timeline[nrow(d_timeline)+1,] <- list("no_value", 
                                  "Please choose the newspaper name <b>and</b> at at least one language",
                                               "1900-01-01", "2000-01-01", "Estonian", "Unknown", 0, 
                                               "Please choose the newspaper name", "#FDD262")



ui <- fluidPage(
  fluidRow(h2("Lifespan of a newspaper", align = "center")),
  fluidRow(
           column(1),
           column(6, 
                  helpText(p("This app display on a timescale how long a newspaper was issued."),  
                           p("The newspaper can be selected from the list or searched by typing the beginning of the title (click at the empty space inside the box and select or start typing)."), 
                           p("By default 5 the longest running newspapers are displayed.")),
                  selectizeInput(
                    "newspaper", "Select the title", multiple = TRUE,
                    choices = NULL
                    ))
           ,
           column(4,
                  helpText("Selected newspapers can be filtered by languages of issue."),
                  checkboxGroupInput("languages", "Select language(s)", 
                                     choices = unique(d_timeline$language),
                                     selected = unique(d_timeline$language))
                  )
           ),
  fluidRow(column(1), 
           column(10,
                  helpText("Additional information (country and language of issue as well as exact dates of issuing) are showed when pointing cursore at the beginning of each horisontal bar."))),
  fluidRow(plotly::plotlyOutput("timeline")
           )
)

server <- function(input, output, session) {

  updateSelectizeInput(session, "newspaper", choices = title_id, server = TRUE, 
                       selected = c("estpostimjututuba", "sakalaew", "tallorahvak", 
                                               "ekmteataja", "JVeestirootsiselts"))
  
  df <- reactive({
    
    if (length(input$newspaper) < 1 | length(input$languages) < 1) {
      
      d_timeline %>% 
        filter(keyid == "no_value")
      
    } else {
      
      d_timeline %>% 
        filter(keyid %in% c(input$newspaper)) %>% 
        filter(language %in% c(input$languages))
      
    }
  })
  
  output$timeline <- renderPlotly({
    vistime(
      df(), 
      col.event = "title", 
      col.group = "keyid", 
      col.color = "l_color",
      col.tooltip = "tooltip"
      )
  })
}

shinyApp(ui, server)

