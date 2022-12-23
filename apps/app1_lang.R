library(shiny)
library(tidyverse)
library(forcats)

dat <- read.csv("data/all_issues_meta.csv") %>% 
  slice_sample(n = 1000)

title_id <- setNames(dat$keyid, dat$title)

ui <- fluidPage(
  
  
  fluidRow(
    column(6,
    selectInput("lang", "Language", choices = dat$language, multiple = TRUE)),
    column(6, 
           sliderInput("year", "Year of issue", min = 1850, max = 2020, value = c(1900,2000),
                       sep = ""))
  ),
  fluidRow(
    column(12, plotOutput("plot"))
  ),
  fluidRow(
    column(6, 
           tableOutput("country")),
    column(6, 
           tableOutput("title"))
    )
)

server <- function(input, output, session) {
  
  selected_lang <- reactive(dat %>% filter(language %in% c(input$lang)))
  
  output$country <- renderTable(
    selected_lang() %>% count(country, sort = T),
    width = "100%"
  )
  
  output$title <- renderTable(
    selected_lang() %>% 
      mutate(title = fct_lump(fct_infreq(title), n = 5)) %>% 
      count(title, sort = F),
    width = "100%"
  )
  
  output$plot <- renderPlot({
    
    selected_lang() %>% 
      filter(year > input$year[1], year < input$year[2]) %>% 
      group_by(language, year) %>% 
      count() %>% 
      ggplot(aes(x = year, y = n, fill = language)) + geom_col()
  }, res = 96)
}

shinyApp(ui, server)