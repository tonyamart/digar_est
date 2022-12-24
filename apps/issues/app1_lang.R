library(shiny)
library(tidyverse)
library(forcats)
library(plotly)
library(wesanderson)

# graphics
theme_set(theme_minimal())
pallette <- c(
  wes_palette("Royal2")[5],
  wes_palette("Rushmore1")[4],
  wes_palette("Royal1")[1], 
  wes_palette("Royal2")[3],
  wes_palette("Rushmore1")[3],
  wes_palette("BottleRocket2")[1], 
  wes_palette("Royal2")[1],
  wes_palette("BottleRocket2")[4:5], 
  wes_palette("Royal1")[4])

#### data load & preparation ####

# dat_full <- read.csv("data/all_issues_meta.csv")
# 
# dat_overview <- dat_full %>%
#   group_by(keyid, year, language, country) %>%
#   count(sort = T) %>%
#   ungroup()

issues <- read.csv("data/issues_overview.csv")

#### app ####

ui <- fluidPage(
  
  fluidRow(h1("Issues metadata overview")),
  
  tabsetPanel(
    tabPanel("Languages",
  
      fluidRow(h2("Languages of newspapers"),
               p("This page allows to select language(s) and a time period to see total number of issues in the language(s) on a timescale together with the data on countries of issue and newspapers publishing dynamics.")),
      
      fluidRow(
        column(6,
        selectInput("lang", "Language", choices = issues$language, multiple = TRUE, 
                    selected = issues$language)),
        column(6, 
               sliderInput("year", "Year of issue", min = 1850, max = 2020, value = c(1918,1928),
                           sep = ""))
      ),
      fluidRow(
        column(12, 
               helpText("Number of issues on a timescale"), 
               plotlyOutput("plot"))
      ),
      fluidRow(
        column(4,
               helpText("Total number of issues in selected language(s)"),
               tableOutput("lang_total")),
        column(4, 
               helpText("Countries of publication"), 
               tableOutput("country")),
        column(4, 
               helpText("Newspapers with most issues in the period"), 
               tableOutput("title"))
        )
      
    ),
    tabPanel("Countries")
  )
)

server <- function(input, output, session) {
  
  selected_lang <- reactive(issues %>% 
                              filter(language %in% c(input$lang)) %>% 
                              filter(year > input$year[1], year < input$year[2])
                              ) 
  
  lang <- reactive(c(input$lang))
  
  
  output$lang_total <- renderTable(
    selected_lang() %>% 
      group_by(language) %>% 
      summarise(Newspapers = n(),
                Issues = sum(n)) %>% 
      arrange(desc(Issues)),
    width = "100%"
  )
  
  output$country <- renderTable(
    selected_lang() %>% 
      filter(country != "") %>% 
      group_by(country) %>% 
      summarise(Newspapers = n(),
                Issues = sum(n)) %>% 
      arrange(desc(Newspapers)) %>% 
      rename(Country = country),
    width = "100%"
  )
  
  output$title <- renderTable(
    selected_lang() %>% 
      group_by(keyid) %>% 
      summarise(years_published = n(),
                total_issues = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(total_issues)) %>% 
      group_by(keyid = ifelse(row_number() < 10, keyid, "Other")) %>% 
      summarise(across(everything(), sum)) %>% 
      mutate(years_published = ifelse(keyid == "Other", "NA", years_published)) %>% 
      arrange(keyid == "Other", -total_issues) %>% 
      rename(`Shorten title` = keyid,
             `Years published` = years_published,
             `Issues total` = total_issues),
    width = "100%"
  )
  
  output$plot <- renderPlotly({
    
    if (length(lang()) < 1 ) {
      p <- selected_lang() %>% 
        ggplot(aes(x = year, y = n))
    } else { 
    
    p <- selected_lang() %>% 
      group_by(year) %>% 
      ggplot(aes(x = year, 
                 y = n, 
                 text = paste0("Issie title: ", keyid), 
                 fill = language)) + 
      geom_col() + 
      labs(y = "Number of issues", 
           x = "Year",
           fill = "Language") + 
      scale_fill_manual(values = pallette)
    
    ggplotly(p)
      
    }
    })
  }


shinyApp(ui, server)