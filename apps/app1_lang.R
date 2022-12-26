library(shiny)
library(tidyverse)
library(forcats)
library(plotly)
library(scales)
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
  wes_palette("Royal1")[4], 
  sample(wes_palette("Darjeeling1"), 5))

wes_palette("Darjeeling1")


#### data load & preparation ####

# dat_full <- read.csv("data/all_issues_meta.csv")
# 
# dat_overview <- dat_full %>%
#   group_by(keyid, year, language, country) %>%
#   count(sort = T) %>%
#   ungroup()

issues <- read.csv("issues_overview.csv")

#### helpers #### 
np_types <- issues %>% filter(document_type == "NEWSPAPER") %>% 
                    select(section) %>% distinct %>% pull()


#### app ####

ui <- fluidPage(
  fluidRow(h1("Issues metadata overview")),
  
  #### sidebar inputs ####
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year of issue", 
                             min = 1800, max = 2020, 
                             value = c(1918,1940),
                             sep = ""), 
      
      selectInput("select_lang", "Language", choices = unique(issues$language), multiple = TRUE,
                selected = unique(issues$language)),
      
      selectInput("select_countries", "Select country or countries", 
                  choices = unique(issues$country),
                  selected = unique(issues$country),
                  multiple = TRUE),
      
      selectInput("select_np_type", "Select newspaper type",
                  choices = np_types,
                  selected = np_types, 
                  multiple = TRUE)
      ),
    
    mainPanel(
              
    #### languages ####  
  tabsetPanel(
    tabPanel("Languages",
  
      fluidRow(h2("Languages of newspapers"),
               p("This page allows to select language(s) and a time period to see total number of issues in the language(s) on a timescale together with the data on countries of issue and newspapers publishing dynamics.")),
      
      fluidRow(
        column(12, 
               helpText("Number of issues on a timescale"), 
               plotlyOutput("plot_languages"))
      ),
      fluidRow(
        column(6,
               helpText("Total number of issues in selected language(s)"),
               tableOutput("lang_total")),
        column(6, 
               helpText("Newspapers with most issues in the period"), 
               tableOutput("title"))
      )
    ),
    
    #### countries ####
    tabPanel("Countries",
             
             fluidRow(h2("Countries of issue"),
                      p("The plot shows number of issues published in Estonia and abroad")
              ),
            
             fluidRow(
               plotlyOutput("plot_countries")
             ),
             fluidRow(
               column(6,
                      helpText("Number of issues per country during the period"),
                      tableOutput("table_countries"))
          )
        ),
    tabPanel("Newspaper types",
             
             fluidRow(h2("Types of newspaper"),
                      p("Regularity of a newspaper publishing")
                      ),
             
             fluidRow(
               plotlyOutput("plot_types")
             ),
             fluidRow(
               column(6,
                      tableOutput("table_types")
                      ),
               column(6,
                      p("Countries of issue of <b>exile</b> newspapers"),
                      tableOutput("exile_countries")
                      )
               
             )
             
             )
      )
    )
  )
)  

##################
##### server #####
server <- function(input, output, session) {

  selected_r <- reactive({
    issues %>% 
      filter(year > input$year[1], year < input$year[2]) %>% 
      filter(language %in% c(input$select_lang)) %>% 
      filter(country %in% c(input$select_countries) & country != "") %>% 
      filter(section %in% input$select_np_type)
  })
  
  
  #### languages ####
  
  output$lang_total <- renderTable(
    selected_r() %>% 
      group_by(language) %>% 
      summarise(Newspapers = n(),
                Issues = sum(n)) %>% 
      arrange(desc(Issues)),
    width = "100%"
  )
  
  output$title <- renderTable(
    selected_r() %>% 
      group_by(keyid) %>% 
      summarise(years_published = n(),
                total_issues = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(total_issues)) %>% 
      group_by(keyid = ifelse(row_number() < 10, keyid, "Other")) %>% 
      summarise(across(everything(), sum)) %>% 
      mutate(years_published = ifelse(keyid == "Other", "NA", years_published)) %>% 
      arrange(keyid == "Other", -total_issues) %>% 
      rename(`Shortened title` = keyid,
             `Years published` = years_published,
             `Issues total` = total_issues),
    width = "100%"
  )
  
  output$plot_languages <- renderPlotly({
    
    if (nrow(selected_r()) < 1 ) {
      
      p <- selected_r() %>% 
        ggplot(aes(x = as.factor(year), y = n)) + 
        labs(x = "No values available for this selection",
             y = "") + 
        theme(axis.text.x = element_text(color = "red"))
      
    } else { 
    
    p <- selected_r() %>% 
      group_by(year) %>% 
      ggplot(aes(x = year, 
                 y = n, 
                 text = paste0("Title: ", keyid), 
                 fill = language)) + 
      geom_col() + 
      labs(y = "Number of issues", 
           x = "Year",
           fill = "Language") + 
      scale_fill_manual(values = pallette) + 
      scale_y_continuous(labels = label_number(accuracy = 1))
    
    ggplotly(p)
      
    }
    })
  
  #### countries ####
  
  output$plot_countries <- renderPlotly({
    
    if (nrow(selected_r()) < 1 ) {
      p <- selected_r() %>% 
        ggplot(aes(x = year, y = n)) + 
        labs(x = "No values available for this selection",
             y = "") + 
        theme(axis.text.x = element_text(color = "red"))

    } else {

      p <- selected_r() %>% 
        group_by(year) %>% 
        ggplot(aes(x = year, 
                   y = n, 
                   text = paste0("Title: ", keyid), 
                   fill = country)) + 
        geom_col() + 
        labs(y = "Number of issues", 
             x = "Year",
             fill = "Country") + 
        scale_fill_manual(values = pallette)
      
      ggplotly(p)
     }
  })
  
  output$table_countries <- renderTable(
    selected_r() %>% 
      count(country, sort = T) %>% 
      rename(Country = country,
             Issues = n)
  )
  
  #### types ####
  
  output$plot_types <- renderPlotly({
    
    if (nrow(selected_r()) < 1 ) {
      p <- selected_r() %>% 
        ggplot(aes(x = as.factor(year), y = n)) + 
        labs(x = "No values available for this selection",
             y = "") + 
        theme(axis.text.x = element_text(color = "red"))
    } else { 
      
      p <- selected_r() %>% 
        filter(document_type == "NEWSPAPER") %>% 
        group_by(year) %>% 
        ggplot(aes(x = year, y = n, 
                   fill = section, text = paste0("Title: ", keyid))) + 
        geom_col() + 
        scale_fill_manual(values = pallette) + 
        labs(x = "Year", y = "Number of issues", fill = "Type")
      
      ggplotly(p)
    }
  })
  
  output$table_types <- renderTable(
    selected_r() %>% 
      count(section, sort = T) %>% 
      rename(`Type of newspaper` = section,
             `Number of issues` = n)
  )
  
  output$exile_countries <- renderTable(
    selected_r() %>% 
      filter(section == "Exile newspaper") %>% 
      count(country, sort = T) %>% 
      rename(Country = country,
             `Number of issues` = n)
  )
  
}


shinyApp(ui, server)

# search by title -- separate plot
# fix html in exile table comment