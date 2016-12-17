# Load the required packages
library(shiny)
library(reshape)
library(ggplot2)
library(Cairo)
options(shiny.usecairo=T)
library(gridExtra)
library(scales)
library(shinyjs)
library(markdown)
require(grid)

# For AWS
#sudo su - -c "R -e \"install.packages(c('reshape','ggplot2', 'gridExtra', 
#'scales', 'markdown','Cairo'), repos='http://cran.rstudio.com/')\""

#runApp("shiny", host="0.0.0.0", port=7543, launch.browser = FALSE)


shinyUI(pageWithSidebar(

  # Title of the App
  headerPanel(h2("What if all Refugees were distributed justly across Europe?"), 
              windowTitle = "Distributing Refugees in Europe"),
  
  ######################
  ## Side-Bar
  ######################
  sidebarPanel(width = 3,
    useShinyjs(),
    
    # Hidden Block 1 (in Side-Bar) 
    # Only show when on one of the two Results tabs
    shinyjs::hidden(
      div(id = "only.results.options",
          # Choose the time range 
          sliderInput("year.range", label = strong("Distribute Asylum Applications from the years"), 
                    min = 2010, max = 2016, value = c(2014, 2015), step = 1, 
                    sep = "", ticks = FALSE),
          # Nested Hidden Block 1.1
          # Only show when on plot output
          div(id = "only.plot.options", style = "padding: 0px;, margin: 0px;",
              selectInput("which.style.plot", "Show which variable in Plot:",
                          choices = c("Share of Refugees" = "ratio", "Number of Refugees" = "abs",
                                      "Refugees per 1000 inhabitants"= "per"))
          ),
          # Nested Hidden Block 1.1
          # Only show when on table output
          div(id = "only.table.options",
              # Show per capita AA numbers in table
              checkboxInput("per.capita", 
                            label = span("Asylum Applicatons", strong("per 1000 inhabitants")), 
                            value = FALSE)
          ),
        # Add/Remove certain country groups
        checkboxGroupInput("countries", label ="Include country groups", 
                           choices = list("Iceland, Norway, Liechtenstein, Switzerland (Schengen area)" = 1, 
                                          "Denmark, Ireland, UK (EU states with opt-outs 
                                          regarding Dublin III Regulation)" = 2)
        ),
        # ON-CLICK: Show/Hide the weight sliders
        a(id = "show.slider.weights", h4("Hide sliders"), href = "#"),
          div(id = "slider.weights",
              # The four sliders to change the weights of the quota
              # Only the first slider (Population) is defined here
              sliderInput("w.pop", "Weight of population size", 
                          min = 0, max = 1, value = 0.4, step = 0.1),
              # The reactive sliders are definded in server.R
              uiOutput("gdp.slider"), 
              uiOutput("asyl.slider"),
              uiOutput("unemp.slider")
          )
      )
    ),
    
    # Hidden Block 2
    # Only show when on the "Base-Data" tab
    shinyjs::hidden(
      # Chooose the base year of the key
      div(id = "only.base.options",
          sliderInput("base.year.range", label = h4("Choose the base year of the quota"), min = 2009, 
                      max = 2015, value = 2015, step = 1, sep ="", ticks = FALSE)
      )
    ),
    
    # Hidden Block 3
    # Only show when NOT on the "Explanation" Tab
    shinyjs::hidden(
        div(id = "not.explanation.options",
          # ON-CLICK: Show/Hide advanced options
          a(id = "show.more.options", h4("Show/hide advanced options"), href = "#"),
          # Nested Hidden Block 3.1
          # Only Show when above link is clicked
          shinyjs::hidden(
            div(id = "advanced.options",
                # Switch the source of (only!) the refugee data
                selectInput("source", label = h4("Source of Refugee Data"), 
                            choices = c("UNHCR Data" = "unhcr", "Eurostat (only new Asylum Applications)" = "first", 
                                           "Eurostat (including repeated Asylum Aplications)" = "all")), 
                           # selected = ""),
                # 2xNested Hidden Block 3.1.1
                # Only NOT Show when on "Base-Var" tab
                shinyjs::hidden(
                  div(id="not.base.options", 
                  selectInput("idx", label = h4("Which Distribution Key to use?"), 
                              choices = c("EU-Key" = "eu", "Grech-Key (corrected EU-Key)" = "grech"), 
                              selected = "grech"))
                  )
                )
            )
          )
        )
    ),
  
  #####################
  ## Main Panel
  #####################
  mainPanel(
    # Hidden Block 4
    # Only Show when NOT on Explanation tab
    shinyjs::hidden(
      div(id = "show.header",
      htmlOutput("header.text"))
      ),
    tabsetPanel(id = "tabs",
                tabPanel("Results via Bar-Plot", plotOutput("plot", height="auto")),
                tabPanel("Results via Table", dataTableOutput("table")),
                tabPanel("Base data for Quota", dataTableOutput("index.table")),
                tabPanel("Explanation", includeMarkdown("markdown.md"))
                )
    )
  )
)

