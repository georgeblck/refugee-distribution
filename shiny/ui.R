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
  headerPanel(h2("Eine gerechtere Verteilung von Asylsuchenden in Europa"), 
              windowTitle = "Die Verteilung von Flüchtlingen in Europa"),
  
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
          sliderInput("year.range", label = strong("Verteile alle Asylanträge der Jahre"), 
                    min = 2010, max = 2016, value = c(2014, 2015), step = 1, 
                    sep = "", ticks = FALSE),
          # Nested Hidden Block 1.1
          # Only show when on plot output
          div(id = "only.plot.options", style = "padding: 0px;, margin: 0px;",
              selectInput("which.style.plot", "Welche Werte sollen angezeigt werden:",
                          choices = c("Anteil an Flüchtlingen" = "ratio", "Anzahl an Flüchtlingen" = "abs",
                                      "Flüchtlinge pro 1000 Einwohner"= "per"))
          ),
          # Nested Hidden Block 1.1
          # Only show when on table output
          div(id = "only.table.options",
              # Show per capita AA numbers in table
              checkboxInput("per.capita", 
                            label = span("Asylanträge", strong("pro 1000 Einwohner")), 
                            value = FALSE)
          ),
        # Add/Remove certain country groups
        checkboxGroupInput("countries", label ="Ländergruppen hinzufügen", 
                           choices = list("Island, Norwegen, Liechtenstein, Schweiz (Schengen-Raum)" = 1, 
                                          "Dänemark, Irland, UK (EU-Staaten mit Ausweichklauseln bzgl. der Dublin III Regulierung)" = 2)
        ),
        # ON-CLICK: Show/Hide the weight sliders
        a(id = "show.slider.weights", h4("Schieberegler verstecken"), href = "#"),
          div(id = "slider.weights",
              # The four sliders to change the weights of the quota
              # Only the first slider (Population) is defined here
              sliderInput("w.pop", "Gewicht der Einwohnerzahl", 
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
          sliderInput("base.year.range", label = h4("Wähle das Basis-Jahr des Schlüssels"), min = 2009, 
                      max = 2015, value = 2015, step = 1, sep ="", ticks = FALSE)
      )
    ),
    
    # Hidden Block 3
    # Only show when NOT on the "Explanation" Tab
    shinyjs::hidden(
        div(id = "not.explanation.options",
          # ON-CLICK: Show/Hide advanced options
          a(id = "show.more.options", h4("Erweiterte Optionen anzeigen"), href = "#"),
          # Nested Hidden Block 3.1
          # Only Show when above link is clicked
          shinyjs::hidden(
            div(id = "advanced.options",
                # Switch the source of (only!) the refugee data
                selectInput("source", label = h4("Quelle der Flüchtlingsdaten"), 
                            choices = c("UNHCR" = "unhcr", "Eurostat (nur neue Asylanträge)" = "first", 
                                           "Eurostat (inkl. wiederholte Asylanträge)" = "all")), 
                           # selected = ""),
                # 2xNested Hidden Block 3.1.1
                # Only NOT Show when on "Base-Var" tab
                shinyjs::hidden(
                  div(id="not.base.options", 
                  selectInput("idx", label = h4("Welcher Schlüssel soll benutzt werden?"), 
                              choices = c("EU-Schlüssel" = "eu", "Grech (verbesserter EU-Schlüssel)" = "grech"), 
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
                tabPanel("Ergebnisse als Grafik", plotOutput("plot", height="auto")),
                tabPanel("Ergebnisse als Tabelle", dataTableOutput("table")),
                tabPanel("Datenbasis des Schlüssels", dataTableOutput("index.table")),
                tabPanel("Erklärung", includeMarkdown("markdown.md"))
                )
    )
  )
)

