# Load the required packages
library(shiny)
library(reshape)
library(ggplot2)
library(Cairo)
library(gridExtra)
library(scales)
library(shinyjs)
library(markdown)
# For AWS
#sudo su - -c "R -e \"install.packages(c('reshape','ggplot2', 'gridExtra', 'scales', 'markdown','Cairo'), repos='http://cran.rstudio.com/')\""



shinyUI(pageWithSidebar(

  # Title of it
  headerPanel(h2("What if Refugess were distributed fair across Europe?"), windowTitle = "Distributing Refugees in Europe"),
  
  sidebarPanel(width = 3,
    useShinyjs(),
    #tags$head(
     # tags$style(type="text/css", "select { max-width: 300px; }"),
    #  tags$style(type="text/css", ".span4 { max-width: 300px; }"),
    #  tags$style(type="text/css", ".well { max-width: 300px; }")#,
      #tags$style(type = "text/css", ".shiny-input-container:not(.shiny-input-container-inline) {margin-bottom: -5px;}")
      #tags$style(type = "text/css", ".checkbox { margin-bottom: -10px; }")
    #),
    shinyjs::hidden(
      div(id = "choice.vars",
        sliderInput("range", label = strong("Distribute Asylum Applications from the years"), min = 2010, 
                    max = 2016, value = c(2014, 2015), step = 1, sep = "", ticks = FALSE),
        div(id = "only.plot.options", style = "padding: 0px;, margin: 0px;",
            #checkboxGroupInput("show", label = h4("Show ... in plot"),
            #                   choices = list("absolute values instead of ratios" = 1,
            #                                  "data on Asylum Applications" = 2))
            #),
            checkboxInput("show", span("Show ", strong("absolute"), "values instead of ratios."), value = FALSE)
        ),
        div(id = "only.table.options",
            checkboxInput("per.capita", label = span("Asylum Applicatons", strong("per 1000 inhabitants")), value = FALSE)
        ),
        # Countries
        checkboxGroupInput("countries", label = h4("Include country groups"), 
                           choices = list("Iceland, Norway, Liechtenstein, Switzerland (Schengen area)" = 1, 
                                          "Denmark, Ireland, UK (EU states with opt-outs regarding Dublin III Regulation)" = 2)
        ),
        a(id = "show.weights", h4("Change weights of key"), href = "#"),
        shinyjs::hidden(
          div(id = "weight.vars",
              sliderInput("w.pop", "Weight of population size", min = 0, max = 1, value = 0.4, step = 0.1),
              uiOutput("gdp.slider"), 
              uiOutput("asyl.slider"),
              uiOutput("unemp.slider")
          )
        )
      )
    ),
    shinyjs::hidden(
      # Basically the same as the first year slider but adapted to the 3rd tab
      div(id = "base.vars",
          sliderInput("range2", label = h4("Choose the base year of the key"), min = 2009, 
                      max = 2015, value = 2015, step = 1, sep ="", ticks = FALSE)
      )
    ),
    shinyjs::hidden(
        div(id = "markdown",
          # Options hyperlink for toggeling
          a(id = "more.options", h4("Show/hide advanced options"), href = "#"),
          shinyjs::hidden(
            div(id = "all.options",
              selectInput("source", label = h4("Source of Refugee Data"), 
                          choices = list("UNHCR Data" = 1, "Eurostat (only new Asylum Applications)" = 2, "Eurostat (including repeated Asylum Aplications)" = 3), 
                          selected = 1),
              shinyjs::hidden(
                div(id="only.idx", 
              selectInput("idx", label = h4("Which Distribution Key to use?"), 
                          choices = list("EU-Key" = 1, "Grech-Key (corrected EU-Key)" = 2), 
                          selected = 2))
              )
            )
          )
        )
      )
  ),
  
  mainPanel(#h4("Output"),
    shinyjs::hidden(
      div(id = "show.header",
      htmlOutput("header.text"))),
            tabsetPanel(id = "tabs",
              tabPanel("Results via Bar-Plot", plotOutput("plot", height="auto")),
              tabPanel("Results via Table", dataTableOutput("table")),
              tabPanel("Base data for Key", dataTableOutput("index.table")),
              tabPanel("Explanation", includeMarkdown("markdown.md"))
            )
    )
))

