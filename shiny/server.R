# Read and match the data (GDP, POP, Unemployment, Asylum applications, etc...)
# Source the functions for plotting/tables etc.
shiny.wd <- getwd()
setwd("..")
source("code/extraFunctions.R")
source("code/readandmatchData.R")
input.list <- list(total = total, asyl = asyl.ls, accept = accept)
setwd(shiny.wd)


shinyServer(function(input, output, session) {
  observe({
    #################################
    ## All the shinyjs show/hide options
    ################################
    # Hidden Block 1
    # Show only when on one of the two Results tabs
    shinyjs::toggle(id = "only.results.options", anim = TRUE, 
                    condition = any(grep("Ergebnisse", input$tabs)),  
                    animType = "slide")
    # Hidden Block 1.1 & 1.2
    # Show only when on plot or table
    shinyjs::toggle(id = "only.plot.options", anim = TRUE, 
                    condition = any(grep("Grafik", input$tabs)),  
                    animType = "slide")
    shinyjs::toggle(id = "only.table.options", anim = TRUE, 
                    condition = any(grep("Tabelle", input$tabs)),  
                    animType = "slide")
    # ON-CLICK: Show/Hide Hidden Block 1.3
    shinyjs::onclick("show.slider.weights",
                     shinyjs::toggle(id = "slider.weights", anim = TRUE))
    # Hidden Block 2
    # Show only when on "Base-Data" tab
    shinyjs::toggle(id = "only.base.options", anim = TRUE, 
                    condition = any(grep("Datenbasis", input$tabs, ignore.case = TRUE)),  
                    animType = "slide")
    # Hidden Block 3
    # Show only when NOT on the "Explanation" tab
    shinyjs::toggle(id = "not.explanation.options", anim = TRUE, 
                    condition = !any(grep("Erklärung", input$tabs, ignore.case = TRUE)),  
                    animType = "slide")
    # ON-CLICK: Show/Hide Hidden Block 3.1
    shinyjs::onclick("show.more.options",
                     shinyjs::toggle(id = "advanced.options", anim = TRUE))
    # Hidden Block 3.1.1
    # Only NOT Show when on "Base-Var" tab
    shinyjs::toggle(id = "not.base.options", anim = TRUE, 
                    condition = !any(grep("Datenbasis", input$tabs, ignore.case = TRUE)),  
                    animType = "slide")
    # Hidden Block 4 (Main-Panel)
    # Only Show when NOT on Explanation/Base-Var tab
    shinyjs::toggle(id = "show.header", anim = TRUE, 
                    condition = !any(grep("Erklärung", input$tabs, ignore.case = TRUE)),  
                    animType = "slide")
  
    ############################
    ## The three reactive sliders
    ############################
    # Thanks to this thread:
    # http://stackoverflow.com/questions/18700589/interactive-reactive-change-of-min-max-values-of-sliderinput
    output$gdp.slider <- renderUI({
      sliderInput("w.gdp", "Gewicht des BIP", min = 0,  max = 1 - input$w.pop, 
                  value = min(0.4, 1 - input$w.pop), step = 0.1)  
    })
    output$asyl.slider <- renderUI( {
      sliderInput("w.asyl", "Gewicht der bisherigen Asylbelastung", 
                  min = 0,  max = round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)), 1), 
                  value = min(0.1,round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)), 1)), 
                  step = 0.1)  
    })
    output$unemp.slider <- renderUI( {
      sliderInput("w.unemp", "Gewicht der Arbeitslosigkeitsrate", 
                  min = 0,  max = round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)- 
                                           max(c(0,input$w.asyl), na.rm = TRUE)), 1), 
                  value = min(0.1, round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)- 
                                            max(c(0,input$w.asyl), na.rm = TRUE)), 1)), 
                  step = 0.1)
    })

    # "Subtitle" output above the main plots/tables
    output$header.text <- renderUI({
      if (any(grep("datenbasis", input$tabs, ignore.case = TRUE))){
        res.string <- paste("Der Verteilungsschlüssel wird mit den Daten aus dem Jahr vor der gewählten Zeitperiode berrechnet.<br>
Z.B. wenn die Asylanträge aus den Jahren 2015-2016 verteilt werden sollen, wird der Schlüssel mit den Länder-Statistiken aus dem Jahr 2014 berechnet.")
      } else {      
          ref.numbers <- get.ref.numbers(input.list, year.range = input$year.range, 
                           which.source = input$source, countries = input$countries)
        str1 <- paste("In den Jahren<b>", input$year.range[1], "</b>bis<b>", input$year.range[2], "</b>stellten<b>",
                      format(ref.numbers[1], big.mark = ",", scientific = FALSE), 
                      "</b> Menschen Asylanträge in den ausgewählten<b>", 
                      ref.numbers[3],"</b>europäischen Ländern."
                      )
          str2 <- paste("<b>", format(ref.numbers[2], big.mark = ",", scientific = FALSE), "(", 
                        round(ref.numbers[2]/ref.numbers[1], 4)*100,
                        "%)</b> dieser Anträge wurden akzeptiert.")
          if (any(grep("Grafik", input$tabs))){
            ref.var <- c("<i>blau</i>", "<i>orange</i>", "<i>gelb</i>")
            str3 <- paste0("Die folgende Grafik zeigt ein <b>theoretisches</b> Szenario 
in dem europaweit <b>alle</b> Asylanträge akzeptiert und dann mithilfe eines fairen Schlüssels (", ref.var[1],") in Europa verteilt werden würden.
Dieses Szenario wird mit der <b>tatsächlichen</b> Verteilung der gestellten Asylanträgen (", ref.var[3],"), bzw. angenommenen Asylanträgen (", ref.var[2],") verglichen.<br>
Für mehr Informationen schau dir die Erklärung an.")
          } else {
            str3 <- paste0("Die folgende Grafik zeigt ein <b>theoretisches</b> Szenario 
in dem europaweit <b>alle</b> Asylanträge akzeptiert und dann mithilfe eines fairen Schlüssels in Europa verteilt werden würden.
Dieses Szenario wird mit der <b>tatsächlichen</b> Verteilung der gestellten Asylanträgen, bzw. angenommenen Asylanträgen verglichen.<br>
Für mehr Informationen schau dir die Erklärung an.")
          }
          res.string <- paste(str1, str2, str3, sep = '<br/>')
      }
        return(HTML(res.string))
    })
    
    # ggplot main output
    output$plot <- renderPlot({
      ref.plot <- get.ref.plot(input.list, year.range = input$year.range,
                         which.source = input$source,  which.show = input$which.style.plot,
                         countries = input$countries, which.idx = input$idx, w.pop = input$w.pop, w.gdp = input$w.gdp, 
                         w.asyl=max(c(0,input$w.asyl), na.rm = TRUE), w.unemp= max(c(0,input$w.unemp), na.rm = TRUE))    
      grid_arrange_shared_legend(ref.plot$q1, ref.plot$q2, nrow = 1, ncol = 2)
    }, height = function() {
      session$clientData$output_plot_width*(3/5)
    }, width = function() {
      session$clientData$output_plot_width*(8/7)
    }, res = 100)
    
    # Table output
    output$table <- renderDataTable({
      get.ref.table(input.list, year.range = input$year.range, 
       which.source = input$source, countries = input$countries, per.capita = input$per.capita, 
       which.idx = input$idx, input$w.pop, input$w.gdp, 
       max(c(0,input$w.asyl), na.rm = TRUE),  max(c(0,input$w.unemp), na.rm = TRUE))
      }, 
      options = list(searching = FALSE, paging = FALSE))
    
    # Table output for index data
    output$index.table <- renderDataTable({
      get.index.data(input.list, year.range = input$base.year.range,
                     which.source = input$source, countries = input$countries)
      }, options = list(searching = FALSE, paging = FALSE))
    })
})
