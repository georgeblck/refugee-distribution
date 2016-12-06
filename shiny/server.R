

shiny.wd <- getwd()
setwd("..")

# Read and match the data (GDP, POP, Unemployment, Asylum applications, etc...)
source("code/extraFunctions.R")
source("code/readandmatchData.R")
input.list <- list(total = total, asyl = asyl.ls, accept = accept)
setwd(shiny.wd)

dat <- data.frame(
  time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)

shinyServer(function(input, output, session) {
  observe({
    # All the shinyjs toggle options
    shinyjs::toggle(id = "show.header", anim = TRUE, condition = !any(grep("Explanation|Data", input$tabs, ignore.case = TRUE)),  animType = "slide")
    shinyjs::toggle(id = "markdown", anim = TRUE, condition = !any(grep("Explanation", input$tabs, ignore.case = TRUE)),  animType = "slide")
    shinyjs::toggle(id = "base.vars", anim = TRUE, condition = any(grep("Data", input$tabs, ignore.case = TRUE)),  animType = "slide")
    shinyjs::toggle(id = "choice.vars", anim = TRUE, condition = any(grep("Results", input$tabs)),  animType = "slide")
    shinyjs::onclick("more.options",
                     shinyjs::toggle(id = "all.options", anim = TRUE))
    shinyjs::onclick("show.weights",
                     shinyjs::toggle(id = "weight.vars", anim = TRUE))
    shinyjs::toggle(id = "only.plot.options", anim = TRUE, condition = any(grep("Plot", input$tabs)),  animType = "slide")
    shinyjs::toggle(id = "only.table.options", anim = TRUE, condition = any(grep("Table", input$tabs)),  animType = "slide")
    shinyjs::toggle(id = "only.idx", anim = TRUE, condition = !any(grep("Data", input$tabs, ignore.case = TRUE)),  animType = "slide")
    
    # The three reactive sliders 
    output$gdp.slider <- renderUI({
      sliderInput("w.gdp", "Weight of GDP (nominal)", min = 0,  max = 1 - input$w.pop, 
                  value = min(0.4, 1 - input$w.pop), step = 0.1)  
    })
    output$asyl.slider <- renderUI( {
      sliderInput("w.asyl", "Weight of Asylum Applications per capita from the preceding 5 years", 
                  min = 0,  max = round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)), 1), 
                  value = min(0.1,round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)), 1)), 
                  step = 0.1)  
    })
    output$unemp.slider <- renderUI( {
      sliderInput("w.unemp", "Weight of unemployment rate", 
                  min = 0,  max = round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)- max(c(0,input$w.asyl), na.rm = TRUE)), 1), 
                  value = min(0.1, round((1 - input$w.pop - max(c(0,input$w.gdp), na.rm = TRUE)- max(c(0,input$w.asyl), na.rm = TRUE)), 1)), 
                  step = 0.1)  
    })
    # Some value switching
    source  <- switch(input$source, "1" = "unhcr", 
                      "2" = "first", "3" = "all")
    idx     <- switch(input$idx, "1" = "eu", "2" = "grech")
    
    # "Subtitle" output above the main plots/tables
    output$header.text <- renderUI({
      ref.numbers <- get.ref.numbers(input.list, year.range = input$range, 
                         which.source = source, countries = input$countries)
      str1 <- paste("From the years<b>", input$range[1], "</b>till<b>", input$range[2], 
                    format(ref.numbers[1], big.mark = ",", scientific = FALSE), 
                    "</b>people applied for asylum in the selected<b>", 
                    ref.numbers[3],"</b>european countries."
                    )
        str2 <- paste("<b>", format(ref.numbers[2], big.mark = ",", scientific = FALSE), "(", round(ref.numbers[2]/ref.numbers[1], 4)*100,
                      "%)</b> of these applications were accepted.")
        str3 <- paste("The below representation shows a theoretical scenario what would happen if <b>all</b> these Asylum Applications would have been accepted and then distributed across Europe by a fair quota.
                      This scenario is compared to the <b>actual</b> distribution of (Accepted) Asylum Applications.<br> For more information have a look at the almost finished Explanation!")
        return(HTML(paste(str1, str2, str3, sep = '<br/>')))#"<br/>",
    })
    
    # ggplot main output
    output$plot <- renderPlot({
      ref.plot <- get.ref.plot(input.list, year.range = input$range, 
                         which.source = source, ratios = !input$show,
                         countries = input$countries, which.idx = idx, input$w.pop, input$w.gdp, 
                         max(c(0,input$w.asyl), na.rm = TRUE),  max(c(0,input$w.unemp), na.rm = TRUE))    
      leg <- g_legend(ref.plot$q1)
      grid.arrange(arrangeGrob(ref.plot$q1 + theme(legend.position="none"),
                              ref.plot$q2 + theme(legend.position="none"),
                             leg, ncol = 3, widths = c(3/7, 3/7, 1/7)))
    }, height = function() {
      session$clientData$output_plot_width*(3/5)
    }, width = function() {
      session$clientData$output_plot_width*(8/7)
    })#width=1000, height=500)
    
    # Table output
    output$table <- renderDataTable({
      get.ref.table(input.list, year.range = input$range, 
       which.source = source, countries = input$countries, per.capita = input$per.capita, 
       which.idx = idx, input$w.pop, input$w.gdp, 
       max(c(0,input$w.asyl), na.rm = TRUE),  max(c(0,input$w.unemp), na.rm = TRUE))
      }, 
      options = list(searching = FALSE, paging = FALSE))
    
    # Table output for index data
    output$index.table <- renderDataTable({
      get.index.data(input.list, year.range = input$range2,
                     which.source = source, countries = input$countries)
      }, options = list(searching = FALSE, paging = FALSE))
    })
})
