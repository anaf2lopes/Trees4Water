
library(shiny)
library(ggplot2)

ui <- fluidPage(
  # App title ----
      titlePanel(h1("Cost-Benefit Analysis of Reforestation Scenarios (Trees4Water)")),
  # Sidebar layout with input and output definitions ----
  tabsetPanel(
    tabPanel("Cost data (Inputs for CBA)", 
        #sidebarLayout(position = "right",
          # Sidebar panel for inputs ---
          #sidebarPanel(h2("Inputs for CBA"),
                       #h3("Costs"),
                          numericInput(inputId = "costs_planting",label = "Cost of Planting Trees (EUR/hectare)",                  value = 103000,min = 0 , max=1000000),
                          numericInput(inputId = "costs_annual_m",label = "Annual Maintenance and Operation Costs (EUR/hectare)" , value = 1600  ,min = 0 , max=5000),
                          numericInput(inputId = "costs_annual_o",label = "Annual Opportunity Costs (EUR/hectare)" ,               value = 50,min = 0 , max=100),
                          numericInput(inputId = "hectares",      label = "Number of hectares for scenario (hectare)" ,            value = 157.06,min = 0 , max=1000) 
          ),
        tabPanel("Benefits data (Inputs for CBA)", 
                       
                       h3("Bio-physical inputs from SWAT"),
                          numericInput(inputId = "sediment_ton",label = "Reductions in Sediments (tons/liter)",   value = 50,min = 0 , max=100),
                          numericInput(inputId = "nitrogen_ton",label = "Reductions in Nutrient (tons/liter)",    value = 50,min = 0 , max=100),
                          numericInput(inputId = "phosphorus_ton",label = "Reductions in Phosphorus (tons/liter)",value = 50,min = 0 , max=100),
          
                       h3("Socio-economic value of improvements"),
                          numericInput(inputId = "sediment_value",label = "Value of Sediment abatement (EUR/ton)",    value = 4369*12,min = 0 , max=10000),
                          numericInput(inputId = "nitrogen_value",label = "Value of Nirogen abatement (EUR/ton)",     value = 0,min = 0 , max=100),
                          numericInput(inputId = "phosphorus_value",label = "Value of Phosphorus abatement (EUR/ton)",value = 0,min = 0 , max=100) 
        ),
    tabPanel("CBA Details", 
                       #h3("CBA Details"),
                          numericInput(inputId = "discount_rate",label = "Social Discount Rate (%)",value = 0.05,min = 0 , max=1),
                          numericInput(inputId = "years",label = "Number of years for CBA",         value = 10,min = 0 , max=100)
    ),

        tabPanel("Visualize Results", 
                 h3(tableOutput("summary")),
                  plotOutput("graph")
        )
      )
    )

server <- function(input, output) {
  

  output$summary <- renderTable({ 
    
    CF0 <- (-input$costs_planting*input$hectares)/((1+input$discount_rate)^(0))
    for(i in 1:input$years){
        assign( paste("CF", i, sep = "") ,  (-input$costs_annual_m*input$hectares-
                                               input$costs_annual_o*input$hectares+
                                               input$sediment_ton*input$sediment_value+
                                               input$nitrogen_ton*input$nitrogen_value+
                                               input$phosphorus_ton*input$phosphorus_value)/((1+input$discount_rate)^(i)) ) }  

    list <- NA
    for (i in 0:input$years) {
      list[i+1] <- paste0("CF",i)
    }
    
    data <- NA
    for(i in 0:input$years){
    data[i+1] <- get(list[i+1])
    }
      paste("The Net Present Value (NPV) of the project is" , print(round(sum(data),0)), "EUR. Below you can see how the Discounted Cash Flows look over the course of the project." )
        
    })
  
      
  output$graph <- renderPlot({ 
    
    CF0 <- (-input$costs_planting*input$hectares)/((1+input$discount_rate)^(0))
    for(i in 1:input$years){
      assign( paste("CF", i, sep = "") ,  (-input$costs_annual_m*input$hectares-
                                             input$costs_annual_o*input$hectares+
                                             input$sediment_ton*input$sediment_value+
                                             input$nitrogen_ton*input$nitrogen_value+
                                             input$phosphorus_ton*input$phosphorus_value)/((1+input$discount_rate)^(i)) ) }  
    
    list <- NA
    for (i in 0:input$years) {
      list[i+1] <- paste0("CF",i)
    }
    
    data <- NA
    for(i in 0:input$years){
      data[i+1] <- get(list[i+1])
    }
    
  CF_data <- data.frame(NA)
  CF_data[1:(input$years+1),1] <- 0:input$years
  CF_data[1:(input$years+1),2] <- data
  CF_data[1:(input$years+1),3] <- cumsum(data)
  
   ggplot(data=CF_data) +
     geom_bar(aes(x=CF_data[,1], y=CF_data[,2]), stat="identity", fill="steelblue") +
     geom_line(aes(x=CF_data[,1], y=CF_data[,3]),stat="identity", color='#000066', size=2) +
     scale_y_continuous(name = "Present Value of Cash Flows")
  
  
  })
  
}
    
shinyApp(ui = ui, server = server)    

