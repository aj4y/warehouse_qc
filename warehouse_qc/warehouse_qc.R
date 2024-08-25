library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("sup_n_values", label = "Supplier - Sample size:",
                      choices = seq(500, 1000, by = 100), selected = 100,width = '600px'),
          selectInput("sup_c_values", label = "Supplier - Acceptance criterion:",
                      choices = seq(0,70,by= 10), selected = 50,width = '600px'),
          selectInput("wh_n_values", label = "Warehouse - Sample size:",
                      choices = seq(500, 1000, by = 100), selected = 100),
          selectInput("wh_c_values", label = "Warehouse - Acceptance criterion:",
                      choices = seq(0,70,by= 10), selected = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("supplier_risk"),
          plotOutput("warehouse_risk")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  p_values <- seq(0.01,0.2,by=0.01)
  sup_results <- reactive({
    expand_grid(n = input$sup_n_values, c = input$sup_c_values, p = p_values) %>%
      mutate(n=as.numeric(n),c = as.numeric(c))%>%
      mutate(prob_accept = pbinom(c,n,p)) %>% 
      mutate(supplier_risk = round(1-prob_accept,2),
             reciever_risk = round(prob_accept,2))
  })
  
  
  output$supplier_risk <- renderPlot({
    sup_results() %>% 
      ggplot(aes(x = p, y = supplier_risk, color = factor(n))) +
      geom_line(size = 1.1,alpha = 0.5) +
      geom_point()+
      geom_text(aes(label = supplier_risk),vjust = -1)+
      scale_y_continuous(labels = scales::percent)+
      scale_x_continuous(labels = scales::percent)+
      theme_minimal()+
      theme(legend.position = "None",
            plot.title = element_text(size = 25),
            axis.text = element_text(size = 15))+
      labs(title = "Supplier risk - Type1 error, for different defect rates p",
           x = "Defect rate", y = "Supplier risk")
  })
  
  wh_results <- reactive({
    expand_grid(n = input$wh_n_values, c = input$wh_c_values, p = p_values) %>%
      mutate(n=as.numeric(n),c = as.numeric(c))%>%
      mutate(prob_accept = pbinom(c,n,p)) %>% 
      mutate(supplier_risk = round(1-prob_accept,2),
             reciever_risk = round(prob_accept,2))
  })
  
  
  output$warehouse_risk <- renderPlot({
    wh_results() %>%
      ggplot(aes(x = p, y = reciever_risk,color= factor(n))) +
      geom_line(size = 1.1,alpha = 0.5) +
      geom_point()+
      geom_text(aes(label = reciever_risk),vjust = -1)+
      scale_y_continuous(labels = scales::percent)+
      scale_x_continuous(labels = scales::percent)+
      theme_minimal()+
      theme(legend.position = "None",
            plot.title = element_text(size = 25),
            axis.text = element_text(size = 15))+
      labs(title = "Warehouse risk - Type2 error, for different defect rates p",
           x = "Defect rate", y = "Warehouse risk")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
