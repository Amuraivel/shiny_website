#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# REMEBER TO ADD IN DOCKERFILE!!!
library(DBI)
library(dplyr)
library(shiny)
library(RSQLite)

# Other packages
library(DT)
library(markdown)
library(plotly)
library(shinyjs)



# 
# wd_path <- "/Users/mark/Documents/Consultancy/WW_Website/shiny_website/app"
# setwd(wd_path)
#Initialization routine
con <- dbConnect(RSQLite::SQLite(), "./data/equities.db")

if (1==0){
# Create an RSQLite database witþ all ðe stock in ðe mergent list.
#Create an all world table on existing database
PHARMA_LISTINGS <- readxl::read_xlsx("~/Favourites/Investing/data/ALL_WORLD.xlsx", 
                                     sheet="LISTINGS",
                                     col_types = "text") 
# Write to the database table
dbWriteTable(con, "listings", PHARMA_LISTINGS, overwrite=TRUE, temporary=FALSE)
dbListTables(con)
dbDisconnect(con)
} else {
  # Get ðe data out of ðe database
  comps <- dbReadTable(con, "listings") %>%
    filter(icb_subsector %in% c("Pharmaceuticals","Biotechnology")) %>%
    arrange(name) %>% 
    select(name,isin, icb_subsector, country) 
    
  # Create a vector of names
  comp_labs   <- setNames(c("(Select)",comps$isin), c("",comps$name))
  
  # Country Labels
  ISO3        <- unique(dbReadTable(con, "listings") %>% select(country) %>% arrange(country) %>% filter(country != "") )
  county_labs <- setNames(c("Any",ISO3$country), c("Any",ISO3$country))
}


# Define UI for application that draws a histogram
ui <- navbarPage(theme = "styles.css",
  # Application title
  img(src = "logoonly.png", width = 60, height = 48),
   
  tabPanel("About",
           tags$body("Arêté is an inferential statistics consultancy. That means we look for the causes of various phenomena using a wide variety of statististical techniques using both the Frequentist and Bayesian epistemological paradigms."),
           tags$h1("Services"),
           tags$ol(
             tags$h2("Statistical Consultancy"),
             tags$body("We answer statistical questions, build models, and implement statistical solutions for decision makers."),
             tags$body("We typically deal with questions related to clinical trials and financial modeling, where causality must be established, but naturally we are happy to answer a broad range of questions."),
             
             tags$h2("Managed Options"),
             tags$body("For friends and family, we manage short-option accounts portfolios using our proprietary software and strategies."),
             tags$h2("℞-Fund"),
             tags$body("The Arêté ℞-Fund is long-short equity fund, which combines information from patents and clinical trials to generate alpha.")
           ),
           
           tags$h1("Contact"),
           tags$b("eMail:"), tags$a(href="mailto:mark@arêté.com", "mark@arêté.com"),
           tags$br(),
           tags$b("LinkedIn:"), tags$a(href="http://www.linkedin.com/in/mark-james-thompson","www.linkedin.com/in/mark-james-thompson"),
           tags$br(),
           tags$b("Phone:"), tags$body("+41 (0) 79 901 03 51"),
           tags$br(),
           tags$b("Address:"),
           tags$br(),
           tags$body("Arêté Statistics AG"),
           tags$br(),
           tags$body("Vorstadtstrasse 24"),
           tags$br(),
           tags$body("5722 Gränichen, Argovia"),
           tags$br(),
           tags$body("Switzerland")
           
  ),
  
  
    
    
  navbarMenu("Consultancy",
             tabPanel("Simple Questions : Fiverr",
                      includeHTML("contact.html")
             ), 
             tabPanel("Large Projects : Statistical Consulting", 
                      tags$html("Or, if you need help with a larger or different type of statistical/data project, please do not hesitate to get in touch.")
                      
                      
             ),
             tabPanel("Academia & Research : Coathorship", 
                      tags$body("I am interested in getting published in medical and biostats journals. If you are a medical researcher or academic, who needs a statistically-savvy unpaid help with research design, pharmacodynamic modeling, causal inteference, data analysis, etc., please do not hesitate to get in touch.")
             )
             
  ),
    
  tabPanel("Managed Options",
      tags$body("This is the login domain for option account holders."),
      passwordInput("password", "Password:"),
      actionButton("go", "Go"),
      verbatimTextOutput("option_login_value")
  ),
  
  tabPanel("℞-Fund",
           useShinyjs(), 
           # Application title
           #titlePanel(),
           
           # Sidebar with a slider input for number of bins 
           fluidPage(
             # Initial panel
             mainPanel(id="selection_panel",
                       tags$h3("Pharma & Biotech Equities"),
                       tags$body("The following is a (comprehensive?) list of listed companies active in the pharmaceutical, and biotech space."),
                       tags$body("If there is an error or omission, please don't hesitate to ask. Select a row from the table to show company pipeline and details."),
                       # Create a new row for the table.
                       textOutput("country"),
                       DT::dataTableOutput("company_table"),
                       plotOutput("distPlot")
             ),
             # Selection panel once 
             mainPanel(id="selected_company_panel",
                       actionButton('back_to_equities_button', 'Back to equities'), 
                       tags$div(id = 'placeholder')
             )
           ) # Sidebar
           
  ),
  

  # 
  # navbarMenu("Models 4 Fun",
  #            tabPanel("Car Pricer"),
  #            tabPanel("Engine Model"),
  #            tabPanel("Weight Loss")
  # ), 
  tags$footer("Written in RStudio using Shiny")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    equities_table <- dbReadTable(con, "listings") %>% 
      filter(!is.na(yahoo_symbol)) %>%
      filter(icb_subsector %in% c("Pharmaceuticals","Biotechnology")) %>%
      select(country, name, yahoo_symbol, isin, currency, icb_subsector) %>%
      arrange(name)
  
  

    
    #### This is for the Pharma Fund's page
    
    ## Observes the table selection
    ## keep track of elements inserted and not yet removed
    inserted <- c()
    
    observeEvent(input$company_table_rows_selected,{
      equity_row <<- equities_table[input$company_table_rows_selected,]
      names(equity_row) <- names(equities_table)
      shinyjs::toggle(id = "selection_panel")
      print(paste0(equity_row))
      show('#placeholder')
      btn <- input$back_to_equities_button
      id <- paste0('txt', btn)
      
      
      tmp <- dbReadTable(con, "yahoo_prices") %>% 
        filter(yahoo_symbol == equity_row["yahoo_symbol"]) %>% 
        select(date, yahoo_symbol, close) 
      
      q <- paste0("SELECT date, yahoo_symbol, close FROM yahoo_prices WHERE yahoo_symbol = ", "\'",equity_row["yahoo_symbol"],"\'")
      
      sq <- dbSendQuery(con,q)
      tmp <- dbFetch(sq) 
      tmp <- tmp %>% 
      transform(date = as.Date(as.numeric(date),origin="1970-01-01"))
      # tail(tmp)
      
      p <- plot_ly(tmp, x = ~date, y = ~close, color = ~yahoo_symbol,
              mode = 'lines',
              type = 'scatter') %>%
        layout(
          title = paste0("Stock price of ",equity_row["name"]),
          xaxis = list(title = "Date"),
          yaxis = list(title = paste0("Close price (",equity_row["currency"],")"))
          )
      
      insertUI(
        selector = '#placeholder',
        ## wrap element in a div with id for ease of removal
        ui = 
          tags$div(
            id = id,
            tags$h1(paste(equity_row["name"])),
            tags$p(paste('ISIN:', equity_row["isin"])),
            tags$p(paste('Ticker:', equity_row["yahoo_symbol"])),
            tags$h2(paste0('Key Metrics')),
            tags$h2(paste0('Stock price')),
            output$price_plot <- renderPlotly(
              {
               p
              }
            ),
            tags$h2("Patent Portfolio"),
            tags$h2("Drug Pipeline"),
            tags$h2("Competitive Position"),
            tags$h2("Clinical Trials")
        )
      )
      inserted <<- c(id, inserted)
    })

    observeEvent(input$back_to_equities_button, {
      shinyjs::toggle(id = "selection_panel")
      removeUI(
      selector = paste0('#', inserted[length(inserted)])
      )
      inserted <<- inserted[-length(inserted)]
    })
####################PHARMA FUND######
  
  
  output$country  <- renderText({ input$country })
  observeEvent(input$do, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  
  output$company_table <- DT::renderDataTable(
    DT::datatable(equities_table,
    options = list(lengthMenu = c(10,1500), pageLength = 10),
    # Just one stock at a time
    selection =  "single"
    )
  )
  
  
  output$trials_table <- DT::renderDataTable({
    # Fix me
    company_trials <- dbReadTable(con,"fda_trials") %>%
      filter(grepl("Odonate",name)) %>%
      mutate(start_date = as.Date(start_date, 
                                  origin = "1970-01-01"))

    DT::datatable(company_trials,
                  options = list(lengthMenu = c(10,1500), pageLength = 10),
                  # Just one stock at a time
                  selection =  "single"
    )}
  )
    
    output$option_login_value <- renderText({
      req(input$go)
      #isolate(input$password)
      "Incorrect password."
  })

}
# Run the application 
shinyApp(ui = ui, server = server)

