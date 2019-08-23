#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DBI)
library(shiny)
# install.packages("RSQLite")
library(RSQLite)
library(dplyr)
library(shiny)
library(markdown)

#Initialization routine
con <- dbConnect(RSQLite::SQLite(), "./data/database")
if (1==0){
# Create an RSQLite database witþ all ðe stock in ðe mergent list.
#Create an all world table on existing database
PHARMA_LISTINGS <- readxl::read_xlsx("~/Favourites/Investing/data/ALL_WORLD.xlsx", 
                                     sheet="LISTINGS",
                                     col_types = "text") %>% 
  filter(icb_subsector %in% c("Pharmaceuticals","Biotechnology")) %>%
  select(-industry_sector,-yahoo_symbol)

dbWriteTable(con, "listings", PHARMA_LISTINGS, overwrite=TRUE)
} else {
  # Get ðe data out of ðe database
  comps <- dbReadTable(con, "listings") %>%
    arrange(name) %>% 
    select(name, ibkr_symbol,isin, icb_subsector,country) 
  
  # Create a vector of names
  comp_labs   <- setNames(c("(Select)",comps$ibkr_symbol), c("",comps$name))
  
  # Country Labels
  ISO3        <- unique(dbReadTable(con, "listings") %>% select(country) %>% arrange(country) %>% filter(country != "") )
  county_labs <- setNames(c("Any",ISO3$country),c("Any",ISO3$country))
  
}

# Define UI for application that draws a histogram
ui <- navbarPage(theme = "styles.css",
  # Application title
  img(src = "logoonly.png", width = 60, height = 48),
   
  tabPanel("About",
           includeHTML("index.html")
  ),
  
  tabPanel("℞-Fund",
           
           # Application title
           #titlePanel(),
           tags$body("The Arêté ℞-Fund combines information on patents and clinical trials to generate alpha in a traditional long-short 130/30 equity strategy."),
           
           # Sidebar with a slider input for number of bins 
           fluidPage(
              #sidebarPanel(
               # selectInput("pharma_biotech", "Subector:",
               #             c("Pharma"="4.1.1",  "Biotech"="4.1.2"), multiple = TRUE),
               # 
               # selectInput("country", "Country:",
               #             county_labs, multiple = TRUE),
               # 
               # selectInput("company", "Company:",
               #             comp_labs, selectize=FALSE),
               # actionButton("find_companies", "Get companies.")
               
              # ),
             # Show a plot of the generated distribution
             mainPanel(
               
               tags$h3("Pharma & Biotech Equities"),
               # Create a new row for the table.
               textOutput("country"),
               DT::dataTableOutput("company_table"),
               plotOutput("distPlot")
             )# Main Panel 
           ) # Sidebar
          
  ),
  
  tabPanel("Contact",
    includeHTML("contact.html")

  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$country  <- renderText({ input$country })
  observeEvent(input$do, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  output$company_table <- DT::renderDataTable(
    DT::datatable(
      {
        dbReadTable(con, "listings") %>% 
          arrange(name) 
      },
    options = list(lengthMenu = c(10,1500), pageLength = 10))
    )

}

# Run the application 
shinyApp(ui = ui, server = server)

