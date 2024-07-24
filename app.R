# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- basicPage(
    h2("放假地圖"),
    
    # Application title
    #titlePanel("放假地圖"),
    
    # Sidebar with a slider input for number of bins 
    ##sidebarLayout(
    #   sidebarPanel(
    ##       selectInput("dataset", "Choose a dataset:",
    ##                  choices = c("rock", "pressure", "cars")),
    #    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("MAP")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$MAP <- renderPlot({
        
        library('sf')
        library('ggplot2')
        library('rvest')
        
        
        min_lon <- 118
        max_lon <- 122.5
        min_lat <- 21
        max_lat <- 25.5
        
        tw_shape <- read_sf(dsn = "./shp/CITY.shp")
        tw_small <- st_crop(tw_shape, xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat)
        html <- read_html("https://www.dgpa.gov.tw/typh/daily/nds.html")
        html %>%
            html_node('table') %>%
            html_table() -> df_holiday
        
        work_list <- c()
        city_list <- c()
        for (city in tw_small$COUNTYNAME){
            info <- df_holiday[df_holiday$縣市名稱 == city,2][[1]]
            city_list <- c(city_list, city)
            if (grepl('停止上班',info)){
                work_list <- c(work_list,1)
            }else{
                work_list <- c(work_list,0)   
            }
        }
        p <- ggplot(tw_small, aes(fill = factor(work_list))) 
        p <- p + geom_sf(color = 'black') 
        p <- p + guides(fill=guide_legend(title="放假"))
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)