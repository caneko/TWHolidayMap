# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library('sf')
library('ggplot2')
library('rvest')

UpdateData <- function(){
    html <- read_html("https://www.dgpa.gov.tw/typh/daily/nds.html")
    #html <- read_html("./testhtml/twoRow.html")
    html %>%
        html_node('table') %>%
        html_table() -> df_gov
    df_gov <- df_gov[-23,-3]
    return(df_gov)
}

LoadMap <- function(){
    min_lon <- 118
    max_lon <- 122.5
    min_lat <- 21
    max_lat <- 26
    tw_shape <- read_sf(dsn = "./shp/CITY.shp")
    tw_small <- st_crop(tw_shape, xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat)
    return (tw_small)
}

tw_small <- LoadMap()
df_gov <- UpdateData()
tw_small <- tw_small[match(df_gov$縣市名稱,tw_small$COUNTYNAME),]

df <- data.frame(df_gov$縣市名稱, factor(rep('未宣布',22),levels = c('放假','上班','未宣布')), factor(rep('未宣布',22),levels = c('放假','上班','未宣布')))
colnames(df) <- c('City',"Today",'Tomorrow')
dayOffChoice <-  c(放假 = 1, 上班 = 0,未宣布 = -1)

dayOffMap <- function(offstr){
    if (offstr == '未宣布'){
        return (-1)
    }else if(offstr == '上班'){
        return (0)
    }else{
        return (1)
    }
}
dayOffMapR <- function(offint){
    if (offint == -1){
        return ('未宣布')
    }else if(offint == 0 ){
        return ('上班')
    }else{
        return ("放假")
    }
}


for (city in df_gov$縣市名稱){
    city_status <- df_gov[df_gov$縣市名稱 == city, 2][[1]] 
    if (grepl('今天停止上班',city_status)) {
        df[df$City == city,'Today'] = '放假'
    }else if(grepl('今天照常上班',city_status)){
        df[df$City == city,'Today'] = '上班'
    }
    if (grepl('明天停止上班',city_status)) {
        df[df$City == city,'Tomorrow'] = '放假'
    }else if(grepl('明天照常上班',city_status)){
        df[df$City == city,'Tomorrow'] = '上班'
    }
}


ui <- basicPage(
    titlePanel("放假地圖"),
    
    # Application title
    #titlePanel("放假地圖"),
    fluidRow(
    # fixedRow(
        column(width = 2,
           wellPanel(h4('今日'),
                lapply(1:nrow(df), function(i) {
                    radioButtons(paste0('Today_', i), paste0('', df[i,1]),choices = dayOffChoice, selected = dayOffMap(df[i,2]) ,inline = TRUE)
                })
            )
        ),
        column(width = 2,
           wellPanel(h4('今日'),
                lapply(1:nrow(df), function(i) {
                    radioButtons(paste0('Tomorrow_', i), paste0('', df[i,1]),choices = dayOffChoice, selected = dayOffMap(df[i,3]), inline = TRUE)
                })
            )
        ),
        column(4,wellPanel(h2('今日'),
                           plotOutput("Map_Today"),
                           h2('明日'),
                           plotOutput("Map_Tomorrow"))),
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    color_vec <- c('#F0E442','#CC79A7','#999999')
    df_react <- df
    makeReactiveBinding("df_react")
    newData <- reactive({
        for (i in 1:nrow(df)){
            df[i,2] <- dayOffMapR(input[[paste0('Today_',i)]])
            df[i,3] <- dayOffMapR(input[[paste0('Tomorrow_',i)]])
        }
        # df[1,2] <- dayOffMapR(input[['Today_1']])
        # df[1,2] <- dayOffMapR(input$Today_1)
        # df[2,2] <- dayOffMapR(input$Today_2)
        df_react <- df
    })
    output$Map_Today <- renderPlot({
        df_react <- newData()
        work_list <- df_react$Today
        p <- ggplot(tw_small, aes(fill = work_list))
        p <- p + geom_sf(color = 'black') 
        p <- p + guides(fill=guide_legend(title="放假"))
        p <- p + scale_fill_manual(values = color_vec,drop = FALSE)
        print(p)
    })
    
    output$Map_Tomorrow <- renderPlot({
        df_react <- newData()
        work_list <- df_react$Tomorrow
        p <- ggplot(tw_small, aes(fill = work_list))
        p <- p + geom_sf(color = 'black') 
        p <- p + guides(fill=guide_legend(title="放假"))
        p <- p + scale_fill_manual(values = color_vec,drop = FALSE)
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)