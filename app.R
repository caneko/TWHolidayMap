# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library('shiny')
library('sf')
library('ggplot2')
library('rvest')

CityList <- c("基隆市","臺北市","新北市","桃園市","新竹市","新竹縣","苗栗縣","臺中市","彰化縣","雲林縣","南投縣","嘉義市","嘉義縣","臺南市","高雄市","屏東縣","宜蘭縣","花蓮縣","臺東縣","澎湖縣","連江縣","金門縣")

UpdateData <- function(){
    html <- read_html("https://www.dgpa.gov.tw/typh/daily/nds.html")
    #html <- read_html("./testhtml/twoRow.html")
    #html <- read_html("./testhtml/tes2.html")
    html %>%
        html_node('table') %>%
        html_table() -> df_gov
    row_to_delete <- c()
    for (i in 1:nrow(df_gov)){
        if (!(df_gov[i,1] %in% CityList)){
            row_to_delete <- c(row_to_delete, i)
        }
    }
    df_gov <- df_gov[-row_to_delete,]
    df_gov <- df_gov[,-3]
    #df_gov <- df_gov[-23,-3]
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
tw_small <- tw_small[match(CityList,tw_small$COUNTYNAME),]

df <- data.frame(CityList, factor(rep('未公布',22),levels = c('放假','部分上班','上班','未公布')), factor(rep('未公布',22),levels = c('放假','部分上班','上班','未公布')))
colnames(df) <- c('City',"Today",'Tomorrow')
dayOffChoice <-  c(放假 = 1, 部分上班 = 0.5, 上班 = 0,未公布 = -1)

dayOffMap <- function(offstr){
    if (offstr == '放假'){
        return (1)
    }else if(offstr == '部分上班'){
        return (0.5)
    }else if(offstr == '上班'){
        return (0)
    }else{
        return (-1)
    }
}
dayOffMapR <- function(offint){
    if (offint == 1){
        return ('放假')
    }else if(offint == 0.5 ){
        return ('部分上班')
    }else if(offint == 0 ){
        return ('上班')
    }else{
        return ("未公布")
    }
}

StringToStatus <- function(status, day){
    if (day == 'Today'){
        if (grepl('今天停止上班',status) && grepl('今天照常上班',status)){
            return (0.5)
        }else if(grepl('今天停止上班',status)){
            return (1)
        }else if(grepl('今天照常上班',status)){
            return (0)
        }else{
            return (-1)
        }    
    }else{
        if (grepl('明天停止上班',status) && grepl('明天照常上班',status)){
            return (0.5)
        }else if(grepl('明天停止上班',status)){
            return (1)
        }else if(grepl('明天照常上班',status)){
            return (0)
        }else{
            return (-1)
        }    
    }
}



for (city in df_gov$縣市名稱){
    city_status <- df_gov[df_gov$縣市名稱 == city, 2][[1]] 
    df[df$City == city,'Today'] = dayOffMapR(StringToStatus(city_status, 'Today'))
    df[df$City == city,'Tomorrow'] = dayOffMapR(StringToStatus(city_status, 'Tomorrow'))
}


ui <- basicPage(
    titlePanel("放假地圖"),
    h3('資料來源：https://www.dgpa.gov.tw/typh/daily/nds.html'),
    h5('僅供程式練習及測試，實際放假請洽行政院人事行政總處。'),
    
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
               wellPanel(h4('明日'),
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
    
    color_vec <- c("#009E73",'#F0E442','#CC79A7','#999999')
    df_react <- df
    makeReactiveBinding("df_react")
    newData_Today <- reactive({
        for (i in 1:nrow(df)){
            df[i,2] <- dayOffMapR(input[[paste0('Today_',i)]])
        }
        # df[1,2] <- dayOffMapR(input[['Today_1']])
        # df[1,2] <- dayOffMapR(input$Today_1)
        # df[2,2] <- dayOffMapR(input$Today_2)
        df_react <- df
    })
    newData_Tomorrow <- reactive({
        for (i in 1:nrow(df)){
            df[i,3] <- dayOffMapR(input[[paste0('Tomorrow_',i)]])
        }
        # df[1,3] <- dayOffMapR(input[['Today_1']])
        # df[1,3] <- dayOffMapR(input$Today_1)
        # df[2,3] <- dayOffMapR(input$Today_2)
        df_react <- df
    })
    output$Map_Today <- renderPlot({
        df_react <- newData_Today()
        work_list <- df_react$Today
        p <- ggplot(tw_small, aes(fill = work_list))
        p <- p + geom_sf(color = 'black') 
        p <- p + guides(fill=guide_legend(title="放假"))
        p <- p + scale_fill_manual(values = color_vec,drop = FALSE)
        print(p)
    })
    
    output$Map_Tomorrow <- renderPlot({
        df_react <- newData_Tomorrow()
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