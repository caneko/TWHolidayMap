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

GetGovData <- function(){
    html <- read_html("https://www.dgpa.gov.tw/typh/daily/nds.html")
    #html <- read_html("./testhtml/twoRow.html")
    #html <- read_html("./testhtml/tes2.html")
    html %>%
        html_node('table') %>%
        html_table() -> df_gov

    RowNotCity <- c()
    for (i in 1:nrow(df_gov)){
        if (!(df_gov[i,1] %in% CityList)){
            RowNotCity <- c(RowNotCity, i)
        }
    }
    df_gov <- df_gov[-RowNotCity,]
    df_gov <- df_gov[,-3]
    return(df_gov)
}

LoadTWMap <- function(){
    min_lon <- 118
    max_lon <- 122.5
    min_lat <- 21
    max_lat <- 26
    map_shp <- read_sf(dsn = "./shp/CITY.shp")
    map_shp <- st_crop(map_shp, xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat)
    return (map_shp)
}

TWMap <- LoadTWMap()
df_gov <- GetGovData()
TWMap <- TWMap[match(CityList,TWMap$COUNTYNAME),]

StatusVec <- c('放假','部分上班','上班','未公布')
StatusChoice <-  c(放假 = 1, 部分上班 = 0.5, 上班 = 0,未公布 = -1)

df_status <- data.frame(CityList, 
                        factor(rep('未公布',22),levels = StatusVec),
                        factor(rep('未公布',22),levels = StatusVec))
colnames(df_status) <- c('City',"Today",'Tomorrow')



StatStr2Num <- function(StatStr){
    if (StatStr == '放假'){
        return (1)
    }else if(StatStr == '部分上班'){
        return (0.5)
    }else if(StatStr == '上班'){
        return (0)
    }else{
        return (-1)
    }
}
StatNum2Str <- function(StatNum){
    if (StatNum == 1){
        return ('放假')
    }else if(StatNum == 0.5 ){
        return ('部分上班')
    }else if(StatNum == 0 ){
        return ('上班')
    }else{
        return ("未公布")
    }
}


SolveStatInfo <- function(StatInfo, day = 'Today', ReturnType = 'Num'){
    if (day == 'Today'){
        if (grepl('今天停止上班',StatInfo) && grepl('今天照常上班',StatInfo)){
            StatStr <- '部分上班'
        }else if(grepl('今天停止上班',StatInfo)){
            StatStr <- '放假'
        }else if(grepl('今天照常上班',StatInfo)){
            StatStr <- '上班'
        }else{
            StatStr <- '未公布'
        }    
    }else{
        if (grepl('明天停止上班',StatInfo) && grepl('明天照常上班',StatInfo)){
            StatStr <- '部分上班'
        }else if(grepl('明天停止上班',StatInfo)){
            StatStr <- '放假'
        }else if(grepl('明天照常上班',StatInfo)){
            StatStr <- '上班'
        }else{
            StatStr <- '未公布'
        }    
    }
    if (ReturnType == "Num"){
        return(StatStr2Num(StatStr))
    }else{
        return(StatStr)
    }
        
}



for (city in df_gov$縣市名稱){
    city_status <- df_gov[df_gov$縣市名稱 == city, 2][[1]] 
    df_status[df_status$City == city,'Today'] = SolveStatInfo(city_status, 'Today', 'Str')
    df_status[df_status$City == city,'Tomorrow'] = SolveStatInfo(city_status, 'Tomorrow', 'Str')
}


ui <- basicPage(
    # Application title
    titlePanel("放假地圖"),
    h3('資料來源：https://www.dgpa.gov.tw/typh/daily/nds.html'),
    h5('僅供程式練習及測試，實際放假請洽行政院人事行政總處。'),
    fluidRow(column(width = 3,h6('上傳歷史資訊'),
                    fileInput("fileInfo", NULL,
                                        multiple = FALSE,
                                        accept = c("text/html"))),
             column(width = 3,h6('讀取最新資訊'),
                    actionButton("GetGov", "更新")),
    ),
    
    fluidRow(
        column(width = 3,
               wellPanel(h4('今日'),
                         lapply(1:nrow(df), function(i) {
                             radioButtons(paste0('Today_', i), paste0('', df_status[i,1]),choices = StatusChoice, selected = StatStr2Num(df_status[i,2]) ,inline = TRUE)
                         })
               )
        ),
        column(width = 3,
               wellPanel(h4('明日'),
                         lapply(1:nrow(df), function(i) {
                             radioButtons(paste0('Tomorrow_', i), paste0('', df_status[i,1]),choices = StatusChoice, selected = StatStr2Num(df_status[i,3]), inline = TRUE)
                         })
               )
        ),
        column(4,wellPanel(h2('今日'),
                           plotOutput("Map_Today"),
                           h2('明日'),
                           plotOutput("Map_Tomorrow"))),
    )
)


server <- function(input, output) {
    
    ColorVec <- c("#009E73",'#F0E442','#CC79A7','#999999')
    df_react <- df_status
    makeReactiveBinding("df_react")
    newData_Today <- reactive({
        for (i in 1:nrow(df_status)){
            df_status[i,2] <- StatNum2Str(input[[paste0('Today_',i)]])
        }
        # df[1,2] <- StatNum2Str(input[['Today_1']])
        # df[1,2] <- StatNum2Str(input$Today_1)
        # df[2,2] <- StatNum2Str(input$Today_2)
        df_react <- df_status
    })
    newData_Tomorrow <- reactive({
        for (i in 1:nrow(df_status)){
            df_status[i,3] <- StatNum2Str(input[[paste0('Tomorrow_',i)]])
        }
        # df[1,3] <- StatNum2Str(input[['Today_1']])
        # df[1,3] <- StatNum2Str(input$Today_1)
        # df[2,3] <- StatNum2Str(input$Today_2)
        df_react <- df_status
    })
    output$Map_Today <- renderPlot({
        df_react <- newData_Today()
        p <- ggplot(TWMap, aes(fill = df_react$Today))
        p <- p + geom_sf(color = 'black') 
        p <- p + guides(fill=guide_legend(title="放假"))
        p <- p + scale_fill_manual(values = ColorVec,drop = FALSE)
        print(p)
    })
    
    output$Map_Tomorrow <- renderPlot({
        df_react <- newData_Tomorrow()
        p <- ggplot(TWMap, aes(fill = df_react$Tomorrow))
        p <- p + geom_sf(color = 'black') 
        p <- p + guides(fill=guide_legend(title="放假"))
        p <- p + scale_fill_manual(values = ColorVec,drop = FALSE)
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)