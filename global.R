GetGovHtml <- function(url = 'https://www.dgpa.gov.tw/typh/daily/nds.html'){
    html <- read_html(url)
    #html <- read_html("./testhtml/twoRow.html")
    #html <- read_html("./testhtml/tes2.html")
    return (html)
}

ParseInfoHtml <- function(html){
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

DefaultStatus <- function(){
    df <-  data.frame(CityList, 
                      factor(rep('未公布',22),levels = StatusVec),
                      factor(rep('未公布',22),levels = StatusVec))
    colnames(df) <- c('City',"Today",'Tomorrow')
    return (df)
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

UpdateStatus <- function(df_status, df_gov){
    for (city in df_gov$縣市名稱){
        city_status <- df_gov[df_gov$縣市名稱 == city, 2][[1]] 
        df_status[df_status$City == city,'Today'] = SolveStatInfo(city_status, 'Today', 'Str')
        df_status[df_status$City == city,'Tomorrow'] = SolveStatInfo(city_status, 'Tomorrow', 'Str')
    }
    return(df_status)
}

library('sf')
library('ggplot2')
library('rvest')

CityList <- c("基隆市","臺北市","新北市","桃園市","新竹市","新竹縣","苗栗縣","臺中市","彰化縣","雲林縣","南投縣","嘉義市","嘉義縣","臺南市","高雄市","屏東縣","宜蘭縣","花蓮縣","臺東縣","澎湖縣","連江縣","金門縣")

ColorVec <- c("#009E73",'#F0E442','#CC79A7','#999999')
StatusVec <- c('放假','部分上班','上班','未公布')
StatusChoice <-  c(放假 = 1, 部分上班 = 0.5, 上班 = 0,未公布 = -1)

TWMap <- LoadTWMap()
TWMap <- TWMap[match(CityList,TWMap$COUNTYNAME),]

df_gov <- ParseInfoHtml(GetGovHtml())
df_status <- DefaultStatus()
df_status <- UpdateStatus(df_status, df_gov)
