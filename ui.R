ui <- basicPage(
    # Application title
    #titlePanel("放假地圖"),
    h1("停班地圖"),
    h4('僅供程式練習及測試，實際放假請洽行政院人事行政總處。'),
    uiOutput("link_Gov"),
    uiOutput("link_History"),
    fluidRow(column(width = 3,h2('上傳歷史資訊'),
                    fileInput("fileInfo", NULL,
                              multiple = FALSE,
                              accept = c("text/html"))),
             column(width = 3,h2('更新最新資訊'),
                    actionButton("ButtonGetGov", "更新")),
    ),
    
    fluidRow(
        column(width = 3,
               wellPanel(h4('今日'),
                         lapply(1:nrow(df_status), function(i) {
                             radioButtons(paste0('Today_', i), paste0('', df_status[i,1]),choices = StatusChoice, selected = StatStr2Num(df_status[i,2]) ,inline = TRUE)
                         })
               )
        ),
        column(width = 3,
               wellPanel(h4('明日'),
                         lapply(1:nrow(df_status), function(i) {
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