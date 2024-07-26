server <- function(input, output, session) {
    url_Gov <- a("天然災害停止上班及上課情形", href="https://www.dgpa.gov.tw/typh/daily/nds.html")
    url_History <- a("歷次天然災害停止上班上課訊息", href="https://www.dgpa.gov.tw/informationlist?uid=374")
    
    output$link_Gov <- renderUI({
        tagList("資料來源：", url_Gov)
    })
    output$link_History <- renderUI({
        tagList("歷史資料：", url_History)
    })
    
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
    
    observe({
        req(input$fileInfo)
        newHtml <- read_html(input$fileInfo[['datapath']])
        df_gov <- ParseInfoHtml(newHtml)
        df_status <- DefaultStatus()
        df_status <- UpdateStatus(df_status, df_gov)
        for (i in 1:nrow(df_status)){
            updateRadioButtons(session, paste0('Today_',i), NULL, NULL, StatStr2Num(df_status[i,2]))
            updateRadioButtons(session, paste0('Tomorrow_',i), NULL, NULL, StatStr2Num(df_status[i,3]))
        }
    })
    
    observeEvent(
        input$ButtonGetGov,
        {
            govhtml <- GetGovHtml()
            df_gov <- ParseInfoHtml(govhtml)
            df_status <- DefaultStatus()
            df_status <- UpdateStatus(df_status, df_gov)
            for (i in 1:nrow(df_status)){
                updateRadioButtons(session, paste0('Today_',i), NULL, NULL, StatStr2Num(df_status[i,2]))
                updateRadioButtons(session, paste0('Tomorrow_',i), NULL, NULL, StatStr2Num(df_status[i,3]))
            }
        })
}