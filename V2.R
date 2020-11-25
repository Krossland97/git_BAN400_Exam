library(shiny)
library(shinydashboard)
library(DT)
library(htmltab)
library(tidyverse)
library(tidyquant)
library(tbl2xts)
library(ggplot2)
library(knitr)
library(dygraphs)
library(timetk)
library(sweep)
library(forecast)

# Collect tickers
tabledata.url <- "https://no.wikipedia.org/wiki/OSEBX-indeksen"

#Create HTML tables of the tables from the tables in the url. Save as dataframe 
OSEBX_indeksen <- htmltab(doc = tabledata.url, #select url 
                          which = 1, #select the table containing the tickers
                          rm_nodata_cols = F) #Do not remove any columns regardsless of content

#Create vector with all the tickers. Adding ".OL". Without .OL code will not work with Yahoo  
OSEBX_indeksen_symbol <- paste0(OSEBX_indeksen[["Ticker"]], ".OL") 

#Remove OSE: from all charachers vector
OSEBX_tickers <-  str_remove_all(OSEBX_indeksen_symbol, "OSE: ") 
OSEBX_tickers2<-c(OSEBX_tickers)

#Add names to the charachers in the vector, 
names(OSEBX_tickers) =  paste0(OSEBX_indeksen[["Navn"]]) 

#Repeat the process for the list of benchnames
Bench<-"^OSEAX" #^OSEAX is the name of the ALl-share Index at Yahoo 
names(Bench) = paste0("Oslo Børs All-share Index") #Asign name to the characher ^OSEAX
benchnames <-c(Bench,OSEBX_tickers) #New vector of OSEBX tickers plus ^OSEAX

#------------------

ui <- dashboardPage(
  dashboardHeader(title = "Stock app"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Analysis", tabName = "stock", icon=icon("search")),
      menuItem("Risk & Forecast", tabName = "risk", icon = icon("chart-line")),
      menuItem("Market Overview", tabName = "osebx", icon=icon("landmark"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("stock",
              box(
                sidebarPanel( 
                  selectizeInput("stock_id", "Select stock", c("Choose stock to examine" = "", OSEBX_tickers)), 
                  dateRangeInput("dates", 
                                 "Date range", 
                                 start = "2019-01-01",
                                 end = as.character(Sys.Date()),
                                 max = as.character(Sys.Date()),
                                 format = "dd/mm/yyyy",
                                 startview = "year"),
                  selectizeInput("bench_id", "Compare to", c("Choose index/stock for comparison" = "", benchnames)),
                  textOutput("valid"),
                  textOutput("omitted"),
                  width = "100%" 
                ), 
                
              ),
              box(plotOutput("stock"), width = 6),
              box(plotOutput("returns"), width = 6),
              box(plotOutput("comp"), width = 6)
      ),
      tabItem("osebx",
              plotOutput("check",width = "100%"),
              dataTableOutput(outputId = "stocksTable", width = "100%")
      ),
      tabItem("risk",
              plotOutput(outputId = "models_tbl",width = "100%"),
              plotOutput("VaR", width = "100%")
      )
    )
  )
)

server<-function(input,output){
  #This is a table for
  withProgress(message = "Loading stock information. This may take a few seconds.", value = 0, {
    stocks <- OSEBX_tickers2
    index<-Bench
    
    rf.rate = "TB1YR" %>%  
      tq_get(
        get = "economic.data") %>%
      arrange(desc(date))
    
    rf.rate = as.numeric(rf.rate[1,3]/100)
    
    Market_return<-index%>%
      tq_get(
        get = "stock.prices",
        from =Sys.Date()-years(1), 
        to = Sys.Date())%>%
      tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period = "daily",
        col_rename = "returns")
    
    
    index_ret<-Market_return%>%
      tbl_xts(
        cols_to_xts =returns)%>%
      Return.annualized()
    
    asset.prices <- sapply(t(stocks), 
                           function(x) {
                             stock_information<-x%>%
                               tq_get(
                                 get = "stock.prices",
                                 from =Sys.Date()-years(1), 
                                 to = Sys.Date())
                             return_stock<-stock_information%>%
                               tq_transmute(
                                 select = adjusted,
                                 mutate_fun = periodReturn,
                                 period = "daily")%>%
                               left_join(Market_return)
                             
                           }, 
                           simplify=FALSE, USE.NAMES=TRUE)
    
  })
    
  withProgress(message = "Loading table information. This may take a few seconds.", value = 0, {
    stocks.df <- data.frame(ticker = names(asset.prices), beta = rep(NA), alpha = rep(NA), 
                            expected.return = rep(NA), return= rep(NA), r2 = rep(NA),
                            Stdev = rep(NA), Sharp = rep(NA),vVar = rep(NA))
    
    
    stocks.df[, c("beta","alpha","r2","expected.return","return","vVar","Stdev","Sharp")] <- t(as.data.frame(
      lapply(asset.prices, 
             function(x){
               b_s<-x%>%
                 tq_performance(
                   Ra = daily.returns,
                   Rb = returns,
                   performance_fun = CAPM.beta)%>%
                 `colnames<-`("beta")
               beta = b_s$beta
               a_s<-x%>%
                 tq_performance(
                   Ra = daily.returns,
                   Rb = returns,
                   performance_fun = CAPM.alpha)%>%
                 `colnames<-`("alpha")
               alpha = a_s$alpha
               lm.fit = lm(x$daily.returns~x$returns)
               r2 = summary(lm.fit)$adj.r.squared
               expected.return = rf.rate + beta * (index_ret - rf.rate)
               xts_return<-x%>%
                 tbl_xts(
                   cols_to_xts = daily.returns)
               
               return = Return.annualized(xts_return)
               Stdev =StdDev.annualized(xts_return)
               Sharp =SharpeRatio.annualized(xts_return, Rf = rf.rate)
               vVar = CVaR(x$daily.returns,p=.95, method="historical")
               round(c(beta,alpha,r2,expected.return,return,vVar,Stdev,Sharp), 8)
               
             }
      )
    ))
    
    
    stocks.df<-stocks.df%>%
      rename('Annualized_Return' = 'return', 
             'Annualized_Sharpe' = 'Sharp',
             'Annualized_StdDev' = 'Stdev',
             'Beta' = 'beta',
             'Alpha' = 'alpha', 
             'R2' = 'r2',
             'Expected_return' = 'expected.return',
             'Historical_VaR'='vVar',
             'Ticker'='ticker')
    
    
  })
  
  dataInput <- reactive({  #reactive means sensitive to user input 
    
    # Require input from user
    validate(
      need(input$stock_id, 
           "Please select a stock from the drop-down"))
    validate(
      need(input$dates[1] < input$dates[2], 
           "Start date (left) must be prior to end date (right)"))
    
    
    stocks_return<- input$stock_id %>% 
      tq_get( #use tidyquant to get stockprices
        get = "stock.prices", #get prices from Yahoo
        from = input$dates[1], #refer to the input dates in the UI
        to = input$dates[2]) %>%
      group_by(symbol) %>% #group by ticker name (stock,benchmark)
      tq_transmute( #select adjusted return column to calculate daily returns 
        select = adjusted,
        mutate_fun = periodReturn, #Add periode returns to the dataframe. Column will be used in plots 
        period = "daily",
        col_rename = "returns")
    
    plot_var <- input$stock_id%>%
      tq_get( #use tidyquant to get stockprices
        get = "stock.prices", #get prices from Yahoo
        from = input$dates[1], #refer to the input dates in the UI
        to = input$dates[2])%>%
      tbl_xts(
        cols_to_xts =adjusted)%>%
      Return.calculate()
    
    
    stock <- input$stock_id %>% #create dataframe with input stock data. Use the input form UI
      tq_get(get = "stock.prices", 
             from = input$dates[1],
             to = input$dates[2])
    
    #dataframe with both stock data and bench data
    bench <-c(input$stock_id, input$bench_id)%>% #use the input from the UI to select which to use 
      tq_get(get = "stock.prices",
             from = input$dates[1],
             to = input$dates[2])
    
    bench_stock <- bench%>% #Use the bench dataframe and normalize the data in order to compare the stock to benchmark
      group_by(symbol)%>% #group by symbol 
      summarize(
        first_day = head(close, n = 1)) %>%
      full_join(bench) %>%
      mutate(close = 100*close/first_day) 
    
    
    stock.prediction <- input$stock_id %>% 
      tq_get( #use tidyquant to get stockprices
        get = "stock.prices", #get prices from Yahoo
        from = input$dates[1],
        to = input$dates[2])%>%
      tq_transmute(
        select = adjusted,
        mutate_fun = to.period,
        period = "day") %>% 
      tk_ts(select = -date)
    
    
    models_list <- list(
      auto.arima = list(
        y = stock.prediction
      ),
      ets = list(
        y = stock.prediction,
        damped = TRUE
      )
    )
    
    
    models_tbl <- enframe(models_list, name = "Method", value = "params")%>%
      mutate(fit = invoke_map(Method, params))%>%
      mutate(fcast = map(fit, forecast, h = 6))%>%
      mutate(sweep = map(fcast, sw_sweep, fitted = FALSE, timetk_idx = TRUE, rename_index = "date"))

    
    validation <- bench %>%
      filter(is.na(close))
    
    
    #In order to refer to the right dataframe when drawing the plots 
    #thus, the different dataframes used are listed. 
    list(stocks_return, stock, bench_stock, stocks.df, plot_var, models_tbl, validation)
  })
  
  #-----
  
  output$returns <- renderPlot({ #Generate the plot
    dataInput<-dataInput()[[1]] # Refer to list the list for stock returns first 
    dataInput%>%
      ggplot(aes(x = date, y = returns))+ #use returns column 
      geom_bar(stat = "identity", fill = palette_light()[[1]]) + 
      labs(title = "Daily Returns",
           x = "", y = "") + 
      geom_smooth(method = "lm") + #include the regression line to show development
      theme_classic() + 
      scale_color_tq() + 
      scale_y_continuous(labels = scales::percent) #Set scale to percent 
  }) 
  output$stock <-renderPlot({
    dataInput<-dataInput()[[2]] #Refer to the second index in the list
    dataInput%>%
      ggplot(aes(x = date, y = adjusted))+ #use adjusted returns column
      geom_line(aes(color = input$stock_id), size = 0.5, show.legend = T) +
      labs(title = "Stock Performance",
           x = "", y = "") +
      # Adding Simple Moving Averages (20-day and 50-day)
      geom_ma(aes(color = 'SMA(20)'), ma_fun = SMA, n = 20, size = 0.5, show.legend = T) +
      geom_ma(aes(color = 'SMA(50)'), ma_fun = SMA, n = 50, size = 0.5, show.legend = T) +
      theme_classic() +
      theme(legend.title=element_blank()) +
      scale_color_tq()
  })
  
  output$comp<-renderPlot({ 
    dataInput<-dataInput()[[3]] #Refer to the third index in the list 
    dataInput%>%
      ggplot(aes(x = date, y = close, group = symbol))+ #group by symbol, meaning stock and benchmark 
      geom_line(aes(color=symbol )) + #diffrent colours to the different lines 
      scale_color_manual(values=c('blue','grey0')) + #Set specific colours for the different lines 
      labs(title = "Comparison",
           x = "", y = "") +
      theme_classic() +
      theme(legend.title=element_blank()) +
      scale_x_date(expand = c(.14, 0))
  })
  output$check<-renderPlot({
    dataInput<-dataInput()[[4]]
    dataInput%>%
      ggplot(aes(y = Annualized_Return, x = Annualized_StdDev)) +
      geom_rect(aes(xmin = -Inf, xmax= Inf), ymin = rf.rate, ymax= Inf, fill = 'green', alpha = 0.01) + 
      geom_rect(aes(xmin = -Inf, xmax= Inf), ymin = -Inf, ymax= rf.rate, fill = 'red', alpha = 0.01) +
      #geom_hline(aes(yintercept = rf.rate)) + 
      geom_label(label = dataInput$Ticker, size = 2) + 
      annotate(geom ='text',
               x=0.85, 
               y=rf.rate, 
               label = paste('One-Year T-bill =',rf.rate), 
               size = 4.5) + 
      theme_bw() + 
      xlab('Standard deviation') + 
      ylab('Anualized return') + 
      ggtitle('Stock Performance vs Risk-Free Instrument (Annualized)') +
      theme(axis.text = element_text(size = 14), 
            plot.title = element_text(size =20, hjust = 0.5),
            axis.title = element_text(size = 16))
  })
  output$stocksTable <- DT::renderDataTable({
    stocks.df <- dataInput()[[4]]
    stocks.df#[, c("ticker", "beta", "alpha", "r2")]
  }, server = TRUE, selection = "single")
  
  output$VaR <-renderPlot({
    dataInput<-dataInput()[[5]]
    chart.VaRSensitivity(dataInput[,1,drop=FALSE],
                         methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), 
                         colorset=bluefocus, lwd=2,
                         main = "Value at risk selected stock")
  })
  output$models_tbl<-renderPlot({
    dataInput<-dataInput()[[6]]
    dataInput %>%
      unnest(sweep) %>%
      ggplot(aes(x = date, y = adjusted, color = key, group = Method)) +
      geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
                  fill = "#D5DBFF", color = NA, size = 0) +
      geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
                  fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
      geom_line(size = 1) +
      facet_wrap(~Method, nrow = 6, labeller =labeller(Method = c("auto.arima"="Arima model", "ets"="Exponential Smoothing")))+
      labs(title = "Stock forecast using using ARIMA and ETS",
           x = "", y = "Stock price") +
      scale_y_continuous(labels = scales::number) +
      theme_tq() +
      scale_color_tq()
    
  })
  # Validate
  output$valid <- renderText({
    
    if (nrow(dataInput()[[1]]) > 1) {
      paste("Data was successfully retrieved from yahoo! finance at", " ", as.character(Sys.time()))
    }
    else {
      paste("Application failed to retrieve data from yahoo! finance.
            Try selecting a different time period.")
    }
  })
  
  # Omitted values
  output$omitted <- renderText({
    
    if (nrow(dataInput()[[7]]) > 0) {
      paste("Note:", "", 
            nrow(dataInput()[[7]]), "", 
            "missing observation(s) omitted for the selected time period.")
    }
    
  })
}


shinyApp(ui,server)
