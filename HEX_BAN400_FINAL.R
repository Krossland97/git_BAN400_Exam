#General sources:
#https://business-science.github.io/tidyquant/articles/TQ05-performance-analysis-with-tidyquant.html
#https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ05-performance-analysis-with-tidyquant.html
#https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ03-scaling-and-modeling-with-tidyquant.html
#https://towardsdatascience.com/building-and-testing-stock-portfolios-in-r-d1b7b6f59ac4
#https://www.r-bloggers.com/r-shiny-stock-analysis/?fbclid=IwAR0mEk_oSN-Dpy_MXI4tgEfBCfA7fR_W3ZJ0BQnUXoH5pJNFpwGev8S_n-g
#https://github.com/business-science/tidyquant
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/

#Url to app https://hom1998.shinyapps.io/BAN400-final/
# ---------------------------------------------------Packages---------------------------------------------------------------------------

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


#---------------------------------------------------Ticker preperation---------------------------------------------------------------------------

DJ<-tq_index("dow") #Acquires the pre downloaded data frame of all listed stocks on the Dow Jones Index.

dow_jones<-paste0(DJ[["symbol"]])%>% # Substituting all "." in the ticker names with "-" as stocks with a dot in the ticker name on Yahoo Finance are listed with -
  str_replace_all("\\.","-")

names(dow_jones)<-paste0(DJ[["company"]]) #Adding names to the characters in the vector
dow_jones<-sort(dow_jones)

#Repeat the process for the list of benchnames
Bench<-"^DJI" #^DJI is the name of the Dow Jones Index at Yahoo Finance 
names(Bench) = paste0("DOW Index") #Assign name to the character ^DJI

benchnames<-c(Bench, dow_jones) #New vector which includes all DJI tickers and ^DJI sorted

#-------------------------------------------------------------User interface----------------------------------------------------------------------

#Creating the User Interface of the Shiny-App
ui<-dashboardPage(
  dashboardHeader(title = "Stock app"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Analysis", tabName = "stock", icon=icon("search")), #Adding 3 options to the sidebar. The app will look cleaner, and be more user friendly with several pages
      menuItem("Risk & Forecast", tabName = "risk", icon = icon("chart-line")), #(search/chart-line/landmark) are three symbols that will be located next to the titles in the sidebar 
      menuItem("Market Overview", tabName = "dji", icon=icon("landmark")) #which helps the user to locate the page they are looking for. This will be more useful with more pages.
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("stock", # creating the UI of the first page, which is called stock
              box(
                sidebarPanel( 
                  selectizeInput("stock_id", "Select stock", c("Choose stock to examine" = "", dow_jones)), #this requires the user to select a stock he/she may want to inspect
                  dateRangeInput("dates", 
                                 "Date range", 
                                 start = "2019-01-01", #this is the start-date the app will be set to when it starts. The user can change this date as he/she wants 
                                 end = as.character(Sys.Date()), 
                                 max = as.character(Sys.Date()), # the end date will automatically be set to today's date. This gives the user the most recent available data
                                 format = "dd/mm/yyyy", #the chosen date-format.
                                 startview = "year"),
                  selectizeInput("bench_id", "Compare to", c("Choose index/stock for comparison" = "", benchnames)), #here the user is required to select either a second stock or the benchmark to compare to the first stock.
                  textOutput("valid"), #validation text if the app is working. This will tell the user that the data was successfully retrieved.
                  textOutput("omitted"), #as with the validation text, this will notify the user if there are some data missing.
                  width = "100%" 
                ), 
                
              ),
              box(plotOutput("stock"), width = 6), #the three different plots that can be found on the "Stock" page.
              box(plotOutput("returns"), width = 6),
              box(plotOutput("comp"), width = 6)
      ),
      tabItem("dji",
              plotOutput("check",width = "100%"), #the plot in the page "Market Overview"
              dataTableOutput(outputId = "stocksTable", width = "100%") #this is a list of all stocks.
      ),
      tabItem("risk",
              plotOutput(outputId = "models_tbl",width = "100%"), #the two plots in the page "Risk & Forecast"
              plotOutput("VaR", width = "100%")
      )
    )
  )
)

#-------------------------------------------------------------Server-------------------------------------------------------------------------------

server<-function(input,output){
  
  #this is a loading message to let the user know the app is currently loading in all the information.
  withProgress(message = "Loading stock information. This may take a few seconds.", value = 0, { 
    
    stocks <- dow_jones #tickers 
    index<-Bench #benchmark
    
    #---------------------------------------------------Analytics table ---------------------------------------------------------------------------
    
    #We wanted to create a table displaying key values for all the stocks in the app. 
    #We found several methodes for this, but decided to be inspired by the following source:
    #https://medium.com/@sermal/how-to-develop-a-stock-market-analytical-tool-using-shiny-and-r-c2385e0d2f89
    #This data is not reactive by purpose
    
    
    rf.rate = "TB1YR" %>%  #finding the monthly risk free rate from the last decade 
      tq_get( #use tidyquant to get economic data.
        get = "economic.data") %>% #get FRED economic data  
      arrange(desc(date)) #sort by date
    
    rf.rate = as.numeric(rf.rate[1,3]/100) # find the risk-free rate from t-bills
    
    Market_return<-index%>%
      tq_get( #use tidyquant to get stockprices
        get = "stock.prices", #get prices from Yahoo
        from =Sys.Date()-years(1), #use one year data, constantly updated to system date
        to = Sys.Date())%>%
      tq_transmute( #Select adjusted return column to calculate daily returns
        select = adjusted,
        mutate_fun = periodReturn,
        period = "daily", 
        col_rename = "returns") #rename  column
    
    
    index_ret<-Market_return%>% #create market index for comparison 
      tbl_xts(#convert to xts, fomat needed to use Return.annualized()
        cols_to_xts =returns)%>%
      Return.annualized() #the annualized returns will be used later in the CAPM 
    
    asset.prices <- sapply(t(stocks), #retrieve stock information about all the stocks
                           function(x) { #use sapply to itterate over the list of stocks and obtain the stock prices 
                             stock_information<-x%>% 
                               tq_get( #use tidyquant to get stockprices
                                 get = "stock.prices", #get prices from Yahoo
                                 from =Sys.Date()-years(1), #get one year data to use in table
                                 to = Sys.Date()) #Sys.Date is used to update the dates to current time 
                             return_stock<-stock_information%>%
                               tq_transmute(  
                                 select = adjusted, #select adjusted return column to calculate daily returns
                                 mutate_fun = periodReturn,
                                 period = "daily")%>% #select daily returns to get most accurate basis of analysis
                               left_join(Market_return) 
                             
                           }, 
                           simplify=FALSE, USE.NAMES=TRUE)
    
  })
  
  withProgress(message = "Loading table information. This may take a few seconds.", value = 0, { #loading box telling the user about the progress
    #create a datafram where all the calculated data is instered
    #the company names and tickers are inserted to get the right length 
    stocks.df <- data.frame(company = names(dow_jones),ticker = names(asset.prices), beta = rep(NA), alpha = rep(NA), 
                            expected.return = rep(NA), return= rep(NA), r2 = rep(NA),
                            Stdev = rep(NA), Sharp = rep(NA),vVar = rep(NA))
    
    
    stocks.df[, c("beta","alpha","r2","expected.return","return","vVar","Stdev","Sharp")] <- t(as.data.frame(
      lapply(asset.prices,#lapply is used to itterate over the list of stock prices, the functions are applied to all the stocks
             function(x){ #for each stock in the lsit perform analysis
               b_s<-x%>%
                 tq_performance( #use performance package to calculate returns and CAPM beta
                   Ra = daily.returns,
                   Rb = returns,
                   performance_fun = CAPM.beta)%>%
                 `colnames<-`("beta")
               beta = b_s$beta #select beta and instert it into stocks.df
               a_s<-x%>% 
                 tq_performance( #Calculate CAPM alpha
                   Ra = daily.returns,
                   Rb = returns,
                   performance_fun = CAPM.alpha)%>%
                 `colnames<-`("alpha")
               alpha = a_s$alpha #select alpha
               lm.fit = lm(x$daily.returns~x$returns) #regression to obtain r2
               r2 = summary(lm.fit)$adj.r.squared 
               expected.return = rf.rate + beta * (index_ret - rf.rate) # CAPM expected return 
               xts_return<-x%>% #need xts format to use the built inn analytics functions
                 tbl_xts(
                   cols_to_xts = daily.returns) #thus, we found it necessary transform to xts
               return = Return.annualized(xts_return) #calcualte annualizedreturns
               Stdev =StdDev.annualized(xts_return) #calculate standard deviation 
               Sharp =SharpeRatio.annualized(xts_return, Rf = rf.rate) #calculate sharpe Ratio 
               vVar = CVaR(x$daily.returns,p=.95, method="historical") #calculate historical value at risk 
               round(c(beta,alpha,r2,expected.return,return,vVar,Stdev,Sharp), 8) # round to eight decimals
               
             }
      )
    ))
    
    #Renaming some terms in the dataframe and adding capital letter.
    stocks.df<-stocks.df%>%
      rename('Annualized_Return' = 'return', 
             'Annualized_Sharpe' = 'Sharp',
             'Annualized_StdDev' = 'Stdev',
             'Beta' = 'beta',
             'Alpha' = 'alpha', 
             'R2' = 'r2',
             'Expected_return' = 'expected.return',
             'Historical_VaR'='vVar',
             'Ticker'='ticker',
             'Company' = 'company')
    
    
  })
  
  
  #---------------------------------------------------Reactive part of server---------------------------------------------------------------------------
  
  dataInput <- reactive({  #reactive means sensitive to user input 
    
    # require input from user
    validate(
      need(input$stock_id, #this requires the user to pick a stock
           "Please select a stock from the drop-down")) 
    validate(
      need(input$dates[1] < input$dates[2], #the user has to pick an older start-date than the end-date
           "Start date (left) must be prior to end date (right)")) 
    
    
    stocks_return<- input$stock_id %>% 
      tq_get( #use tidyquant to get stockprices
        get = "stock.prices", #get prices from Yahoo
        from = input$dates[1], #refer to the input dates in the UI
        to = input$dates[2]) %>%
      group_by(symbol) %>% #group by ticker name (stock,benchmark)
      tq_transmute( #select adjusted return column to calculate daily returns 
        select = adjusted,
        mutate_fun = periodReturn, #add periode returns to the dataframe. Column will be used in plots 
        period = "daily", #we want daily returns
        col_rename = "returns") #rename the column
    
    plot_var <- input$stock_id%>% #creating dataframe with input stock data
      tq_get( #use tidyquant to get stockprices
        get = "stock.prices", #get prices from Yahoo
        from = input$dates[1], #refer to the input dates in the UI
        to = input$dates[2])%>%
      tbl_xts(
        cols_to_xts =adjusted)%>%
      Return.calculate()
    
    
    stock <- input$stock_id %>% #create dataframe with input stock data. Use the input from UI
      tq_get(get = "stock.prices", #use tidyquant to get stockprices
             from = input$dates[1],
             to = input$dates[2])
    
    #dataframe with both stock data and bench data
    bench <-c(input$stock_id, input$bench_id)%>% #use the input from the UI to select which to use 
      tq_get(get = "stock.prices", #use tidyquant to get stockprices
             from = input$dates[1],
             to = input$dates[2])
    
    bench_stock <- bench%>% #use the bench dataframe and normalize the data in order to compare the stock to benchmark
      group_by(symbol)%>% #group by symbol 
      summarize(
        first_day = head(close, n = 1)) %>%
      full_join(bench) %>%
      mutate(close = 100*close/first_day) 
    
    #---------------------------------------------------Stock prediction---------------------------------------------------------------------------
    #We wanted to create two forecast plots, and we were inspired by the approach from the following sources:
    #https://business-science.github.io/sweep/articles/SW01_Forecasting_Time_Series_Groups.html
    #https://cran.rstudio.com/web/packages/sweep/vignettes/SW02_Forecasting_Multiple_Models.html
    #We found this method easy and understandable. Thus, we decided to adjust most of it to our data
    
    stock.prediction <- input$stock_id %>% 
      tq_get( #use tidyquant to get stockprices
        get = "stock.prices", #get prices from Yahoo
        from = "2010-01-01", #download data form 2010 until today to secure stable data
        to = input$dates[2])%>%
      tq_transmute( #restructure to monthly returns 
        select = adjusted,
        mutate_fun = to.period,
        period = "months") %>% 
      tk_ts(select = -date, #create timeseries using tk_ts
            start = c(2010,1),
            freq = 12)
    
    list_mod <- list( #create a nested list, one for ets and one for arima forecast
      auto.arima = list( 
        y = stock.prediction #list for arima
      ),
      ets = list( #list for ets or exponential smootheing 
        y = stock.prediction,
        damped = TRUE
      )
    )
    tbl_model <- enframe(list_mod, name = "Method", value = "params")%>% #convert to dataframe
      mutate(fit = invoke_map(Method, params))%>% #combine mutate and invoke_map function to create a fitted model
      mutate(fcast = map(fit, forecast, h = 12))%>%  #forecast to 12 months ahead  
      mutate(sweep = map(fcast, sw_sweep, #convert forecast into a tibble  
                         fitted = FALSE, #remove fitted values 
                         timetk_idx = TRUE,  #use dates instead of numeric data 
                         rename_index = "date")) 
    
    
    validation <- bench %>% 
      filter(is.na(close))
    
    #In order to refer to the right dataframe when creating the plots 
    #the different dataframes used are listed. 
    list(stocks_return, stock, bench_stock, stocks.df, plot_var, tbl_model, validation)
  })
  
  #-------------------------------------------------------------Plots------------------------------------------------------------------------------------------
  
  output$returns <- renderPlot({ #generate the plot for stock returns
    dataInput<-dataInput()[[1]] # refer to list the list for stock returns first 
    dataInput%>% #create a new ggplot
      ggplot(aes(x = date, y = returns))+ #use returns column 
      geom_bar(stat = "identity", fill = palette_light()[[1]]) + #aesthetic mappings and components 
      labs(title = "Daily Returns",
           x = "", y = "") + 
      geom_smooth(method = "lm") + #include the regression line to show development
      theme_classic() + #A custom theme
      scale_color_tq() + 
      scale_y_continuous(labels = scales::percent) #Set scale to percent 
  }) 
  
  output$stock <-renderPlot({ #generate the plot for stock price 
    dataInput<-dataInput()[[2]] #refer to the second index in the list
    dataInput%>% #create a new ggplot
      ggplot(aes(x = date, y = adjusted))+ #use adjusted returns column
      geom_line(aes(color = input$stock_id), size = 0.5, show.legend = T) +
      labs(title = "Stock Performance",
           x = "", y = "") +
      # Adding Simple Moving Averages (20-day and 50-day)
      geom_ma(aes(color = 'SMA(20)'), ma_fun = SMA, n = 20, size = 0.5, show.legend = T) + #aesthetic mappings
      geom_ma(aes(color = 'SMA(50)'), ma_fun = SMA, n = 50, size = 0.5, show.legend = T) +
      theme_classic() + #classic theme used for most plots
      theme(legend.title=element_blank()) +
      scale_color_tq()
  })
  
  output$comp<-renderPlot({  #generate comparison plot
    dataInput<-dataInput()[[3]] #refer to the third index in the list 
    dataInput%>% #create a new ggplot
      ggplot(aes(x = date, y = close, group = symbol))+ #group by symbol, meaning stock and benchmark 
      geom_line(aes(color=symbol )) + #diffrent colours to the different lines 
      scale_color_manual(values=c('blue','grey0')) + #set specific colours for the different lines 
      labs(title = "Comparison",
           x = "", y = "") +
      theme_classic() + #classic theme
      theme(legend.title=element_blank()) +
      scale_x_date(expand = c(.14, 0))
  })
  
  #plot inspired form the following source
  #https://towardsdatascience.com/how-to-measure-stock-portfolio-performance-using-r-847c992195c2
  output$check<-renderPlot({ #generate the plot
    dataInput<-dataInput()[[4]] #refer to the fourth index in the list
    dataInput%>% #create a new ggplot
      ggplot(aes(y = Annualized_Return, x = Annualized_StdDev)) +
      geom_rect(aes(xmin = -Inf, xmax= Inf), ymin = rf.rate, ymax= Inf, fill = 'green', alpha = 0.01) + 
      geom_rect(aes(xmin = -Inf, xmax= Inf), ymin = -Inf, ymax= rf.rate, fill = 'red', alpha = 0.01) + 
      geom_label(label = dataInput$Ticker, size = 2) + 
      annotate(geom ='text',#add risk free rate indikator
               x=0.85, 
               y=rf.rate, 
               label = paste('One-Year T-bill =',rf.rate), 
               size = 4.5) + 
      theme_bw() + #A custom theme
      xlab('Standard deviation') +  #adding custom components 
      ylab('Anualized return') + 
      ggtitle('Stock Performance vs Risk-Free Instrument (Annualized)') +
      theme(axis.text = element_text(size = 14), 
            plot.title = element_text(size =20, hjust = 0.5),
            axis.title = element_text(size = 16))
  })
  output$stocksTable <- DT::renderDataTable({ #generate the table
    stocks.df <- dataInput()[[4]]
    stocks.df#[, c("ticker", "beta", "alpha", "r2")]
  }, server = TRUE, selection = "single")
  
  output$VaR <-renderPlot({ #generate the VaR plot
    dataInput<-dataInput()[[5]] #refer to the fifth index in the list
    chart.VaRSensitivity(dataInput[,1,drop=FALSE], #use plot from tq_performance package 
                         methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"),#show differences in VaR approach
                         colorset=bluefocus, lwd=2,
                         main = "Value at risk selected stock",
                         ylim = c(-0.2,0))
  })
  output$models_tbl<-renderPlot({ #generate the plot forecast plot
    dataInput<-dataInput()[[6]] #refer to the sixth index in the list
    dataInput %>%
      unnest(sweep) %>% #unnest the data
      rename(
        Data = key)%>% #rename column
      ggplot(aes(x = date, y = adjusted, color = Data, group = Method)) +  #ggplot for forecast
      geom_ribbon(aes(ymin = lo.95,
                      ymax = hi.95),  #confidence level 95
                  fill = "#D5DBFF", color = NA, size = 0) + #color
      geom_ribbon(aes(ymin = lo.80, 
                      ymax = hi.80, 
                      fill = key),#confidence level 80
                  fill = "#596DD5", color = NA, size = 0, alpha = 0.8) + #color
      geom_line(size = 1) +
      #wrap the plots and format the plots
      facet_wrap(~Method, 
                 nrow = 6, 
                 labeller =labeller(Method = c("auto.arima"="Arima model",
                                               "ets"="Exponential Smoothing"))) +
      labs(title = "Stock forecast using using ARIMA and Exponential Smoothing", #titles on the plot and axis
           x = "", 
           y = "Stock price") +
      scale_y_continuous(labels = scales::dollar) +
      theme_tq() +
      scale_color_tq()
    
  })
  # Validate
  output$valid <- renderText({ #Generate the text message
    
    if (nrow(dataInput()[[1]]) > 1) { #If the data is successfully retrieved, the app will return a "Successful" message to the user.
      paste("Data was successfully retrieved from yahoo! finance at", " ", as.character(Sys.time()))
    }
    else { #And it will also notify the user if there either is a problem with Yahoo Finance or a problem with the chosen time interval.
      paste("Application failed to retrieve data from yahoo! finance. 
            Try selecting a different time period.")
    }
  })
  
  # Omitted values
  output$omitted <- renderText({ #Generate the text message
    
    if (nrow(dataInput()[[7]]) > 0) { #notifies the user if there are some omitted values. 
      paste("Note:", "", 
            nrow(dataInput()[[7]]), "", 
            "missing observation(s) omitted for the selected time period.")
    }
    
  })
}


shinyApp(ui,server) # This line of code will launch the app

#Url to app: https://hom1998.shinyapps.io/BAN400-final/
