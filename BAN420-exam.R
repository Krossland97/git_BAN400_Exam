#Script for the shiny app

## Some more work for Github

#Install needed packages----------------------------

#install.packages("shiny")
#install.packages("htmltab")
#install.packages("tidyverse")
#install.packages("tidyquant")
#install.packages("ggplot2")
#install.packages("knitr")
#install.packages("shinydashboard")



#Collect the needed packages from library---------------------------------

library(shiny)
library(htmltab)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(knitr)
library(shinydashboard)

#Sources---------------------------------

#Source1: https://www.r-bloggers.com/r-shiny-stock-analysis/?fbclid=IwAR0mEk_oSN-Dpy_MXI4tgEfBCfA7fR_W3ZJ0BQnUXoH5pJNFpwGev8S_n-g
#Source2:https://github.com/business-science/tidyquant
#Source3: https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/


#Main script

# Data ----------------------------------------------------------------------
#Prepeare the list of tickernames and benchmark

tabledata.url <- "https://no.wikipedia.org/wiki/OSEBX-indeksen" #url for where the ticker data is colleted 

#Create HTML tables of the tables from the tables in the url. Save as dataframe 
OSEBX_indeksen <- htmltab(doc = tabledata.url, #select url 
                          which = 1, #select the table containing the tickers
                          rm_nodata_cols = F) #Do not remove any columns regardsless of content

#Create vector with all the tickers. Adding ".OL". Without .OL code will not work with Yahoo  
OSEBX_indeksen_symbol <- paste0(OSEBX_indeksen[["Ticker"]], ".OL") 

#Remove OSE: from all charachers vector
OSEBX_tickers <-  str_remove_all(OSEBX_indeksen_symbol, "OSE: ") 

#Add names to the charachers in the vector, 
names(OSEBX_tickers) =  paste0(OSEBX_indeksen[["Navn"]]) 

#Repeat the process for the list of benchnames
Bench<-"^OSEAX" #^OSEAX is the name of the ALl-share Index at Yahoo 
names(Bench) = paste0("Oslo Børs All-share Index") #Asign name to the characher ^OSEAX
benchnames <-c(Bench,OSEBX_tickers) #New vector of OSEBX tickers plus ^OSEAX



# UI ----------------------------------------------------------------------
ui<-shinyUI(   #Call the shinyUI function to enable the diffrent features
  
  pageWithSidebar( #sidebar for app input 
    
    headerPanel("BAN420 Stock Explorer"), #Header
    
    sidebarPanel( #Create sidebar to make the app easy to follow
      
      #Prefixed set of stocks from Oslo Stock Exchange saved in the vector
      selectInput("stock_id", "OSEBX Aksjer:", OSEBX_tickers), 
      
      #Date imput, 
      dateRangeInput("dates", 
                     "Date range", #Asign name
                     start = "2019-01-01",#starting date. Can be changed if wanted in the app
                     end = as.character(Sys.Date())), #End date set to the same as system date.

     
      selectInput("bench_id", "Pick a benchmark:", benchnames),  #Benchmark for comparison
      ), #The vector is the same as the OSEBX_tickers, only with OSEAX added 
    
    #Main dashboard body
    dashboardBody( #Activate bodyfunction 
      fluidRow( #Create a fluid row 
        #Create one box for each plot. 
        column(7,box(plotOutput("stock"),width=100)), #Print the charts created in the server function 
#The two last plots will be beside each other. Width difference is of esthetic reason
        column(6,box(plotOutput("comp"),width=15 )), 
        column(6,box(plotOutput("returns"),width=15))  
      )
      )
    )
  )#End of UI

### Server ------------------------------------------------------------------
##Inspiration for this section is the third session of the BAN420 seminar   
server <- function(input, output){ #server function 
  
  dataInput <- reactive({  #reactive means sensitive to user input 
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
    
    stock_price <- input$stock_id %>% #create dataframe with input stock data. Use the input form UI
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
    
    #In order to refer to the right dataframe when drawing the plots 
    #thus, the different dataframes used are listed. 
    list(stocks_return, stock_price, bench_stock)
  })
  
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
      labs(title = "Comparison to benchmark",
           x = "", y = "") +
      theme_classic() +
      theme(legend.title=element_blank()) +
      scale_x_date(expand = c(.14, 0))
  })
}

shinyApp(ui = ui, server = server)

#We have published the app, the URL is pasted underneath 
#appURL: https://hom1998.shinyapps.io/BAN400/
