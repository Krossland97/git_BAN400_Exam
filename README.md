Termpaper BAN400R


Introduction:

In this project we wanted to create an interactive application for simple stock analysis using the Shiny Package.
Originally, our aim was to let the user select one of the listed stocks on OSEBX and compare it with either a benchmark or another stock, 
where the user determines the time interval himself. The application will then provide useful information such as development in stock price, 
moving averages, VaR and daily returns. 


About Shiny:

The shiny-package is an package in R that makes it easy ro build interactive web apps in R. 
Shiny lets the user create standalone apps on a webpage, or the apps could be integrated in R Markdown-documents or even dashboards. 
The main three components of a Shiny-app are a User Interface (UI) object, a Server function and a call to the Shiny-app function. 
The UI object lets the user design both the layout and design of the application. 
The instructions the running computer requires to build the application are found in the server function. 
Finally the call will create and run the Shiny app from the UI and Server pair.


Execution: 

Originally the plan was to use the OSEBX-Index, but several problems arose when Euronext took over the Oslo Stock Exchange.
As a result of the acquistition 200 tickers were changed, and Yahoo Finance treated the ticker-changes as new listings. 
This meant that we only had data from December 2. for the stocks that got a new ticker name.
We therefore did not have sufficient data to be able to perform the various analyzes that we had included in the application.
We decided to switch to US stocks, specifically the 30 that are in the Dow Jones index. 
This meant that we could use the dataframe of the Dow Jones which can be found in the Tidyquant-package. 
This method however, didn't require any scraping from HTML-tables, but we decided to include the original scraping-code in this ReadMe, 
as it will work again when Yahoo Finance has fixed so that the updated ticker names are not considered as new listings.


Problems with scraping:

Our main issue in this project was the scraping part of the code.
In this phase, there was a lot of uncertainty as to which tickers would actually be used by Yahoo Finance when we submitted our project.
In addition we also did not know which tickers would be in the HTML tables we scraped from.
Although there was a lot of uncertainty, we agreed that all this is just a kind of transitional phase. After a while, Yahoo Finance will most likely overlap the old and new ticker names.
As we did not dare to speculate in this, we decided to submit a script that we were absolutely sure would work by the submission date.



Below is the original code where we scraped an HTML table from a Nasdaq-site containing the tickers from OSEBX. 
------------------------------------------------------------------------------------------------------------------------------------
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

library(xml2)
library(rvest)

url <- "http://www.nasdaqomxnordic.com/shares/listed-companies/norwegian-listed-shares" #url for where the ticker data is colleted 

raw <- read_html(url)

OSEBX_indeksen <- as_tibble(html_table(raw)[[1]]) %>%
  mutate(Symbol = substr(Symbol, 1, nchar(Symbol)-1))

#Create vector with all the tickers. Adding ".OL". Without .OL code will not work with Yahoo  
OSEBX_tickers <- paste0(OSEBX_indeksen[["Symbol"]], ".OL") 
OSEBX_tickers2 <- c(OSEBX_tickers)

#Add names to the charachers in the vector
names(OSEBX_tickers) =  paste0(OSEBX_indeksen[["Name"]]) 
