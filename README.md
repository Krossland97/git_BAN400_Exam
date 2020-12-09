#BAN400
Termpaper BAN400R


Litt om problemet vårt:


Litt om shiny:
Utførelsen:


Hva er det hvilke problemer møtte vi på:

Problems with scrapping:
Problemet er den overgangsfasen:
I denne fasen vil det være veldig stor usikkerhet på hvilke data som er tilgjengelig fra hvilke tabeller.
Vi vil da ikke spekulere i dette, og heller levere noe trygt som vi vet virker. 



In this project we wanted to create an interactive application for simple stock analysis using the Shiny Package.
Our aim was to let the user select one of the 69 listed stocks on OSEBX and compare it with either a benchmark or another stock, 
where the user determines the time interval himself. 
The application will then provide useful information such as development in stock price, moving averages, VaR and daily returns.
 

Originally the plan was to use the OSEBX-Index, but several problems arose when Euronext took over the Oslo Stock Exchange.
Over 200 tickers were changed, and Yahoo Finance treated the ticker-changes as new listings. 
This meant that we only had data from December 2. for the stocks that got a new ticker name.
We therefore did not have sufficient data to be able to perform the various analyzes that we had included in the application.
We decided to switch to US stocks, specifically the 30 that are in the Dow Jones index. This meant that we could use the dataframe 
of the Dow Jones which can be found in the Tidyquant-package. This method didn't require any scraping from HTML-tables,
but we decided to include the original scraping-code, as it will work again when Yahoo Finance has fixed so that the updated ticker 
names are not considered as new listings. Furthermore, Wikipedia must also update its page with the new ticker names to make our
original code to work, as it currently only returns data older than November 30.

Below is the original code where we scraped an HTML table from Wikipedia containing the tickers from OSEBX. 
------------------------------------------------------------------------------------------------------------------------------------

tabledata.url <- "https://no.wikipedia.org/wiki/OSEBX-indeksen" #url for where the ticker data is collected 

#Create HTML tables of the tables from the tables in the url. Save as dataframe 
OSEBX_indeksen <- htmltab(doc = tabledata.url, #select url 
                          which = 1, #select the table containing the tickers
                          rm_nodata_cols = F) #Do not remove any columns regardless of content

#Create vector with all the tickers. Adding ".OL". Without .OL code will not work with Yahoo  
OSEBX_indeksen_symbol <- paste0(OSEBX_indeksen[["Ticker"]], ".OL") 

#Remove OSE: from all characters vector
OSEBX_tickers <-  str_remove_all(OSEBX_indeksen_symbol, "OSE: ") 
OSEBX_tickers2<-c(OSEBX_tickers,"^OSEAX")

#Add names to the characters in the vector, 
names(OSEBX_tickers) =  paste0(OSEBX_indeksen[["Navn"]]) 

#Repeat the process for the list of benchnames
Bench<-"^OSEAX" #^OSEAX is the name of the ALl-share Index at Yahoo 
names(Bench) = paste0("Oslo Børs All-Share Index") #Assign name to the character ^OSEAX
benchnames <-c(Bench,OSEBX_tickers) #New vector of OSEBX tickers plus ^OSEAX
