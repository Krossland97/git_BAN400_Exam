library(xml2)
library(rvest)

url <- "http://www.nasdaqomxnordic.com/shares/listed-companies/norwegian-listed-shares" #url for where the ticker data is colleted 

raw <- read_html(url)

OSEBX_indeksen <- as_tibble(html_table(raw)[[1]]) %>%
  mutate(Symbol = substr(Symbol, 1, nchar(Symbol)-1))

#Create vector with all the tickers. Adding ".OL". Without .OL code will not work with Yahoo  
OSEBX_tickers <- paste0(OSEBX_indeksen[["Symbol"]], ".OL") 
OSEBX_tickers2 <- c(OSEBX_tickers)

#Add names to the charachers in the vector, 
names(OSEBX_tickers) =  paste0(OSEBX_indeksen[["Name"]]) 
