library(httr)
library(XML)
library(RCurl)
library(rlist)
library(stringr)
library(lubridate)
library(dplyr)

#Set start and end date to get the data
start_date = "2019-01-01"
end_date = "2022-02-24"

#Create the tables to insert the data

ons_url <- paste0("http://sdro.ons.org.br/SDRO/DIARIO/"
                  ,str_replace_all(start_date,"-","_")
                  ,"/HTML/09_ProducaoTermicaUsina.html")
webpage_ons <- getURL(ons_url)
webpage_ons <- readLines(tc <- textConnection(webpage_ons)); close(tc)
pagetree <- htmlTreeParse(webpage_ons, error=function(...){}, useInternalNodes = TRUE)

# Extract table
results <- xpathSApply(pagetree, "//*/table[@id='grd_geracaoTermica']", xmlValue)

tables <- readHTMLTable(webpage_ons)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

tb_ger_termica <- tables[[which.max(n.rows)]]
colnames(tb_ger_termica) <- tb_ger_termica[1,]
#rownames(tb_ger_termica) <- tb_ger_termica[,1]
tb_ger_termica = tb_ger_termica[-1,]

# There's one table for the programmed information, another for verified and the 
# last one is the deviation. We will use the verified information
tb_verif <- data.frame(tb_ger_termica[,c(1,3)])
#rownames(tb_verif) <- tb_ger_termica[,1]
colnames(tb_verif) <- c("Usina",start_date)

# For of the following dates, get the information on the ONS website and insert in the 
# corresponding dataframe
for(i in 1:difftime(end_date, start_date, "days")){
  #for(i in 1:1){
  
  url_date = toString(str_replace_all(as.Date(start_date)+i,"-","_"))
  #url_date = toString(str_replace_all(as.Date("2022-01-01"),"-","_"))
  ons_url <- paste0("http://sdro.ons.org.br/SDRO/DIARIO/"
                    ,str_replace_all(url_date,"-","_")
                    ,"/HTML/09_ProducaoTermicaUsina.html")
  
  webpage_ons <- getURL(ons_url)
  webpage_ons <- readLines(tc <- textConnection(webpage_ons)); close(tc)
  pagetree <- htmlTreeParse(webpage_ons, error=function(...){}, useInternalNodes = TRUE)
  
  results <- xpathSApply(pagetree, "//*/table[@id='grd_geracaoTermica']", xmlValue)
  
  tables <- readHTMLTable(webpage_ons)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  tb_ger_temp <- tables[[which.max(n.rows)]]
  tb_ger_temp = data.frame(tb_ger_temp[-1,])
  tb_ger_temp = data.frame(tb_ger_temp[,c(1,3)])
  colnames(tb_ger_temp) <- c("Usina",str_replace_all(url_date,"_","-"))
  
  tb_verif <- merge(tb_verif, tb_ger_temp, by="Usina", all=TRUE)  # merge by row names (by=0 or by="row.names")
  
}

write.csv(tb_verif,"./Ger_termica_diaria.csv", row.names = FALSE)


