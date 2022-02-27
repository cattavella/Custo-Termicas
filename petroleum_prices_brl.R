library(httr)
library(XML)
library(RCurl)
library(rlist)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyverse)


files_to_read <- c("./Download_manual/Cambio/cambio_2019S2.csv"
                   , "./Download_manual/Cambio/cambio_2020S1.csv"
                   , "./Download_manual/Cambio/cambio_2020S2.csv"
                   , "./Download_manual/Cambio/cambio_2021S1.csv"
                   , "./Download_manual/Cambio/cambio_2021S2.csv"
                   , "./Download_manual/Cambio/cambio_2022S1.csv")

#Create a table for exchange rate
tb_brl_usd_sem <- read.csv(files_to_read[1],sep=";")
tb_brl_usd_sem <- tb_brl_usd_sem[-1,c(1,3,4)]
colnames(tb_brl_usd_sem) <- c("Data", "Compra", "Venda")


for(i in 1:nrow(tb_brl_usd_sem)){
  tb_brl_usd_sem[i,2] <- as.numeric(tb_brl_usd_sem[i,2])
  tb_brl_usd_sem[i,3] <- as.numeric(tb_brl_usd_sem[i,3])
  tb_brl_usd_sem[i,4] <- (as.numeric(tb_brl_usd_sem[i,2])+as.numeric(tb_brl_usd_sem[i,3]))/2
}

tb_cambio <- tb_brl_usd_sem
                          
for(k in 2:length(files_to_read)){
  tb_brl_usd_sem <- read.csv(files_to_read[k],sep=";")
  tb_brl_usd_sem <- tb_brl_usd_sem[-1,c(1,3,4)]
  colnames(tb_brl_usd_sem) <- c("Data", "Compra", "Venda")
  
  for(i in 1:nrow(tb_brl_usd_sem)){
    tb_brl_usd_sem[i,2] <- as.numeric(tb_brl_usd_sem[i,2])
    tb_brl_usd_sem[i,3] <- as.numeric(tb_brl_usd_sem[i,3])
    tb_brl_usd_sem[i,4] <- (as.numeric(tb_brl_usd_sem[i,2])+as.numeric(tb_brl_usd_sem[i,3]))/2
  }
  
  tb_cambio <- rbind(tb_cambio,tb_brl_usd_sem)
}   

#Create a table for brent
tb_brent_usd <- read.csv("./Brent.csv", sep=";")
colnames(tb_brent_usd) <- c("Data", "brent_usd")


tb_brent_brl <- tb_brent_usd
for(i in 1:nrow(tb_brent_usd)){
    day = tb_brent_brl[i,1]
    row_index <- which(tb_cambio[,1] == day)
    
    if (length(row_index) == 0){
      for(k in 1:20){
        day = tb_brent_brl[i-1,1]
        row_index <- which(tb_cambio[,1] == day)
        if(length(row_index) >0 ){
          break
        }
      }
    }
    
    if (length(row_index) > 0){
      tb_brent_brl[i,2] = as.numeric(tb_brent_brl[i,2])*as.numeric(tb_cambio[row_index,2])
    } else {
      tb_brent_brl[i-1,2] 
    }
}

for(i in 1:nrow(tb_brent_brl)){ 
  if (nchar(tb_brent_brl[i,1]) >= 10){
    new_string <- as.Date(paste0(str_sub(tb_brent_brl[i,1], 7,10),
                         "-",str_sub(tb_brent_brl[i,1], 4,5),
                         "-",str_sub(tb_brent_brl[i,1],1,2)))
    tb_brent_brl[i,1] <- new_string
  } 
}

v_custo_total <- data.frame(read.csv("./custo_diario_total.csv"))

new_name <- data_frame()
for(i in 2:length(colnames(v_custo_total))){
  old_name <- colnames(v_custo_total[i])
  new_name[i-1,1] <- as.Date(paste0(str_sub(old_name, 2,5),
                                    "-",str_sub(old_name, 7,8),
                                    "-",str_sub(old_name,10,11)))
}

tb_custo <- data.frame(new_name)

for (i in 2:nrow(tb_custo)){
  tb_custo[i-1,2] <- v_custo_total[1,i]
}
colnames(tb_custo) = c("Data", "Custo")

tb_grafico_petr <- merge(tb_custo, tb_brent_brl, by="Data", all=FALSE) %>% na.omit()
colnames(tb_grafico_petr) = c("Data", "Custo", "Brent_brl")

p1 <- ggplot(tb_grafico_petr, aes(Data, Custo)) +
  geom_line() +
  ggtitle("Custo de geração térmica ao longo do tempo em R$/MWh") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

p2 <- ggplot(tb_grafico_petr, aes(Data, Brent_brl)) +
  geom_line() +
  ggtitle("Preço do petróleo - Brent em reais") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

grid.arrange(p1, p2, nrow=2, ncol=1)


ggplot(tb_grafico_ger, aes(x=Energia, y=Custo)) + geom_point() +
  labs(y = "CVU (R$/MWh)", x = "Preço do brent em reais") +
  ggtitle("Preço do petróleo X Aumento do custo unitário de geração")





