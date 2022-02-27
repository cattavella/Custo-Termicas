library(rvest)
library(httr)
library(XML)
library(RCurl)
library(stringr)
library(lubridate)
library(dplyr)
library(pdftools)
library(tidyverse)
library(hablar)

end_date <- "2022-02-24"


tbl_cvu <- read.csv("./Ger_termica_PMO_limpa.csv", sep=";")
tbl_corresp <- read.csv("./Corresp_nome_mod.csv", sep=";")

colnames(tbl_corresp)[2]  = "Usina"

tbl_cvu <- merge(tbl_corresp, tbl_cvu, by="Usina", all=FALSE)
ncols_cvu = ncol(tbl_cvu)


tbl_cvu_diario <- data.frame(tbl_cvu[,"Nome_boletim_diario"])
tbl_cvu_diario <- cbind(tbl_cvu_diario, tbl_cvu[,ncols_cvu])
ncols_cvu_d = ncol(tbl_cvu_diario)
old_col_name <- colnames(tbl_cvu)[ncols_cvu]
new_col_name <- paste0(str_sub(old_col_name, nchar(old_col_name)-3, nchar(old_col_name)),
                       "-",str_sub(old_col_name,nchar(old_col_name)-6, nchar(old_col_name)-5),
                       "-",str_sub(old_col_name,nchar(old_col_name)-9, nchar(old_col_name)-8))
colnames(tbl_cvu_diario)[2]  = new_col_name

next_old_col_name <- colnames(tbl_cvu)[ncols_cvu-1]
next_new_col_name <- paste0(str_sub(next_old_col_name, nchar(next_old_col_name)-3, nchar(next_old_col_name)),
                       "-",str_sub(next_old_col_name,nchar(next_old_col_name)-6, nchar(next_old_col_name)-5),
                       "-",str_sub(next_old_col_name,nchar(next_old_col_name)-9, nchar(next_old_col_name)-8))
kmax = as.numeric(difftime(as.Date(next_new_col_name), as.Date(new_col_name), units = "days")) - 1

for(k in 1:kmax){
  tbl_cvu_diario <- cbind(tbl_cvu_diario, tbl_cvu[,ncols_cvu])
  old_col_name <- colnames(tbl_cvu)[ncols_cvu]
  new_col_name <- paste0(str_sub(old_col_name, nchar(old_col_name)-3, nchar(old_col_name)),
                         "-",str_sub(old_col_name,nchar(old_col_name)-6, nchar(old_col_name)-5),
                         "-",str_sub(old_col_name,nchar(old_col_name)-9, nchar(old_col_name)-8))
  new_col_name <- toString(as.Date(new_col_name) + k)
  colnames(tbl_cvu_diario)[ncol(tbl_cvu_diario)]  = new_col_name
}

for (i in 1:(ncols_cvu-4)){
  print(paste0("i: ", i))
  j = ncols_cvu-i
  tbl_cvu_diario <- cbind(tbl_cvu_diario, tbl_cvu[,j])
  old_col_name <- colnames(tbl_cvu)[j]
  new_col_name <- paste0(str_sub(old_col_name, nchar(old_col_name)-3, nchar(old_col_name)),
                         "-",str_sub(old_col_name,nchar(old_col_name)-6, nchar(old_col_name)-5),
                         "-",str_sub(old_col_name,nchar(old_col_name)-9, nchar(old_col_name)-8))
  colnames(tbl_cvu_diario)[ncol(tbl_cvu_diario)]  = new_col_name
  
  next_old_col_name <- colnames(tbl_cvu)[ncols_cvu-i-1]
  next_new_col_name <- paste0(str_sub(next_old_col_name, nchar(next_old_col_name)-3, nchar(next_old_col_name)),
                              "-",str_sub(next_old_col_name,nchar(next_old_col_name)-6, nchar(next_old_col_name)-5),
                              "-",str_sub(next_old_col_name,nchar(next_old_col_name)-9, nchar(next_old_col_name)-8))

  if(as.numeric(difftime(as.Date(end_date), as.Date(new_col_name), units = "days")) > 7){
    kmax = as.numeric(difftime(as.Date(next_new_col_name), as.Date(new_col_name), units = "days")) - 1
  } else if(as.numeric(difftime(as.Date(end_date), as.Date(new_col_name), units = "days")) > 0) {
    kmax = as.numeric(difftime(as.Date(end_date), as.Date(new_col_name), units = "days"))
  }
  else {
    break
  }
  
  # Replicate the columns for the resto of the week 
  for(k in 1:kmax){
    tbl_cvu_diario <- cbind(tbl_cvu_diario, tbl_cvu[,j])
    old_col_name <- colnames(tbl_cvu)[j]
    new_col_name <- paste0(str_sub(old_col_name, nchar(old_col_name)-3, nchar(old_col_name)),
                             "-",str_sub(old_col_name,nchar(old_col_name)-6, nchar(old_col_name)-5),
                             "-",str_sub(old_col_name,nchar(old_col_name)-9, nchar(old_col_name)-8))
    new_col_name = toString(as.Date(new_col_name) + k)
    colnames(tbl_cvu_diario)[ncol(tbl_cvu_diario)]  = new_col_name
  }
  
}

colnames(tbl_cvu_diario)[1]  = "Usina"
start_date <- colnames(tbl_cvu_diario)[2]

tbl_ger_termica_diaria <- read.csv("./Ger_termica_diaria.csv", sep=";")

n <- 0
for (i in 2:ncol(tbl_ger_termica_diaria)){
  old_col_name <- colnames(tbl_ger_termica_diaria)[i]
  new_col_name <- paste0(str_sub(old_col_name, nchar(old_col_name)-3, nchar(old_col_name)),
                         "-",str_sub(old_col_name,nchar(old_col_name)-6, nchar(old_col_name)-5),
                         "-",str_sub(old_col_name,nchar(old_col_name)-9, nchar(old_col_name)-8))
  colnames(tbl_ger_termica_diaria)[i]  = new_col_name
  
  # Compare with the first information there's data for the CVU
  if (new_col_name < start_date){
    n <- n + 1
  }
}

tbl_ger_termica_diaria <- tbl_ger_termica_diaria[,-c(2:(n+1))]
total_geracao_diaria <- summarise_all(tbl_ger_termica_diaria[,-1], sum_)

tbl_peso_ger_termica <- tbl_ger_termica_diaria
# Calculate the weigths
for(i in 1:nrow(tbl_ger_termica_diaria)){
  for(j in 2:ncol(tbl_ger_termica_diaria)){
    tbl_peso_ger_termica[i,j] <- tbl_ger_termica_diaria[i,j]/total_geracao_diaria[j-1]
  }
}

tbl_custo_termicas_diario <- tbl_ger_termica_diaria
for(i in 1:nrow(tbl_ger_termica_diaria)){
  print(paste("i: ", i))
  for(j in 2:ncol(tbl_custo_termicas_diario)){
    col_index <- which(colnames(tbl_cvu_diario) == colnames(tbl_ger_termica_diaria)[j]) 
    row_index <- which((tbl_cvu_diario[,1]) == tbl_ger_termica_diaria[i,1]) 
    if(length(col_index) == 0 | length(row_index) == 0){
      tbl_custo_termicas_diario[i,j] <- NA
    } else {
      tbl_custo_termicas_diario[i,j] <- mean(tbl_peso_ger_termica[i,j])*mean(tbl_cvu_diario[row_index,col_index])
    }
  }
}

custo_total_diario <- summarise_all(tbl_custo_termicas_diario[,-1], sum_)

write.csv(tbl_peso_ger_termica,"./peso_ger_termica_diaria.csv")
write.csv(tbl_cvu_diario,"./cvu_diario_pmo.csv")
write.csv(tbl_custo_termicas_diario,"./custo_diario_termicas.csv")
write.csv(custo_total_diario,"./custo_diario_total.csv")
write.csv(total_geracao_diaria,"./geracao_diario_total.csv")

