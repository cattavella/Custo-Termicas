library(rvest)
library(httr)
library(XML)
library(RCurl)
library(stringr)
library(lubridate)
library(dplyr)
library(pdftools)


#Set start and end date to get the data. For the operation week of 2019-10-12 and before,
# I coudn't obtain the information contained in the tables. Maybe before until this date
# the tables were images.
start_date = "2019-10-19"
end_date = "2022-02-24"

#Get the links contained on the html saved manually from ONS webpage
html_file = "./Download_manual/ONS_Programa_PMO.html"
arq_html <- read_html(html_file)

all_links <- data.frame(arq_html %>% 
  html_nodes(css = "body") %>% 
  html_nodes(css = "table") %>% 
  html_nodes(css = "a") %>% 
  html_attr("href"))
colnames(all_links) <- c("link")

links_PMO <-all_links  %>% filter(str_detect(link, 'Informe_PMO'))
links_PMO <-links_PMO  %>% filter(!str_detect(link, 'download'))

#Extract the information from the tables for all ONS files during the desired period
for(k in 1:length(links_PMO[[1]])){
  ons_url <- paste0("http://www.ons.org.br",links_PMO[k,1])
  
  pdf_file <- ons_url
  txt_pdf_completo <- pdf_text(pdf_file)
  print(paste("Link ONS: ",links_PMO[k,1], "k: ", k))
  
  #Get the start of week operation
  txt_header <- txt_pdf_completo[1]
  pos <- unlist(gregexpr("Semana Operativa", txt_header))
  data_sem_op = paste(substr(txt_header, pos+26, pos+29)
                      , substr(txt_header, pos+23, pos+24)
                      , substr(txt_header, pos+20, pos+21)
                      , sep='-')
  
  if (data_sem_op < start_date){
    break 
  }
  
  # This code is just to organize ehr information obtained from pdf file
  # In 14/03/2020 and before the table from SE/CO and south were in page 5
  if (data_sem_op != "2020-03-14"){
    txt_tabela <- txt_pdf_completo[4]
  } else {
    txt_tabela <- txt_pdf_completo[5]
  }
  
  pdf_table <- str_split(txt_tabela, "\n", simplify = TRUE)
  
  if(data_sem_op > "2020-06-13") {
    table_start <- stringr::str_which(pdf_table, "REGIÃO SUDESTE/CENTRO-OESTE")
    table_end <- stringr::str_which(pdf_table, "REGIÃO SUL")
  } else if(data_sem_op != "2020-03-14") {
    table_start <- stringr::str_which(pdf_table, "EGIÃO SE/CO")
    table_end <- stringr::str_which(pdf_table, "REGIÃO SUL")
  } else {
    table_start <- stringr::str_which(pdf_table, "R EGI ÃO S E/ C O")
    table_end <- stringr::str_which(pdf_table, "R EGI ÃO S U L")
  }
  
  
  # Remove colums with non-desired information
  pdf_lines <- pdf_table[1, (table_start + 2):(table_end - 1)]
  pdf_lines <- pdf_lines[-c(1:4,(length(pdf_lines)-2):length(pdf_lines))]
  
  #Inserting a separator char maker easier to find the elements
  pdf_lines <- str_replace_all(pdf_lines, "\\s{2,}", "&")
  
  # Inserting the first element and configuring the table
  cells_line <- strsplit(pdf_lines[1], split = "&")
  cells_line <- unlist(cells_line) 
  
  nome_usina = cells_line[2]
  cvu_usina = str_replace(cells_line[4],",",".")
  
  tb_cvu <- data.frame(nome_usina,as.numeric(cvu_usina))
  cvu_date = data_sem_op
  colnames(tb_cvu) <- c("Usina",cvu_date)
  
  # Insert the other elements from the SE/CO table
  for(i in 2:length(pdf_lines)){

    #print(paste0("SE/CO: ", pdf_lines))
    
    cells_line <- strsplit(pdf_lines[i], split = "&")
    cells_line <- unlist(cells_line)
    #print(cells_line)
    
    # This if was necessary because in some cases the first element of the array is not empty
    if (!is.na(nchar(cells_line[1]))){
      if (nchar(cells_line[1]) == 0){
        nome_usina = cells_line[2]
        cvu_usina = str_replace(cells_line[4],",",".")
      } else {
        nome_usina = str_replace(cells_line[1]," ","")
        cvu_usina = str_replace(cells_line[3],",",".")
      }
      
      tb_cvu <- rbind(tb_cvu, c(nome_usina,as.numeric(cvu_usina)))
    }
    
  }
  
  
  # Now the table for the south region
  # Don't need to change the pdf_table variable because this table is on the same page
  if(data_sem_op != "2020-03-14") {
    table_start <- stringr::str_which(pdf_table, "REGIÃO SUL")
  } else {
    table_start <- stringr::str_which(pdf_table, "R EGI ÃO S U L")
  }
  table_end <- stringr::str_which(pdf_table, "Este informativo é publicado após a etapa")
  
  # Remove colums with non-desired information
  pdf_lines <- pdf_table[1, (table_start + 2):(table_end - 1)]
  pdf_lines <- pdf_lines[-c(1:4,(length(pdf_lines)-2):length(pdf_lines))]
  
  #Inserting a separator char maker easier to find the elements
  pdf_lines <- str_replace_all(pdf_lines, "\\s{2,}", "&")
  
  # Finding the end of the information required. The last text is not in all files
  if(data_sem_op != "2020-03-14") {
    line_total <- stringr::str_which(pdf_lines, "TOTAL SUL")
  } else {
    line_total <- stringr::str_which(pdf_lines, "T OT A L S U L")
  }
  
  pdf_lines <- pdf_lines[-c((line_total-1):length(pdf_lines))]
  
  # Insert the other elements from the South table
  for(i in 1:length(pdf_lines)){
    #print(paste0("Sul: ", pdf_lines))
    
    cells_line <- strsplit(pdf_lines[i], split = "&")
    cells_line <- unlist(cells_line)
    #print(cells_line)
    
    if (!is.na(nchar(cells_line[1]))){
      if (nchar(cells_line[1]) == 0){
        nome_usina = cells_line[2]
        cvu_usina = str_replace(cells_line[4],",",".")
      } else {
        nome_usina = str_replace(cells_line[1]," ","")
        cvu_usina = str_replace(cells_line[3],",",".")
      }
      
      tb_cvu <- rbind(tb_cvu, c(nome_usina,as.numeric(cvu_usina)))
    }
    
  }
  
  
  # Now for the northeast region
  # Necessary because this table is in another page
  if (data_sem_op != "2020-03-14"){
    txt_tabela <- txt_pdf_completo[5]
  } else {
    txt_tabela <- txt_pdf_completo[4]
  }
  
  # This code is just to organize ehr information obtained from pdf file
  pdf_table <- str_split(txt_tabela, "\n", simplify = TRUE)
  
  if(data_sem_op != "2020-03-14") {
    table_start <- stringr::str_which(pdf_table, "REGIÃO NORDESTE")
    table_end <- stringr::str_which(pdf_table, "REGIÃO NORTE")
  } else {
    table_start <- stringr::str_which(pdf_table, "R EGI ÃO N OR D ES T E")
    table_end <- stringr::str_which(pdf_table, "R EGI ÃO N OR T E")
  }
  
  # Remove colums with non-desired information
  # One less column than 
  pdf_lines <- pdf_table[1, (table_start + 2):(table_end - 1)]
  pdf_lines <- pdf_lines[-c(1:3,(length(pdf_lines)-2):length(pdf_lines))]
  
  #Inserting a separator char maker easier to find the elements
  pdf_lines <- str_replace_all(pdf_lines, "\\s{2,}", "&")
  
  # Insert the other elements from the Northeast table
  for(i in 2:length(pdf_lines)){
    
    #print(paste0("Nordeste: ", pdf_lines))
    
    cells_line <- strsplit(pdf_lines[i], split = "&")
    cells_line <- unlist(cells_line)
    #print(cells_line)
    
    # This if was necessary because in some cases the first element of the array is not empty
    if (!is.na(nchar(cells_line[1]))){
      if (nchar(cells_line[1]) == 0){
        nome_usina = cells_line[2]
        cvu_usina = str_replace(cells_line[4],",",".")
      } else {
        nome_usina = str_replace(cells_line[1]," ","")
        cvu_usina = str_replace(cells_line[3],",",".")
      }
      
      tb_cvu <- rbind(tb_cvu, c(nome_usina,as.numeric(cvu_usina)))
    }
    
  }
  
  # The last table, for north region
  # Don't need to change the pdf_table variable because this table is on the same page
  if(data_sem_op != "2020-03-14") {
    table_start <- stringr::str_which(pdf_table, "REGIÃO NORTE")
  } else {
    table_start <- stringr::str_which(pdf_table, "R EGI ÃO N OR T E")
  }
  table_end <- stringr::str_which(pdf_table, "Este informativo é publicado após a etapa")
  
  # Remove colums with non-desired information
  pdf_lines <- pdf_table[1, (table_start + 2):(table_end - 1)]
  pdf_lines <- pdf_lines[-c(1:4,(length(pdf_lines)-2):length(pdf_lines))]
  
  #Inserting a separator char maker easier to find the elements
  pdf_lines <- str_replace_all(pdf_lines, "\\s{2,}", "&")
  
  # Finding the end of the information required. The last text is not in all files
  if(data_sem_op != "2020-03-14") {
    line_total <- stringr::str_which(pdf_lines, "TAL NORTE")
  } else {
    line_total <- stringr::str_which(pdf_lines, "T OT A L N OR T E")
  }
  pdf_lines <- pdf_lines[-c((line_total-1):length(pdf_lines))]
  
  # Insert the other elements from the North table
  for(i in 1:length(pdf_lines)){
    
    #print(paste0("Norte: ", pdf_lines))
    cells_line <- strsplit(pdf_lines[i], split = "&")
    cells_line <- unlist(cells_line)
    #print(cells_line)
    
    if (!is.na(nchar(cells_line[1]))){
      if (nchar(cells_line[1]) == 0){
        nome_usina = cells_line[2]
        cvu_usina = str_replace(cells_line[4],",",".")
      } else {
        nome_usina = str_replace(cells_line[1]," ","")
        cvu_usina = str_replace(cells_line[3],",",".")
      }
      
      tb_cvu <- rbind(tb_cvu, c(nome_usina,as.numeric(cvu_usina)))
    }
    
  }
  
  #Create the complete table if it is the first element
  if(k==1){
    tb_cvu_completa <- tb_cvu
  } else {
    #tb_cvu_completa <- cbind(tb_cvu_completa, tb_cvu[,2])
    #colnames(tb_cvu_completa)[ncol(tb_cvu_completa)]  = colnames(tb_cvu[2])
    tb_cvu_completa <- merge(tb_cvu_completa, tb_cvu, by="Usina", all=TRUE)  # merge by row names (by=0 or by="row.names")
    
  }
}

write.csv(tb_cvu_completa,"./Ger_termica_PMO.csv", row.names = FALSE)
