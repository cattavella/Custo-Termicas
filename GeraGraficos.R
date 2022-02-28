library("tidyverse")
library("dplyr")
library("stringr")
library("ggplot2")
library("grid")
library("gridExtra")

tbl_cvu <- read.csv("./Ger_termica_PMO_limpa.csv", sep=";")


cvu_hoje <- tbl_cvu[,2] %>% na.omit() 
cvu_hoje <- data.frame(sort(cvu_hoje))

cvu_hoje[,2] <- row_number(cvu_hoje)
colnames(cvu_hoje) = c("CVU", "order")

cvu_hoje %>%  
  ggplot( aes(x=order, y=CVU,colour = "CVU")) +
  geom_line(size=1.2) +
  labs(title = "Custo de geração ordenado pelo CVU atual", y = "Custo variável unitário (R$/MWh)", x = "Ordem") +
  theme_bw() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent",colour = NA))
  
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

v_geracao_total <- data.frame(read.csv("./geracao_diario_total.csv"))

new_name <- data_frame()
for(i in 2:length(colnames(v_geracao_total))){
  old_name <- colnames(v_geracao_total[i])
  new_name[i-1,1] <- as.Date(paste0(str_sub(old_name, 2,5),
                          "-",str_sub(old_name, 7,8),
                          "-",str_sub(old_name,10,11)))
}

tb_geracao <- data.frame(new_name)
for (i in 2:nrow(tb_geracao)){
  tb_geracao[i-1,2] <- v_geracao_total[1,i]
}
colnames(tb_geracao) = c("Data", "Energia")

tb_grafico_ger <- merge(tb_geracao, tb_custo, by="Data", all=FALSE) 



tb_grafico_ger %>%  
  ggplot( aes(x=Data, y=Custo,colour = "tb_custo")) +
  geom_line(size=0.6) +
  labs(title = "Custo de geração ordenado pelo CVU atual", y = "Custo variável unitário (R$/MWh)", x = "Ordem") +
  theme_bw() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(legend.position = "none", panel.background = element_rect(fill = "transparent",colour = NA))

p1 <- ggplot(tb_grafico_ger, aes(Data, Custo)) +
  geom_col() +
  labs(y = "Custo variável unitário") +
  ggtitle("Custo de geração térmica ao longo do tempo - R$/MWh") +
  theme(axis.title.x=element_blank())

p2 <- ggplot(tb_grafico_ger, aes(Data, Energia)) +
  geom_col() +
  labs(y = "Energia térmica (MWmed)") +
  ggtitle("Necessidade de geração térmica ao longo do tempo") +
  theme(axis.title.x=element_blank())

grid.arrange(p1, p2, nrow=2, ncol=1)

lmCusto = lm(Custo~Energia, data = tb_grafico_ger)
ggplot(tb_grafico_ger, aes(x=Energia, y=Custo)) + geom_point() +
  labs(y = "CVU (R$/MWh)", x = "Energia térmica (MWmed)") +
  ggtitle("Demanda pela energia térmica X Aumento do custo unitário")
  
  

