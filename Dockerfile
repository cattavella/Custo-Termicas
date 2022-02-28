
FROM rocker/tidyverse

WORKDIR "./Custo-Termicas"

RUN R -e "install.packages('httr')"
RUN R -e "install.packages('XML')"
RUN R -e "install.packages('RCurl')"
RUN R -e "install.packages('rlist')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('gridExtra')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('rvest')"
RUN R -e "install.packages('pdftools')"
RUN R -e "install.packages('hablar')"
RUN R -e "install.packages('tidyverse')"

COPY "./Brent.csv"						"./Brent.csv"
COPY "./custo_diario_termicas.csv"		"./custo_diario_termicas.csv"
COPY "./custo_diario_total.csv"			"./custo_diario_total.csv"
COPY "./cvu_diario_pmo.csv"				"./cvu_diario_pmo.csv"
COPY "./Ger_termica_diaria.csv"			"./Ger_termica_diaria.csv"
COPY "./Ger_termica_PMO.csv"			"./Ger_termica_PMO.csv"
COPY "./Ger_termica_PMO_limpa.csv"		"./Ger_termica_PMO_limpa.csv"
COPY "./geracao_diario_total.csv"		"./geracao_diario_total.csv"
COPY "./peso_ger_termica_diaria.csv"	"./peso_ger_termica_diaria.csv"
COPY "./getCVU.R"						"./getCVU.R"
COPY "./peso_ger_termica_diaria.csv"	"./peso_ger_termica_diaria.csv"

COPY "./CalculaCustoPonderado.R"		"./CalculaCustoPonderado.R"
COPY "./getCVU.R"						"./getCVU.R"
COPY "./getDataONS.R"					"./getDataONS.R"
COPY "./petroleum_prices_brl.R"			"./petroleum_prices_brl.R"
COPY "./GeraGraficos.R"					"./GeraGraficos.R"

CMD Rscript "./GeraGraficos.R"


