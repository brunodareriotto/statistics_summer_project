#pacotes#
library("basedosdados")
library("dplyr")
library("geobr")
library("ggplot2")
library("viridis")
library("readxl")
library("tm")
library("showtext")
library("grid")
library("gridExtra")
library("ggrepel")
library("readr")



##note: export pdfs 5x7, landscape##
font_add_google("Merriweather", family = "Merriweather")
showtext_auto()
texto_azul <- element_text(color = "#201c5c", family = "Merriweather")



#dados_nacionais_rais --- evolucao de rendas no brasil por nacionalidade#
setwd("~/statistics_summer_project")
basedosdados::set_billing_id("projeto-do-bruno-dare")

query_rais <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, sigla_uf, valor_remuneracao_media, nacionalidade, tipo_vinculo) %>%
  filter(ano > 1985) %>% 
  group_by(nacionalidade, ano) %>%
  summarise(remuneracao_media=mean(valor_remuneracao_media, na.rm = TRUE), remuneracao_sd=sd(valor_remuneracao_media))
rais <- bd_collect(query_rais) 

query_dic <- bdplyr("br_me_rais.dicionario")
dic <- bd_collect(query_dic)
dic <- dic %>% filter(nome_coluna == "nacionalidade") %>% rename(nacionalidade = `chave`)

rais_nac <- left_join(rais, dic, by = "nacionalidade")
rais_nac <- rais_nac %>% select(ano, remuneracao_media, remuneracao_sd, valor) %>% rename(nac = `valor`)
rais_nac <- na.omit(rais_nac)

rais_plot <- rais_nac %>% filter(nac == "Brasileira" | nac == "Haitiano" | nac == "Indiano" | nac == "Japonesa" | nac == "Italiana" | nac == "Venezuelano" | nac == "Colombiano")

Nacionalidade <- factor(rais_plot$nac)
ggplot(rais_plot, aes(ano, remuneracao_media)) + geom_line(aes(colour = Nacionalidade)) +
  geom_point(aes(colour = Nacionalidade)) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Remuneração Média Mensal em R$') + 
  xlab('Ano') + ggtitle('Renda Média no Brasil') +
  scale_x_continuous(breaks=seq(1985, 2021, 2)) +
  scale_y_continuous(breaks=seq(750, 13500, 1250))



#dados_nacionais_rais --- evolucao de vinculos no brasil por nacionalidade#
query_rais_vinc <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, nacionalidade, vinculo_ativo_3112) %>%
  filter(ano > 1985) %>% 
  group_by(nacionalidade, ano) %>%
  summarise(vinculos=sum(vinculo_ativo_3112, na.rm = TRUE))
rais_vinc <- bd_collect(query_rais_vinc) 

rais_nac_vinc <- left_join(rais_vinc, dic, by = "nacionalidade")
rais_nac_vinc <- rais_nac_vinc %>% select(ano, vinculos, valor) %>% rename(nac = `valor`)
rais_nac_vinc <- na.omit(rais_nac_vinc)

rais_plot_vinc <- rais_nac_vinc %>% filter(nac == "Haitiano" | nac == "Indiano" | nac == "Japonesa" | nac == "Italiana" | nac == "Venezuelano" | nac == "Colombiano" | nac == "Argentina" | nac == "Portuguesa")

Nacionalidade <- factor(rais_plot_vinc$nac)

ggplot(rais_plot_vinc, aes(ano, vinculos)) + geom_col(aes(fill = Nacionalidade), width = 0.75, alpha = 0.8, size = 2) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Número de Vínculos') + 
  xlab('Ano') + ggtitle('Número de Vínculos Formais no Brasil') + 
  scale_x_continuous(breaks=seq(1985, 2021, 4)) + scale_y_continuous(breaks=seq(0, 125000, 12500))



#dados_por_mun_rj --- concentracao de renda entre brasileiros no rio de janeiro#
query_mun <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, sigla_uf, id_municipio, valor_remuneracao_media, nacionalidade) %>%
  filter(ano == 2018) %>% 
  group_by(nacionalidade, id_municipio) %>%
  summarise(remuneracao_media=mean(valor_remuneracao_media, na.rm = TRUE))
rais_mun <- bd_collect(query_mun) 

rais_nac_mun <- left_join(rais_mun, dic, by = "nacionalidade")
rais_nac_mun <- rais_nac_mun %>% select(remuneracao_media, valor, id_municipio) %>% rename(nac = `valor`)
rais_nac_mun <- na.omit(rais_nac_mun)

rais_mun_plot_br <- rais_nac_mun %>% filter(nac == "Brasileira")

brazil_rio <- read_municipality(code_muni = "RJ", year = 2019)
brazil_rio <- brazil_rio %>% rename(sigla_uf = `abbrev_state`, id_municipio = `code_muni`)
brazil_rio$id_municipio <- as.character(brazil_rio$id_municipio)

brazil_rio_br <- left_join(brazil_rio, rais_mun_plot_br, by = 'id_municipio')
`Remuneração Média em R$` <- brazil_rio_br$remuneracao_media

ggplot() + geom_sf(data = brazil_rio_br, aes(fill = `Remuneração Média em R$`), colour = NA) + labs(subtitle = "", size=8) + scale_color_viridis(discrete = FALSE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = FALSE, option = "G") + theme(text = texto_azul, panel.background = element_rect(fill = "white"), axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather')) +
  ggtitle('Distribuição de Renda no Rio de Janeiro')



#dados_por_mun_sp --- concentracao de renda entre brasileiros em sao paulo#
brazil_sp <- read_municipality(code_muni = "SP", year = 2018)
brazil_sp <- brazil_sp %>% rename(sigla_uf = `abbrev_state`, id_municipio = `code_muni`)
brazil_sp$id_municipio <- as.character(brazil_sp$id_municipio)

brazil_sp_br <- left_join(brazil_sp, rais_mun_plot_br, by = 'id_municipio')
`Remuneração Média em R$` <- brazil_sp_br$remuneracao_media

ggplot() + geom_sf(data = brazil_sp_br, aes(fill = `Remuneração Média em R$`), colour = NA) + labs(subtitle = "", size=8) + scale_color_viridis(discrete = FALSE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = FALSE, option = "G") + theme(text = texto_azul, panel.background = element_rect(fill = "white"), axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather')) +
  ggtitle('Distribuição de Renda em São Paulo')



#dados_do_mun_rj --- renda entre habitantes da cidade do rio de janeiro#
query_rio <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, id_municipio, valor_remuneracao_media, nacionalidade, vinculo_ativo_3112, raca_cor, tipo_vinculo, bairros_rj, quantidade_horas_contratadas) %>%
  filter(ano == 2018, id_municipio == '3304557') 
rais_rio <- bd_collect(query_rio) 

dic_bairros <- read_excel("inputs/RAIS_vinculos_layout.xls", sheet = "BAIRRO_RJ", range = "A2:C177") %>% 
  rename(bairros_rj = `Valor na Fonte`, nomes_bairros_rj = `Descrição`) %>%
  select(bairros_rj, nomes_bairros_rj)

dic_bairros$bairros_rj <- as.character(dic_bairros$bairros_rj)

rais_rio_n <- left_join(rais_rio, dic_bairros, by = "bairros_rj") %>% 
  select(ano, id_municipio, valor_remuneracao_media, nacionalidade, vinculo_ativo_3112, raca_cor, tipo_vinculo, nomes_bairros_rj, quantidade_horas_contratadas)

rais_rio_n <- left_join(rais_rio_n, dic, by = "nacionalidade") %>% 
  select(ano, id_municipio, valor_remuneracao_media, nacionalidade, vinculo_ativo_3112, raca_cor, tipo_vinculo, nomes_bairros_rj, quantidade_horas_contratadas, valor) %>% 
  rename(nac = `valor`)

##por_nacionalidade##

rais_rio_cng <- rais_rio_n %>% filter(nac == "Congolês")
rais_rio_br <- rais_rio_n %>% filter(nac == "Brasileira") 
rais_rio_vnz <- rais_rio_n %>% filter(nac == "Venezuelano") 

rais_rio_plot <- rais_rio_n %>% filter(nac == "Haitiano" | nac == "Indiano" | nac == "Japonesa" | nac == "Italiana" | nac == "Venezuelano" | nac == "Congolês" | nac == "Sul-Africano" | nac == "Portuguesa" | nac == "Chinesa")

cng <- ggplot(rais_rio_cng, aes(valor_remuneracao_media)) +
  geom_histogram(bins=100, size = 2, fill = "#201c5c") +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  xlab('Remuneração em R$ de Congoleses') + ylab('Count')

vnz <- ggplot(rais_rio_vnz, aes(valor_remuneracao_media)) + 
  geom_histogram(bins=100, size = 2, fill = "#201c5c") + 
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  xlab('Remuneração em R$ de Venezuelanos') + ylab('Count')

grid.arrange(cng, vnz, ncol = 2, top = textGrob("Distribuição de Renda no Rio de Janeiro (2018)", gp = gpar(fontfamily = 'Merriweather', fontface = 'bold', col = "#201c5c")))



Nacionalidade <- factor(rais_rio_plot$nac)
ggplot(rais_rio_plot, aes(x = valor_remuneracao_media, fill = Nacionalidade)) + 
  geom_histogram(bins=200, size = 2) + 
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Número de Vínculos') + 
  xlab('Remuneração Média em R$') + ylab('Count') +xlim(0,10000) + ggtitle('Histograma de Renda no Brasil entre Nacionalidades (2018)')


rais_rio_plot_p <- rais_rio_n %>% filter(nac == "Haitiano" | nac == "Italiana" | nac == "Venezuelano" | nac == "Congolês" | nac == "Chinesa")

Nacionalidade <- factor(rais_rio_plot_p$nac)
ggplot(rais_rio_plot_p, aes(x = valor_remuneracao_media, y = quantidade_horas_contratadas, colour = Nacionalidade)) + geom_point(size = 1.25, alpha = 0.8) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
         scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Horas Contratadas') + 
         xlab('Remuneração Média em R$') +xlim(0,40000) + ggtitle('Distribuição entre Horas e Remuneração (2018)')



##brasileiros_por_bairro##
rais_rio_bairros <- rais_rio_n %>% filter(nomes_bairros_rj == "Centro" | nomes_bairros_rj == "Barra da Tijuca" | nomes_bairros_rj == "Botafogo" | nomes_bairros_rj == "Gavea" | nomes_bairros_rj == "Jacarepagua" | nomes_bairros_rj == "Leblon" | nomes_bairros_rj == "Madureira")

Bairros <- factor(rais_rio_bairros$nomes_bairros_rj)
ggplot(rais_rio_bairros, aes(x = valor_remuneracao_media, fill = Bairros)) + 
  geom_histogram(bins=100, size = 2) + 
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Número de Vínculos') + 
  xlab('Remuneração Média em R$') + ylab('Count') +xlim(0,20000) +ggtitle('Histograma de Renda no Rio de Janeiro entre Bairros (2018)')

a <- ggplot(rais_rio_bairros, aes(x = valor_remuneracao_media, y = quantidade_horas_contratadas, colour = Bairros)) + geom_point(size = 1.25, alpha = 0.75) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Horas Contratadas') + 
  xlab('Remun em R$') +xlim(0,40000)

b <- ggplot(rais_rio_bairros, aes(x = valor_remuneracao_media, y = quantidade_horas_contratadas, colour = Bairros)) + geom_point(size = 1.25, alpha = 0.75) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Horas Contratadas') + 
  xlab('Remun em R$') +xlim(40000, 80000)

c <- ggplot(rais_rio_bairros, aes(x = valor_remuneracao_media, y = quantidade_horas_contratadas, colour = Bairros)) + geom_point(size = 1.25, alpha = 0.75) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Horas Contratadas') + 
  xlab('Remun em R$') +xlim(80000, 120000)

d <- ggplot(rais_rio_bairros, aes(x = valor_remuneracao_media, y = quantidade_horas_contratadas, colour = Bairros)) + geom_point(size = 1.25, alpha = 0.75) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Horas Contratadas') + 
  xlab('Remun em R$') +xlim(120000, 160000)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(d)

a <- a + theme(legend.position="none")
b <- b + theme(legend.position="none")
c <- c + theme(legend.position="none")
d <- d + theme(legend.position="none")



grid.arrange(a, b, legend, c, d, ncol = 3, nrow = 2, 
             top = textGrob("Horas x Remuneração", gp = gpar(fontfamily = 'Merriweather', fontface = 'bold', col = "#201c5c")))



##por_raça/cor##
query_dic <- bdplyr("br_me_rais.dicionario")
dic_cor <- bd_collect(query_dic)
dic_cor <- dic_cor %>% filter(nome_coluna == "raca_cor") %>% rename(raca_cor = `chave`)

rais_rio_n <- left_join(rais_rio_n, dic_cor, by = "raca_cor")
rais_rio_n <- rais_rio_n %>% select(ano, id_municipio, valor_remuneracao_media, nacionalidade, vinculo_ativo_3112, raca_cor, tipo_vinculo, nomes_bairros_rj, quantidade_horas_contratadas, nac, valor) %>% rename(cor = `valor`)

rais_rio_cor <- rais_rio_n %>% filter(cor == "Parda" | cor == "Amarela" | cor == "Branca" | cor == "Indígena" | cor == "Preta")

`Raça/Cor` <- factor(rais_rio_cor$cor)
ate_10 <- ggplot(rais_rio_cor, aes(x = valor_remuneracao_media, fill = `Raça/Cor`)) + 
  geom_histogram(size = 2, bins = 70) + 
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Número de Vínculos') + 
  xlab('Remun em R$') + ylab('Count') +xlim(0,10000) 

ate_60 <- ggplot(rais_rio_cor, aes(x = valor_remuneracao_media, fill = `Raça/Cor`)) + 
  geom_histogram(size = 2, bins = 70) + 
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Número de Vínculos') + 
  xlab('Remun em R$') + ylab('Count') +xlim(10000, 60000) 

legend <- get_legend(ate_60)

ate_60 <- ate_60 + theme(legend.position="none")
ate_10 <- ate_10 + theme(legend.position="none")

grid.arrange(ate_10, ate_60, legend, ncol = 3, nrow = 1, 
             top = textGrob("Histograma de Renda no RJ entre Raça/Cor (2018)", gp = gpar(fontfamily = 'Merriweather', fontface = 'bold', col = "#201c5c")), widths=c(3, 3, 1.3))




query_rais <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, sigla_uf, valor_remuneracao_media, raca_cor) %>%
  filter(ano > 1985) %>% 
  group_by(raca_cor, ano) %>%
  summarise(remuneracao_media=mean(valor_remuneracao_media, na.rm = TRUE), remuneracao_sd=sd(valor_remuneracao_media))
rais_cor_evolucao <- bd_collect(query_rais) 
rais_cor_evolucao <- na.omit(rais_cor_evolucao)

rais_cor_evolucao <- left_join(rais_cor_evolucao, dic_cor, by = "raca_cor")
rais_cor_evolucao <- rais_cor_evolucao %>% select(ano, remuneracao_sd, remuneracao_media, valor) %>% rename(cor = `valor`)


rais_rio_plot <- rais_cor_evolucao %>% filter(cor == "Parda" | cor == "Amarela" | cor == "Branca" | cor == "Preta")


`Raça/Cor` <- factor(rais_rio_plot$cor)
ggplot(rais_rio_plot, aes(ano, remuneracao_media)) + geom_line(aes(colour = `Raça/Cor`)) +
  geom_point(aes(colour = `Raça/Cor`)) + 
  geom_errorbar(aes(ymin=remuneracao_media-remuneracao_sd/20, ymax=remuneracao_media+remuneracao_sd/20), width=.2, colour = 'black', alpha = 0.25) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Remuneração Média Mensal em R$') + 
  xlab('Ano') + 
  scale_x_continuous(breaks=seq(2006, 2021, 2)) +
  scale_y_continuous(breaks=seq(750, 3750, 250)) + ggtitle('Renda Média no Brasil')



query_rais <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, sigla_uf, valor_remuneracao_media, raca_cor, tipo_vinculo) %>%
  filter(ano > 1985) %>% 
  group_by(raca_cor, ano, tipo_vinculo) %>%
  summarise(remuneracao_media=mean(valor_remuneracao_media, na.rm = TRUE), remuneracao_sd=sd(valor_remuneracao_media))
rais_cor_evolucao_vinc <- bd_collect(query_rais) 
rais_cor_evolucao_vinc <- na.omit(rais_cor_evolucao_vinc)

query_dic <- bdplyr("br_me_rais.dicionario")
dic_vinc <- bd_collect(query_dic)
dic_vinc <- dic_vinc %>% filter(nome_coluna == "tipo_vinculo") %>% rename(tipo_vinculo = `chave`)

rais_cor_evolucao_vinc <- left_join(rais_cor_evolucao_vinc, dic_vinc, by = "tipo_vinculo")
rais_cor_evolucao_vinc <- left_join(rais_cor_evolucao_vinc, dic_cor, by = "raca_cor")
rais_cor_evolucao_vinc <- rais_cor_evolucao_vinc %>% select(ano, remuneracao_media, remuneracao_sd, valor.x, valor.y) %>% rename(cor = `valor.y`, vinculo = `valor.x`)

firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

rais_cor_evolucao_vinc$vinculo <- firstup(rais_cor_evolucao_vinc$vinculo)
rais_cor_evolucao_vinc$vinculo[rais_cor_evolucao_vinc$vinculo == "Clt u/pj ind"] <-"CLT Urbano/PJ IND"




rais_ev_plot <- rais_cor_evolucao_vinc %>% filter(cor == "Parda" | cor == "Amarela" | cor == "Branca" | cor == "Preta") %>% filter(vinculo == "Avulso" | vinculo == "Diretor" | vinculo == "Temporario" | vinculo == "Clt" | vinculo == "CLT Urbano/PJ IND")

rais_ev_plot_2019 <- rais_ev_plot %>% filter(ano == '2019') 

`Remuneração Média (R$)` <- rais_ev_plot_2019$remuneracao_media
ggplot(rais_ev_plot_2019, aes(x = cor, y = vinculo)) + geom_point(aes(fill = `Remuneração Média (R$)`), size = 5, shape = 23) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = FALSE, option = "G", end = 0.95) + ylab('') + xlab('') +
  scale_fill_viridis(discrete = FALSE) + ggtitle('Renda Média em 2019 Por Grupos')

rais_ev_plot_diretores <- rais_ev_plot %>% filter(vinculo == 'Diretor')

`Cor` <- rais_ev_plot_diretores$cor
ggplot(rais_ev_plot_diretores, aes(ano, remuneracao_media)) + geom_line(aes(colour = Cor)) +
  geom_point(aes(colour = Cor)) + 
  geom_errorbar(aes(ymin=remuneracao_media-remuneracao_sd/20, ymax=remuneracao_media+remuneracao_sd/20), width=.2, colour = 'black', alpha = 0.25) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Remuneração Média Mensal em R$') + 
  xlab('Ano') + 
  scale_x_continuous(breaks=seq(1985, 2021, 2)) +
  scale_y_continuous(breaks=seq(750, 20500, 1250)) + ggtitle('Salário de Diretores')

rais_ev_plot_temp <- rais_ev_plot %>% filter(vinculo == 'Temporario')

`Cor` <- rais_ev_plot_temp$cor
ggplot(rais_ev_plot_temp, aes(ano, remuneracao_media)) + geom_line(aes(colour = Cor)) +
  geom_point(aes(colour = Cor)) +
  geom_errorbar(aes(ymin=remuneracao_media-remuneracao_sd/40, ymax=remuneracao_media+remuneracao_sd/40), width=.2, colour = 'black', alpha = 0.25) +
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Remuneração Média Mensal em R$') + 
  xlab('Ano') + 
  scale_x_continuous(breaks=seq(1985, 2021, 2)) +
  scale_y_continuous(breaks=seq(0, 2000, 200)) + ggtitle('Salário de Temporários')


###diretores###
query_rio <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, id_municipio, valor_remuneracao_media, nacionalidade, vinculo_ativo_3112, raca_cor, tipo_vinculo, quantidade_horas_contratadas) %>%
  filter(ano == 2018, tipo_vinculo == '80') 
rais_diretores <- bd_collect(query_rio) 


rais_diretores <- left_join(rais_diretores, dic_cor, by = "raca_cor")
rais_diretores <- rais_diretores %>% select(ano, id_municipio, valor_remuneracao_media, nacionalidade, vinculo_ativo_3112, quantidade_horas_contratadas, valor) %>% rename(cor = `valor`)

rais_diretores_plot <- rais_diretores %>% filter(cor == "Branca" | cor == "Parda" | cor == "Preta")

`Cor` <- rais_diretores_plot$cor
ggplot(rais_diretores_plot, aes(x = valor_remuneracao_media, fill = `Cor`)) + 
  geom_histogram(bins=100, size = 0.001, identity = 1, alpha = 0.8) + 
  theme(panel.background = element_rect(fill = "#f5f5fb"), axis.line = element_line(color="#f5f5fb", size = 0.25), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather'), axis.title = element_text(color = '#201c5c', family = 'Merriweather'), axis.text = texto_azul, legend.text = texto_azul) +
  scale_color_viridis(discrete = TRUE, option = "G", end = 0.95) +
  scale_fill_viridis(discrete = TRUE, option = "G", end = 0.95) + ylab('Número de Vínculos') + 
  xlab('Remuneração Média em R$') + ylab('Count') +ylim(0,500) + ggtitle('Histograma de Renda no Brasil entre Diretores (2018)')




#gap_salarial_entre_brancos_e_pretos#
query_rais <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, sigla_uf, valor_remuneracao_media, raca_cor) %>%
  filter(ano == 2016) %>% filter(raca_cor == "2" | raca_cor == "4") %>%
  group_by(raca_cor, sigla_uf) %>%
  summarise(remuneracao_media=mean(valor_remuneracao_media, na.rm = TRUE), remuneracao_sd=sd(valor_remuneracao_media))
rais_gap_cor <- bd_collect(query_rais) 
rais_gap_cor <- na.omit(rais_gap_cor)

rais_brancos <- rais_gap_cor %>% filter(raca_cor == "2")
rais_pretos <- rais_gap_cor %>% filter(raca_cor == "4")
rais_gap <- inner_join(rais_brancos, rais_pretos, by = 'sigla_uf') %>% mutate(`Gap Salarial` = (remuneracao_media.x - remuneracao_media.y)/remuneracao_media.x) %>% select(sigla_uf, `Gap Salarial`)

brazil_map <- read_state(code_state = "all", year = 2019)
brazil_map <- brazil_map %>% rename(sigla_uf = `abbrev_state`)
brazil_map$sigla_uf <- as.character(brazil_map$sigla_uf)

rais_gap_plot_br <- left_join(brazil_map, rais_gap, by = 'sigla_uf')

br_points <- sf::st_point_on_surface(rais_gap_plot_br)
br_coords <- as.data.frame(sf::st_coordinates(br_points))
br_coords$name_state <- rais_gap_plot_br$name_state


ggplot() + geom_sf(data = rais_gap_plot_br, aes(fill = `Gap Salarial`), colour = '#f5f5fb', size = 0.15) + scale_color_viridis(discrete = FALSE, option = "G") +
  scale_fill_viridis(discrete = FALSE, option = "G") + theme(text = texto_azul, panel.background = element_rect(fill = "white"), axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather')) +
ggtitle('Gap Racial Por Estado Brasileiro (2016)') + geom_label_repel(data = br_coords, aes(X, Y, label = name_state), colour = "black", family = "Merriweather", fontface = 'bold', size = 1.5, inherit.aes = TRUE)



#gap_salarial_entre_brancos_e_pretos_rj#
query_rais <- bdplyr("br_me_rais.microdados_vinculos") %>%
  select(ano, id_municipio, valor_remuneracao_media, raca_cor, sigla_uf) %>%
  filter(ano == 2016, sigla_uf == 'RJ') %>% filter(raca_cor == "2" | raca_cor == "4") %>%
  group_by(raca_cor, id_municipio) %>%
  summarise(remuneracao_media=mean(valor_remuneracao_media, na.rm = TRUE), remuneracao_sd=sd(valor_remuneracao_media))
rais_gap_cor <- bd_collect(query_rais) 
rais_gap_cor <- na.omit(rais_gap_cor)

rais_brancos <- rais_gap_cor %>% filter(raca_cor == "2")
rais_pretos <- rais_gap_cor %>% filter(raca_cor == "4")
rais_gap <- inner_join(rais_brancos, rais_pretos, by = 'id_municipio') %>% mutate(`Gap Salarial` = (remuneracao_media.x - remuneracao_media.y)/remuneracao_media.x) %>% select(id_municipio, `Gap Salarial`)

brazil_map <- read_municipality(code_muni = "RJ", year = 2019)
brazil_map <- brazil_map %>% rename(id_municipio = `code_muni`)
brazil_map$id_municipio <- as.character(brazil_map$id_municipio)

rais_gap_plot_rio <- left_join(brazil_map, rais_gap, by = 'id_municipio')

br_points <- sf::st_point_on_surface(rais_gap_plot_rio)
br_coords <- as.data.frame(sf::st_coordinates(br_points))
br_coords$name_muni <- rais_gap_plot_rio$name_muni


ggplot() + geom_sf(data = rais_gap_plot_rio, aes(fill = `Gap Salarial`), colour = '#f5f5fb', size = 0.15) + scale_color_viridis(discrete = FALSE, option = "G") +
  scale_fill_viridis(discrete = FALSE, option = "G") + theme(text = texto_azul, panel.background = element_rect(fill = "white"), axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), title = element_text(color = '#201c5c', face = 'bold', family = 'Merriweather')) +
  ggtitle('Gap Racial Por Município Fluminense (2016)') + geom_label_repel(data = br_coords, aes(X, Y, label = name_muni), colour = "black", family = "Merriweather", fontface = 'bold', size = 1.5, inherit.aes = TRUE)



##datasets##
gap_ufs <- rais_gap_plot_br[order(-rais_gap_plot_br$`Gap Salarial`),] %>% as.data.frame() %>% select(sigla_uf, name_state, `Gap Salarial`)
write_csv(gap_ufs, file = 'outputs/gap_ufs.csv')
gap_mun_rj <- rais_gap_plot_rio[order(-rais_gap_plot_rio$`Gap Salarial`),] %>% as.data.frame() %>% select(id_municipio, name_muni, `Gap Salarial`)
write_csv(gap_mun_rj, file = 'outputs/gap_mun_rj.csv')






