install.packages('car')
install.packages('relaimpo')
install.packages("ggExtra")
install.packages("Rmisc")
install.packages("ggplot2")
install.packages("psych")
install.packages('GPArotation')
install.packages('grid')
install.packages('MASS')
install.packages('hrbrthemes')

## ----------------------------------------------------------------------------------- ##
##  Replication files for: Lei é lei? Maurice Duverger e as eleições para o Senado no Brasil
##  Source: https://seer.ufrgs.br/debates/article/view/99047
##  Authors: José Alexandre SJ.; Willber Nascimento; Albany Lima; Widyane Omena
##  License: LGPL v3
## ----------------------------------------------------------------------------------- ##

# load packages

library(foreign)
library(dplyr)
library(ggplot2)
library(agricolae)
library(psych)


library(GPArotation)
library(car)

library(ggExtra)
library(relaimpo)
library(car)
library(readxl)



## load and clean data / Preparação da Base:

Lei <- read_excel("./data/Lei.xlsx")
Lei <- as.data.frame(Lei)

# agregado pelo dplyr

Lei3 <- Lei %>%
  group_by(Ano) %>%
  summarise(Nep = mean(Nep), n_candidatos = mean(Ncand),
            percvotos = mean(PercVot, na.rm = TRUE), RazPerd = mean(RazPerd, na.rm = TRUE))



## Análise Descritiva dos Dados - NEP:

describe.by(Lei$Nep, Lei$Ano)
describe.by(Lei$Ncand, Lei$Ano)
describe.by(Lei$PercVot, Lei$Ano)

# Figura 1 - Distribuição do Número de Partidos (NEP)
# por eleição e por magnitude (1998-2018)

library(grid)

G01 <- ggplot(Lei3, aes(x = Ano, y = Nep ))+ geom_smooth(method = "loess", se = FALSE, color = "black")+
  scale_x_continuous(breaks = seq(1998,2018,4)) + scale_y_continuous(breaks = seq(0,10,0.5)) + theme_light(base_size = 18)+
  xlab('Eleição') + ylab('Número Efetivo de Partidos')


G02<- ggplot(Lei, aes(as.factor(Ano), Nep, fill = factor(Magnitude))) +
  geom_boxplot() + coord_flip() + xlab('Eleição')+
  ylab('Número Efetivo de Partidos')+scale_y_continuous(breaks = seq(0,10,0.5))+theme_light(base_size = 18)+
  theme(legend.position="bottom")+scale_fill_discrete(name="Magnitude",breaks=c("Um Terço", "Dois Terços"),labels=c("Um Terço", "Dois Terços"))+
  scale_fill_manual(values = c("grey45", "grey80"),name="Magnitude",breaks=c("Um Terço", "Dois Terços"),labels=c("Um Terço", "Dois Terços"))


pushViewport(viewport(layout = grid.layout(1, 2)))
print(G01, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(G02, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


# Figura 2 - Distribuição do Número Efetivo de Partidos (NEP) por magnitude
# da eleição (1998-2018)


G03<- ggplot(Lei, aes(x=Nep, fill = Magnitude)) +
  geom_histogram(binwidth=0.5, alpha=.6, position="identity") + theme_light(base_size = 18)+
  xlab ("Número de Partidos Efetivos") + ylab("Frequência")+scale_fill_discrete(name="Magnitude",breaks=c("Um Terço", "Dois Terços"),labels=c("Um Terço", "Dois Terços"))+
  scale_fill_manual(values = c("grey45", "grey80"),name="Magnitude",breaks=c("Um Terço", "Dois Terços"),labels=c("Um Terço", "Dois Trços"))+
  theme(legend.position="bottom")+scale_x_continuous(breaks =seq(0,10,0.5) )+
  geom_vline(aes(xintercept= 4.43), color="grey45", linetype="dashed", size=0.8)+
  geom_vline(aes(xintercept=2.51), color="grey80", linetype="dashed", size=0.8)

describe.by(Lei$Nep, Lei$Magnitude)

G04 <- ggplot(Lei, aes(as.factor(Magnitude), Nep)) +
  geom_boxplot() + xlab('Magnitude')+
  ylab('Número Efetivo de Partidos')+scale_y_continuous(breaks = seq(0,10,0.5))+theme_light(base_size = 18)+
  geom_jitter(color = "grey")

pushViewport(viewport(layout = grid.layout(1, 2)))
print(G03, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(G04, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

# Figura 3 - Diferença entre as médias de NEP por ano e por magnitude
# (1998-2018)

library(Rmisc)

Leia <- summarySE(Lei, measurevar = 'Nep', groupvars = c("Magnitude", "Ano") )

Leia$round <- factor(Leia$Magnitude)
Leia$ano <- factor(Leia$Ano)

G05 <-  ggplot(Leia, aes(x=Ano, y=Nep, group=Magnitude, color=Magnitude)) +
  geom_line(linetype="solid", size=0.8)+scale_color_manual(values=c('grey45','grey80'))+
  geom_errorbar(aes(ymin=Nep-ci, ymax=Nep+ci), size = 0.8, width=.2) +theme_light(base_size = 18)+
  geom_point(size = 2)+theme(legend.position="bottom")+ xlab('Ano')+
  ylab('Número Efetivo de Partidos')+scale_y_continuous(breaks = seq(0,8,0.5))+
  scale_x_continuous(breaks = seq(1998,2018,4))


Leib <- ggplot(Lei, aes(x=Magnitude, y=Nep)) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.09, colour='grey80')

G06 <- Leib + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                           geom="errorbar", color="grey45", width=0.2, size = 1.3) +
  stat_summary(fun.y=mean, geom="point", color="grey45", size =4)+
  theme_bw(base_size = 18) +scale_y_continuous(breaks = seq(0,10,0.5))+
  xlab("Magnitude") +  ylab("N?mero Efetivo de Partido")



pushViewport(viewport(layout = grid.layout(1, 2)))
print(G05, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(G06, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))



# Figura 4 - Percentual de unidade federativas e média do percentual
# de partidos com dois candidatos

Lan <- read_excel("Lan.xlsx")
Lan <- as.data.frame(Lan)

L <- data.frame(
  Eleição = rep(c("2002", "2010", "2018"), 2),
  Variáveis = c(rep("UF (%)", 3), rep("Partido (%)", 3)),
  Number_of_Occurrences = c(33.33, 62.96, 81.48,18.33, 24.11, 16.71)
)


library(reshape)

L.m <- melt(L)


ggplot(L.m, aes(Variáveis, value, fill = Eleição)) + theme_light(base_size = 18)+
  geom_bar(stat="identity", position = "dodge")+ coord_flip()+ylab('Percentual')+
  theme(legend.position="bottom")+ scale_y_continuous(breaks = seq(0,80,5))+
  scale_fill_manual(values = c("grey45", "grey80", "grey25"))


# Figura 5 - Número de Candidatos por eleição e número efetivo de partidos
# (1998-2018)

#Teste de diferença entre médias:

describe.by(Lei$Nep, group = Lei$Magnitude)
leveneTest(Lei$Nep, Lei$Magnitude)
t.test(Lei$Nep ~ Lei$Magnitude, var.equal = T)


Leic <- summarySE(Lei, measurevar = 'Ncand', groupvars = c("Magnitude", "Ano") )


G07 <- ggplot(Leic, aes(x=Ano, y=Ncand, group=Magnitude, color=Magnitude)) +
  geom_line(linetype="solid", size=0.8)+scale_color_manual(values=c('grey45','grey80'))+
  geom_errorbar(aes(ymin=Ncand-ci, ymax=Ncand+ci), size = 0.8, width=.2) +theme_light(base_size = 18)+
  geom_point(size = 2)+theme(legend.position="bottom")+ xlab('Ano')+
  ylab('Número de Candidato')+scale_y_continuous(breaks = seq(1,16,1))+
  scale_x_continuous(breaks = seq(1998,2018,4))



# Teste de diferença entre médias:

describe.by(Lei$Ncand, group = Lei$Magnitude)
leveneTest(Lei$Ncand, Lei$Magnitude)
t.test(Lei$Ncand ~ Lei$Magnitude, var.equal = T)



G08 <- ggplot(Lei, aes(x=Nep, y=Ncand, color=Magnitude, size=Magnitude, group = Magnitude)) +
  geom_point(size = 2.2) + theme_light(base_size = 18)+geom_smooth(method = 'lm', size = 1)+
  theme(legend.position="bottom")+scale_color_manual(values = c("black", "Grey65"))+
  xlab('Número Efetivo de Partidos')+ylab('Número de Candidatos')+scale_x_continuous(breaks = seq(0,32,4))+
  scale_x_continuous(breaks =seq(1,10,0.5))


pushViewport(viewport(layout = grid.layout(1, 2)))
print(G07, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(G08, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


# Correlações:

LeiUmt <- filter(Lei, Magnitude == 'Um Terço')
LeiDoist <- filter(Lei, Magnitude == 'Dois Terços')

corr.test(LeiUmt$Ncand, LeiUmt$Nep, method = 'pearson')
corr.test(LeiDoist$Ncand, LeiDoist$Nep, method = 'pearson')



# Figura 6 - Razão entre o percentual de votos dos candidatos derrotados
# (1998-2018)

G09 <- ggplot(Lei3, aes(x = Ano, y = RazPerd ))+ geom_smooth(method = "loess", se = FALSE, color = "black")+
  scale_x_continuous(breaks =seq(1998,2018,4)) + scale_y_continuous(breaks = seq(1,15,1)) + theme_light(base_size = 18)+
  xlab('Eleição') + ylab('Razão Derrotados')


G10 <- ggplot(Lei, aes(as.factor(Ano), RazPerd, fill = factor(Magnitude))) +
  geom_boxplot() + coord_flip() + xlab('Eleição')+geom_jitter(color = "grey55")+
  ylab('Razão Derrotados')+scale_y_continuous(breaks = seq(0,55,5))+theme_light(base_size = 18)+
  theme(legend.position="bottom")+scale_fill_discrete(name="Magnitude",breaks=c("Um Terço", "Dois Terços"),labels=c("Um Terço", "Dois Ter?os"))+
  scale_fill_manual(values = c("grey45", "grey80"),name="Magnitude",breaks=c("Um Terço", "Dois Terços"),labels=c("Um Terço", "Dois Ter?os"))


pushViewport(viewport(layout = grid.layout(1, 2)))
print(G09, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(G10, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))




# Gráficos de Barra - Partidos no Equilíbrio (Princiais Competidores)

part <- read_excel("Part.xlsx")
part <- as.data.frame(part)

library(forcats)

filter(part, ANO_ELEICAO == 1998)%>%
  ggplot(aes(x=fct_reorder(SIGLA_PARTIDO, perc54), y = perc54))+
  geom_col(fill = "black", width = 0.5) +
  labs(title = '1998',
       x = "Partidos",
       y = "Frequência (%)")+scale_y_continuous(breaks = seq(0,30,5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+theme_minimal()


filter(part, ANO_ELEICAO == 2002)%>%
  ggplot(aes(x=fct_reorder(SIGLA_PARTIDO, perc54), y = perc54))+
  geom_col(fill = "black", width = 0.5) +
  labs(title = '2002',
       x = "Partidos",
       y = "Frequência (%)")+scale_y_continuous(breaks = seq(0,30,5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+theme_minimal()



filter(part, ANO_ELEICAO == 2010)%>%
  ggplot(aes(x=fct_reorder(SIGLA_PARTIDO, perc54), y = perc54))+
  geom_col(fill = "black", width = 0.5) +
  labs(title = '2010',
       x = "Partidos",
       y = "Frequência (%)")+scale_y_continuous(breaks = seq(0,30,5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+theme_minimal()

filter(part, ANO_ELEICAO == 2014)%>%
  ggplot(aes(x=fct_reorder(SIGLA_PARTIDO, perc54), y = perc54))+
  geom_col(fill = "black", width = 0.5) +
  labs(title = '2014',
       x = "Partidos",
       y = "Frequência (%)")+scale_y_continuous(breaks = seq(0,30,5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+theme_minimal()


filter(part, ANO_ELEICAO == 2018)%>%
  ggplot(aes(x=fct_reorder(SIGLA_PARTIDO, perc54), y = perc54))+
  geom_col(fill = "black", width = 0.5) +
  labs(title = '2018',
       x = "Partidos",
       y = "Frequência (%)")+scale_y_continuous(breaks = seq(0,30,5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()+theme_minimal()



# Figura 10 - Número e Percentual de Participação no Grupo de Principais
# Competidores (1998 - 2018)

# Possivel Conflito com o psych

library(dplyr)
library(ggplot2)
library(grid)
library(hrbrthemes)

part2 <- group_by(part, ANO_ELEICAO)%>%
  summarize(count = n())


G11 <-  ggplot(part2, aes(x = ANO_ELEICAO, y = count ))+ geom_smooth(method = "loess", se = FALSE, color = "black")+
  scale_x_continuous(breaks =seq(1998,2018,4)) + scale_y_continuous(breaks = seq(0,20,2)) + theme_light(base_size = 18)+
  xlab('Eleição') + ylab('Número de Partidos')


G12 <- ggplot(part, aes(x=perc54, fill=Magnitude)) +
  geom_histogram( color="#e9ecef", alpha=0.6,
                  position = 'identity') +
  scale_fill_manual(values=c("#808080", "#C0C0C0")) +
  theme_ipsum() + ylab('Frequência')+xlab('Percentual de Participação')+
  labs(fill="")+theme_minimal()+ theme(legend.position="bottom")+
  scale_y_continuous(breaks = seq(0,12,1))+
  scale_x_continuous(breaks = seq(0,35,2))


pushViewport(viewport(layout = grid.layout(1, 2)))
print(G11, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(G12, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


# Figura 11 - Concentração de votos (Presidente e Governadores) e NEP do
# Senado (1998 - 2018)



G13 <- ggplot(Lei, aes(x=Nep, y=ConcPres, color=Magnitude, size=Magnitude, group = Magnitude)) +
  geom_point(size = 2.2) + theme_light(base_size = 18)+geom_smooth(method = 'lm', size = 1)+
  theme(legend.position="bottom")+scale_color_manual(values = c("black", "Grey65"))+
  xlab('Número Efetivo de Partidos')+ylab('Concentração de Votos (Presidente)')+
  scale_x_continuous(breaks = seq(0,32,4))+
  scale_x_continuous(breaks =seq(1,10,0.5))


G14 <- ggplot(Lei, aes(x=Nep, y=ConcGov, color=Magnitude, size=Magnitude, group = Magnitude)) +
  geom_point(size = 2.2) + theme_light(base_size = 18)+geom_smooth(method = 'lm', size = 1)+
  theme(legend.position="bottom")+scale_color_manual(values = c("black", "Grey65"))+
  xlab('Número Efetivo de Partidos')+ylab('Concentração de Votos (Governador)')+
  scale_x_continuous(breaks = seq(0,32,4))+
  scale_x_continuous(breaks =seq(1,10,0.5))


# Correlações:

Lei1T <- Lei%>%
  filter(Magnitude == 'Um Terço')

Lei2T <- Lei%>%
  filter(Magnitude == 'Dois Terços')


cor(Lei1T$Nep, Lei1T$ConcPres)
cor(Lei2T$Nep, Lei2T$ConcPres)

cor(Lei1T$Nep, Lei1T$ConcGov)
cor(Lei2T$Nep, Lei2T$ConcGov)


## Análise Inferencial - Modelo de MQO ##

# Análise de distribuições das variáveis


ggplot(Lei, aes(x=Nep)) +
  geom_histogram(binwidth=0.5, alpha=.6, position="identity")

Lei$Neplog <- log(Lei$Nep)

ggplot(Lei, aes(x=Ncand)) +
  geom_histogram(binwidth=0.8, alpha=.6, position="identity")

Lei$Ncandlog <- log(Lei$Ncand)

ggplot(Lei, aes(x=PercVot)) +
  geom_histogram(binwidth=1.6, alpha=.6, position="identity")

Lei$PercVotlog <- log(Lei$PercVot)

ggplot(Lei, aes(x=ConcGov)) +
  geom_histogram(binwidth=0.01, alpha=.6, position="identity")

ggplot(Lei, aes(x=RazPerd)) +
  geom_histogram(binwidth= 2.5, alpha=.6, position="identity")

Lei$RazPerdlog <- log(Lei$RazPerd)



## Modelo de Regressão Linear - Dados Agrupados ##

install.packages("GGally")
install.packages("broom")
library(GGally)
library(broom)


m01 <- lm(Neplog ~ Mag + PercVotlog + Ncandlog + ConcGov + RazPerdlog, Lei)


summary(m01)


gm1 <- ggcoef(m01, exclude_intercept = TRUE, errorbar_height = .2,
             color = 'grey', sort = "descending", size = 2, errorbar_size = 0.8)




gm1 +theme_light(base_size = 19)+labs(x = "Coeficientes", y = "Variáveis")+
  scale_y_discrete(labels=c('Magnitude', 'N. Candidatos (log)',
                            'Votos Válidos (log)',  'Razão Derrotados (log)',
                            'Concentração Governador'))+
  scale_x_continuous(breaks = seq(-1.0,0.6,0.2))


## Anexos Com Bônus ##


# Análise de Resíduo - Modelo de Regressão Linear - Dados Agrupados

install.packages('lmtest')
library(lmtest)

dwtest(m01)
bgtest(m01)
bptest(m01)
vif(lm(Neplog ~ Mag + PercVotlog + Ncandlog + ConcGov + RazPerdlog, Lei))


# Modelo de Regressão Linear com Interação - Dados Agrupados

# Interação

Lei$MagRaz <- Lei$Mag*Lei$RazPerdlog

m03 <- lm(Neplog ~ Mag + PercVotlog + Ncandlog + ConcGov + RazPerdlog + MagRaz, Lei)

summary(m03)


# Modelos Regressão Linear com Dados de Painel

install.packages("plm")
library(plm)
install.packages("systemfit")
library("systemfit")


m02 <- plm(Neplog ~ Mag + PercVotlog + Ncandlog + ConcGov + RazPerdlog, Lei,index = c("Ano"), effect = "time")
summary(m02)

gm2 <- ggcoef(m02, exclude_intercept = TRUE, errorbar_height = .2,
             color = 'grey', sort = "descending",  size = 2, errorbar_size = 0.8)

gm2 +theme_light(base_size = 19)+labs(x = "Coeficientes", y = "Variáveis")+
  scale_y_discrete(labels=c('Magnitude', 'N. Candidatos (log)',
                            'Votos Válidos (log)',  'Razão Derrotados (log)',
                            'Concentração Governador'))+
  scale_x_continuous(breaks = seq(-1.0,0.6,0.2))




