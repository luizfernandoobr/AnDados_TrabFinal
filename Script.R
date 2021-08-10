# Trabalho Final - Analise de Dados
setwd("D:/luizf/Documents/PPGCP/2021.1/AnDados/Trab final/")
options(scipen = 9999)
options(max.print =100000)
require(data.table)
memory.limit(24576)
TS_ALUNO_9EF <- data.table::fread(input='TS_ALUNO_9EF.csv',integer64='character')

require(tidyverse)
dados <- TS_ALUNO_9EF %>% 
  select(ID_ALUNO, ID_UF, PROFICIENCIA_LP_SAEB, TX_RESP_Q002, ID_TURNO,
         TX_RESP_Q006B, TX_RESP_Q018B, 
         TX_RESP_Q017E, TX_RESP_Q015) %>% 
  filter(ID_UF == 26,
         PROFICIENCIA_LP_SAEB != is.na(PROFICIENCIA_LP_SAEB),
         TX_RESP_Q002 != "F") %>% 
  mutate(TX_RESP_Q002 = recode(TX_RESP_Q002, 
                        "A" = "Branco", "B" = "Não branco",
                        "C" = "Não branco", "D" = "Não branco",
                        "E" = "Não branco"),
         TX_RESP_Q006B = recode(TX_RESP_Q006B, 
                               "A" = "FALSE", "B" = "TRUE", 
                               "C" = "TRUE"),
         TX_RESP_Q017E = recode(TX_RESP_Q017E, 
                                "A" = "FALSE", "B" = "TRUE", 
                                "C" = "TRUE", "D" = "TRUE"),
         TX_RESP_Q018B = recode(TX_RESP_Q018B, 
                                "A" = "FALSE", "B" = "TRUE", 
                                "C" = "TRUE"),
         TX_RESP_Q015 = recode( TX_RESP_Q015, 
                               "A" = "FALSE", "B" = "TRUE", 
                               "C" = "TRUE"), 
         Nivel_3 = PROFICIENCIA_LP_SAEB >= 250,
         Turno_N = ID_TURNO == 3)

dados <- dados %>% mutate(TX_RESP_Q002 = as_factor(TX_RESP_Q002),
       Nivel_3 = as.numeric(Nivel_3)) %>%
  filter(TX_RESP_Q002 != "." & TX_RESP_Q002 != "*",
         TX_RESP_Q006B != "." & TX_RESP_Q006B != "*",
         TX_RESP_Q015 != "." & TX_RESP_Q015 != "*",
         TX_RESP_Q017E != "." & TX_RESP_Q017E != "*",
         TX_RESP_Q018B != "." & TX_RESP_Q018B != "*")

# Nota por Cor/raça
dados %>% group_by(TX_RESP_Q002) %>%
  summarise(media = mean(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            min = min(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            max = max(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            sd = sd(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            median = median(PROFICIENCIA_LP_SAEB, na.rm = TRUE))
# Nota trabalha/não trabalha
dados %>% group_by(TX_RESP_Q017E) %>%
  summarise(media = mean(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            min = min(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            max = max(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            sd = sd(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            median = median(PROFICIENCIA_LP_SAEB, na.rm = TRUE))
# Nota Reprovado
dados %>% group_by(TX_RESP_Q015) %>%
  summarise(media = mean(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            min = min(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            max = max(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            sd = sd(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            median = median(PROFICIENCIA_LP_SAEB, na.rm = TRUE))
# Nota Turno
dados %>% group_by(Turno_N) %>%
  summarise(media = mean(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            min = min(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            max = max(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            sd = sd(PROFICIENCIA_LP_SAEB, na.rm = TRUE),
            median = median(PROFICIENCIA_LP_SAEB, na.rm = TRUE))

# Testes de Hipotese
# Distribuicao Notas
#Graficos
ggplot(dados, aes(x = PROFICIENCIA_LP_SAEB)) + 
  geom_histogram(aes(y =..density..), colour="black", fill="gray")+
  geom_density(alpha =.3, fill ="#FF6666") +
  labs(title ="Histograma das Notas em Língua Portuguesa dos Alunos do 9º ano de Pernambuco no SAEB 2019",
       x ="Nota LP", y = "Densidade")+
  theme_classic()
#Testes
skewness(dados$PROFICIENCIA_LP_SAEB)
kurtosis(dados$PROFICIENCIA_LP_SAEB)
jarqueberaTest(dados$PROFICIENCIA_LP_SAEB)
shapiro.test(dados$PROFICIENCIA_LP_SAEB)
# QQ-PLOT 
qqnorm(dados$PROFICIENCIA_LP_SAEB, col="blue3")
qqline(dados$PROFICIENCIA_LP_SAEB, col = "red")

# Teste t
t.test(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q002, data = dados)
t.test(PROFICIENCIA_LP_SAEB ~ Turno_N, data = dados)
t.test(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q006B, data = dados)
t.test(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q015, data = dados)
t.test(PROFICIENCIA_LP_SAEB ~ TX_RESP_Q017E, data = dados)
#Qui-quadrado
chisq.test(dados$Nivel_6, dados$TX_RESP_Q017E)
chisq.test(dados$TX_RESP_Q017E, dados$Turno_N)


# Graficos


# Modelo Logístico
logit <- glm(Nivel_3 ~ TX_RESP_Q002 + TX_RESP_Q006B + Turno_N + 
            TX_RESP_Q015 + TX_RESP_Q017E + TX_RESP_Q018B,
             data = dados, family = binomial)
summary(logit)

logitIT <- glm(Nivel_3 ~ TX_RESP_Q002 + TX_RESP_Q006B + Turno_N + 
              TX_RESP_Q015 + TX_RESP_Q017E + TX_RESP_Q018B +
              (Turno_N*TX_RESP_Q017E),
              data = dados, family = binomial)

summary(logitIT)
