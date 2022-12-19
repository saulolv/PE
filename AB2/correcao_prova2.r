# Correcao prova ab2
# Monitores: Leonardo Vinicius e Vitor Magno

# ----------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Retira-se uma amostra de (350 + xx) lâmpadas de um total de 35.000 lâmpadas 
# fabricadas, e obtém-se uma vida média de (800 + xx) horas e desvio padrão de 100 
# horas.

# a. Calcular o intervalo de Confiança, com nível de 99% de confiança para a vida 
# média das lâmpadas. 
amostraLampadas = 378
vidaMediaLampadas = 828
desvioPadraoLampadas = 100
alfa = 0.01
p = 1-alfa/2
zc = qnorm(p, 0,1)
zc = round(zc,2)
erro = zc * desvioPadraoLampadas/sqrt(amostraLampadas)
erro = round(erro, 5)
cat("[",vidaMediaLampadas - erro, ",", vidaMediaLampadas + erro, "]")
## [ 814.7299 , 841.2701 ]

# b. Calcular o tamanho da amostra para estimar a média com 95% de confiança 
# com um erro de 0,08.
alfa = 0.05
p = 1-alfa/2
zc = qnorm(p, 0,1)
zc = round(zc,2)
amostraLampadas = ((zc * desvioPadraoLampadas)/0.08)^2
amostraLampadas
## [1] 6002500
cat("a amostra teria que ser de ",amostraLampadas," lampadas para que o erro fosse 0.08")
## a amostra teria que ser de 6002500 lampadas para que o erro fosse 0.08
# ----------------------------------------------------------------------------------------------------------------------------------------------------

# 2. Considere os valores: 127, 126, 126, 124, 123, 122, 128, 125, 128, 124, 127, 125, 
# 120, 124, 124, 127, 125, 128, 126 e (xx + 85) 
# a) A um nível de significância de 1%, podemos afirmar que a média é 127? 
#  Justifique sua resposta. 

valores = c(127, 126, 126, 124, 123, 122, 128, 125, 128, 124, 127, 125, 120, 124, 124, 127, 125, 128, 126, 113)
# valores = c(127, 126, 126, 124, 123, 122, 128, 125, 128, 124, 127, 125, 120, 124, 124, 127, 125, 128, 126, 104)
t.test(valores, conf= 0.99, mu = 127)
# Nao podemos afirmar que a media eh 127 a 99% de confianca

# b) Para o nível de confiança de 94%, podemos afirmar que a média é menor ou 
# igual a 127? Justifique sua resposta e compare os resultados dos itens a) e b). 
t.test(valores, conf= 0.94, mu = 127, alternative = "greater")
t.test(valores, conf= 0.94, mu = 127)

# Nao rejeitamos a hipotese nula e podemos afirmar que a media eh menor que 127 a 94% de
# confianca. Rejeitamos a hipotese nula no segundo teste, assim não podemos afirmar que a
# media eh igual a 127. Comparando os resultados da letra a e b, vemos que o intervalo de
# confianca menor gera um maior intervalo numerico
# ----------------------------------------------------------------------------------------------------------------------------------------------------

# 3. Considere as amostras: 
# * Procedimento 1: 45 51 50 62 43 42 53 50 48 55 (XX + 10) (XX + 5) 
# * Procedimento 2: 45 35 43 59 48 45 41 43 49 39 (XX + 3) (XX - 2) 
# a) Considerando os dados pareados, nível de significância de 4 %, as hipóteses: 
# H0 : ????0 ??? ????1 ??? 0 
# H1 : ????0 ??? ????1 > 0 
# Calcular t, o p-value, interpretar os resultados e desenhar os gráficos com as 
# áreas. 

procedimentoUm = c(45,51, 50, 62 ,43, 42, 53, 50, 48, 55, (28 + 10), (28 + 5))
procedimentoDois = c(45, 35, 43, 59, 48, 45, 41, 43, 49, 39, (28 + 3), (28 - 2))
t.test(procedimentoUm, procedimentoDois, paired = TRUE, conf=0.96, alternative ="greater")
# Rejeitamos a hipotese nula, o que significa que as medias dos procedimentos sao diferentes a
# 96% de confianca

# b) Considerando que os dados são não pareados faça a análise e interprete os 
# resultados a 5% de significância. 
t.test(procedimentoUm, procedimentoDois, paired = FALSE, conf=0.95, alternative ="greater")

# Nao rejeitamos a hipotese nula, o que significa que as medias dos procedimentos sao iguais a
# 95% de confianca
# ----------------------------------------------------------------------------------------------------------------------------------------------------

# 4. Calcular com os dados abaixo. 
# Calcular: 
 

# a) A matriz de correlações entre as variações e explicar os resultados.
x <- c(11.20, 8.6, 11, 9.8, 11, 14, 6, 4, 12, 7.4, 10.8, 9.5)
y <- c(9.5, 6.6, 7.6, 8.8, 8.3, 9.9, 7.25, 4.16, 10.8, 4.5, 8.25, 6.33)
z <- c(8.25, 5.76, 7.7, 8.84, 8.47, 7.22, 5.75, 10.5, 5.5, 7.9, 6.58, 6.33)
tabela <- data.frame(x, y, z)
cor(tabela)
# Para interpretar podemos notar que as relações entre x e y tem uma paridade de 83%,, enquanto
# que de x e z é negativa e de y e z também é negativa, portanto há uma correlação entre x e y.

# b) Escolher as duas variáveis com maior correlação e calcular a equação da reta e o
# coeficiente de determinação de Pearson (R2) e interprete o resultado. 
reg = lm(x~y, data = tabela)
summary(reg)
# coeficiente de determinacao: 0.6931 ou 69.31%
# significa que esse modelo de regressão conseguiu explicar 69,31% da variavel prevista, tem uma relação entre os dados x e y.

# c) Verificar se os resíduos são aderentes a distribuição normal.
shapiro.test(x)
shapiro.test(y)
shapiro.test(z)

# ----------------------------------------------------------------------------------------------------------------------------------------------------

# 5. Deseja-se testar se existem diferenças na quantidade de jacarés em três locais do 
# pantanal mato-grossense. 
# Calcular: 
# a) Elaborar e interpretar o quadro de Análise de Variância (ANOVA); 
library(openxlsx)
dados <- read.xlsx("prova.xlsx")
dados
resposta <- aov(dados$quantidade ~ factor(dados$local))
anova(resposta)

# b) Verificar as divergências entre as médias dos locais e interpretar os resultados a 
# 5% de significância.
TukeyHSD(resposta)

# Analisando se o p-valor é > 5 ou < 5 tiramos as conclusões:
# Se < 5 - diferente, se > 5 igual, temos:
# Local 2 e local 1 - são iguais a 5%
# Local 3 e local 1 - são diferentes 5%
# Local 4 e local 1 - são iguais a 5%
# Local 3 e local 2 - são diferentes de 5%
# Local 4 e local 2 - são iguais a 5%
# Local 4 e local 3 - são diferentes de 5%