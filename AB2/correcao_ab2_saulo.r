# Correção prova 2
# Saulo | Matrícula: 	20211551
xx = 2+2+1+1+5+5+1
# xx = 17
# 1. Retira-se uma amostra de (350 + xx) l?mpadas de um total de 35.000 l?mpadas 
# fabricadas, e obt?m-se uma vida m?dia de (800 + xx) horas e desvio padr?o de 100 
# horas.

# a. Calcular o intervalo de Confian?a, com n?vel de 99% de confian?a para a vida 
# m?dia das l?mpadas.
amostraLampadas = 350+17
vidaMediaLampadas = 800+17
desvioPadraoLampadas = 100
alfa = 0.01
p = 1-alfa/2
zc = qnorm(p, 0, 1)
zc = round(zc, 2)
erro = zc * desvioPadraoLampadas / sqrt(amostraLampadas)
erro = round(erro, 5)
cat("[", vidaMediaLampadas - erro, ",", vidaMediaLampadas + erro, "]")
# [ 803.5325 , 830.4675 ]

# b. Calcular o tamanho da amostra para estimar a m?dia com 95% de confian?a 
# com um erro de 0,08.
alfa = 0.05
p = 1-alfa/2
zc = qnorm(p, 0, 1)
zc = round(zc, 2)
amostraLampadas = ((zc * desvioPadraoLampadas)/0.08)^2
amostraLampadas
# [1] 6002500
cat("A amostra teria que ser de ",amostraLampadas," lampadas para que o erro fosse 0.08")
# A amostra teria que ser de 6002500 lampadas para que o erro fosse 0.08



# 2. Considere os valores: 127, 126, 126, 124, 123, 122, 128, 125, 128, 124, 127, 125, 
# 120, 124, 124, 127, 125, 128, 126 e (xx + 85) 
# a) A um n?vel de signific?ncia de 1%, podemos afirmar que a média é 127? 
#  Justifique sua resposta. 
xx = 17
valores = c(127, 126, 126, 124, 123, 122, 128, 125, 128, 124, 127, 125, 120, 124, 124, 127, 125, 128, 126, (xx + 85))
t.test(valores, conf = 0.99, mu = 127)
# como o p-valor deu acima de 2%, desse caso 2,9%, ele está na zona de aceitação, significa que
# podemos não rejeitar a hipótese nula(h0) , portanto, podemos afirmar que a média é igual a 127.

# b) Para o n?vel de confian?a de 94%, podemos afirmar que a m?dia ? menor ou 
# igual a 127? Justifique sua resposta e compare os resultados dos itens a) e b). 
t.test(valores, conf = 0.94, mu = 127, alternative = "greater")
t.test(valores, conf = 0.94, mu = 127)
# Como nessa questão estamos fazendo uma análise unilateral a direita, vemos se a hipótese
# alternativa com um nível de confiança de 94% é rejeitada ou não rejeitada, e como vemos que o p-
# valor é um valor acima de 6%, não rejeitamos a hipótese alternativa e rejeitamos a hipótese
# nula(h0), portanto, não podemos afirmar que a média é menor ou igual a 127.



# 3. Considere as amostras: 
# * Procedimento 1: 45 51 50 62 43 42 53 50 48 55 (XX + 10) (XX + 5) 
# * Procedimento 2: 45 35 43 59 48 45 41 43 49 39 (XX + 3) (XX - 2) 
# a) Considerando os dados pareados, nivel de significancia de 4 %, as hipoteses: 
# H0 : μ1 − μ2 ≤ 0
# H1 : μ1 − μ2 > 0 
# Calcular t, o p-value, interpretar os resultados e desenhar os graficos com as 
# areas. 
xx = 17
procedimentoUm = c(45, 51, 50, 62, 43, 42, 53, 50, 48, 55, (xx + 10), (xx + 5))
procedimentoDois = c(45, 35, 43, 59, 48, 45, 41, 43, 49, 39, (xx + 3), (xx - 2))
t.test(procedimentoUm, procedimentoDois, paired = TRUE, conf = 0.96, alternative = "greater")
# Como p-valor deu abaixo de 4% ̈, sendo um número bastante pequeno, rejeitamos a hipótese
# nula, e consideramos a hipótese alternativa, portanto o procedimento1 tem maior média que o
# procedimento2. O t-valor nesse caso é t = 2.7371, e p-valor é 0.009666, ou 0,9%.

# b) Considerando que os dados são não pareados faça a analise e interprete os 
# resultados a 5% de significância. 
t.test(procedimentoUm, procedimentoDois, paired = FALSE, conf=0.95, alternative ="greater")
# Nao rejeitamos a hipotese nula, o que significa que as medias dos procedimentos sao iguais a
# 95% de confianca

# 4. Calcular com os dados abaixo. 
# Calcular: 


# a) A matriz de correlações entre as variações e explicar os resultados.
x <- c(11.20, 8.6, 11, 9.8, 11, 14, 6, 4, 12, 7.4, 10.8, 8.5)
y <- c(9.5, 6.6, 7.6, 8.8, 8.3, 9.9, 7.25, 4.16, 10.8, 4.5, 8.25, 5.67)
z <- c(8.25, 5.76, 7.7, 8.84, 8.47, 7.22, 5.75, 10.5, 5.5, 7.9, 6.58, 5.67)
tabela = data.frame(x, y, z)
cor(tabela)
# Para interpretar podemos notar que as relações entre x e y tem uma paridade de 83%,, enquanto
# que de x e z é negativa e de y e z também é negativa, portanto há uma correlação entre x e y.

# b) Escolher as duas variáveis com maior correlação e calcular a equação da reta e o
# coeficiente de determinação de Pearson (R2) e interprete o resultado.
reg = lm(x~y, data = tabela)
summary(reg)
# coeficiente de determinacao: 0.705 ou 70.50%
# significa que esse modelo de regress?o conseguiu explicar 70.50% da variavel prevista, tem uma relação entre os dados x e y.

# c) Verificar se os resíduos são aderentes a distribuição normal.
shapiro.test(x)
shapiro.test(y)
shapiro.test(z)
# Como o p-valor deu acima de 5%, os dados de x, y e z têm distribuição normal



# 5. Deseja-se testar se existem diferenças na quantidade de jacarés em três locais do
# pantanal mato-grossense.

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
# Local 2 e local 1 - s?o iguais a 5%
# Local 3 e local 1 - s?o diferentes 5%
# Local 4 e local 1 - s?o iguais a 5%
# Local 3 e local 2 - s?o diferentes de 5%
# Local 4 e local 2 - s?o iguais a 5%
# Local 4 e local 3 - s?o diferentes de 5%

dir()







