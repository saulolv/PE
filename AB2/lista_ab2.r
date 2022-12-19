# 1 - Uma fábrica Brasileira de cosméticos, trabalhava com variância de 14 dias. Em uma
# amostra de 120 cosméticos produzidos, foi possível obter um tempo médio de produção
# de 9 dias. Obtenha o intervalo para um nível de confiança de 90%.
dp = sqrt(14)
amostraComestico = 120
tempoMedio = 9
alfa = 0.10
p = 1-alfa/2
zc = qnorm(p, 0, 1)
zc = round(zc, 2)
erro = zc * dp / sqrt(amostraComestico)
erro = round(erro, 5)
cat("[", tempoMedio - erro, ",", tempoMedio + erro, "]")


# 2 - No Hospital Unimed, 10 pacientes do sexo feminino que estavam na sala de espera
# foram sorteadas para realizar a medição da pressão sanguínea arterial, obtendo os
# seguintes resultados: (80, 75, 71, 82, 77, 64, 78, 67, 81, 79). Determine o intervalo de
# confiança para a pressão arterial média feminina com coeficiente de confiança de 98%.
valores <- c(80, 75, 71, 82, 77, 64, 78, 67, 81, 79)
alfa = 0.02
p = 1-alfa/2
n = length(valores)
desvio = sd(valores)
media = mean(valores)
tc = qt(p, n-1)
tc = round(tc, 3)
erro = tc * desvio / sqrt(n)
erro = round(erro, 3)
cat("[", media - erro, ",", media + erro, "]")


# 3 - Um partido deseja estimar a proporção de eleitores favoráveis a um determinado
# candidato a prefeito. Uma amostra piloto de 3500 eleitores revelou que 65% dos
# eleitores são favoráveis a este candidato. Elaborar um intervalo de confiança de 95%.
alfa = 0.05
n = 3500
p =  0.65
zc = qnorm(1-alfa/2, 0, 1)
zc = round(zc, 2)
erro = zc * sqrt(p * (1-p) / n)
erro = round(erro, 5)
cat("[", p - erro, ",", p + erro, "]")
prop.test(x = 2275, n = 3500, conf.level = 0.95)

# (testes de hipótese - bilateral)
# 4 - De uma população normal com variância 36, tira-se uma amostra aleatória de
# tamanho 20, obtendo-se uma média de 43. Ao nível de significância de 10%, testar as
# hipóteses: H0 : u = 45, H1 : u ≠ 45.
alfa = 0.10
dp = sqrt(36)
media = 43
n = 20
z = (media - 45) / (dp/sqrt(n))
z
# Aceitamos H0, pois a diferença existente é por conta da variação amostral


# (testes de hipótese - unilateral à direita)
# 5 - Um fabricante de contêineres realizou modificações em sua fabricação para
# aumentar a resistência média, que é de 510 Kg. Ao retirar uma amostra de 15
# contêineres, obteve-se uma média de 550 Kg. Sabendo que o desvio padrão de 25 Kg,
# com um nível de significância de 5%, pode o fabricante afirmar que a resistência média
# dos contêineres aumentou?
alfa = 0.05
dp = 25
media = 550
n = 15
z = (media - 510) / (dp/sqrt(n))
z
# Como Zcalc > Za, rejeita-se Ho, isto é, ao nivel de 5% o fabricante pode concluir que a resistencia média aumentou.


#(testes de hipótese com variância desconhecida)
# 6 - O instituto de engenharia de uma universidade aplica um teste vocacional para os
# calouros. Nos últimos anos tem sido admitida uma nota média de 127. Um teste foi
# realizado no semestre atual, onde foram obtidas as seguintes notas: (125, 124, 125, 125, 125, 125, 124, 123, 122, 123, 123, 123, 123, 124, 124). Queremos saber se a média
# do semestre atual foi diferente dos anteriores, então, realize o teste de hipótese,
# admitindo um nível de significância de 5% para efetuar o teste.
valores = c(125, 124, 125, 125, 125, 125, 124, 123, 122, 123, 123, 123, 123, 124, 124)
t.test(valores, conf = 0.95, mu = 127)
# Como o p-valor deu muito baixo, rejeitamos a hipotese nula(h0), ou seja, não podemos afirmar que a média é igual a 127


#(teste de hipótese para dados pareados)
# 7 - Na disciplina de teoria dos grafos, o professor passou como atividade avaliativa, a
# implementação de um algoritmo específico de grafos, que deveria ser implementado
# utilizando dois tipos de busca: busca em largura e busca em profundidade. O professor
# coletou os dados de tempo de execução (em milissegundos) de 15 alunos, para os dois
# métodos de busca. Existe diferença na velocidade de execução do algoritmo para os
# dois tipos de busca? Verificar a um nível de 5% de significância.

# H0: bfs = dfs
# H1: bfs != dfs

# ▪ bfs = (32, 27, 38, 35, 33, 29, 25, 19, 31, 24)
# ▪ dfs = (26, 21, 20, 37, 30, 18, 19, 25, 32, 34)
bfs = c(32, 27, 38, 35, 33, 29, 25, 19, 31, 24)
dfs = c(26, 21, 20, 37, 30, 18, 19, 25, 32, 34)
t.test(bfs, dfs, paired = TRUE, conf.level = 0.95)
# Como p-valor deu acima de 5%, precisamente 26,31%, aceitamos a hipotese nulaonde não há diferença entre bfs e dfs


# 8 - Um pesquisador estudou os efeitos de determinada dieta alimentar sobre o aumento
# do peso corporal em cobaias adultas. Coletou seus pesos antes e três meses após a
# gestão da nova dieta e obteve:
# ▪ Antes: 54 61 50 74 79 58 55 49 63
# ▪ Três meses Depois: 57 66 53 73 82 58 56 53 63
# Considere as hipóteses: H0: μD = μA
#                         H1: μD ≠ μA
# Considere α = 0,05 e justifique sua resposta com relação às hipóteses estabelecidas.
antes = c(54, 61, 50, 74, 79, 58, 55, 49, 63)
depois = c(57, 66, 53, 73, 82, 58, 56, 53, 63)
t.test(antes, depois, paired = TRUE, conf.level = 0.95)
# Rejeitamos H0 e aceitamos H1, pois p-valor é menor que 0.05, sendo ele 0.019. Portanto, o peso depois da dieta é diferente


# 9 - Um professor afirma que os alunos vão baixando seus coeficientes de rendimento à
# medida que avançam no curso. Oito alunos foram escolhidos aleatoriamente e
# observado seus rendimentos nos semestres anterior e atual.
# H0: atual < anterior
# H1: atual >= anterior
# ▪ Anterior: 89, 84, 96, 82, 74, 92, 85 e 91.
# ▪ Atual: 83, 83, 92, 84, 76, 91, 80 e 91.
# Assumindo que os coeficientes são distribuídos normalmente, existe evidência
# suficiente para apoiar a afirmação do professor para um nível de significância de 10%?
anterior = c(89, 84, 96, 82, 74, 92, 85, 91)
atual = c(83, 83, 92, 84, 76, 91, 80, 91)
t.test(atual, anterior, paired = TRUE, conf.level = 0.90, alternative = "less")
# Rejeitamos a hipotese nula, então não podemos afirmar que os alunos vão baixando seus coeficientes a
# 90% de confianca


# 10 - Uma nova metodologia de desenvolvimento de software se propõe a reduzir o
# tempo de projeto e desenvolvimento de sistemas de informação. Assim, foram
# considerados 24 projetos, sendo 12 de tecnologia atual e 12 com a nova proposta. Os
# valores, em horas, estão a seguir:
# ▪ TecAtual: 300, 280, 344,385, 372, 360, 288, 321, 376, 290, 301, 283
# ▪ TecNova: 274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 332, 263

# Considerando nível de significância de 2%, as hipóteses:
# H0 : μଵ − μଶ ≤ 0
# H1 : μଵ − μଶ > 0
# Calcule t, o p-value e interpretar os resultados.
tecAtual = c(300, 280, 344,385, 372, 360, 288, 321, 376, 290, 301, 283)
tecNova = c(274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 332, 263)

t.test(tecAtual, tecNova, paired = TRUE, conf.level = 0.98, alternative = "greater")
# Rejeitamos H0 e aceitamos a hipotese alternativa, pois o p-valor é igual 1.6%, sendo abaixo do nivel de significância(2%)