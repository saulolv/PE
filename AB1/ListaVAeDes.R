#          LISTA

# 2- Em uma fabrica��o artesanal de componentes ceramicos 12% apresentam defeitos. Utilizando distribui��o Binomial, calcular a probabilidade de, em um lote de quarenta componentes, encontrar:

  # a) Entre 3 e 5 componentes estejam defeituosos, inclusive.
  res2a = pbinom(5, size = 40, prob = 0.12) - pbinom(2, size = 40, prob = 0.12)
  res2a

  # b) Pelo menos dois componentes defeituosos.
  resp2b = pbinom(1, size = 40, prob = 0.12, lower.tail = F)
  resp2b
  
  # c) No m�ximo 3 componentes defeituosos.
  res2c = pbinom(3, size = 40, prob = 0.12)
  res2c
  
  # d) Pelo menos 39 componentes de qualidade.
  res2d = pbinom(38, size = 40, prob = 0.88, lower.tail = F)
  res2d
  
  # e) No m�ximo 39 componentes de qualidade.
  res2e = pbinom(39, size = 40, prob = 0.88)
  res2e

# 3- O Corpo de Bombeiros de uma determinada cidade recebe, em m�dia, 3 chamadas por dia. Qual a probabilidade de receber:
  
  Clambda = 3
  # a) 4 chamadas num dia.
  res3a = dpois(4, lambda = Clambda)
  res3a
  
  # b) Nenhuma chamada em um dia.
  res3b = dpois(0, lambda = Clambda)
  res3b
  
  # c) 20 chamadas em uma semana.
  res3c = dpois(20, lambda = Clambda*7)
  res3c
  
  
# 4- Tem-se uma carteira com 15 a��es. No preg�o de ontem 75% das a��es na bolsa de valores ca�ram de pre�o. Supondo que as a��es que perderam valor t�m distribui��o binomial, calcule:
  
  # a) Quantas a��es da carteira se espera que tenham ca�do de pre�o?
  res4a = 0.75 * 15
  res4a
  
  # b) Qual o desvio padr�o das a��es da carteira?
  res4b = sqrt(15*0.75*(1-0.75))
  res4b
  
  # c) Qual a probabilidade que as 15 a��es da carteira tenham ca�do?
  res4c = dbinom(15, 15, 0.75)
  res4c
  
  # d) Qual a probabilidade que tenham ca�do de pre�o exatamente 10 a��es?
  res4d = dbinom(10, 15, 0.75)
  res4d
  
  # e) Qual a probabilidade que 13 ou mais a��es da carteira tenham ca�do de pre�o?
  res4e = pbinom(12, size = 15, prob = 0.75, lower.tail = F)
  res4e
  
# 5- A dura��o de um certo componente eletr�nico tem m�dia de 850 dias e desvio padr�o de 40 dias. Sabendo que a dura��o � normalmente distribu�da, calcule a probabilidade de o componente durar:
  
  # a) Entre 700 e 1000 dias;
  res5a = pnorm(1000, mean = 850, sd = 40, lower.tail = T) - pnorm(700, mean = 850, sd=40, lower.tail = T)
  res5a
  
  # b) Mais de 800 dias;
  res5b = pnorm(800, mean = 850, sd = 40, lower.tail = F)
  res5b
  
  
  # c) Menos de 750 dias.
  res5c = pnorm(750, mean = 850, sd = 40, lower.tail = T)
  res5c
  
# 6- Uma moeda � lan�ada 6 vezes, encontre a probabilidade de:
  
  # a) Ocorrer 4 coroas;
  r6a = dbinom(4, size = 6, prob = 0.5)
  r6a
  
  # b) Ocorrer pelo menos 2 coroas;
  r6b = pbinom(1, 6, 0.5, lower.tail = F)
  r6b
  
  # c) Ocorrer no m�ximo 3 coroas.
  r6c = pbinom(3, 6, 0.5, lower.tail = T)
  r6c
  
# 7- A probabilidade de um arqueiro acertar um alvo com uma �nica flecha � de 0,20. Lan�a 30 flechas no alvo. Qual a probabilidade de que:
  
  # a) Exatamente 4 flechas acertem o alvo?
  r7a = dbinom(4, 30, 0.2)
  r7a
  
  # b) Pelo menos 3 acertem o alvo?
  r7b = pbinom(2, 30, 0.2, lower.tail = F)
  r7b
  
# 8- O volume de correspond�ncia recebido por uma firma quinzenalmente tem distribui��o normal com m�dia de 4.000 cartas e desvio padr�o de 200 cartas. Qual a Porcentagem de quinzenas em que a firma recebe:
  
  # a) Entre 3.600 e 4.250 cartas?
  r8a = pnorm(4250, mean = 4000, sd = 200, lower.tail = T) - pnorm(3600, 4000, 200, lower.tail = T)
  print(paste(round(r8a*100, 3), "%"))
  
  # b) Menos de 3.400 cartas?
  r8b = pnorm(3400, mean = 4000, sd = 200, lower.tail = T)
  print(paste(round(r8b*100, 3), "%"))
  
  # c) Mais de 4.636 cartas?
  r8c = pnorm(4636, mean = 4000, sd = 200, lower.tail = F)
  print(paste(round(r8c*100, 3), "%"))
  
# 9- Uma VA tem distribui��o normal, com m�dia 100 e desvio padr�o XX. Qual a Probabilidade (90 ??? X ??? 110)? Considere XX a soma dos d�gitos da sua matr�cula.
  xx = 17
  pnorm(110, mean = 100, sd = 17, lower.tail = T) - pnorm(90, mean = 100, sd = 17, lower.tail = T)
  
# 10- A probabilidade de que um presum�vel cliente aleatoriamente escolhido fa�a uma compra � de 20%. Se um vendedor visita seis presum�veis clientes, qual a probabilidade de que ele fa�a no m�nimo, quatro vendas?
  probabilidade = 0.2
  n = 6
  pbinom(3, size = 6, prob = probabilidade, lower.tail = F)
  
# 11. Se a probabilidade de um indiv�duo acusar rea��o negativa a inje��o de determinado soro � de 0,1%. Determine a probabilidade de que, 1000 indiv�duos, exatamente 3 acusarem rea��o.
  probabilidade = 0.1/100
  n = 1000
  k = 3
  
  dbinom(k, n, probabilidade)
  
# 12-  No Lan�amento de 30 moedas honestas, qual a probabilidade de sa�rem:
  # a) Exatamente 12 caras;
    dbinom(12, 30, 0.5)
  # b) Mais de 20 caras.
    pbinom(20, 30, 0.5, lower.tail = F)
# 13- A distribui��o dos pesos de coelhos criados numa granja pode muito bem ser representada por uma distribui��o Normal, com m�dia 5 kg e desvio padr�o 0,9 kg. Um abatedouro comprar� 5000 coelhos e pretende classific�-los de acordo com o peso do seguinte modo: 15% dos mais leves como pequenos, os 50% seguintes como m�dios, os 20% seguintes como grandes e os 15% mais pesados como extras. Quais os limites de peso para cada classifica��o?
    pequenos = 0.15
    medios = 0.50
    grandes = 0.20
    extras = 0.15
    
    pnorm(5000*0.15, mean = 5, sd = 0.9)
    