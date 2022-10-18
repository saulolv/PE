# -------------- AULA REVISÃO 17-10-2022 ------------------


# Exemplo 1: Em uma empresa, a montagem de certa peça é feita em duas etapas. 
# Os tempos para essas etapas são independentes e tem as seguintes distribuições:
# X1 ??? N ( 75 seg ,  16,81 seg2 )
# X2 ??? N ( 129 seg , 106,09 seg2 )

# Qual a probabilidade de se montar a peça em menos de 200 segundos?

# Gerar a nova distribuição Normal W 
EW = 75 + 129
VarW = 16.81 + 106.09
DesPadW = sqrt(VarW)
pnorm ( 200, mean= EW , sd= DesPadW, lower.tail = TRUE )

# Exemplo 2: Uma máquina automática enche latas baseada em pesos brutos.
# O peso bruto tem distribuição normal com ???? = 1.000 g e ???? = 20 g. 
# As latas tem peso bruto distribuídos normalmente, com ????= 90 g e????= 10 g. 
# Qual a probabilidade de que uma lata tenha de peso liquido: 
# a) Menos de 850 g?
# b) Mais de 870 g?
# c) Entre 860 e 920 g?
# ????1:peso bruto ; ????2:peso  da lata ; ????: peso liquido --> X = X1 - X2

EX = 1000 - 90
VarX = 20^2 + 10^2
DesPadX = sqrt (VarX)
# a) Menos de 850 g?
pnorm ( 850 , mean= EX , sd= DesPadX, lower.tail = TRUE )

# b) Mais de 870 g?
pnorm ( 870 , mean= EX , sd= DesPadX, lower.tail = FALSE )

# c) Entre 860 e 920 g?
pnorm ( 920 , mean= EX , sd= DesPadX, lower.tail = TRUE ) - 
  pnorm ( 860 , mean= EX , sd= DesPadX, lower.tail = TRUE ) 

# Exemplo 3.Um elevador foi projetado com resistência ao peso dado por uma V.A.
# de média 300 kg e desvio padrão 10 kg. Se a carga do elevador superar a
# resistência um freio é acionado e o elevador para.Numa viagem 4 pessoas 
# com média 70 Kg e desvio 13 Kg. 
# Qual a probabilidade de ser acionado o freio de emergência?
# T = W (pesos das pessoas) - X (resistencia ao peso - elevador)
EW = 70 * 4 
VarW = 13^2 * 4
ET = EW - 300
VarT = VarW + 10^2
DesPadT = sqrt (VarT)
# será acionado quando T = W - X for maior que zero, ou seja os pesos das pessoas
# for superior a resistência do elevador (300 Kg).
pnorm ( 0 , mean= ET , sd= DesPadT, lower.tail = FALSE ) 

# Distribuição Exponencial

# Exemplo 1 - Suponha que o tempo de espera para realizar um exame qualquer em 
# uma determinada clinica tenha distribuição exponencial com média de três dias.
# Qual a probabilidade de alguém esperar mais de 2 dias?
  
  pexp (2, rate = 1/3, lower.tail = FALSE)
 
# Qual a probabilidade de alguém esperar entre  2 e 4 dias?
  
  pexp (4, rate = 1/3, lower.tail = T) - pexp (2, rate = 1/3, lower.tail = T)

# Exemplo 4 - Uma companhia fabrica lâmpadas especiais com uma duração média 
# de 100 horas e com distribuição exponencial.
# Qual deve ser a garantia do fabricante para repor apenas 5% da produção?
    
    qexp ( 0.05, rate = 1/100)
    
# b) Qual a probabilidade de uma lâmpada durar de 163 a 185 horas?
    
    pexp (185, rate= 1/100,lower.tail= T) - pexp (163, rate= 1/100,lower.tail= T)
    
#    Distribuição Poisson
# Numa central telefônica chegam 300 telefonemas por hora e pode processar
# no máximo 10 ligações por minuto. Qual a probabilidade de que:
    
# a) Num minuto não haja nenhum chamado?
    Clambda = 300/60
    dpois ( 0 , lambda = Clambda )
# b) Em dois minutos haja 2 chamadas?  
    dpois ( 2 , lambda = Clambda * 2 )
# c) Qual a probabilidade da capacidade dacentral telefônica ser ultrapassada?
    ppois( 10 , lambda = Clambda, lower.tail = FALSE)

# Exemplo 3: O número de mortes por afogamento em fins de semana, numa
# cidade praiana, é de 2 para cada 50.000 habitantes.
# Qual a  probabilidade de que em:
    
     Clambda = 2/50000 
     
# a) 200.000 habitantes ocorram 5 afogamentos?
    
    dpois ( 5 , lambda = Clambda * 200000 )
    
# b) 112.500 habitantes ocorram pelo menos 3 afogamentos?  
    
    ppois ( 2 , lambda = Clambda * 112500, lower.tail = FALSE )
    

# Distribuição Binonoial  
    
# Exemplo 4 - Vasco e Botafogo jogam entre si 6 vezes.
# a) Encontre a probabilidade do Vasco ganhar 4 jogos?
    
    dbinom(4, size = 6 , prob = 1/3)
    
# b) Probabilidade do Vasco não ganhar nenhuma partida?
  
    dbinom(0, size = 6 , prob = 1/3)
    
# c) Probabilidade do Vasco ganhar no máximo duas partidas?

    pbinom( 2 , size = 6 , prob = 1/3)
    
# d) Probabilidade do Vasco ganhar pelo menos três partidas?
  
    pbinom( 6 , size = 6 , prob = 1/3, lower.tail = T) -
    pbinom( 2 , size = 6 , prob = 1/3, lower.tail = T)

# Elaborar a distruição de probabilidade (todos os valores).
    

      dbinom( 0:6 , size = 6 , prob = 1/3)
      
      for (i in 1:7){ X[i] = (dbinom (i-1, size = 6, prob= 1/3));
      print (i-1) 
      print (X[i])} 
      
      for (i in 1:7){ X[i] = (dbinom (i-1, size = 6, prob= 1/3));
      res = cbind (i, X[i]) } 
      
      
      
   print(res)  
      
   