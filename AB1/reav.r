# Prova de reavaliação da AB1

# Saulo Roberto dos Santos
xx = 17

# 1
# a)
figuras = 3*6
copas = 13
total = 6*13
resposta = figuras/(total) + copas/(total) - 3/total
resposta
# 0.3589744 ou 35.89%

# b)
resposta = 1/total * 13/total * 13/total * 13/total * 13/total
resposta
# 0.0001286008 ou 0.012%

# 2
# Considerando de 1 a 17
moedas = 17
# O numero maior ou igual a nove que é divisivel por 3 são 9, 12, 15
resposta = 1/17 + 1/17 + 1/17
resposta
# 0.1764706 ou 17.64%


# 3
media = 170
dp = 17-5
# a)
resultado = pnorm(200, mean = 170, sd = dp, lower.tail = T) - pnorm(139, mean = 170, sd = dp, lower.tail = T)
resultado
# 0.9888978 ou 98.8%

# b)
grafico = curve(dnorm(x, media, dp), media-3*dp, media+3*dp)
# se concentra em 170

# 4 
a = 0.17
b = 0.25
c = (100-xx-25)/100
defA = 0.01
defB = 0.05
defC = 0.04
def_total = a * defA + b * defB + c * defC
resposta = (a * defA) / def_total
resposta
# 0.04545455 ou 4.54%

# 5 -
perfeitas = 17
total = 80
defGrave = 5
pequenoDef = 80-17-5
# a)
dbinom(4, size = total, prob = 17/80)
# 4.203099e-05

resposta = perfeitas / total * perfeitas / total * perfeitas /total * perfeitas /total
resposta

# b)
dbinom(2, size = total, prob = 17/80) + dbinom(2, total, prob = pequenoDef/total)


# 6 - 
# a)
prob = 1/2
n = 30
dbinom(12, size = n, prob = 1/2)
# 0.0805 ou 8.06% de probabilidade de tirar exatamente 12 caras

# b)
pbinom(20, size = n, prob = 1/2, lower.tail = F)
#  0.021 ou 2.1% de tirar mais de 20 caras