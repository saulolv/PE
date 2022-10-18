# --------------------- Monitoria 2 ------------------------------
# Resolucao da prova AB1 do periodo passado

xx <- 19
yy <- 51

#----- 1°)
# a)
# modo 1:
prob = 2/3
n = 5
k = 2
p2 <- choose(5, 2) * ( ((2/3)^k ) * ((1/3)^(n-k)  ) )
p2
# modo 2:
dbinom(2, 5, prob = 2/3)

# b)
# modo 1: 1 - (prob(0) + prob(1) + prob(2))
p0 <- choose(5, 0) * ( ((2/3)^0) * ((1/3)^(5-0)) )
p1 <- choose(5, 1) * ( ((2/3)^1) * ((1/3)^(5-1)) )
p2 <- p2
resposta <- (1 - (p0 + p1 + p2))
resposta
# modo 2:
pbinom(2, 5, prob=2/3, lower.tail = F)
# modo 3:
1 - pbinom(2, 5, prob=2/3)
# modo 4:
1 - ( dbinom(0, 5, prob=2/3) + dbinom(1, 5, prob=2/3) + dbinom(2, 5, prob=2/3) )

#----- 2°)
esperanca <- 100
dp <- 10
# a)
pnorm(19, 100, 10, lower.tail = T)

# grafico
grafico <- curve(dnorm(x, esperanca, dp), esperanca-3*dp, esperanca+3*dp)
# meu xx é muito menor que a esperança, e nesse plot vai de 70 a 130, entao 19 nao aparecerá
# portanto eu coloquei 110 aleatoriamente so para demonstrar
lines(c(110, 110), c(0, dnorm(110, 100, 10)), col="red")


#b)
1 - pnorm(39, 100, 10) 
pnorm(39, 100, 10, lower.tail = F)
# grafico
grafico
# aqui tambem vai servir só para demonstracao ja que 39 eh um numero mt pequeno
lines(c(80, 80), c(0, dnorm(80, 100, 10)), col="blue")

# c)
pnorm(39, 100, 10, lower.tail = T) - pnorm(9, 100, 10, lower.tail = T)
# grafico
grafico
# novamente so demonstracao...
lines(c(80, 80), c(0, dnorm(80, 100, 10)), col="blue")
lines(c(120, 120), c(0, dnorm(120, 100, 10)), col="red")


#----- 3°)

# a)
dbinom(3, 24, 0.1) + dbinom(4, 24, 0.1) + dbinom(5, 24, 0.1)
# b)
1 - pbinom(1, 24, 0.1, lower.tail = T)
1 - ( dbinom(0, 24, 0.1) + dbinom(1, 24, 0.1) )
pbinom(1, 24, 0.1, lower.tail=F)

#----- 4°)
# a)
pbinom(7, 24, 0.8)

# b)
1 - pbinom(4, 24, 0.8)

#----- 6°)
dbinom(3, 8, 0.55)

#----- 7°)
# a)
def_a <- 0.19 * 0.0015
def_i <- 0.30 * 0.02
def_c <- 0.51 * 0.0275
def_total <- def_a  + def_i  + def_c
def_total

# b)
prob <- def_i / def_total
prob