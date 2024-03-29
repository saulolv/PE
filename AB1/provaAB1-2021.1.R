xx = 2+0+2+1+1+5+5+1
yy = 51

# 1- O Nascimento de qu�ntuplos � raro, mas considere que aconteceu em Macei� em 2022. Considerando que tenham vida saud�vel e que a probabilidade de um alagoano completar (XX+50) anos seja de 2/3. Calcular a probabilidade que daqui h� (XX+50) anos:
  prob = 2/3
  n = 5
  k = 2  
  # a) Exatamente dois dos qu�ntuplos estejam vivos;
    dbinom(k, size = n, 2/3)
  
  # b) Pelo menos tr�s qu�ntuplos estejam vivos.
    pbinom(2, size = n, prob = 2/3, lower.tail = F)
  
# 2- Considere W uma vari�vel aleat�ria com distribui��o normal com m�dia 100 e vari�ncia 100. Calcule:
  esperanca = 100
  dp = sqrt(100)
  
  # a) P ( W < XX)
    pnorm(xx, mean = esperanca, sd = dp, lower.tail = T)
    
    # grafico
      grafico <- curve(dnorm(x, esperanca, dp), esperanca-3*dp, esperanca+3*dp)
      # meu xx � muito menor que a esperan�a, e nesse plot vai de 70 a 130, entao 19 nao aparecer�
      # portanto eu coloquei 110 aleatoriamente so para demonstrar
      lines(c(110, 110), c(0, dnorm(110, 100, 10)), col="red")
  
  # b) P ( W ??? XX+20)
    pnorm(17+20, esperanca, dp, lower.tail = F)
      
    # grafico
      grafico
      # aqui tambem vai servir s� para demonstracao ja que 39 eh um numero mt pequeno
      lines(c(80, 80), c(0, dnorm(80, 100, 10)), col="blue")
    
  # c) P [ (XX -10) ??? W ??? (XX+ 30) ]
    pnorm(17+30, 100, 10) - pnorm(17-10, 100, 10)
      
      # grafico
        grafico
        # novamente so demonstracao...
        lines(c(80, 80), c(0, dnorm(80, 100, 10)), col="blue")
        lines(c(120, 120), c(0, dnorm(120, 100, 10)), col="red")
        
# 3- Em uma fabrica��o artesanal de componentes ceramicos 10% apresentam defeitos. Calcular a probabilidade de, em um lote de XX + 5 componentes, encontrar:
 
  # a) Entre 3 e 5 componentes estejam defeituosos, inclusive.
    pbinom(5, size=17+5, prob = 1/10, lower.tail = T) - pbinom(2, size=17+5, prob = 1/10, lower.tail = T)
  # b) Pelo menos dois componentes defeituosos.
    pbinom(1, size=17+5, prob = 1/10, lower.tail = F)
        
# 4- Uma aula intensiva em um final de semana aumentou os conhecimentos dos alunos em 80%. Se XX alunos participam do intensivo, calcular a probablide de:

  # a) N�o mais do sete alunos aumentarem seus conhecimentos.
    pbinom(7, size = 17, prob = 0.8, lower.tail = T)
  # b) Pelo menos cinco alunos aumentarem seus conhecimentos.
    pbinom(4, size = 17, prob = 0.8, lower.tail = F)
      
# 5- Suponha que (XX + 90) falhas de portugu�s est�o distribu�das aleatoriamente em uma disserta��o de mestrado de 200 p�ginas. Encontre a probabilidade de que em 3 p�ginas contenham:
  Clambda = (17+90) / 200
  
  # a) Cinco falhas de portugu�s.
    dpois(5, lambda = Clambda*3)
    
  # b) Duas ou mais falhas de portugu�s.
    ppois(1, lambda = Clambda*3, lower.tail = F)

# 6- Uma empresa adquiriu 27 impressoras, das quais 15 s�o impressoras Epson � laser e 12 s�o Epson Ecotank L396. Oito das 25 foram aleatoriamente selecionadas para verifica��o t�cnica, qual a probabilidade de que exatamente tr�s delas sejam a laser?
  pEL = 15/27
  dbinom(3, 8, 15/27)
  
# 7- O minist�rio da sa�de adquiriu vacinas de tr�s fabricantes estrangeiros. O fabricante
#  Americano � XX % do total entregue, o Ingl�s por 30 e o Chin�s yy % (yy = 100 - xx - 30) o
#  complemento para 100 % do total entregue. Cada fabricante, no entanto, produz uma
#  propor��o de produtos com defeituosos na propor��o de 1,5%, 2,0% e 2,75%,
#  respectivamente. No minist�rio da sa�de � feito a inspe��o de qualidade.
  def_am = 0.17*0.015
  def_in = 0.30*0.02
  def_ch = (100-17-30)/100 * 0.0275
  
  # a) Calcular a probabilidade de encontrar uma vacina defeituosa durante a inspe��o?
    def_total = def_am + def_in + def_ch
    def_total
  
  # b) Encontrado uma vacina com defeito, qual a probabilidade de ser inglesa?
    prob = def_in / def_total
    prob