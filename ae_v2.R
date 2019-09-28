# Algoritmo evolutivo, versão 2
# Esse algoritmo avalia uma população de indivíduos com base em seu número de bits.
# Aplica mutação e recombinação
# Entrada:
# - tamanho da população  (tp)
# - tamanho do cromossomo (tc)
# - número de gerações    (ng)
# Saída:
# - geração onde a melhor solução foi encontrada
# - número de bits da melhor solução
ae_v2 <- function(tp, tc, ng) {
  
  # Cria uma população de indivíduos
  # Cada cromossomo é uma linha da matriz
  pop <- matrix(round(runif(tp * tc)), nrow = tp)
  print(pop)
  
  # Cria um array para armazenar as aptidões
  fitness <- c()
  
  # Avalia cada indivíduo
  # Neste exemplo, a aptidão é exatamente o valor da soma dos bits
  for (i in 1:nrow(pop)) {
    fitness = c(fitness, sum(pop[i,]))
  }
  
  print(max(fitness))
  
  # Início do processo evolutivo
  
  for (i in 1:ng) {
    # Seleciona dois indivíduos para reprodução
    reprodutor <- sample(nrow(pop), 2)
    
    # Seleciona um ponto para ocorrer o crossover
    pcross <- sample(tc-1, 1)
    
    # Aplica o crossover de 1-ponto, gerando dois filhos
    f1 <- c(pop[reprodutor[1], 1:pcross],
                pop[reprodutor[2], (pcross+1):tc])
    f2 <- c(pop[reprodutor[2], 1:pcross],
                pop[reprodutor[1], (pcross+1):tc])
    
    # Seleciona um gene do filho 1 para sofre mutação
    pmut <- sample(tc, 1, replace=TRUE)
    f1[pmut] <- !f1[pmut]
    
    # Seleciona um gene do filho 2 para sofre mutação
    pmut <- sample(tc, 1, replace=TRUE)
    f2[pmut] <- !f2[pmut]
    
    # Calcula o fitness de cada filho
    fitness_f1 <- sum(f1)
    fitness_f2 <- sum(f2)
    
    # Encontra os dois indivíduos de menor fitness na população
    pos <- order(fitness)[1:2]
    
    # Se os filhos têm melhor fitness que os dois piores indivíduos
    #  então realiza uma substituição
    if (fitness_f1 > fitness[pos[1]]) {
      pop[pos[1],] <- f1
      fitness[pos[1]] = fitness_f1
    }
    if (fitness_f2 > fitness[pos[2]]) {
      pop[pos[2],] <- f2
      fitness[pos[2]] = fitness_f2
    }
    
    # Recalcula o fitness de cada indivíduo da 
    #fitness <- c()
    #for (i in 1:nrow(pop)) {
    #  fitness = c(fitness, sum(pop[i,]))
    #}
    print(max(fitness))
  }
}
