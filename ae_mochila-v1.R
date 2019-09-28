# Parâmtros globais do problema da mochila
lucros = c(10, 20, 15, 2, 30, 10, 30)
pesos  = c( 1,  5, 10, 1,  7,  5,  1)
capacidade = 20

# Função objetivo
# Entrada: Um cromossomo binário
# Saída:
# - valor do lucro da mochila, se a soma dos pesos respeita sua capacidade
# - 0, caso contrário
mochila <- function(cromossomo) {
    peso = 0
    lucro = 0
    
    # Caso o item seja selecionado, calcula o peso da mochila
    #  e o lucro obtido
    for (i in 1:length(cromossomo)) {
        if (cromossomo[i] == 1) {
            peso <- peso + pesos[i]
            lucro <- lucro + lucros[i]
        }
    }

    # Caso o peso dos itens exceda o da mochila,
    #  penaliza a solução, atribuindo-lhe fitness = 0
    if (peso > capacidade) {
        return(0)
    } else {
        return(lucro)
    }
}

# Algoritmo evolutivo para o problema da mochila
# Esse algoritmo avalia uma população de indivíduos
# Aplica os operadores genéticos
# Entrada:
# - tamanho da população    (tp)
# - tamanho do cromossomo   (tc)
# - número de gerações      (ng)
# Saída:
# - média e desvio-padrão da população final
# - também imprime a população final
ae_mochila <- function(tp, tc, ng) {

  # Cria uma população (matrix) de individuos
  pop <- matrix(round(runif(tp * tc)), nrow = tp)

  # Cria um array para armazenar as aptidões
  fitness <- c()
  
  # Avalia cada indivíduo
  # Neste exemplo, a aptidão é exatamente o valor da soma dos bits
  for (i in 1:nrow(pop)) {
    fitness = c(fitness, mochila(pop[i,]))
  }

  # Imprime a aptidão do melhor indivíduo, do pior e a média
  cat(0, "\t", max(fitness), "\t", mean(fitness), "\t", min(fitness), "\n")
    
  # Inicia o processo evolutivo
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
    fitness_f1 <- mochila(f1)
    fitness_f2 <- mochila(f2)
    
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
    
    # Imprime a aptidão do melhor indivíduo, do pior e a média
    cat(i, "\t", max(fitness), "\t", mean(fitness), "\t", min(fitness), "\n")
  }
  
  cat("\n População final:\n")
  print(pop)
  
  return(c(mean(fitness), sd(fitness)))
}
