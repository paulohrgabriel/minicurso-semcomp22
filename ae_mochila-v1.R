# Parâmtros do problema da mochila
lucros = c(10, 20, 15, 2, 30, 10, 30)
pesos  = c( 1,  5, 10, 1,  7,  5,  1)
capacidade = 20

# Cálculo do valor da aptidão do indivíduo
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

ae_mochila <- function(tp, tc, ng) {
  
  # Cria uma população (matrix) de individuos
  pop <- matrix(round(runif(tp * tc)), nrow = tp)
  print(pop)

  # Cria um array para armazenar as aptidões
  fitness <- c()
  
  # Avalia cada indivíduo
  # Neste exemplo, a aptidão é exatamente o valor da soma dos bits
  for (i in 1:nrow(pop)) {
    fitness = c(fitness, mochila(pop[i,]))
  }
  print(max(fitness))

  # Inicia o processo evolutivo
  for (i in 1:ng) {
    reprodutor <- sample(nrow(pop), 2)

    pcross <- sample(tc-1, 1)
    
f1 <- c(pop[reprodutor[1], 1:pcross],
            pop[reprodutor[2], (pcross+1):7])
f2 <- c(pop[reprodutor[2], 1:pcross],
            pop[reprodutor[1], (pcross+1):7])

#print(pop[reprodutor[1],])
#print(pop[reprodutor[2],])
#print(f1)
#print(f2)

pmut <- sample(7, 1, replace=TRUE)

f1[pmut] <- !f1[pmut]

pmut <- sample(7, 1, replace=TRUE)
f2[pmut] <- !f2[pmut]

fitness_f1 <- sum(f1)
fitness_f2 <- sum(f2)

pos <- order(fitness)[1:2]
if (fitness_f1 > fitness[pos[1]]) {
    pop[pos[1],] <- f1
    fitness[pos[1]] = fitness_f1
    
}
if (fitness_f2 > fitness[pos[2]]) {
    pop[pos[2],] <- f2
    fitness[pos[2]] = fitness_f2
    
}
print(max(fitness))
  }
}
