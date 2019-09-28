# Segunda versão do algoritmo evolutivo para Problema da Mochila
# Utiliza o pacote GA, que precisa ser instalado:
# install.package("GA")

# Carregando pacote GA
require(GA)

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
resultado <- ga(
  type = "binary",        # Define que o cromossomo será binário
  fitness = mochila,      # Define a função objetivo (no caso, a função mochila()
  nBits = length(lucros)  # Define o tamanho do cromossomo
  )                       # Os demais parâmetros foram mantidos como padrão
  
# Mostra um resumo da execução do algoritmo genético
summary(resultado)

# Gera um gráfico com o histórico da evolução
plot(resultado)
