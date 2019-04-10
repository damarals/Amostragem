fSI = function(data, m){
  N = length(data);
  arr_n = rep(0, N);
  for(i in 1:N){
    size = m - sum(arr_n);
    prob = 1/(N - i + 1);
    arr_n[i] = rbinom(1, size, prob);
    if(size == 0){
      break;
    }
  }
  return(arr_n);
}



N = c(10, 50, 100, 500, 1000)
m = c(0.1, 0.2, 0.4)

frequencia_de_elementos = list()
contador = 1

for (tamanho_populacional in 1:length(N)){
  
  for(proporcao_amostral in 1:length(m)){
    
    N_atual = N[tamanho_populacional]
    m_atual = N_atual * m[proporcao_amostral]
    
    frequencia_de_elementos[[contador]] = rep(0,N_atual)
    
    for(i in 1:1000){
      set.seed(385802 + 1612*N_atual + i*1987)
      populacao_atual = runif(n = N_atual, min = 1, max = 100)
      
      amostra_atual = fSI(populacao_atual, m_atual) 
    
      frequencia_de_elementos[[contador]] = frequencia_de_elementos[[contador]] + amostra_atual
      
    }
    contador = contador + 1
  }
  
}


aux = list(1:10,1:10,1:10, c(1:10, 41:50),c(1:10, 41:50),c(1:10, 41:50),c(1:10, 41:50, 91:100),c(1:10, 41:50, 91:100),c(1:10, 41:50, 91:100), c(1:10, 41:50, 91:100, 491:500),c(1:10, 41:50, 91:100, 491:500),c(1:10, 41:50, 91:100, 491:500),c(1:10, 41:50, 91:100, 491:500, 991:1000),c(1:10, 41:50, 91:100, 491:500, 991:1000),c(1:10, 41:50, 91:100, 491:500, 991:1000))



nomes = vector()

for(i in N){
  for(j in m){
    nomes = append(nomes, paste('N:', i, 'm:', j))
  }
}

d = data.frame(k = aux[[15]])


for(i in 1:15){
    
    frequencias = frequencia_de_elementos[[i]][aux[[i]]]
    d[[nomes[i]]] = c(frequencias, rep("-", 50 - length(frequencias)))
    
}




