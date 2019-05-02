# Load libraries
require(TeachingSampling)

# load data
data("BigLucy")

# visualizing head
head(BigLucy)

# HiperParametros
N = nrow(BigLucy)
n = 5000

# Amostragem Bernoulli
amostra_BE_ix = S.BE(N = N, prob = 0.1)
amostra_BE = BigLucy[amostra_BE_ix,]

y_BE = amostra_BE$Income
est_BE = E.BE(y = y_BE, prob = 0.1)

# Amostragem Poisson
pik_PO = (n * BigLucy$Income) / sum(BigLucy$Income)
amostra_PO_ix = S.PO(N = N, Pik = pik_PO)
amostra_PO = BigLucy[amostra_PO_ix,]

y_PO = amostra_PO$Income
est_PO = E.PO(y = y_PO, Pik = pik_PO[amostra_PO_ix])

# Amostragem AAS sem reposição
amostra_SIs_ix = S.SI(N, n)
amostra_SIs = BigLucy[amostra_SIs_ix,]

y_SIs = amostra_SIs$Income
est_SIs = E.SI(N, n, y_SIs)

# Amostragem AAS com reposição
m = 5000
amostra_SIc_ix = S.WR(N = N, m = m)
amostra_SIc = BigLucy[amostra_SIc_ix,]

y_SIc = amostra_SIc$Income
est_SIc = E.WR(N, m, y_SIc)

# Amostram Sistemática
a = N/n
amostra_SY_ix = S.SY(N = N, a = a)
amostra_SY = BigLucy[amostra_SY_ix,]

y_SY = amostra_SY$Income
est_SY = E.SY(N = N, a = a, y = y_SY)

# Amostragem PPT
m = 5000
x = BigLucy$Employees
amostra_PPT_ix = S.PPS(m = m, x = x)
amostra_PPT = BigLucy[amostra_PPT_ix,]

y_PPT = amostra_PPT$Income
pk = amostra_PPT_ix[,2]
est_PPT = E.PPS(y = y_PPT, pk = pk)

# Amostragem piPT
x = BigLucy$Employees
amostra_piPT_ix = S.piPS(n = n, x = x)
amostra_piPT = BigLucy[amostra_piPT_ix,]

y_PPT = amostra_piPT$Income
Pik = amostra_piPT_ix[,2]
est_piPT = E.piPS(y = y_PPT, Pik = Pik)

## Adicionais :)

# Intervalos de Confiança
intervalSampling = function(tipo = 'total', est, confianca = 0.95){
  
  a = est[1, 2]
  b = est[2, 2]
  
  if(tipo == 'mean'){
    N = est[1, 1]
    a = est[1, 2] / N
    b = est[2, 2] / (N^2)
  }
  
  alpha = 1 - confianca
  quantil = qnorm(1 - (alpha/2))
  
  c = quantil * b
  
  Li = a - c
  Ls = a + c
  
  return(cbind(Li, Ls))
}

intervalSampling(tipo = 'mean', est = est_BE)