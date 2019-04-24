# Load libraries
library(TeachingSampling)

# load data
data("BigLucy")

# visualizing head
head(BigLucy)

# Summary for count Level's
sm = summary(BigLucy$Level)
count_level = as.numeric(as.vector(sm))

# Especifications
n = 5000
N = sum(count_level)

# Length of stratuns
estratos = (n*count_level)/N

# function to distribute the last elements of sample
get_last_element = function(estratos, n){
  if(sum(floor(estratos)) == n){
    return(estratos)
  }
  else{
    len_ind = length(estratos)
    p = estratos - floor(estratos)
    p = p/sum(p)
    ix = sample(1:len_ind, 1, prob = p)
    estratos_final = floor(estratos)
    add_elem = n - sum(floor(estratos))
    estratos_final[ix] = estratos_final[ix] + add_elem
    return(estratos_final)
  }
}
estratos = get_last_element(estratos, n)

# printing the E.SI for the 3 levels
set.seed(1612)
aas = list()
level = c('Big', 'Medium', 'Small')
for(i in 1:3){
  ix = S.SI(N = count_level[i], n = estratos[i])
  s = BigLucy[BigLucy$Level == level[i], 'Income']
  aas[[i]] = s[ix]
  print(paste('E.SI for Level', level[i], '- Income'))
  print(E.SI(N = count_level[i], n = estratos[i], aas[[i]]))
}

# E.STSI for the 3 levels
M = S.STSI(BigLucy$Level, count_level, estratos)
M1 = BigLucy[M, ]; head(M1)
summary(M1$Level)
ESTSI = E.STSI(M1$Level, count_level, estratos, M1$Income)
ESTSI

# E.STpiPS for the 3 levels
M = S.STpiPS(BigLucy$Level, BigLucy$Income, estratos)
M1 = BigLucy[M, ]; head(M1)
summary(M1$Level)
ESTpiPS = E.STpiPS(M1$Income, M[,2], M1$Level)
ESTpiPS

# E.STPPS for the 3 levels
M = S.STPPS(BigLucy$Level, BigLucy$Income, estratos)
M1 = BigLucy[M$sam, ]; head(M1)
summary(M1$Level)
ESTPPS = E.STPPS(M1$Income, M[,2], estratos, M1$Level)
ESTPPS