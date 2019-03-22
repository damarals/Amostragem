# AAS com reposição sequencial
fSI = function(data, m){
  N = dim(data)[1];
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
