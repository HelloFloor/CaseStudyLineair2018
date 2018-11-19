gemeentes_cbs <- data$Gemeente
gemeentes_verkiezingen <- uitslag$Gemeente

for (i in 1:391){
  data_ontbreekt <- T
  for (j in 1:388){
    if(gemeentes_verkiezingen[i]==gemeentes_cbs[j]){
      data_ontbreekt <- F
    }
  }
  if(data_ontbreekt==T){
    print(gemeentes_verkiezingen[i])
  }
}

