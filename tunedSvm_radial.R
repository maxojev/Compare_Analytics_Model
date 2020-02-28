



tunedSvm_radial = function(train,valid) {
  
  trainset = rbind(train,valid)
 
  perf = 45678903456789945
  perfect_tuned = 0
  vecteurGauss = seq(0.001, 0.3, by= 0.01)
  vecteurC = seq(0.01, 5, by= 0.1)

  for (gaussien_gamma in vecteurGauss){
    
    for (c in vecteurC){
      
      tuned = tune.svm(as.formula(paste(colnames(trainset)[positionCible],"~.",sep="")), data = trainset, gamma = gaussien_gamma, cost = c,kernel = 'radial',tunecontrol=tune.control(cross=10)) 
	  
	  if (tuned$performances$error<perf){
		perf = tuned$performances$error
		perfect_tuned = tuned
	  }
    }
  }
  return(perfect_tuned)
}

tunedSvm_poly = function(train,valid) {
  
  trainset = rbind(train,valid)
  
  perf = 45678903456789945
  perfect_tuned = 0
  for (deg in 2:5){
    
    for (c in 0.0001:100){
      
      tuned = tune.svm(as.formula(paste(colnames(trainset)[positionCible],"~.",sep="")), data = trainset, degree = deg, cost = c,kernel = 'radial',tunecontrol=tune.control(cross=10)) 
      if (tuned$performances$error<perf){
        perf = tuned$performances$error
        perfect_tuned = tuned
      }
    }
  }
  return(perfect_tuned)
}