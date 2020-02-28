dessiner_frontiere_NN = function(X,y,model, xm,xM,ym,yM, cols){
  
  dev.new()
  if(length(table(y))==2){
    plot(X[which(y==0),1:2], col = cols[1], xlim = c(xm,xM), ylim = c(ym,yM))
    points(X[which(y==1),1:2], col = cols[2])
  }
  else{
    plot(X[which(y==1),1:2], col = cols[1], xlim = c(xm,xM), ylim = c(ym,yM))
    for(j in 2:length(table(y))){
      
      points(X[which(y==j),1:2], col = cols[j])
      
    }
    
  }	
  nb = 60
  x1 = seq(xm,xM, length.out = nb)
  x2 = seq(ym, yM, length.out = nb)
  
  for(i in 1:nb){
    for(j in 1:nb){
      p= ifelse(predict(model, rbind(c(x1[i],x2[j])))>0.5,1,0)
    if(length(table(y))==2){
        if(p==0){points(x1[i],x2[j], col = cols[1], pch = 3)}
        else{points(x1[i],x2[j], col = cols[2], pch = 3)}
      }
      else{
        
        points(x1[i],x2[j], col = cols[p], pch = 3)
        
      }
      
      
      
      
      
    }
  }	
  
  
  
}