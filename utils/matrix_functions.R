rotation <- function(angle, p){
  
  # ROTATION Returns a pxp rotation matrix constructed in the same way as in the GO-GARCH paper by van der Weide (2002).
  
  # allocate storage for rotation matrix
  rot = diag(p)
  
  # Parameter counter
  tal = 1
  
  for (i in 1:(p-1)){
    for (j in (i+1):p){
      #Construct pxp matrix with 2x2 rotation spanning one dimension
      help <- diag(p)
      help[i,i] <- cos(angle[tal])
      help[j,j] <- cos(angle[tal])
      help[i,j] <- sin(angle[tal])
      help[j,i] <- -sin(angle[tal])
      
      #update rotation
      rot = rot %*% help
      tal = tal +1 # update parameter counter
    }
  }
  return(rot)
}