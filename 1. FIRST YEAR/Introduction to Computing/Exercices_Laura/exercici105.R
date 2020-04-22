v <- c(5,7,6,5,4,5,6,7,6,5)

Hi_es <- function (e, v){
  if( length(v) > 0){
    for(i in 1:length(v)){
      if( v[i] == e){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

uniq <- function (v){
  x <- vector()
  
  for ( i in 1: length (v)){
    e <- v[i]
   
     if ( !Hi_es (e,x) ){
      x <- c(x, e)
    }
   
    }
  return(x)
}
