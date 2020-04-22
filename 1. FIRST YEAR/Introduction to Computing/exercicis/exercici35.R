for (i in 100:999){
	u <- i %%10
	d <- (i%%100)%/%10
	cent <- i%/%100
	
	if(cent+d == u){
		cat(i,"\n")
	}
}