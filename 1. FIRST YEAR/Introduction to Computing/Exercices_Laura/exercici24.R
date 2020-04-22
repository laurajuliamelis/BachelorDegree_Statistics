NomEquipA    <- "Barça"
NomEquipB    <- "Madrid"
PuntsEquipA  <- 75
PuntsEquipB  <- 75
GolsFavorA   <- 80
GolsFavorB   <- 80
GolsContraA  <- 40
GolsContraB  <- 40
GolsAPartit1 <- 2
GolsBPartit1 <- 2
GolsAPartit2 <- 1
GolsBPartit2 <- 1

if(PuntsEquipA > PuntsEquipB){
	cat("L'equip guanyador és:",NomEquipA,"\n")
}else if(PuntsEquipB > PuntsEquipA){
	cat("L'equip guanyador és:",NomEquipB,"\n")
}else{

	DiferenciaGolsA <- GolsFavorA-GolsContraA
	DiferenciaGolsB <- GolsFavorB-GolsContraB

	if(DiferenciaGolsA > DiferenciaGolsB){
		cat("L'equip guanyador és:",NomEquipA,"\n")
	}else if (DiferenciaGolsA < DiferenciaGolsB){
		cat("L'equip guanyador és:",NomEquipB,"\n")
	}else{
		SumaGolsA <- GolsAPartit1+GolsAPartit2
		SumaGolsB <- GolsBPartit1+GolsBPartit2
		
		if( SumaGolsA > SumaGolsB){
			cat("L'equip guanyador és:",NomEquipA,"\n")
		}else if( SumaGolsA < SumaGolsB){
			cat("L'equip guanyador és:",NomEquipB,"\n")
		}else{
			cat("Els dos equips han empatat!\n")
		}
	}
		
}

