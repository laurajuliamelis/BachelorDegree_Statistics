#####################
# wrap the call to render in an R function,
################################

renderMyDocument <- function(ruta.in, ruta.out, nombre.fichero) {
  rmarkdown::render("SeqAna_3param_E.Rmd", params = list(ruta.in=mypath_in, 
                                                         ruta.out=mypath_out, 
                                                         nombre.fichero = input.file),
                    output_file = file.path(mypath_out, paste(strsplit(input.file,"\\.")[[1]][1],
                                  "_", Sys.Date(), output_format ='.html', sep='')), encoding = "UTF8")
}




mypath_in <- "E:/BIOCIÈNCIES/Entregues/4/Solució/Fasta files"
mypath_out <-"E:/BIOCIÈNCIES/Entregues/4/Solució/Fasta files_out" # Folder must exist

ficheros = list.files(path=mypath_in, pattern = ".fa")

#i <- ficheros[1]
for (input.file in ficheros){
  cat("procesando: ", input.file)
  renderMyDocument(mypath_in, mypath_out, input.file)
}

