#En el objeto ficheros almacenamos todos los ficheros .fasta que se van a analizar
mypath_in <- "E:/BIOCIÈNCIES/Entregues/4/Solució/Fasta files"
mypath_out <-"E:/BIOCIÈNCIES/Entregues/4/Solució/Fasta files_out" # Folder must exist

ficheros = list.files(path=mypath_in, pattern = ".fa")
#i <- ficheros[1]
for (input.file in ficheros){
  rmarkdown::render("SeqAna_3param_E.Rmd", params = list(ruta.in=mypath_in, ruta.out=mypath_out, nombre.fichero = input.file),
                    output_file = file.path(mypath_out, paste(strsplit(input.file,"\\.")[[1]][1], 
                                                          "_", Sys.Date(), output_format ='.html', sep='')), encoding = "UTF8")
  
}


