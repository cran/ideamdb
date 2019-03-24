#' Create a matrix with IDEAM's data
#'
#' Create a data frame that it is available to export as a CSV file
#' @param file IDEAM file path or file name if it is on the working directory
#' @param write If True a CSV file is returned to the working directory, otherwise
#'               only a data frame is shown
#' @param outfile Outfile name that will be saved on the working directory
#' @return a data frame or a CSV file
#' @examples
#' # Retreive example dataset
#' Example_IDEAM <- system.file("extdata", "Example_IDEAM", package = "ideamdb")
#' # Create a temporal file
#' example.ideam.wide <- tempfile()
#' write.csv(IdeamWide(Example_IDEAM), file = example.ideam.wide)
#' read.csv(example.ideam.wide)
#'
#' @export
IdeamWide <- function(file, write = FALSE, outfile = "MatrizIdeamAncha"){

  # Create a file to manipulate easily IDEAM's data
  # Crea un archivo para manipular fácilmente los registros del IDEAM
  #
  # Args:
  #  file:  IDEAM's file path or file name if it is on the working directory
  #  write: If True a CSV file is returned to the working directory, otherwise
  #         only a data frame is shown
  #  outfile Outfile name that will be saved on the working directory
  #
  # Returns:
  #  Return a data frame and a CSV file

  RelocateComments <- function(value.col, comment.col){
    # Change the symbol "+" from columns' values  to columns' comments
    # Cambiando el + de la columna de valores a la de observaciones
    #
    # Args:
    #  value.col:   data frame column with values
    #  comment.col: data frame with comments associated with values of value.col
    #
    # Reteruns:
    #  Return comment.col with "+" symbol from value.col to the corresponding comment.col position
    auxiliar.vector <- grepl(pattern = "[+]", value.col)
    auxiliar.vector <- gsub(pattern = "FALSE", replacement = "", x = auxiliar.vector)
    auxiliar.vector <- gsub(pattern = "TRUE", replacement = "+", x = auxiliar.vector)
    relocated.comments <- paste(auxiliar.vector, comment.col,sep="")
    return(relocated.comments)
  }

  outfile <- paste(outfile, ".csv", sep = "")

  db.ideam <- readLines(file, skipNul = TRUE)

  # Ubica las filas en las que se encuentra el nombre y el código de la estación
  rows.station <- grep(pattern = "ESTACION : ", x = db.ideam)
  #Toma el nombre y el código de la estación
  station <- substring(db.ideam[rows.station], 105)
  # Se obtiene el código de la estación
  station.code <- gsub(pattern = "[ ]+[A-z]+",replacement = "",x = station)
  #Se deja exclusivamente el nombre de la estación
  station <- gsub(pattern = "[0-9]+ +", replacement = "",x = station)
  #Se obtiene el nombre de la estación
  station.name <- stringr::str_trim((gsub(pattern = "[0-9]{4,}",replacement = " ", x = station)),side = c("both"))
  #Ubica la fila en la que se encuentra el nombre de la variable
  years <- substring(db.ideam[rows.station], 60, 63) #Toma el valor de los años
  rows.variable <- grep("NACIONAL AMBIENTAL", db.ideam)
  # Toma el contenido de la fila dada
  variable <- db.ideam[rows.variable]
  #Se elimina es término Nacional Ambiental
  variable <- gsub(pattern = "NACIONAL AMBIENTAL", replacement = "", x = variable)
  #Se eliminan espacios vacios antes y después del nombre de la variable
  variable <- stringr::str_trim(string = variable ,side = c("both"))

  #cada una de las filas con registros diarios, se acota a los primeros 20 espacios para corregir errores detectados
  rows.values <- grep(pattern = " {10,}[0-9]{2}", x = substring(db.ideam,1,20))

  df.values <- data.frame(Datos = substring(db.ideam[rows.values], 21, 133))

  station.name <- rep(station.name, times = 1, each = 31)
  station.code <- rep(station.code, times = 1, each = 31)
  variable <- rep(variable, times = 1, each = 31)
  years <- rep(years, times = 1, each = 31)

  df.values <- data.frame(Id = 1:length(rows.values),
                          Estacion = station.name,
                          Codigo = station.code,
                          Variable = variable,
                          Ano = years,
                          Dia = rep(seq(1:31), times = length(rows.variable)),
                          Datos = df.values)

  # encontrar direccion del viento
  df.wind.variable <- dplyr::filter(df.values,
                             df.values$Variable == "VALORES MEDIOS(V) DIARIOS DE VELOCIDAD DEL VIENTO (m/s)")
  df.remaining.variables <- dplyr::filter(df.values,
                                   df.values$Variable != "VALORES MEDIOS(V) DIARIOS DE VELOCIDAD DEL VIENTO (m/s)")

  wind.values <- df.wind.variable$Datos
  remaining.values <- df.remaining.variables$Datos

  df.wind.values <- data.frame(Ene = substring(wind.values, 4,7), ObEne = stringr::str_trim(substring(wind.values, 1,2)),
                               Feb = substring(wind.values, 13,16), ObFeb = stringr::str_trim(substring(wind.values, 10,11)),
                               Mar = substring(wind.values, 22,25), ObMar = stringr::str_trim(substring(wind.values, 19,20)),
                               Abr = substring(wind.values, 31,34), ObAbr = stringr::str_trim(substring(wind.values, 28,29)),
                               May = substring(wind.values, 40,43), ObMay = stringr::str_trim(substring(wind.values, 37,38)),
                               Jun = substring(wind.values, 49,52), ObJun = stringr::str_trim(substring(wind.values, 46,47)),
                               Jul = substring(wind.values, 58,61), ObJul = stringr::str_trim(substring(wind.values, 55,56)),
                               Ago = substring(wind.values, 67,70), ObAgo = stringr::str_trim(substring(wind.values, 64,65)),
                               Sep = substring(wind.values, 76,79), ObSep = stringr::str_trim(substring(wind.values, 73,74)),
                               Oct = substring(wind.values, 85,88), ObOct = stringr::str_trim(substring(wind.values, 82,83)),
                               Nov = substring(wind.values, 94,97), ObNov = stringr::str_trim(substring(wind.values, 91,92)),
                               Dic = substring(wind.values, 103,106), ObDic = stringr::str_trim(substring(wind.values, 100,101)))

  df.remaining.values <- data.frame(Ene = substring(remaining.values, 1,5), ObEne = stringr::str_trim(substring(remaining.values, 7,8)),
                                    Feb = substring(remaining.values, 10,14), ObFeb = stringr::str_trim(substring(remaining.values, 16,17)),
                                    Mar = substring(remaining.values, 19,23), ObMar = stringr::str_trim(substring(remaining.values, 25,26)),
                                    Abr = substring(remaining.values, 28,32), ObAbr = stringr::str_trim(substring(remaining.values, 34,35)),
                                    May = substring(remaining.values, 37,41), ObMay = stringr::str_trim(substring(remaining.values, 43,44)),
                                    Jun = substring(remaining.values, 46,50), ObJun = stringr::str_trim(substring(remaining.values, 52,53)),
                                    Jul = substring(remaining.values, 55,59), ObJul = stringr::str_trim(substring(remaining.values, 61,62)),
                                    Ago = substring(remaining.values, 64,68), ObAgo = stringr::str_trim(substring(remaining.values, 70,71)),
                                    Sep = substring(remaining.values, 73,77), ObSep = stringr::str_trim(substring(remaining.values, 79,80)),
                                    Oct = substring(remaining.values, 82,86), ObOct = stringr::str_trim(substring(remaining.values, 88,89)),
                                    Nov = substring(remaining.values, 91,95), ObNov = stringr::str_trim(substring(remaining.values, 97,98)),
                                    Dic = substring(remaining.values, 100,104), ObDic = stringr::str_trim(substring(remaining.values, 106,107)))

  df.wind.variable <- cbind.data.frame(df.wind.variable, df.wind.values)
  df.wind.variable <- df.wind.variable[, -7] # eliminar la fila datos originales

  df.remaining.variables <- cbind.data.frame(df.remaining.variables, df.remaining.values)
  df.remaining.variables <- df.remaining.variables[, -7] # eliminar la fila datos originales

  # unir ambas matrices
  db.ideam.wide <- rbind.data.frame(df.remaining.variables, df.wind.variable)



  #for (i in 7:29){    ## aquí estaba el problema porque se estaba recorriendo cada columna y era necesario columna de por medio.
  for(i in seq(7, 29, by = 2)){
    db.ideam.wide[,(i+1)] <- RelocateComments(db.ideam.wide[,i],
                                              db.ideam.wide[,(i+1)])
  }

  # eliminando el + de los campos de valores
  for(i in seq(7, 29, by = 2)){
    db.ideam.wide[,i] <- suppressWarnings(as.numeric(as.character(db.ideam.wide[,i])))
  }

  # Unificando datos IDEAM
  db.ideam.wide <- db.ideam.wide[order(db.ideam.wide$Id),]

  if(write){
    # Escribiendo salida
    utils::write.csv(x = db.ideam.wide, file = outfile,
              row.names = FALSE)
  }
  return(db.ideam.wide)
}
