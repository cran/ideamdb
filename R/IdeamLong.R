#' Create a day by day IDEAM's data serie
#'
#' Create a data frame with one value by row. The df is available to export as
#' a CSV file
#' @param file IDEAM file path or file name if it is on the working directory
#' @param write If True a CSV file is returned to the working directory, otherwise
#'               only a data frame is shown
#' @param outfile Outfile name that will be saved on the working directory
#' @return dataframe or a CSV file
#' @examples
#' # Retreive example dataset
#' Example_IDEAM <- system.file("extdata", "Example_IDEAM", package = "ideamdb")
#' # Create a temporal file
#' example.ideam.long <- tempfile()
#' write.csv(IdeamLong(Example_IDEAM), file = example.ideam.long)
#' read.csv(example.ideam.long)
#'
#' @export
IdeamLong <- function(file, write = FALSE, outfile = "MatrizIdeamLarga"){
  # Create a file to manipulate easily IDEAM's data
  # Crea un archivo para manipular fácilmente los registros del IDEAM
  # Values are organized day by day, therefore it is a long file.
  # Las valores son organizados día a día, por lo cual se obtiene un extenso
  # archivo.
  #
  # Args:
  #  file:  Matrix file obtained with the function IdeamWide of this package.
  #  write: If True, a CSV file is returned to the working directory,
  #  otherwise only a data frame is shown
  #  outfile Outfile name that will be saved on the working directory
  #
  # Returns:
  #  Return a data frame and a CSV file

  outfile <- paste(outfile, ".csv", sep = "")

  db.ideam.wide <- IdeamWide(file, write = FALSE)
  values <- cbind.data.frame(db.ideam.wide[1:6], db.ideam.wide[seq(7, 30, by = 2)])
  colnames(values) <- c("Id", "Estacion", "Codigo","Variable", "Ano",
                        "Dia", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  comments <- cbind.data.frame(db.ideam.wide[1:6], db.ideam.wide[seq(8, 30, by = 2)])
  values.long <- tidyr::gather(values, key = "Mes", value = "Valor", 7:18)
  # Se asignan nulls para cerrar un Warning sobre binding Global Variables
  ObEne <- ObFeb <- ObMar <- ObAbr <- ObMay <- ObJun <- NULL
  ObJul <- ObAgo <- ObSep <- ObOct <- ObNov <- ObDic <- NULL
  comments.long <- tidyr::gather(comments, key = "MesO", value = "Comentario",
                                 ObEne, ObFeb, ObMar, ObAbr, ObMay, ObJun, ObJul,
                                 ObAgo, ObSep, ObOct, ObNov, ObDic)

  db.ideam.long <- cbind.data.frame(values.long, comments.long[8])

  dates <- paste(db.ideam.long$Ano, db.ideam.long$Mes, db.ideam.long$Dia)

  dates.ymd <- as.Date.character(dates, format = "%Y %m %d")
  db.ideam.long <- cbind.data.frame(db.ideam.long[1:5], db.ideam.long[7],
                                    db.ideam.long[6], Fecha = dates.ymd, db.ideam.long[8:9])
  db.ideam.long$Mes <- as.integer(as.character(db.ideam.long$Mes))
  db.ideam.long$Valor <- as.numeric(as.character(db.ideam.long$Valor))

  # se eliminan las fechas no existentes
  db.ideam.long <- db.ideam.long[stats::complete.cases(db.ideam.long$Fecha),]

  db.ideam.long <- db.ideam.long[order(db.ideam.long$Estacion, db.ideam.long$Codigo, db.ideam.long$Variable,
                                db.ideam.long$Fecha),]

  db.ideam.long$Id <- seq(1:length(db.ideam.long$Variable))

  # Para reordenar los rownumber se usa row.names
  row.names(db.ideam.long) <- NULL

  if(write){
    # Escribiendo salida
    utils::write.csv(x = db.ideam.long, file = outfile,
              row.names = FALSE)
  }
  return(db.ideam.long)
}
