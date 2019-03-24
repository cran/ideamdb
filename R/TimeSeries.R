#' Graphical exploratory charts
#'
#' Create a time series chart and a boxplot of every data sample
#' @param file IDEAM file path or file name if it is on the working directory
#' @param station a single station name or type "all" or "todas" to plot everything
#' @param variable a single variable name or type "all" or "todas" to plot everything
#'               only a data frame is shown
#' @return time series plot and boxplot for every variable data and station
#' @examples
#' Example_IDEAM <- system.file("extdata", "Example_IDEAM", package = "ideamdb")
#' TimeSeries(Example_IDEAM, station = "LUCERO")
#' # Print time series chart and boxplots for all variables
#' # collected in an specific station

#'
#' @export
TimeSeries <- function(file, station = "all", variable = "all") {
  # This funtion plots time series and boxplots of dataframe db.ideam.long
  #
  # Args:
  #  file: IDEAM file path or file name if it is on the working directory
  #  station: character with station's name, type "all" or "todas" as entry to print all data
  #  variable: character with variable's name, type "all" or "todas" as entry to print all data
  #
  # Returns:
  #  time series and boxplots of station and variable select or
  #  all stations and variables

  db.ideam.long <- IdeamLong(file, write = FALSE)
  if (station %in% c("todas", "all") && variable %in% c("todas", "all")) {
    stations <- unique(db.ideam.long$Estacion)
    variables <- unique(db.ideam.long$Variable)

    for (i in 1:length(stations)){
      for(j in 1:length(variables)){
        station.i <- stations[i]
        variable.i <- variables[j]
        data.plot <- subset(db.ideam.long, db.ideam.long$Estacion == station.i &
                              db.ideam.long$Variable == variable.i)
        if (length(data.plot$Variable) == 0){
          NULL
        } else {
          ts.graph <- ggplot2::ggplot(data.plot, ggplot2::aes(data.plot$Fecha, data.plot$Valor)) +
            ggplot2::geom_line() + ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                                axis.title.x = ggplot2::element_text(size = 8)) +
            ggplot2::xlab("A\u00F1os") + ggplot2::ylab(variable.i) + ggplot2::ggtitle(station.i)
          graphics::plot(ts.graph)
          ts.boxplot <- ggplot2::ggplot(data.plot, ggplot2::aes(factor(data.plot$Mes), data.plot$Valor)) +
            ggplot2::geom_boxplot() +
            ggplot2::stat_summary(fun.y = mean, geom = "point", col = "blue", cex= 2.0, pch=19)+
            ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                  axis.title.x = ggplot2::element_text(size = 8)) +
            ggplot2::ylab(variable.i) + ggplot2::xlab("Mes")+
            ggplot2::ggtitle (station.i) +
            ggplot2::theme(legend.position = "bottom")
          graphics::plot(ts.boxplot)
        }
      }
    }
  } else if (station %in% c("todas", "all") && !(variable %in% c("todas", "all"))) {
    stations <- unique(db.ideam.long$Estacion)

    for (i in 1:length(stations)){
      station.i <- stations[i]
      data.plot <- subset(db.ideam.long, db.ideam.long$Estacion == station.i &
                            db.ideam.long$Variable == variable)
      if (length(data.plot$Variable) == 0){
        NULL
      } else {
        ts.graph <- ggplot2::ggplot(data.plot, ggplot2::aes(data.plot$Fecha, data.plot$Valor)) +
          ggplot2::geom_line() + ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                              axis.title.x = ggplot2::element_text(size = 8)) +
          ggplot2::xlab("A\u00F1os") + ggplot2::ylab(variable) + ggplot2::ggtitle(station.i)
        graphics::plot(ts.graph)
        ts.boxplot <- ggplot2::ggplot(data.plot, ggplot2::aes(factor(data.plot$Mes), data.plot$Valor)) +
          ggplot2::geom_boxplot() +
          ggplot2::stat_summary(fun.y = mean, geom = "point", col = "blue", cex= 2.0, pch=19)+
          ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                axis.title.x = ggplot2::element_text(size = 8)) +
          ggplot2::ylab(variable) + ggplot2::xlab("Mes")+
          ggplot2::ggtitle (station.i) +
          ggplot2::theme(legend.position = "bottom")
        graphics::plot(ts.boxplot)
      }
    }
  } else if (!(station %in% c("todas", "all")) && variable %in% c("todas", "all")){
    variables <- unique(db.ideam.long$Variable)

    for (i in 1:length(variables)){
      variable.i <- variables[i]
      data.plot <- subset(db.ideam.long, db.ideam.long$Estacion == station &
                            db.ideam.long$Variable == variable.i)
      if (length(data.plot$Variable) == 0){
        NULL
      } else {
        ts.graph <- ggplot2::ggplot(data.plot, ggplot2::aes(data.plot$Fecha, data.plot$Valor)) +
          ggplot2::geom_line() + ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                              axis.title.x = ggplot2::element_text(size = 8)) +
          ggplot2::xlab("A\u00F1os") + ggplot2::ylab(variable.i) + ggplot2::ggtitle(station)
        graphics::plot(ts.graph)
        ts.boxplot <- ggplot2::ggplot(data.plot, ggplot2::aes(factor(data.plot$Mes), data.plot$Valor)) +
          ggplot2::geom_boxplot() +
          ggplot2::stat_summary(fun.y = mean, geom = "point", col = "blue", cex= 2.0, pch=19)+
          ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                axis.title.x = ggplot2::element_text(size = 8)) +
          ggplot2::ylab(variable.i) + ggplot2::xlab("Mes")+
          ggplot2::ggtitle (station) +
          ggplot2::theme(legend.position = "bottom")
        graphics::plot(ts.boxplot)
      }
    }
  } else {
    data.plot <- subset(db.ideam.long, db.ideam.long$Estacion == station &
                          db.ideam.long$Variable == variable)
    if (length(data.plot$Variable) == 0){
      NULL
    } else {
      ts.graph <- ggplot2::ggplot(data.plot, ggplot2::aes(data.plot$Fecha, data.plot$Valor)) +
        ggplot2::geom_line() + ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                            axis.title.x = ggplot2::element_text(size = 8)) +
        ggplot2::xlab("A\u00F1os") + ggplot2::ylab(variable) + ggplot2::ggtitle(station)
      graphics::plot(ts.graph)
      ts.boxplot <- ggplot2::ggplot(data.plot, ggplot2::aes(factor(data.plot$Mes), data.plot$Valor)) +
        ggplot2::geom_boxplot() +
        ggplot2::stat_summary(fun.y = mean, geom = "point", col = "blue", cex= 2.0, pch=19)+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
              axis.title.x = ggplot2::element_text(size = 8)) +
        ggplot2::ylab(variable) + ggplot2::xlab("Mes")+
        ggplot2::ggtitle (station) +
        ggplot2::theme(legend.position = "bottom")
      graphics::plot(ts.boxplot)
    }
  }
}
