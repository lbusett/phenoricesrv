#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param in_data PARAM_DESCRIPTION
#' @param GP_VAR PARAM_DESCRIPTION
#' @param X_VAR PARAM_DESCRIPTION
#' @param Y_VAR PARAM_DESCRIPTION
#' @param common_range PARAM_DESCRIPTION, Default: TRUE
#' @param title PARAM_DESCRIPTION, Default: NULL
#' @param subtitle PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lb_plotregr
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom data.table as.data.table setkey
#' @importFrom dumbify dumbify
#' @importFrom ggplot2 ggplot geom_point geom_smooth facet_wrap geom_blank
#'   geom_text scale_x_date scale_y_date scale_x_continuous scale_y_continuous
#'   ggtitle expand_scale
lb_plotregr <- function(in_data,
                        X_VAR,
                        Y_VAR,
                        GP_VAR = NULL,
                        LABEL_VAR = NULL,
                        nudge_y  = 0,
                        common_range = TRUE,
                        title = NULL,
                        subtitle = NULL) {

  #   __________________________________________________________________________
  #   Perform regression analysis to get info for the labels                ####

  regr <- lb_linregr(in_data, X_VAR, Y_VAR, GP_VAR)

  #   __________________________________________________________________________
  #   Dumbify columns to avoid scoping problems                             ####

  in_data  <- dumbify::dumbify(in_data, c(X_VAR, Y_VAR))

  if (!is.null(GP_VAR)) {
    in_data  <- dumbify::dumbify(in_data, c(GP_VAR))
    regr     <- dumbify::dumbify(regr, c(GP_VAR))
  }

  if (!is.null(LABEL_VAR)) {
    in_data  <- dumbify::dumbify(in_data, c(LABEL_VAR))
  }
browser()
  #   __________________________________________________________________________
  #   Compute the ranges to be used if neeeded                              ####
  if (common_range & !is.null(GP_VAR)) {

    ranges <- data.table::as.data.table(in_data) %>%
      data.table::setkey(GP_VAR) %>%
      .[, list(ymin = min(Y_VAR, na.rm = TRUE),
               ymax = max(Y_VAR, na.rm = TRUE),
               xmin = min(X_VAR, na.rm = TRUE),
               xmax = max(X_VAR, na.rm = TRUE)) , by = GP_VAR]

    ranges[["range"]]     <- as.numeric(ranges[["ymax"]] - ranges[["ymin"]])
    ranges[["mid"]]       <- ranges[["ymin"]] + ranges[["range"]]/2
    max_range             <- max(ranges[["range"]], na.rm = T)
    ranges[["facet_min"]] <- ranges[["mid"]] - max_range/2
    ranges[["facet_max"]] <- ranges[["mid"]] + max_range/2

    keep_cols <- c("GP_VAR", "xmin", "xmax", "facet_min", "facet_max")
    dummy <- ranges %>%
      .[, ..keep_cols]  %>%
      data.table::melt(id.vars = c("GP_VAR", "xmin", "xmax"),
           measure_vars = c("facet_min", "facet_max"),
           value.name = "Y_VAR",
           variable.name = "dummy1") %>%
      data.table::melt(id.vars = c("GP_VAR", "dummy1", "Y_VAR"),
           measure.vars = c("xmin","xmax"),
           value.name = "X_VAR",
           variable.name = "dummy2")

  }


  #   ____________________________________________________________________________
  #   build the plot                                                          ####

  plot <- ggplot(in_data, aes(x = X_VAR, y = Y_VAR)) +
    geom_point() +
    geom_smooth(method = "lm", se = T, color = "black")

  if (!is.null(GP_VAR)) {
    # plot <- plot + facet_wrap(~ GP_VAR, ncol = 2, scales = "free")
    plot <- plot + facet_wrap(~ GP_VAR, ncol = 2)
    if (common_range){
      plot <- plot + geom_blank(data = dummy)
    }
  }

  plot <- plot + geom_text(data = regr, x = -Inf, y = Inf, aes(label = lab),
                           hjust = -0.05, vjust = 1.5)

  if (class(in_data[["X_VAR"]]) == "Date") {
    plot <- plot + scale_x_date(X_VAR, date_breaks = "10 days",
                                date_labels = "%b %d")
  } else {
    plot <- plot + xlab(X_VAR)
  }

  if (class(in_data[["Y_VAR"]]) == "Date") {
    plot <- plot + scale_y_date(Y_VAR, date_breaks = "10 days",
                                date_labels = "%b %d",
                                expand = expand_scale(mult = c(0.05, 0.1)))
  } else {
    plot <- plot + scale_y_continuous(Y_VAR,
                                      expand = expand_scale(mult = c(0.05, 0.1)))
  }

  if (!is.null(title)) {
    plot <- plot + ggtitle(title,
                           subtitle = subtitle )
  }

  if (!is.null(LABEL_VAR)) {
    plot <- plot + geom_text(aes(label = LABEL_VAR), nudge_y = nudge_y)
  }
  plot

}
