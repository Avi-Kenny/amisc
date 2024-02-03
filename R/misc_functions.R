#' Alias for as.integer function
#'
#' @description Alias for as.integer function; see \code{as.integer}
#'     documentation for more info.
#' @param x object to be coerced or tested
#' @return Integer output
#' @examples
#' In(3!=4)
#' @export
In <- as.integer



#' Logit function
#'
#' @description Computes the logit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' logit(0.9)
#' @export
logit <- function(x) { log(x/(1-x)) }



#' Expit function
#'
#' @description Computes the expit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' expit(3)
#' @export
expit <- function(x) { 1 / (1+exp(-x)) }



#' Derivative of the logit function
#'
#' @description Computes the derivative of the logit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' deriv_logit(0.9)
#' @export
deriv_logit <- function(x) { 1 / (x-x^2) }



#' Derivative of the expit function
#'
#' @description Computes the derivative of the expit function
#' @param x Numeric input
#' @return Numeric output
#' @examples
#' deriv_expit(3)
#' @export
deriv_expit <- function(x) { exp(x) / ((1+exp(x))^2) }



#' Debugging checks
#'
#' @description A helper function for debugging that parses and outputs a
#'     message containing a number, a timestamp, and (optionally) a message.
#' @param num A number to be printed (useful to reference a specific place in a
#'     code file
#' @param msg A message to be printed
#' @return NULL
#' @examples
#' chk(1, "code execution began")
#' x <- runif(10)
#' chk(2, "runif completed")
#' y <- rnorm(10)
#' chk(3, "rnorm completed")
#' @export
chk <- function(num, msg="") {
  if (msg=="") {
    str <- paste0("Check ", num, ": ", Sys.time())
  } else {
    str <- paste0("Check ", num, " (", msg, "): ", Sys.time())
  }
  message(str)
}



#' Memoise a function
#'
#' @description This is a lightweight and faster version of the \code{memoise}
#'     function from the \code{memoise} package. The speed comes at the cost of
#'     fewer error handling checks.
#' @param fnc A function to be memoised
#' @return A memoised version of function
#' @examples
#' f <- function(x) { Sys.sleep(1); return(x^2); }
#' f_mem <- memoise2(f)
#' system.time(f_mem(4)) # First call
#' system.time(f_mem(4)) # Second call
#' @export
memoise2 <- function(fnc) {

  htab <- new.env()
  ..new_fnc <- function() {
    ..e <- parent.env(environment())
    ..mc <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    key <- rlang::hash(..mc)
    val <- ..e$htab[[key]]
    if (is.null(val)) {
      val <- do.call(..e$fnc, ..mc)
      ..e$htab[[key]] <- val
    }
    return(val)
  }

  # Set formals and set up environment
  formals(..new_fnc) <- formals(fnc)
  f_env <- new.env(parent=environment(fnc))
  f_env$arg_names <- names(formals(fnc))
  f_env$htab <- htab
  f_env$fnc <- fnc
  environment(..new_fnc) <- f_env

  return(..new_fnc)

}

#' Visualize one or more one-dimensional distributions
#'
#' @description Produce a grid of one-dimensional jittered scatterplots.
#' @param val A vector of numeric data values
#' @param grp A vector denoting groups
#' @param ref_vals A named vector denoting reference values to be overlaid on
#'     plot; see examples
#' @param common_scale If TRUE, a common axis scale is used for all groups
#' @return A ggplot2 object
#' @examples
#' data(iris)
#' dis_plots(
#'   val = iris$Petal.Length,
#'   grp = iris$Species
#' )
#' dis_plots(
#'   val = iris$Petal.Length,
#'   grp = iris$Species,
#'   ref_vals = c(setosa=1.5, versicolor=4, virginica=5.5)
#' )
#' @export
dis_plot <- function(val, grp, ref_vals=NULL, common_scale=TRUE) {

  df_plot <- data.frame(
    x = val,
    grp = grp,
    y = rep(0, length(val))
  )

  # Export 8" x 5"
  plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x=x, y=y, color=factor(grp)))
  if (!is.null(ref_vals)) {
    df_ref <- data.frame(
      grp = names(ref_vals),
      x = as.numeric(ref_vals)
    )
    plot <- plot + ggplot2::geom_vline(ggplot2::aes(xintercept=x), df_ref,
                                       alpha=0.5) # linetype="dashed"
  }
  plot <- plot + ggplot2::geom_jitter(width=0, height=1, alpha=0.3, size=3) +
    ggplot2::labs(x=NULL, y=NULL) +
    ggplot2::ylim(-2,2) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle=0),
      legend.position = "none"
    )

  if (common_scale) {
    plot <- plot + ggplot2::facet_wrap(~grp, ncol=1, strip.position="left")
  } else {
    plot <- plot + ggplot2::facet_wrap(~grp, ncol=1, strip.position="left",
                                       scales="free_x")
  }

  return(plot)

}



#' Return a vector of colorblind-friendly colors
#'
#' @description Returns a vector of colorblind-friendly colors
#' @return A vector of colorblind-friendly colors
#' @examples
#' cb_colors()
#' @export
cb_colors <- function() {
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999")
}
