#' @title Prepare data from a chalkboard.json file for plotting
#' @description Formats json chalkboard data for plotting using the `ggplot2` package.
#' @details The output tibble creates points to corresponding to pen/chalk strokes, with the number of points indicated by the density.
#' @import stats
#' @import dplyr
#' @import jsonlite
#' @import purrr
#' @import tibble
#' @import tidyr
#' @param chalkboard_json_filepath path to data that can be read with `jsonlite::fromJSON()`
#' @param board_bg string indicating the background color of the board
#' @param density integer indicating the number of points per pen stroke
#' @return tibble with data formatted for `ggplot2` plotting
#' @seealso \code{\link{plot_malk}}
#' @export
#' @examples
#' 
#' data(chalkboard)
#' 
#' malk(chalkboard)
malk <- function(chalkboard_json_filepath, board_bg = "black", density = 10) {
    raw_chalk_json <- jsonlite::fromJSON(chalkboard_json_filepath)
    board_colors <- tibble::tibble(color = seq(0,6,1), color_string = c("white", "blue", "red", "green", "orange", "purple", "yellow"))
    purrr::pluck(raw_chalk_json, "data")  |> 
      tibble::enframe() |>
      tidyr::unnest_wider(2) |>
      purrr::pluck("events") |>
      purrr::pluck(2) |>
      data.frame() |>
      tibble::as_tibble() |>
      dplyr::left_join(board_colors, by = "color") |> 
      dplyr::mutate(color_string = replace(color_string, type == "erase", "black")) |> 
      dplyr::mutate(color_string = replace(color_string, board_bg != "black" & color == 0, "black")) |> 
      dplyr::mutate(color_string = replace(color_string, board_bg != "black" & type == "erase", board_bg)) |> 
      dplyr::filter(type %in% c("draw","erase")) |> 
      identity() -> chalk_dat
    #
    chalk_dat |>
      dplyr::select(board, time, type, color, color_string, x1, x2, y1, y2) |> 
      stats::na.omit() |> 
      dplyr::group_by(board, time, type, color_string) |>
      dplyr::mutate(xseq = purrr::map2(.x = x1, .y = x2, .f = ~ seq(from = .x, to = .y, length.out = density))) |>
      dplyr::mutate(yseq = purrr::map2(.x = y1, .y = y2, .f = ~ seq(from = .x, to = .y, length.out = density))) |> 
      dplyr::select(-x1,-x2,-y1,-y2) |>
      tidyr::unnest(cols = c(xseq,yseq)) |> 
      dplyr::ungroup() |>
      identity() -> chalk_dat_long
  return(chalk_dat_long)
}


#' @title Plot data from a chalkboard.json file that's been processed with the `malk` function
#' @description Recreates a json chalkboard image using the `ggplot2` package.
#' @details The output `ggplot2` object uses points to recreate pen/chalk strokes, with the number of points indicated by the density.
#' @import purrr
#' @import ggplot2
#' @param malk_output_tibble the output of the `malk` function, which has been applied to data that can be read with `jsonlite::fromJSON()`
#' @param board_bg string indicating the background color of the board
#' @param point_size float indicating the size of points in the penstroke 
#' @return theme added to a `ggplot2` object
#' @seealso \code{\link{malk}}
#' @export
#' @examples
#' data(chalkboard)
#' 
#' malk(chalkboard) |> 
#'   plot_malk()
plot_malk <- function(malk_output_tibble, board_bg = "black", point_size = 0.1) {
  malk_output_tibble |> 
     ggplot2::ggplot(data = _) +
     ggplot2::geom_point(ggplot2::aes(x = xseq, y = yseq, color = color_string, fill = color_string), size = point_size) +
     ggplot2::scale_fill_identity() +
     ggplot2::scale_color_identity() +
     ggplot2::scale_y_reverse() +
     ggplot2::coord_equal() +
     ggplot2::facet_wrap(facets = ~ board) +
     #ggdark::dark_mode(ggplot2::theme_void()) +
     ggplot2::theme_void() +
     ggplot2::theme(panel.background = ggplot2::element_rect(color = board_bg, fill = board_bg), plot.background = ggplot2::element_rect(color = board_bg, fill = board_bg), strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(color = ifelse(board_bg == "black", "white", "black"))) +
     ggplot2::theme(legend.position = "none") |> 
     identity() -> malk_plot
  return(malk_plot)
}

#' a JSON file generated from a quarto chalkboard
#'
#' The *chalkboard* data contains a character variable recognized by `jsonlite` as JSON.
#'
#' @docType data
#' @keywords datasets
#' @name chalkboard
#' @usage data(chalkboard)
#' @format A character variable recognized by `jsonlite` as JSON.
NULL

