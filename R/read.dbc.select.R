#' Read selected variables from a DBC file
#'
#' Decompresses a DBC file and extracts only the requested
#' variables, avoiding the creation of a complete data.frame
#' containing all variables.
#'
#' @param file Path to the DBC file.
#' @param select Character vector containing the variables to read.
#'
#' @return A data.frame containing only the selected variables.
#'
#' @export
read.dbc.select <- function(file, select, col_types = NULL) {
  
  if (!is.character(file) || length(file) != 1L) {
    stop("'file' must be a single character string.")
  }

  if (!file.exists(file)) {
    stop("DBC file does not exist: ", file)
  }

  if (!is.character(select) || length(select) == 0L) {
    stop("'select' must contain at least one variable name.")
  }

  if (anyNA(select) || any(!nzchar(select))) {
    stop("'select' contains invalid variable names.")
  }

  input_file <- normalizePath(
    file,
    winslash = "/",
    mustWork = TRUE
  )

  output_file <- tempfile(
    fileext = ".csv"
  )

  on.exit(
    unlink(output_file),
    add = TRUE
  )

  ret_code <- 0L

  error_str <- paste(
    rep(" ", 256L),
    collapse = ""
  )

  result <- .C(
    "dbc2csv_select",
    input = as.character(input_file),
    output = as.character(output_file),
    fields = as.character(select),
    nfields = as.integer(length(select)),
    ret_code = as.integer(ret_code),
    error_str = as.character(error_str),
    PACKAGE = "read.dbc"
  )

  if (result$ret_code != 0L) {

    error_message <- trimws(result$error_str)

    if (nchar(error_message) == 0L) {
      error_message <- paste(
        "DBC selective reading failed with code",
        result$ret_code
      )
    }

    stop(error_message)
  }

    result <- data.table::fread(
    output_file,
    encoding = "Latin-1",
    data.table = FALSE,
    check.names = FALSE
  )

  if ("DT_NOTIFIC" %in% names(result)) {
    result$DT_NOTIFIC <- as.Date(
      as.character(result$DT_NOTIFIC),
      format = "%Y%m%d"
    )
  }
 
  if (!is.null(col_types)) {

    for (col in names(col_types)) {

      if (!col %in% names(result))
        next

      type <- col_types[[col]]

      if (type == "character") {
        result[[col]] <- as.character(result[[col]])
      }

      if (type == "numeric") {
        result[[col]] <- as.numeric(result[[col]])
      }

      if (type == "integer") {
        result[[col]] <- as.integer(result[[col]])
      }

      if (type == "date") {
  result[[col]] <- as.Date(
    as.character(result[[col]]),
    format = "%Y%m%d"
  )
}
    }
  }

  return(result)
}
