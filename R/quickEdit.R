#' Quickly edit R data object in system's default program.
#'
#' This function exports and opens a data object in your system's default editor for .csv or .txt files, then reloads the data to its original variable when changes to the file are saved.
#'
#' @param x The variable for your R object.
#'
#' @examples
#' quickEdit(df)
#'
#' @export
quickEdit <- function(x) {
  z <- x
  if (class(x) %in% c("data.frame", "tbl", "list")) {
    ext <- ".csv"
    colNames <- TRUE
  } else {
    ext <- ".txt"
    colNames <- FALSE
  }

  file <- paste0("quickEdit", ext)

  # Write data to a file
  write.table(x, file, col.names = colNames, row.names = FALSE) # save data to file

  # Get last modifide time at start
  startState <-
    file.info(file)$mtime  # get the "last modified" time of the file

  # open the file in default CSV editor
  shell(file, wait = FALSE)


  cat(
    paste0(
      "\n####################################\n",
      "Writing data to quickEdit.csv and trying to open. \nEdit the data in your system's default CSV editor. \nWhen you are finished, save the file and close it. \n...Waiting for any modification to the file."
    )
  )
  # Go edit the CSV/txt in your default program. Save & close when you are done. The data will be re-imported to the same variable in your R environment.

  # Monitor the file for changes:
  monitor <- TRUE
  while (monitor) {
    changeState <- file.info(file)$mtime
    if (startState < changeState) {
      break
    } else {
      Sys.sleep(1)
    }
  }

  # read the object back into R
  cat(paste0(
    "\n####################################\n",
    "Reading the data."
  ))

  if (class(x) %in% c("data.frame", "tbl", "list")) {
    y <- read.csv(file)
  } else {
    y <- gsub("\\\"", "", read_lines(file))
  }

  name <- deparse(substitute(x))
  assign(name, y, envir = .GlobalEnv)
}

# quickEdit(df)
