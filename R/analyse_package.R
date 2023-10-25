#' Analyse the function structure in an R package
#'
#' @description The `analyse_package` function finds and analyses all the .R files it can find in a package folder given by `directory`.
#'
#' @param directory A string indicating the directory of the package to be analysed.
#'
#' @param is_local A logical indicating whether or not the package is local or a URL (this decides the use of "\" or "/" respectively). Defaults to FALSE.
#'
#' @param is_here A logical indicating whether or not the given `directory` points to a folder with the files, or if it points to the folder of a local package, and thus an "R" must be added to the file paths. Defaults to FALSE.
#'
#' @return A dataframe of all the .R files found in an R package
#'
#' @importFrom magrittr %>% extract2
#' @importFrom stringr str_count str_extract str_remove str_extract_all str_detect str_replace
#' @importFrom RCurl getURL
#' @importFrom jsonlite fromJSON
#'
#' @export analyse_package

analyse_package <- function(directory, is_local = FALSE, is_here = FALSE){

  # ---V--- Check Input ---V---
  stopifnot("directory is not a character" = is.character(directory))
  # ---^--- Check Input ---^---

  # Figure out what and where directory is
  if(is_local == TRUE | str_detect(directory, pattern="[A-Z]{1}:")){
    # Set up to study local repo

    # Whether or not to add a \\R\\
    if(is_here == FALSE){
      file_names <- list.files(paste0(directory, "\\R\\"))
      directory <- paste0(directory, "\\R\\")
    }
    else{
      file_names <- list.files(paste0(directory, "\\"))
      directory <- paste0(directory, "\\")
    }

    # separator is used when pasting on the function names later
    separator = "\\"
  }
  else{ # directory is external (assumed to be a github repo)
    #directory <- str_c(directory, "/tree/main/R")
    website_string <- getURL(paste0(directory, "/tree/main/R")) # get the website in the form of a json string.
    file_names <- fromJSON(website_string)$payload$tree$items$name

    # Change directory in order to access the "raw" text files
    directory <- str_replace(directory, pattern = "github.com", replacement = "raw.githubusercontent.com")
    directory <- paste0(directory, "/main/R/")

    # separator is used when pasting on the function names later
    separator = "/"
  }

  # Get the names of all the files in the `R` folder of the directory
  #file_names <- list.files(paste(directory, "R", sep=separator))

  # Check for non .R files
  # consider using str_detect instead
  #n_nonRfiles <- (!str_count(file_names, pattern=paste0(separator, ".R$"))) %>% sum()
  #if(n_nonRfiles > 0){cat("Warning: There are", n_nonRfiles, "files that are not .R files in this folder.\n")}

  # Create a dataframe for all the functions
  function_data <- data.frame(matrix(nrow=0, ncol=4)) # 4 columns from line below
  colnames(function_data) <- c("File", "Name", "Arguments", "Lines")

  # Analyse the functions in each file
  for(file in file_names){
    # Read the lines in `file` as a character vector
    lines <- readLines(paste(directory, file, sep = separator))

    function_list <- c()

    i <- 1
    in_a_function <- FALSE
    n_lines <- 0 # Counts the number of lines in a function
    while(i <= length(lines)){

      # Check if this line starts a function
      #function_match <- str_count(lines[i], "^[a-zA-Z_.0-9]{1,} <- function") == 1
      function_match <- str_count(lines[i], "((^[a-zA-Z]{1})|(^[.]{1}\\D))[a-zA-Z_.0-9]{0,}[ ]{0,1}((<-)|(=))[ ]{0,1}function") == 1
      if(function_match & in_a_function == FALSE){# A function was found
        in_a_function <- TRUE
        n_lines <- n_lines + 1

        # Extract function name
        #fu_name <- str_extract(lines[i], "^[a-zA-Z-_.0-9]{1,}")
        fu_name <- str_extract(lines[i], "((^[a-zA-Z]{1})|(^[.]{1}\\D))[a-zA-Z_.0-9]{0,}")

        function_list <- c(function_list, fu_name)
        #cat("\tFound function:", fu_name, "\n")

        # Extract arguments
        # first remove "function_name <- function", then extract the possible arguments
        fu_arguments <- str_remove(lines[i], pattern=".{1,}function") %>%
          str_extract_all(pattern="[a-z_.]{1,}") %>%
          extract2(1) # output should be a list of one element. Get the first element.

        # Check if this is a one-line function
        if(str_detect(lines[i], "\\}$")){
          # Save this function
          function_data[nrow(function_data)+1,] <- c(file,
                                                     fu_name,
                                                     paste0("(", paste(fu_arguments, collapse=", "), ")"),
                                                     n_lines)
          # Move on to next line
          n_lines <- 0
          in_a_function <- FALSE
          i <- i + 1
          next
        }

        i <- i + 1
        next
      }

      # This is basically an else, because the loop skips to next iteration if the if statement above is TRUE
      if(in_a_function == TRUE){

        # Check that `line[i]` does not end the current function (one-line functions are identified earlier, in the function start if stament).
        if(str_detect(lines[i], "\\}$")){
          n_lines <- n_lines + 1
          in_a_function <- FALSE
          # Save information in function data frame because this functions is ended
          function_data[nrow(function_data)+1,] <- c(file,
                                                     fu_name,
                                                     paste0("(", paste(fu_arguments, collapse=", "), ")"),
                                                     n_lines)
          n_lines <- 0
        }

        if(in_a_function == TRUE){n_lines <- n_lines + 1}

        # Skip to next line
        i <- i + 1
        next
      }

      # This `line[i]` is not part of a function, so move on to next line
      i <- i + 1
      next
    } # Analyse each line in a file
  } # Analyse each file

  # Return a dataframe with information about the functions
  return(function_data)
}

