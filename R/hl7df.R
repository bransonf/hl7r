#' Convert a Parsed HL7 Tree to a Data.frame
#'
#' @usage hl7df(messages)
#'
#' @param messages A parsed HL7 Tree; The result of \code{parsehl7()}
#'
#' @return A Data.frame of HL7 Messages
#'
#' @description This function returns a dataframe based on a parsed HL7 tree.
#' It names columns as the equivalent route to a node.
#' Values not present in a tree are returned as NA.
#' An attempt is made to normalize paths between longer and shorter branches.
#'
#' @export
hl7df <- function(messages){
  # For Every Message, we need to create a data.frame
  dfs <- lapply(messages, function(x){
    # Create an Empty Data.frame
    df <- data.frame(matrix(nrow = 1, ncol = 0))

    # Within Each Message, Parse each Segment
    for (segment in names(x)){

       # Within Each Segment, Parse Each Field
      for(field in seq_along(x[[segment]])){

        # Within Each Field, Parse Each Repetition
        for(repetition in seq_along(x[[segment]][[field]])){

          # Within Each Repetition, Parse Each Component
          for(component in seq_along(x[[segment]][[field]][[repetition]])){

            # Within Each Component, Parse Each Subcomponent
            for(subcomp in seq_along(x[[segment]][[field]][[repetition]][[component]])){
              column <- paste(segment, field, repetition, component, subcomp, sep = '.')
              df[[column]] <- x[[segment]][[field]][[repetition]][[component]][[subcomp]]
            }
          }
        }
      }
    }
    return(df)
  })

  # Join these data.frames
  joined <- dplyr::bind_rows(dfs)
  return(joined)
}
