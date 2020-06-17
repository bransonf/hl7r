#' A Versatile Parser for HL7 Version 2.x Feeds
#'
#' @usage parsehl7(feed, file)
#'
#' @param feed A vector representing an HL7 feed, the equivalent output of \code{readLines(file.hl7)}
#' @param file A file containing a valid HL7 feed
#'
#' @return A List of Parsed HL7 Messages
#'
#' @description This function accepts either a character vector containing separate HL7 segments or a valid HL7 file as input, and returns a parsed R data structure.
#'
#' @export
parsehl7 <- function(feed, file){

  # Check for the Specification of Arguments
  if(sum(missing(feed), missing(file)) != 1){
    stop('Only one of `feed` or `file` must be specified')
  }

  # Load the Data (If Necessary)
  if(!missing(file)){
    feed <- readLines(file)
  }

  #Validate that Data is Valid HL7 (May need to be improved)
  if(!grepl('^MSH', feed[1])){
    stop('The data specified does not appear to be valid HL7. The first line of the feed should begin with "MSH"')
  }

  # Allocate A List for Length of Messages
  n_msg <- sum(grepl('^MSH', feed))
  messages <- vector('list', n_msg)

  # Identify the Field Separator
  field_sep <- substr(feed[1], 4, 4)
  if(field_sep != '|'){
    warning('This HL7 data uses a different field seperator than is recommended. `|` is recommended. `', field_sep, '` is used.')
  }

  # Identify Other Separators
  MSH.2 <- strsplit(feed[1], field_sep, fixed = TRUE)[[1]][2]
  if(nchar(MSH.2) != 4){
    stop('Non-Standard Number of Encoding Characters Found in MSH.2. Expected 4, got ', nchar(MSH.2))
  }
  component_sep <- substr(MSH.2, 1, 1)
  repitition_sep <- substr(MSH.2, 2, 2)
  escape_char <- substr(MSH.2, 3, 3)
  subcomponent_sep <- substr(MSH.2, 4, 4)

  # Iterate Through Segments, Incrementing at MSH Segments
  index = 0
  for (line_n in seq_along(feed)){
    # Get String
    line <- feed[line_n]

    # Increment on New Message
    if(grepl('^MSH', line)){
      index = index + 1
      messages[[index]] <- list()
    }
    # Split on Field Separator
    fields <- as.list(unlist(strsplit(line, field_sep, fixed = TRUE)))

    # Get the Segment Key
    segment <- fields[[1]]
    # Validate
    if(!grepl('[A-Z 0-9]{3}', segment)){
      warning('Non-Standard Segment Key Detected, Expected 3 capital characters/numbers, got ', segment, 'at line ', line_n)
    }

    # While Key Exists, Add 1 to Name
    while(segment %in% names(messages[[index]])){
      segment_key <- regmatches(segment, regexec('\\d*$', text = segment), invert = TRUE)[[1]][1]
      n <- regmatches(segment, regexec('\\d*$', text = segment))[[1]][1]
      if(n == ''){n <- '.2'}
      else{n <- as.numeric(n) + 1}
      segment <- paste0(segment_key, n)
    }

    # For Any Fields Containing the Component Separator, Generate Lists
    for(field in seq_along(fields)){
      if(grepl(component_sep, fields[[field]], fixed = TRUE)){
        # Ignore MSH.2
        if(fields[[1]] == 'MSH' && field == 2){
          NULL
        }else{
          sub_split <- as.list(unlist(strsplit(fields[[field]], component_sep, fixed = TRUE)))
          # For Any Component Containing the Sub Component Separator, Generate Lists
          for (sub in seq_along(sub_split)) {
            if(grepl(subcomponent_sep, sub_split[[sub]], fixed = TRUE)){
              sub_split[[sub]] <- as.list(unlist(strsplit(sub_split[[sub]], subcomponent_sep, fixed = TRUE)))
            }
          }

          # Assign The Hierarchy to the Field
          fields[[field]] <- sub_split
        }
      }
    }

    # Assign The Fields to the Key (Skipping the Segment Key)
    # Special Case for MSH
    if(segment == 'MSH'){
      messages[[index]][[segment]] <- append(fields[2:length(fields)], field_sep, 0)
    }else{
      messages[[index]][[segment]] <- fields[2:length(fields)]
    }

  }

  return(messages)
}
