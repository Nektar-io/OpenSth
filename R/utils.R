#' Build URL
#' 
#' Build API query URL 
#' 
#' @param url
#' @param path
#' @param params list of named parameters

build_url <- function(url, path, params) {
	# Remove leadning and trailing slashes from all URL components
	url <- str_replace_all(url, "^[/]+|[/]+$", "")
	path <- str_replace_all(path, "^[/]+|[/]+$", "")
	
	# Create the URL and param parts
	full_url <- paste(url, paste(path, collapse="/"), sep="/")
	full_params <- paste(names(params),params, sep="=", collapse="&")
	
	# Create and return the full query
	full_query <- paste(full_url, full_params, sep="?")
	return(full_query)
}


#' Calculate distance in RT90 2.5
#' 
#' Calculate distance in RT90 2.5
#' 
#' @param c1 C1
#' @param c2 C2
#' @export

GetRTDistance <- function(c1, c2) {
	sqrt(
		(c1[1] - c2[1])^2 + (c1[2] - c2[2])^2
	)
}


#' Convert list to table
#' 
#' Flattens out a nested list 
#' 
#' @param x list
#' @export
list_to_table <- function(x) {
	do.call(
		"rbind.fill",
		lapply(x, function(y) {
			data.frame(t(unlist(y)))
		})
	)
}
