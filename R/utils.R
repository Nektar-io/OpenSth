#' Build URL
#' 
#' Build API query URL 
#' 
#' @param url
#' @param path
#' @param params list of named parameters
#' @export

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
#' @param ... arguments passed to `data.frame()
#' @export
list_to_table <- function(x, ...) {
	do.call(
		"rbind.fill",
		lapply(x, function(y) {
			data.frame(t(unlist(y)), ...)
		})
	)
}

#' Remove list fields
#' 
#' Remove named fields (attributes) from a list.
#' Helpful to use when tidying converted JSON-objects.
#' 
#' @param lst list
#' @param names field names
remove_list_fields <- function(lst, names) { 
	lapply(lst, function(x) {
		for(name in names) {
			x[[name]] <- NULL
		}
		return(x)
	})
}
