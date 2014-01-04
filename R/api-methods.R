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

#' Get XML
#' 
#' Get XML data from the API.
#' 
#' @param path URL path
#' @param query URL query
# get_xml <- function(path, endpoints, query) {
# 	url <- build_url(
# 		url = .url,
# 		path = file.path(path, paste(endpoints,collapse="/")),
# 		query = query
# 	)
# 	x <- paste(readLines(url, warn = FALSE), collapse="")
# 	xmlToDataFrame(xmlParse(x))
# }


#' Get JSON
#' 
#' Get JSON data from the API.
#' 
#' @param path URL path
#' @param query URL query
# get_json <- function(path, endpoints, query) {
# 	url <- modify_url(
# 		url = .url,
# 		path = file.path(path, paste(endpoints,collapse="/"), "json"),
# 		query = query
# 	)
# 	x <- paste(readLines(url, warn = FALSE), collapse="")
# 	l <- fromJSON(x)
# 	
# 	df <- data.frame(l, stringsAsFactors=FALSE)
# 	
# 	for(i in 2:length(l)) {
# 		df_temp <- data.frame(l[[i]], stringsAsFactors=FALSE)
# 		df <- rbind(df, df_temp)
# 	}
# 	
# 	return(df)
	
# 	types <- sapply(data, class)
# 	
# 	while("list" %in% types {
# 		colname <- types["list" %in% types][1]
# 		
# 		
# 		
# 		types <- sapply(data, class)
# 	}
# }

#' Get JSON for schools
#' 
#' Get JSON school data from the API.
#' 
#' @param path URL path
#' @param endpoints Node hierarchy
#' @param query URL query
#' @param n Number of rows to return
get_json_nearest <- function(path, endpoints, query, n) {
	url <- build_url(
		url = .url,
		path = c("ServiceGuideService",endpoints,"json"),
		params = query
	)
	cat(url, "\n")
	
	x <- paste(readLines(url, warn = FALSE), collapse="")
	l <- fromJSON(x)
	
	closestSchools <- head(l, n=n)
}

#' Get Service Unit data
#' 
#' Get data from the Service Unit API
#' 
#' @examples
#' \dontrun{
#' GetServiceUnit(endpoints = "ServiceUnitTypes")
#' }
#' 
#' @export
# GetServiceUnit <- function(
# 	endpoints = list(),
# 	apiKey = .serviceGuideKey,
# 	...
# ) {
# 	x <- get_xml(
# 		path = "ServiceGuideService",
# 		endpoints = endpoints,
# 		query = list(
# 			apiKey = apiKey,
# 			geographicalPosition = pos
# 		)
# 	)
# 	
# 	
# }


#' Get Service Unit data for schools
#' 
#' Get data for schools from the Service Unit API
#' 
#' @export
GetNearestServiceUnit <- function(
	unitType = 7,
	coords = c(6577574, 1627933),
	n = 5,
	apiKey = .serviceGuideKey,
	...
) {
	endpoints <- c(
		"ServiceUnitTypeGroups",
		as.character(unitType),
		"DetailedServiceUnits"
	)
	
	x <- get_json_nearest(
		path = "ServiceGuideService",
		endpoints = endpoints,
		query = list(
			apiKey = apiKey,
			geographicalPosition = paste(coords,collapse=","),
			sortBy = "DistanceToGeographicalPosition",
			sortDirection = "Ascending"
		),
		n = n
	)
	
	return(x)
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