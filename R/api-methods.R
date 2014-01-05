#' Get XML from LvWS
#' 
#' Get XML data from the LvWS API.
#' TODO: Can we switch from modify_url() to build_url()?
#' 
#' @param path URL path
#' @param query URL query
lvws_get_xml <- function(path, query) {
	url <- modify_url(
		url = .lvwsUrl,
		path = file.path(.lvwsPath, path),
		query = query
	)
	
	# Replace silly percent encoding with other silly percent encoding
	for(i in 1:6) {
		url <- str_replace_all(
			url, 
			c("%C3%A5","%C3%A4","%C3%B6","%C3%85","%C3%84","%C3%96")[i],
			c("%E5","%E4","%F6","%C5","%C4","%D6")[i]
		)
	}
	
	cat(url,"\n")
	x <- paste(readLines(url, warn = F), collapse="")
	xmlParse(x)
}

#' Get parking places from LvWS
#' 
#' TODO: add support for lon, lat instead of street!
#' See http://openstreetgs.stockholm.se/Home/Parking
#' 
#' @examples
#' \dontrun{
#' # Get parking places at Birkagatan
#' GetStreetParking(streetName = "Birkagatan")
#' }
#' @export
GetStreetParking <- function(
	streetName = NULL,
	foreskrift = "ptillaten",
	operation = file.path("street", streetName),
	apiKey = .lvwsKey
) {
	if (is.null(operation)) stop("Please provide streetName or operation")
	
	url <- modify_url(
		url = "http://openparking.stockholm.se",
		path = sprintf("LTF-Tolken/v1/%s/%s", foreskrift, operation),
		query = list(
			apiKey = apiKey,
			outputFormat = "json"
		)
	)
	x <- paste0(readLines(url, warn = F))
	x <- fromJSON(x)$features
	x <- remove_list_fields(x, c("geometry_name", "geometry"))
	x <- list_to_table(x)	
	return(x)
}

#' Get Street Adresses
#' 
#' Get street adresses from the LvWS API.
#' Returns street numbers, postal codes, coordinates, etc.
#' 
#' @examples
#' \dontrun{
#' GetAddresses(streetName = "Birkagatan")
#' }
#' 
#' @export
GetAddresses <- function(
	apiKey = .lvwsKey,
	municipalityPattern = "",
	streetName = "",
	streetNumPattern = "",
	postalCodePattern = "",
	postalAreaPattern = "",
	includeAddressConnectionsForTrafficTypes = "0"
) {
	x <- lvws_get_xml(path = "GetAddresses", query = as.list(environment()))
	xmlToDataFrame(x, stringsAsFactors = F)
}

#' Get Street Names
#' 
#' Get street names from the LvWS API.
#' It's also possible to provide wildcards.
#' 
#' @param apiKey API key
#' @param streetNamePattern Street Name Pattern
#' @param optionalMunicipality Municipality (optional)
#' @param optionalPostalArea Postal Area (optional)
#' @param optionalPostalCode Postal Code (optional)
#' 
#' @examples
#' \dontrun{
#' GetStreetNames(streetNamePattern = "B*")
#' }
#' 
#' @export
GetStreetNames <- function(
	apiKey = .lvwsKey,
	streetNamePattern = "",
	optionalMunicipality = "",
	optionalPostalArea = "",
	optionalPostalCode = ""
) {
	x <- lvws_get_xml(path = "GetStreetNames", query = as.list(environment()))
	x <- xmlToList(x, simplify = T)
	x <- x[rownames(x) %in% "StreetName"]
	unlist(x)
}

#' Get coords in different coordinate systems from LvWS
#' 
#' Get coords in different coordinate systems from WKT (e.g. RT90 2.5 from WGS 84) through the LvWS API
#' 
#' @param WKT WKT point from GetAddresses()
#' @export

GetCoords <- function(
	WKT,
	apiKey = .lvwsKey,
	targetCoordSrid = 3021
) {
	# The "WKT" string contains the point coordinates of an address
	strings <- str_extract_all(WKT, "[0-9\\.]*")[[1]]
	strings <- strings[str_length(strings) > 0]
	WKTx <- strings[1]
	WKTy <- strings[2]
	
	coords <- lvws_get_xml(path = "TransformGeometry", query = list(
		apikey = apiKey,
		wkt = paste("POINT (", WKTx, " ", WKTy, ")", sep=""),
		fromSrid = 4326,
		toSrid = targetCoordSrid
	))
	
	return_string <- unlist(xpathApply(coords, "//ns:string/text()", xmlValue, namespaces="ns"))
	return_string <- str_extract_all(return_string, "[0-9\\.]*")[[1]]
	return_string <- return_string[str_length(return_string) > 0]
	
	coords <- list(
		RT90 = as.double(return_string[c(2,1)]),
		WGS84 = as.double(strings[c(2,1)])
	)
	
	# We change the order of the values since LvWS returns the values in 
	# [easting, northing] order, which is the opposite of what it should be.
	message("Coordinates generated on [northing, easting] form")
	return(coords)
}

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
		url = .unitUrl,
		path = c("ServiceGuideService", endpoints, "json"),
		params = query
	)
	cat(url, "\n")
	
	x <- paste(readLines(url, warn = F), collapse="")
	l <- fromJSON(x)
	
	closestSchools <- head(l, n=n)
}



#' Get data on the n nearest Service Units
#' 
#' Get data on the n nearest Service Units from the Service Unit API
#' 
#' @export
GetNearestServiceUnit <- function(
	unitType = 7,
	coords = c(6577574, 1627933),
	n = 5,
	apiKey = .unitKey,
	groups = TRUE,
	...
) {
	if (groups) {
		endpoints <- c(
			"ServiceUnitTypeGroups",
			as.character(unitType),
			"DetailedServiceUnits"
		)
	} else {
		endpoints <- c(
			"ServiceUnitTypes",
			as.character(unitType),
			"DetailedServiceUnits"
		)
	}
	
	x <- get_json_nearest(
		path = "ServiceGuideService",
		endpoints = endpoints,
		query = list(
			apiKey = apiKey,
			geographicalPosition = paste(coords, collapse = ","),
			sortBy = "DistanceToGeographicalPosition",
			sortDirection = "Ascending"
		),
		n = n
	)
	
	# TODO: NOT FINISHED
	# Restructure the format of attributes
	for(i in 1:length(x)) {
		x[[i]]$Attributes <- sapply(x[[i]]$Attributes, function(i) {
			new <- list()
			new[[i$Id]] <- list(
				Name = i$Name,
				Value = i$Value,
				Description = i$Description,
				Group = i$Group,
				GroupDescription = i$GroupDescription
			)
			return(new)
		})
	}
	
	# Remove some attributes that has a bad structure
	x <- remove_list_fields(x, c("RelatedServiceUnits"))
	
	x <- list_to_table(x)
	return(x)
}