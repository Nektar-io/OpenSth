e <- l[[1]]
df <- data.frame(l[[1]], stringsAsFactors=FALSE)
df2 <- df

for(i in 2:length(l)) {
	df_temp <- data.frame(l[[i]], stringsAsFactors=FALSE)
	df2 <- rbind(df2, df_temp)
}


## Skapa ett adressobjekt
jag <- sthAddr("Erkenskroken", 24)

# Hämta data om närmaste enheter ur Stockholms Enhets-API
# Info om vilka typer som kan skickas till GetNearestServiceUnit se här:
# http://api.stockholm.se/ServiceGuideService/ServiceUnitTypeGroups?apiKey=0eb1055a722f4b65986f545cb67bd44e
closestSchools <- GetNearestServiceUnit(7, jag$RT90)
closestCulture <- GetNearestServiceUnit(8, jag$RT90)
closestLeisure <- GetNearestServiceUnit(2, jag$RT90)


# Hämta data för närmaste skolan
school1 <- LvWS:::list_to_table(closestSchools[[1]]$Attributes)
addr <- as.character(school1[school1$Id == "StreetAddress","Value"])
addr <- str_split(addr, " ", n=2)[[1]]
street <- addr[1]
number  <- as.integer(addr[2])
addrObj <- sthAddr(street, number)