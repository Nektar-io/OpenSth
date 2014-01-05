require(projectX)

## Skapa ett adressobjekt
jag <- sthAddr("Erkenskroken", 24)

# Hämta data om närmaste enheter ur Stockholms Enhets-API
# Info om vilka typgrupper som kan skickas till GetNearestServiceUnit se här:
# http://api.stockholm.se/ServiceGuideService/ServiceUnitTypeGroups?apiKey=0eb1055a722f4b65986f545cb67bd44e
closestSchools <- GetNearestServiceUnit(7, jag$RT90, n=10)
closestCulture <- GetNearestServiceUnit(8, jag$RT90)
closestLeisure <- GetNearestServiceUnit(2, jag$RT90)

# Hämta enhetsdata för specifika enhetstyper, t.ex. "Grundskola".
# Info om vilka typer som finns i API:t finns här:
# http://api.stockholm.se/ServiceGuideService/ServiceUnitTypes?apiKey=0eb1055a722f4b65986f545cb67bd44e
closestPrimarySchools <- GetNearestServiceUnit("61c1cc6e-99bf-409a-85ca-4e3d0c137d5f", jag$RT90, n=10, groups=FALSE)
closestPrimarySchools <- GetNearestServiceUnit("61c1cc6e-99bf-409a-85ca-4e3d0c137d5f", jag$RT90, n=10, groups=FALSE)



# Hämta data för närmaste skolan
school1 <- list_to_table(closestPrimarySchools[[1]]$Attributes)
addr <- as.character(school1[school1$Id == "StreetAddress","Value"])
addr <- str_split(addr, " ", n=2)[[1]]
street <- addr[1]
number  <- addr[2]
if (str_detect(number, "[[:digit:]][[:blank:]|\\-]+")) {
	number <- str_split(number, "[[:blank:]|\\-]")[[1]][1]
}
number <- as.integer(number)
addrObj1 <- sthAddr(street, number)

school5 <- list_to_table(closestPrimarySchools[[5]]$Attributes)
addr <- as.character(school5[school5$Id == "StreetAddress","Value"])
addr <- str_split(addr, " ", n=2)[[1]]
street <- addr[1]
number  <- addr[2]
if (str_detect(number, "[[:digit:]][[:blank:]|\\-]+")) {
	number <- str_split(number, "[[:blank:]|\\-]")[[1]][1]
}
number <- as.integer(number)
addrObj5 <- sthAddr(street, number)


school10 <- list_to_table(closestPrimarySchools[[10]]$Attributes)
addr <- as.character(school10[school10$Id == "StreetAddress","Value"])
addr <- str_split(addr, " ", n=2)[[1]]
street <- addr[1]
number  <- addr[2]
if (str_detect(number, "[[:digit:]][[:blank:]|\\-]+")) {
	number <- str_split(number, "[[:blank:]|\\-]")[[1]][1]
}
number <- as.integer(number)
addrObj10 <- sthAddr(street, number)


# Mät avståndet från min adress till skolorna
GetRTDistance(jag$RT90, addrObj1$RT90)
GetRTDistance(jag$RT90, addrObj5$RT90)
