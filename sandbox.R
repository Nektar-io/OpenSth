require(projectX)
require(LvWS)

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
addrObj1 <- sthAddr(street, number)

school5 <- LvWS:::list_to_table(closestSchools[[5]]$Attributes)
addr <- as.character(school5[school5$Id == "StreetAddress","Value"])
addr <- str_split(addr, " ", n=2)[[1]]
street <- addr[1]
number  <- as.integer(addr[2])
addrObj5 <- sthAddr(street, number)

# Mät avståndet från min adress till skolorna
GetRTDistance(jag$RT90, addrObj1$RT90)
GetRTDistance(jag$RT90, addrObj5$RT90)
