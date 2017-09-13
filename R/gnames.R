#' @title gauge names
#' 
#' @description streamflow measurement gauge names along the rhine and tributaries.
#' With the \code{set} argument, different subsets can be chosen.
#' \tabular{llll}{
#' all(default)  \tab all 196 names in alphabetical order                \cr
#' large         \tab 55 gauges at large streams                         \cr
#' app           \tab large minus two stations with few data - for the \code{\link{rfsApp}} \cr
#' trend         \tab 35 gauges for the local trend app                  \cr
#' rawr,rawt     \tab 10 or 5 gauges along the rhine and tributaries - for the local raw data app          \cr
#' rhine         \tab 12 gauges along the actual Rhine river             \cr
#' poster        \tab 9 regime-representative gauges                     \cr
#' paper         \tab 4 gauges selected to be in the scientific article  \cr
#' }
#' 
#' @return Vector with names (charstrings)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{help}}, \code{\link{help}}
# @importFrom package fun1 fun2
#' @export
#' @examples
#' gnames()
#' 
#' @param set Character: Which subset of the gauge names should be returned? 
#'            Can be abbreviated as per \code{\link{match.arg}}.
#'            The options are described above. DEFAULT: "all"
#' 
gnames <- function(
 set="all"
)
{
set <- match.arg(set, choices=c("all","large", "app","trend","rawr","rawt",
                                "rhine","poster","paper"))

###dput(sortDF(meta[meta$Q100>400,], "Q100")$name)
lll <- c("Lobith", "Rees", "Ruhrort", "Duesseldorf", "Koeln", "Bonn", "Andernach",
"Kaub", "Mainz", "Worms", "Speyer", "Maxau", "Basel_Rheinhalle", "Basel_Schifflaende",
"Rheinfelden", "Rekingen", "Neuhausen", "Rheinklingen", "Diepoldsau", "Oberriet_Blatten",
"Schermbeck_1", "Haltern", "Kesseler_3",
"Hattingen", "Villigst", "Menden_Hoenne", "Altena", "Roenkhausen",
"Menden", "Eitorf",
"Cochem", "Trier_Up", "Bollendorf", "Fremersdorf",
"Kalkofen", "Leun",
"Grolsheim", "Boos", "Martinstein",
"Frankfurt", "Kleinheubach", "Steinbach", "Wuerzburg", "Schweinfurt", "Pettstadt", "Kemmern", "Schwuerbitz",
"Rockenau", "Lauffen", "Plochingen", "Horb_Neckar",
"Pforzheim_Enz", "Bad_Rotenfels", "Schwaibach", "Halden")

if(set=="large") return(lll)
if(set=="app") return(lll[-c(14,20)])

if(set=="trend") 
return( c("Lobith", "Rees", "Ruhrort", "Duesseldorf", "Koeln", "Bonn", "Andernach",
"Kaub", "Mainz", "Worms", "Speyer", "Maxau", "Basel_Rheinhalle", "Rekingen", "Neuhausen", "Diepoldsau", 
"Cochem", "Trier_Up", "Bollendorf", "Fremersdorf",
"Frankfurt", "Kleinheubach", "Steinbach", "Wuerzburg", "Schweinfurt", "Pettstadt", "Kemmern", "Schwuerbitz",
"Rockenau", "Lauffen", "Plochingen", "Horb_Neckar", "Kalkofen", "Hattingen", "Schermbeck_1"))


if(set=="rawr")
return(c("Lobith", "Koeln", "Andernach", "Mainz", "Worms", "Maxau", 
         "Basel_Rheinhalle", "Rekingen", "Rheinklingen", "Diepoldsau"))
if(set=="rawt")
return(c("Hattingen", "Cochem", "Kalkofen", "Frankfurt", "Rockenau"))


if(set=="rhine")
return(c("Diepoldsau","Rekingen","Basel_Rheinhalle","Maxau","Rockenau",
          "Worms", "Wuerzburg","Frankfurt","Mainz","Cochem","Koeln","Lobith"))

if(set=="poster")
return(c("Cochem","Frankfurt","Rockenau",
         "Basel_Rheinhalle","Rekingen","Diepoldsau",
         "Maxau","Worms","Mainz"))

if(set=="paper")
return(c("Rekingen","Cochem","Mainz","Koeln"))

# All names in alphabetical order:
# load("data/dismeta.Rdata")  ;  dput(meta$name)
lll <- c("Albisheim", "Alsdorf", "Alsdorf_Oberecken", "Altena", "Altenbamberg",
"Altensteig", "Althornbach", "Andernach", "Arloff", "Asslar",
"Bad_Imnau", "Bad_Kissingen", "Bad_Mergentheim", "Bad_Rotenfels",
"Bad_Vilbel", "Basel_Rheinhalle", "Basel_Schifflaende", "Bentfeld",
"Berghausen", "Betzdorf", "Bieberehren", "Biedenkopf", "Bliesheim",
"Bobenthal", "Bollendorf", "Bonn", "Boos", "Bruchenbruecken",
"Burgen", "Coburg", "Cochem", "Contwig", "Davos", "Densborn",
"Dhrontalsperre", "Diepoldsau", "Dillenburg", "Doerzbach", "Dreis",
"Duesseldorf", "Eitorf", "Eppstein", "Erlabrueck", "Eschenau",
"Essershausen", "Ettlingen", "Euthal", "Frankfurt", "Fremersdorf",
"Friedberg", "Friesheim", "Gemuend_Our", "Gemuenden", "Gensingen",
"Glesch", "Glueder", "Grolsheim", "Gross_Bieberau", "Gsteig",
"Hagen_Ambrock", "Hagen_Eckesey", "Hagen_Haspe", "Hainstadt",
"Halden", "Haltern", "Hanau", "Hardheim", "Harreshausen", "Hattingen",
"Heddesheim", "Heimbach_Bhf", "Heimborn", "Heinersdorf", "Hentern",
"Hoffnungsthal", "Hopfreben", "Horb", "Horb_Neckar", "Hornbach",
"Huettendorf", "Hundwil", "Ilbenstadt", "Imsweiler", "Isenthal",
"Jossa", "Juenkerath", "Kalkofen", "Kallenfels", "Kaub", "Kellenbach",
"Kemmern", "Kesseler_3", "Kickenbach", "Kleinheubach", "Kloster_Arnstein",
"Kloster_Engelport", "Klosters", "Kluserbruecke", "Koeln", "Langenich",
"Laufermuehle", "Lauffen", "Lebach", "Leun", "Leven", "Lobith",
"Lohbruecke", "Lollar", "Lorsch", "Mainz", "Marburg", "Martinstein",
"Maxau", "Menden", "Menden_Hoenne", "Meschede", "Michelstadt",
"Monsheim", "Morenhoven", "Mosbach", "Muesch", "Nalbach", "Nauheim",
"Nettegut", "Neubrueck", "Neuhausen", "Neukenroth", "Neustadt_Wstr",
"Niedaltdorf", "Nieder_Florstadt", "Niederaden", "Niederbrechen",
"Niederwetter", "Nordborchen", "Nuernberg", "Ober_Ofleiden",
"Oberriet_Blatten", "Oberstein", "Odenbach", "Oeventrop", "Opladen",
"Oppenweiler", "Papiermuehle", "Peltzerhaus", "Pettstadt", "Pfaffental",
"Pforzheim_Enz", "Pforzheim_Wuerm", "Planig", "Plochingen", "Pruemzurlay",
"Rees", "Reinheim", "Rekingen", "Rheinfelden", "Rheinklingen",
"Riederich", "Rockenau", "Roenkhausen", "Roth_Klaeranlage", "Ruhrort",
"Salmbacher_Passage", "Salz", "Schenkenau", "Schermbeck_1", "Schorndorf",
"Schulmuehle", "Schwabsberg", "Schwaibach", "Schweinfurt", "Schwuerbitz",
"Seifen", "Siebeldingen", "Speyer", "Steinau", "Steinbach", "Suessen",
"Sythen_I", "Tauberbischofsheim", "Thaleischweiler", "Thoerishaus",
"Trier_Up", "Unter_schmitten", "Unterlangenstadt", "Untersulzbach",
"Villigst", "Weidenau", "Weinaehr", "Weine", "Wernerseck", "Westtuennen",
"Wiesloch", "Windecken", "Wolfsmuenster", "Worms", "Wuerzburg")

meta <- get("meta")

if(!all(lll %in% meta$name)) warning("Not all names are in meta$name.")
if(!all(meta$name %in% lll)) warning("Not all meta$name are in current gnames list.")

if(set=="all") lll else stop("set was not matched correctly.")
 
}
