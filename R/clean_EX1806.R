# Function to clean SeaTube Annotations that are in the comments section for all EX1806 dives

# Called by: NOAA Explore QAQC_Annotations.Rmd

# NOAA Exploration Science and Technology Data Analysis team: Groves, Egan
# Last update: De 2023


#' Clean EX1806 annotations found in the comments column
#' @export
#' @description
#' Removes annotation from the Comments column to the Taxonomy column and pulls Taxon Path from WoRMS
#' @returns a dataframe of EX1806 dives
#' @details
#' Cool blurb about NOAA Exploration
#' @seealso \code{\link{https://github.com/shgroves/NOAA.ASPIRE}}
#' @examples
#'
#'EX1806_dat <- clean_EX1806()
#'

clean_EX1806 <- function(){
dat1806 <- all_rov_clean |>
  # Filter to 1806 and bio annotations
  dplyr::filter(cruise == "EX1806",
                annotation_type == "biological") |>
  # Filter to rows with blank taxon path column and remove annotations for
  dplyr::filter(Taxon_Path == "",
                Comment != "Dead coral?",
                Comment != "Anthropogenic debris; fishing net",
                Comment != "End EX1806_DIVE17_SPEC04BIO:",
                Comment != "END EX1806_DIVE16_SPEC01BIO: water sample",
                Comment != "START EX1806_DIVE16_SPEC01BIO: water sample",
                Comment != "Start EX1806_DIVE17_SPEC04BIO:",
                Comment != "Same school of bumper? Following ROV downward?",
                Comment != "Start EX1806_DIVE02_SPEC02BIO:",
                Comment != "Start EX1806_DIVE10_SPEC04BIO:",
                Comment != "Merman Dan Other Vertebrate:") |>

  # Remove all the common names in parentheses (Glass Sponge, Octocorals, etc.)
  dplyr::mutate(Comment = gsub("\\s*\\([^\\)]+\\)","", as.character(Comment))  ) |>

  # In most rows, we want the last word before the : to populate the Taxon column
  dplyr::mutate(Taxon = sub(".*\\s(.*):.*", "\\1", Comment)) |>

  # The above line fixed a lot of the rows but not all, let's do some manual fixing, yay!

  dplyr::mutate(Taxon = dplyr::case_when(Comment == "Arthropoda Crustacea Malacostraca Decapoda Galatheoidea or Chirostyloidea: on Bathypathes" ~ "Anomura",
                                         Comment == "Arthropoda Crustacea Malacostraca Decapoda Galatheoidea or Chirostyloidea: under bryozoan" ~ "Anomura",
                                         Comment == "Annelida Polychaeta Canalipalpata Siboglinidae: several of the bacterial mats have many tubeworms" ~ "Siboglinidae",
                                         # Bryozoans
                                         grepl("Bryozoa|Bryozoan|Bryozon", Comment, ignore.case = TRUE) ~ "Bryozoa",

                                         # Ctenaphores
                                         Comment == "Carbonate:Ctenophora Tentaculata Platyctenida:Arthropoda Crustacea Malacostraca Decapoda:OverhangVideo highlight:" ~ "Platyctenida",

                                         # Molluscs
                                         grepl("Bathymodiolus", Comment, ignore.case = TRUE) ~ "Bathymodiolus",
                                         grepl("Illex", Comment, ignore.case = TRUE) ~ "Illex",
                                         grepl("Bathypolypus bairdii", Comment, ignore.case = TRUE) ~ "Bathypolypus bairdii",
                                         grepl("Graneledone verrucosa", Comment, ignore.case = TRUE) ~ "Graneledone verrucosa",
                                         grepl("Lobatus", Comment, ignore.case = TRUE) ~ "Lobatus",
                                         grepl("Naticidae", Comment, ignore.case = TRUE) ~ "Naticidae",
                                         grepl("Nudibranchia|nudibranch", Comment, ignore.case = TRUE) ~ "Naticidae",
                                         Comment == "Mollusca Gastropoda: Genus: Lobatus?" ~ "Gastropoda",

                                         # Nemertea
                                         grepl("Nemertean worm", Comment, ignore.case = TRUE) ~ "Nemertea",
                                         grepl("Naticidae", Comment, ignore.case = TRUE) ~ "Naticidae",

                                         # Annelida
                                         grepl("Echiuran", Comment, ignore.case = TRUE) ~ "Echiura",
                                         grepl("Sclerolinum contortum", Comment, ignore.case = TRUE) ~ "Sclerolinum contortum",
                                         grepl("Sabellid|fan worm", Comment, ignore.case = TRUE) ~ "Sabellidae",
                                         grepl("polynoid", Comment, ignore.case = TRUE) ~ "Polynoidae",
                                         grepl("Limulus polyphemus", Comment, ignore.case = TRUE) ~ "Limulus polyphemus",
                                         grepl("Spoonworm|Tubeworms", Comment, ignore.case = TRUE) ~ "Annelida",
                                         grepl("Terebellidae", Comment, ignore.case = TRUE) ~ "Terebellidae",
                                         Comment == "Annelida Polychaeta: scale worm on Bathypathes sp branch" ~ "Polynoidae",
                                         Comment == "Tube worms many small" ~ "Annelida",

                                         # Fish
                                         grepl("Squalus", Comment, ignore.case = TRUE) ~ "Squalus",
                                         grepl("Leomonema", Comment, ignore.case = TRUE) ~ "Leomonema",
                                         grepl("Luciobrotula", Comment, ignore.case = TRUE) ~ "Luciobrotula",
                                         grepl("Aluterus", Comment, ignore.case = TRUE) ~ "Aluterus",
                                         grepl("Urophycis regia", Comment, ignore.case = TRUE) ~ "Urophycis regia",
                                         grepl("Chaunax", Comment, ignore.case = TRUE) ~ "Chaunax",
                                         grepl("hatchetfish", Comment, ignore.case = TRUE) ~ "Sternoptychidae",
                                         grepl("hagfish", Comment, ignore.case = TRUE) ~ "Myxini",
                                         grepl("Blackbelly rosefish", Comment, ignore.case = TRUE) ~ "Helicolenus dactylopterus",
                                         grepl("Dicrolene intronigra", Comment, ignore.case = TRUE) ~ "Dicrolene introniger",
                                         grepl("Bathypterois phenax", Comment, fixed = TRUE) ~ "Bathypterois phenax",
                                         grepl("Bathypterois", Comment, fixed = TRUE) ~ "Bathypterois",
                                         grepl("Conger oceanicus", Comment, fixed = TRUE) ~ "Conger oceanicus",
                                         grepl("Conger", Comment, fixed = TRUE) ~ "Conger",
                                         grepl("Nemicthys curvirostris|Snipe eel", Comment, ignore.case = TRUE) ~ "Nemichthys curvirostris",
                                         grepl("Chordata Actinopterygii Teleostei Cottunculus thomsonii", Comment, ignore.case = TRUE) ~ "Cottunculus thomsonii",
                                         grepl("Lophioides beroe", Comment, ignore.case = TRUE) ~ "Lophiodes beroe",
                                         grepl("Peristedion", Comment, ignore.case = TRUE) ~ "Peristediinae",
                                         grepl("Goosefish", Comment, ignore.case = TRUE) ~ "Lophiidae",
                                         grepl("Scyliorhinus retifer", Comment, ignore.case = TRUE) ~ "Scyliorhinus retifer",
                                         grepl("Neocyttus", Comment, ignore.case = TRUE) ~ "Neocyttus",
                                         grepl("Anthias", Comment, ignore.case = TRUE) ~ "Anthias",
                                         grepl("Aldrovandia phalacra", Comment, ignore.case = TRUE) ~ "Aldrovandia phalacra",
                                         grepl("Argentina|Argentinia", Comment, ignore.case = TRUE) ~ "Argentina",
                                         grepl("Antimora", Comment, ignore.case = TRUE) ~ "Antimora",
                                         grepl("Lycodes terranovae", Comment, ignore.case = TRUE) ~ "Lycodes terraenovae",
                                         grepl("Zoarcidae|Eelpout", Comment, ignore.case = TRUE) ~ "Zoarcidae",
                                         grepl("Bathysaurus", Comment, ignore.case = TRUE) ~ "Bathysaurus",
                                         grepl("Laemonema melanurum", Comment, ignore.case = TRUE) ~ "Laemonema melanurum",
                                         grepl("Laemonema barbatulum", Comment, ignore.case = TRUE) ~ "Laemonema barbatulum",
                                         grepl("Laemonema", Comment, ignore.case = TRUE) ~ "Laemonema",
                                         grepl("halosaur|Halosauridae|Halosauropsis", Comment, ignore.case = TRUE) ~ "Halosauridae",
                                         grepl("Monomitopus", Comment, ignore.case = TRUE) ~ "Monomitopus",
                                         grepl("Toadfish or wrymouth", Comment, ignore.case = TRUE) ~ "Teleostei",
                                         grepl("Mola mola", Comment, ignore.case = TRUE) ~ "Mola mola",
                                         grepl("Trachyscorpia", Comment, ignore.case = TRUE) ~ "Trachyscorpia",
                                         grepl("Trachyscorpia cristulata", Comment, ignore.case = TRUE) ~ "Trachyscorpia cristulata",
                                         grepl("Merluccius australis|Southern hake", Comment, ignore.case = TRUE) ~ "Merluccius australis",
                                         grepl("Merluccius albidus", Comment, ignore.case = TRUE) ~ "Merluccius albidus",
                                         grepl("Hake|hake", Comment, fixed = TRUE) ~ "Merluccius",
                                         grepl("greeneyes", Comment, ignore.case = TRUE) ~ "Chlorophthalmidae",
                                         grepl("Chauliodus", Comment, ignore.case = TRUE) ~ "Chauliodus",
                                         grepl("morid", Comment, ignore.case = TRUE) ~ "Moridae",
                                         grepl("Scyliorhinidae", Comment, ignore.case = TRUE) ~ "Scyliorhinidae",
                                         grepl("Fenestraja plutonia", Comment, ignore.case = TRUE) ~ "Fenestraja plutonia",
                                         grepl("Scyliorhinus meadi", Comment, ignore.case = TRUE) ~ "Scyliorhinus meadi",
                                         grepl("Chloroscombrus chrysurus", Comment, ignore.case = TRUE) ~ "Chloroscombrus chrysurus",
                                         grepl("Stomiiform fish - probably large Cyclothone", Comment, fixed = TRUE) ~ "Stomiiformes",
                                         grepl("Vinciguerria", Comment, fixed = TRUE) ~ "Vinciguerria",
                                         Comment == "Dactylobatus armatus" ~ "Dactylobatus armatus",
                                         Comment == "Lanternfish  possibly Ceratoscopelus warmingii" ~ "Myctophidae",

                                         # Tunicates
                                         grepl("Salpida", Comment, ignore.case = TRUE) ~ "Salpida",
                                         grepl("Tunicata", Comment, ignore.case = TRUE) ~ "Tunicata",
                                         grepl("Pyuridae|Pyruidae", Comment, ignore.case = TRUE) ~ "Pyuridae",

                                         # Echinoderms
                                         grepl("Gorgonocephalidae", Comment, ignore.case = TRUE) ~ "Gorgonocephalidae",
                                         grepl("Ophiomusium", Comment, ignore.case = TRUE) ~ "Ophiomusium",
                                         grepl("Brisingid|Brisingida|Brisingids", Comment, ignore.case = TRUE) ~ "Brisingida",
                                         grepl("Goniaster|Goneasterid", Comment, ignore.case = TRUE) ~ "Goniaster",
                                         grepl("neomorphaster", Comment, ignore.case = TRUE) ~ "Neomorphaster",
                                         grepl("Astropectinidae|Asteropectinidae", Comment, ignore.case = TRUE) ~ "Astropectinidae",
                                         grepl("Solasteridae", Comment, ignore.case = TRUE) ~ "Solasteridae",
                                         grepl("Henricia", Comment, ignore.case = TRUE) ~ "Henricia",
                                         grepl("Tamaria", Comment, ignore.case = TRUE) ~ "Tamaria",
                                         grepl("Solaster", Comment, ignore.case = TRUE) ~ "Solaster",
                                         grepl("Solaster", Comment, ignore.case = TRUE) ~ "Solaster",
                                         grepl("Zenometra columnaris", Comment, ignore.case = TRUE) ~ "Zenometra columnaris",
                                         grepl("Cidaroid", Comment, ignore.case = TRUE) ~ "Cidaroida",
                                         grepl("Echinothuriidae", Comment, ignore.case = TRUE) ~ "Echinothuriidae",
                                         grepl("Cidaris|Cedaris", Comment, ignore.case = TRUE) ~ "Cidaris",
                                         grepl("Phormosoma", Comment, ignore.case = TRUE) ~ "Phormosoma",
                                         grepl("Hygrosoma", Comment, ignore.case = TRUE) ~ "Hygrosoma",
                                         grepl("Psolidae|Psolids|Psolid", Comment, ignore.case = TRUE) ~ "Psolidae",
                                         grepl("Benthodytes typica", Comment, ignore.case = TRUE) ~ "Benthodytes typica",
                                         grepl("asteroschma", Comment, ignore.case = TRUE) ~ "Asteroschema",
                                         grepl("Ophiomusa", Comment, ignore.case = TRUE) ~ "Ophiomusa",
                                         grepl("Plinthaster dentatus", Comment, ignore.case = TRUE) ~ "Plinthaster dentatus",

                                         Comment == "Echinodermata  Asteroidea:goneasteridPlinthaster dentatus" ~ "Plinthaster dentatus",
                                         Comment == "Echinodermata  Asteroidea: Goniasterid eating a Plumarella" ~ "Goniaster",
                                         Comment == "Echinodermata Echinoidea: Phormosoma placenta with scale worm associate" ~ "Phormosoma placenta",
                                         Comment == "Echinodermata Ophiuroidea: on Cnidaria Anthozoa Hexacorallia Zoanthidea: and small living branch of the acanthogorgia" ~ "Ophiuroidea",
                                         Comment == "Echinodermata Ophiuroidea: on Paramuricea" ~ "Ophiuroidea",
                                         Comment == "Echinodermata Ophiuroidea:asteroschma on Paramuricea" ~ "Asteroschema",
                                         Comment == ":urchin test" ~ "Echinodermata",


                                         #Cnidaria
                                         grepl("Keratoisis", Comment, ignore.case = TRUE) ~ "Keratoisis",
                                         grepl("Stoloniferan", Comment, ignore.case = FALSE) ~ "Stolonifera",
                                         grepl("Anthoptilum", Comment, ignore.case = TRUE) ~ "Anthoptilum",
                                         grepl("Acanella", Comment, ignore.case = TRUE) ~ "Acanella",
                                         grepl("Clavulariidae", Comment, ignore.case = TRUE) ~ "Clavulariidae",
                                         grepl("Paragorgia", Comment, ignore.case = TRUE) ~ "Paragorgia",
                                         grepl("Tubulariidae", Comment, ignore.case = TRUE) ~ "Tubulariidae",
                                         grepl("Ceriantharia", Comment, ignore.case = TRUE) ~ "Ceriantharia",
                                         grepl("Actinoscyphia", Comment, ignore.case = TRUE) ~ "Actinoscyphia",
                                         grepl("Actinernus", Comment, ignore.case = TRUE) ~ "Actinernus",
                                         grepl("Actinoscyphia", Comment, ignore.case = TRUE) ~ "Actinoscyphia",
                                         grepl("Anthomastus", Comment, ignore.case = TRUE) ~ "Anthomastus",
                                         grepl("Bathypathes", Comment, ignore.case = TRUE) ~ "Bathypathes",
                                         grepl("Alternatipathes", Comment, ignore.case = TRUE) ~ "Alternatipathes",
                                         grepl("Leiopathes", Comment, ignore.case = TRUE) ~ "Leiopathes",
                                         grepl("Parantipathes", Comment, ignore.case = TRUE) ~ "Parantipathes",
                                         grepl("Heteropathes", Comment, ignore.case = TRUE) ~ "Heteropathes",
                                         grepl("Stauropathes", Comment, ignore.case = TRUE) ~ "Stauropathes",
                                         grepl("acanthogorgia", Comment, ignore.case = TRUE) ~ "Acanthogorgia",
                                         grepl("Flabellum", Comment, ignore.case = TRUE) ~ "Flabellum",
                                         grepl("lophelia pertusa", Comment, ignore.case = TRUE) ~ "Desmophyllum pertusum",
                                         grepl("Lophelia|Lophellia|Lophilia", Comment, ignore.case = TRUE) ~ "Desmophyllum",
                                         grepl("Madrepora oculata ", Comment, ignore.case = TRUE) ~ "Madrepora oculata ",
                                         grepl("Madrepora", Comment, ignore.case = TRUE) ~ "Madrepora",
                                         grepl("Solenosmilia", Comment, ignore.case = TRUE) ~ "Solenosmilia",
                                         grepl("Candidella", Comment, ignore.case = TRUE) ~ "Candidella",
                                         grepl("Stichpathes|Stichopathes", Comment, ignore.case = TRUE) ~ "Stichopathes",
                                         grepl("Metallogorgia melanotrichos", Comment, ignore.case = TRUE) ~ "Metallogorgia melanotrichos",
                                         grepl("Metallogorgia", Comment, ignore.case = TRUE) ~ "Metallogorgia",
                                         grepl("Enallopsammia", Comment, ignore.case = TRUE) ~ "Enallopsammia",
                                         grepl("anthomastus", Comment, ignore.case = TRUE) ~ "Anthomastus",
                                         grepl("plumarella", Comment, ignore.case = TRUE) ~ "Plumarella",
                                         grepl("Thouarella", Comment, ignore.case = TRUE) ~ "Thouarella",
                                         grepl("Gorgonian|Sea Whip", Comment, ignore.case = TRUE) ~ "Octocorallia",
                                         grepl("Paramuricea", Comment, ignore.case = TRUE) ~ "Paramuricea",
                                         grepl("Isididae", Comment, ignore.case = TRUE) ~ "Keratoisididae",
                                         grepl("Acanthogorgia", Comment, ignore.case = TRUE) ~ "Acanthogorgia",
                                         grepl("Chrysogorgidae|Chrysogorgiidae", Comment, ignore.case = TRUE) ~ "Chrysogorgiidae",
                                         grepl("anthothela/eunicella|Eunicella/Anthothela", Comment, ignore.case = TRUE) ~ "Malacalcyonacea",
                                         grepl("Aquaumbridae", Comment, ignore.case = TRUE) ~ "Aquaumbridae",
                                         grepl("Clavularia purple stoloniferan", Comment, ignore.case = TRUE) ~ "Clavularia",
                                         grepl("Duva florida", Comment, ignore.case = TRUE) ~ "Duva florida",
                                         grepl("Nephtheidae|Neptheidae Duva", Comment, ignore.case = TRUE) ~ "Nephtheidae",
                                         grepl("Umbellula", Comment, ignore.case = TRUE) ~ "Umbellula",
                                         grepl("Primnoidae", Comment, ignore.case = TRUE) ~ "Primnoidae",
                                         grepl("Pseudoanthomastus", Comment, ignore.case = TRUE) ~ "Pseudoanthomastus",
                                         grepl("corymorphid", Comment, ignore.case = TRUE) ~ "Corymorphidae",
                                         grepl("Tubularia|Tubularia many", Comment, ignore.case = TRUE) ~ "Tubularia",
                                         grepl("Cyanea", Comment, ignore.case = TRUE) ~ "Cyanea",
                                         grepl("Rhopalonematid", Comment, ignore.case = TRUE) ~ "Rhopalonematidae",
                                         grepl("Beroid", Comment, ignore.case = TRUE) ~ "Beroida",
                                         grepl("Tubastraea", Comment, ignore.case = TRUE) ~ "Tubastraea",
                                         grepl("Bathypsammia|Batypsammia", Comment, ignore.case = TRUE) ~ "Bathypsammia",
                                         grepl("cup coral|cup corals", Comment, ignore.case = TRUE) ~ "Scleractinia",
                                         grepl("Cnidaria Anthozoa Octocorallia Alcyonacea: Eunicella|Cnidaria Anthozoa Octocorallia Alcyonacea: Eunicela 3|Cnidaria Anthozoa Octocorallia Alcyonacea:Eunicella|Cnidaria Octocorallia Alcyonacea :Eunicella", Comment, ignore.case = TRUE) ~ "Eunicella",
                                         grepl("Plexaurid", Comment, ignore.case = TRUE) ~ "Plexauridae",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: Anthothela" ~ "Anthothela",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea:anthothela" ~ "Anthothela",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: Anthothelidae?" ~ "Anthothela",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: possible Anthothela" ~ "Malacalcyonacea",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea Calcaxonia Chrysogorgiidae: with Arthropoda Crustacea Malacostraca Decapoda Galatheoidea or Chirostyloidea:" ~ "Chrysogorgiidae",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea Holaxonia Plexauridae|Plexaurid" ~ "Plexauridae",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: with Echinodermata Crinoidea Articulata: on coarl" ~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: Pleaxauride many" ~ "Plexauridae",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea Holaxonia Plexauridae: Thesia or Eunicella?" ~ "Plexauridae",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea Holaxonia Plexauridae?" ~ "Plexauridae",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea Holaxonia Plexauridae" ~ "Plexauridae",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: skeleton with  Cnidaria Anthozoa Hexacorallia Zoanthidea:" ~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: yellow" ~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: 3+" ~ "Octocorallia",

                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea:3+"~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: 5+" ~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea:" ~ "Octocorallia",

                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea:few"~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea:many" ~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea: on larger rocks" ~ "Octocorallia",
                                         Comment == "Cnidaria Anthozoa Octocorallia Alcyonacea:several" ~ "Octocorallia",

                                         Comment == "coral" ~ "Cnidaria",
                                         Comment == "Cnidaria Meduszoa small" ~ "Medusa",
                                         Comment == "Cnidaria Anthozoa Hexacorallia Zoanthidea: on Paguroidea " ~ "Zoantharia",
                                         Comment == "Cnidaria Medusozoa Hydrozoa:on lophelia" ~ "Hydrozoa",
                                         Comment == "Cnidaria Meduszoa attracting many Echinodermata Echinoidea:" ~ "Medusa",
                                         Comment == "Cnidaria Meduszoa on seafloor surrounded but urchins" ~ "Medusa",
                                         Comment == "Cnidaria Meduszoa attracting many Echinodermata Echinoidea:" ~ "Medusa",

                                         # Arthropoda
                                         grepl("Paguroidea", Comment, ignore.case = TRUE) ~ "Paguroidea",
                                         grepl("Bathynectes", Comment, ignore.case = TRUE) ~ "Bathynectes",
                                         grepl("Cancer", Comment, ignore.case = TRUE) ~ "Cancer",
                                         grepl("Chaceon", Comment, ignore.case = TRUE) ~ "Chaceon",
                                         grepl("Chasmodes", Comment, ignore.case = TRUE) ~ "Chasmodes",
                                         grepl("Lithodid", Comment, ignore.case = TRUE) ~ "Lithodidae",
                                         grepl("Rochinia", Comment, ignore.case = TRUE) ~ "Rochinia",
                                         grepl("Pleisopenaeus armatus", Comment, ignore.case = TRUE) ~ "Cerataspis monstrosus",
                                         grepl("Dendrobranchiata or Pleocyemata", Comment, ignore.case = TRUE) ~ "Decapoda",
                                         grepl("Euminida picta", Comment, ignore.case = TRUE) ~ "Eumunida picta",
                                         grepl("Chirostyloidea  or Euminididae", Comment, ignore.case = TRUE) ~ "Chirostyloidea", #Chirostylidae & Eumunididae are families of Chirostyloidea
                                         grepl("Galatheoidea or Chirostyloidea", Comment, ignore.case = TRUE) ~ "Anomura", # Galatheoidea or Chirostyloidea are superfamilies of Decapoda, infraorder Anomura
                                         grepl("gastroptychus", Comment, ignore.case = TRUE) ~ "Gastroptychus",
                                         grepl("Munidopsis", Comment, ignore.case = TRUE) ~ "Munidopsis",
                                         grepl("Galatheid|Galatheids|Squat lobster", Comment, ignore.case = TRUE) ~ "Galatheidae",
                                         grepl("Nematocarcinid", Comment, ignore.case = TRUE) ~ "Nematocarcinidae",
                                         grepl("Gnathophausia", Comment, ignore.case = TRUE) ~ "Gnathophausia",
                                         grepl("Pycnogonida", Comment, ignore.case = TRUE) ~ "Pycnogonida",
                                         grepl("Chaceon fenneri|Golden Crab", Comment, ignore.case = TRUE) ~ "Chaceon fenneri",
                                         grepl("Glyphocrangon", Comment, ignore.case = TRUE) ~ "Glyphocrangon",

                                         Comment == "Arthropoda Crustacea Malacostraca Decapoda: Galatheid squat lobster" ~ "Galatheidae",
                                         Comment == "Arthropoda Pycnogonida: x2 on Acanella" ~ "Pycnogonida",
                                         Comment == "small shrimp" ~ "Decapoda",

                                         # Sponges
                                         grepl("Tretopleura", Comment, ignore.case = TRUE) ~ "Tretopleura",
                                         grepl("Dercitus bucklandi", Comment, fixed = TRUE) ~ "Dercitus (Dercitus) bucklandi",
                                         grepl("Phakellia", Comment, ignore.case = TRUE) ~ "Phakellia",
                                         grepl("Dercitus", Comment, ignore.case = TRUE) ~ "Dercitus",
                                         grepl("Chondrocladia", Comment, ignore.case = TRUE) ~ "Chondrocladia",
                                         grepl("Geodia", Comment, ignore.case = TRUE) ~ "Geodia",
                                         grepl("Polymastia", Comment, ignore.case = TRUE) ~ "Polymastia",
                                         grepl("Aphrocallistes|Aphrocallistidae|Aphrocallistes", Comment, ignore.case = TRUE) ~ "Aphrocallistidae",
                                         grepl("Euplectellidae", Comment, ignore.case = TRUE) ~ "Euplectellidae",
                                         grepl("Heterochone", Comment, ignore.case = TRUE) ~ "Heterochone",
                                         grepl("Rossellidae|Rosellid", Comment, ignore.case = TRUE) ~ "Rossellidae",
                                         grepl("Saccocalyx", Comment, ignore.case = TRUE) ~ "Saccocalyx",
                                         grepl("Geodia pachydermata", Comment, ignore.case = TRUE) ~ "Geodia pachydermata",
                                         grepl("Halipteris", Comment, ignore.case = TRUE) ~ "Balticinidae",
                                         Comment == "Porifera Demospongiae" ~ "Demospongiae",

                                         # Specimens
                                         Comment == "Collection D2_DIVE03_SPEC02BIO Chordata Tunicata: Pyruidae" ~ "Pyuridae",

                                         Comment == "END EX1806_D2_DIVE02_SPEC02BIO: Demospongiae" ~ "Demospongiae",

                                         Comment == "Start EX1806_DIVE01_SPEC01BIO: Demospongiae" ~ "Demospongiae",
                                         Comment == "End EX1806_DIVE01_SPEC01BIO: Demospongiae" ~ "Demospongiae",

                                         Comment == "Start EX1806_DIVE02_SPEC04BIO: Demospongiae" ~ "Demospongiae",
                                         Comment == "End EX1806_DIVE02_SPEC04BIO: Demospongiae" ~ "Demospongiae",

                                         Comment == "START EX1806_DIVE02_SPEC05BIO Demospongiae on rock with sediment" ~ "Demospongiae",
                                         Comment == "END EX1806_DIVE02_SPEC05BIO Demospongae on rock with sediment" ~ "Demospongiae",

                                         Comment == "Start EX1806_DIVE05_SPEC01BIO: Eunicella" ~ "Eunicella",
                                         Comment == "End EX1806_DIVE05_SPEC01BIO: Eunicella" ~ "Eunicella",

                                         Comment == "Start EX1806_DIVE05_SPEC05BIO: Swiftia" ~ "Swiftia",
                                         Comment == "End EX1806_DIVE05_SPEC05BIO: Swiftia" ~ "Swiftia",

                                         Comment == "End EX1806_DIVE10_SPEC04BIO: urchin test" ~ "Echinodermata",

                                         Comment == "Start EX1806_DIVE11_SPEC02BIO: Ophiuroidea along with sediment scoop" ~ "Ophiuroidea",
                                         Comment == "End EX1806_DIVE11_SPEC02BIO: Ophiuroidea along with sediment scoop" ~ "Ophiuroidea",

                                         Comment == "Start EX1806_DIVE12_SPEC01BIO: sea pen" ~ "Octocorallia",
                                         Comment == "End EX1806_DIVE12_SPEC01BIO: sea pen" ~ "Octocorallia",

                                         Comment == "Start EX1806_DIVE16_SPEC05BIOHyalinoecia sp." ~ "Hyalinoecia",
                                         Comment == "End EX1806_DIVE16_SPEC05BIOHyalinoecia sp." ~ "Hyalinoecia",

                                         Comment == "Start EX1806_DIVE02_SPEC03BIO: Geodia pachydermata" ~ "Geodia pachydermata",
                                         Comment == "End D2_DIVE02_SPEC03BIO Demospongiae: Geodia pachydermata" ~ "Geodia pachydermata",

                                         Comment == "START EX1806_DIVE06_SPEC03BIO: Geodia pachydermata" ~ "Geodia pachydermata",
                                         Comment == "End EX1806_DIVE06_SPEC03BIO: Geodia pachydermata" ~ "Geodia pachydermata",

                                         Comment == "Start EX1806_DIVE04_SPEC12BIO: Hexactinellida" ~ "Hexactinellida",
                                         Comment == "End EX1806_DIVE04_SPEC12BIO: Hexactinellida" ~ "Hexactinellida",

                                         Comment == "Start EX1806_DIVE06_SPEC01BIO: Hexactinellida" ~ "Hexactinellida",
                                         Comment == "End EX1806_DIVE06_SPEC01BIO: Hexactinellida" ~ "Hexactinellida",

                                         Comment == "Start EX1806_DIVE10_SPEC02BIO: Hydrozoa" ~ "Hydrozoa",
                                         Comment == "End EX1806_DIVE10_SPEC02BIO: Hydrozoa" ~ "Hydrozoa",

                                         Comment == "Start EX1806_DIVE10_SPEC05BIO: Octocorallia" ~ "Octocorallia",
                                         Comment == "End EX1806_DIVE10_SPEC05BIO: Octocorallia" ~ "Octocorallia",

                                         Comment == "Start EX1806_DIVE13_SPEC02BIO: Octocorallia" ~ "Octocorallia",
                                         Comment == "End EX1806_DIVE13_SPEC02BIO: Octocorallia" ~ "Octocorallia",

                                         Comment == "Start EX1806_DIVE13_SPEC03: Corallimorph" ~ "Corallimorpharia",
                                         Comment == "End EX1806_DIVE13_SPEC03:Corallimorph" ~ "Corallimorpharia",

                                         Comment == "Start EX1806_DIVE13_SPEC03BIO: Actiniaria" ~ "Actiniaria",
                                         Comment == "End EX1806_DIVE13_SPEC03BIO: Actiniaria" ~ "Actiniaria",

                                         Comment == "Start EX1806_DIVE15_SPEC01BIO: Nemertea" ~ "Nemertea",
                                         Comment == "End EX1806_DIVE15_SPEC01BIO: Nemertea" ~ "Nemertea",

                                         Comment == "Start EX1806_DIVE15_SPEC02BIO: soft coral" ~ "Octocorallia",
                                         Comment == "End EX1806_DIVE15_SPEC02BIO: soft coral" ~ "Octocorallia",

                                         Comment == "START EX1806_DIVE16_SPEC02BIO: Gastropoda" ~ "Gastropoda",
                                         Comment == "END EX1806_DIVE16_SPEC02BIO: Gastropoda" ~ "Gastropoda",

                                         Comment == "Start EX1806_DIVE17_SPEC01BIO: Chrysogorgia" ~ "Chrysogorgiidae",
                                         Comment == "End EX1806_DIVE17_SPEC01BIO: Chrysogorgia" ~ "Chrysogorgiidae",

                                         # Misc
                                         grepl("Unknown sessile invert", Comment, ignore.case = TRUE) ~ "Animalia",
                                         grepl("Sargassum", Comment, ignore.case = TRUE) ~ "Sargassum",
                                         Comment == "Other Mobile Benthic Invertebrate:  Possible enteropneuse in white/grey patch" ~ "Animalia",

                                         TRUE ~ as.character(Taxon))) |>

  # Rename some taxon values based on taxonomic reclassification or incorrect spelling

  dplyr::mutate(Taxon = dplyr::case_when(Taxon == "Paragorgiidae"~"Coralliidae",
                                         Taxon ==  "Leomonema"~"Laemonema",
                                         Taxon ==  "Lophiformes"~"Lophiiformes",
                                         Taxon ==  "Xenophyophorea"~"Xenophyophoroidea",
                                         Taxon ==  "Zoanthidea"~"Zoantharia",
                                         Taxon ==  "Medusa"~"Cnidaria incertae sedis",
                                         Taxon ==  "Cnidaria Meduszoa"~"Cnidaria incertae sedis",
                                         Taxon ==  "Meduszoa"~"Cnidaria incertae sedis",
                                         TRUE ~ as.character(Taxon))) |>

  # Remove the : and everything after and the ?
  dplyr::mutate(Taxon = gsub("\\:.*","",Taxon),
                Taxon = gsub("\\?.*","",Taxon)) |>

  # Remove the taxonomy columns bc those will be replaced by what we pull from WoRMS
  dplyr::select(-c(Kingdom, Phylum, Class, Order, Family, Genus))

# Use this df to compare what is in the original notes column to the new taxon column
compare <- dat1806 |> dplyr::select(Comment, Taxon)
#write.csv(compare, "EX1806_comparison.csv", row.names = F)

# Use the worrms::worrms R package to match the Taxon column to WoRMS

## Create a list of the names in the Taxon column
obs <- unique(dat1806$Taxon)

# use the wm_records_names column to pull the info for each Taxon item into a list
obs_dat <- worrms::wm_records_names(name = obs) |>
  dplyr::bind_rows(.id = "AphiaID") |>
  # Filter out
  dplyr::filter(status != "taxon inquirendum",
                status != "unaccepted",
                status != "quarantined",
                status != "junior subjective synonym",
                status != "junior objective synonym",
                valid_AphiaID != 602367,    # This gets rid of the Clavularia that is a Chromist, we need the Cnidarian only
                valid_AphiaID != 163921) |> # This gets rid of the Ctenophora that is a Chromist, we need the Animalia only
  # Select the columns of interest
  dplyr::select(scientificname, valid_name, 13:18)

names(obs_dat) <- stringr::str_to_title(names(obs_dat))

# Merge with the EX1806 dataset

EX1806_clean <- dplyr::left_join(dat1806, obs_dat, by = dplyr::join_by(Taxon == Scientificname),  relationship = "many-to-one" ) |>
  dplyr::mutate(Subphylum = dplyr::case_when(Taxon == "Tunicata" ~ "Tunicata", TRUE ~ as.character(Subphylum)),
                Subclass = dplyr::case_when(Taxon == "Octocorallia" ~ "Octocorallia",
                                            Taxon == "Articulata" ~ "Articulata",
                                            Taxon == "Cirripedia" ~ "Cirripedia",
                                            Taxon == "Ceriantharia" ~ "Ceriantharia",
                                            Taxon == "Heterobranchia" ~ "Heterobranchia",
                                            Taxon == "Echiura" ~ "Echiura", TRUE ~ as.character(Subclass)),
                Gigaclass = dplyr::case_when(Taxon == "Actinopterygii" ~ "Actinopterygii", TRUE ~ as.character(Gigaclass)),
                Superfamily = dplyr::case_when(Taxon == "Chirostyloidea" ~ "Chirostyloidea",
                                               Taxon == "Paguroidea" ~ "Paguroidea", TRUE ~ as.character(Superfamily)),
                Subfamily = dplyr::case_when(Taxon == "Peristediinae" ~ "Peristediinae", TRUE ~ as.character(Subfamily)),
                Superorder = dplyr::case_when(Taxon == "Decapodiformes" ~ "Decapodiformes", TRUE ~ as.character(Superorder)),
                Suborder = dplyr::case_when(Taxon == "Stolonifera" ~ "Stolonifera",
                                            Taxon == "Xenophyophoroidea" ~ "Xenophyophoroidea", TRUE ~ as.character(Suborder)),
                Infraorder = dplyr::case_when(Taxon == "Anomura" ~ "Anomura", TRUE ~ as.character(Infraorder))) |>
  # Fill in the Taxonomy column
  dplyr::mutate(Taxonomy = "WoRMS",
                # Create the Taxon_Path column then clean it up. There is probably a more efficient way to do this...
                Taxon_Path = paste("Biota", Kingdom, Phylum,  Subphylum,  Gigaclass,  Class,  Subclass,  Superorder, Order,
                                   Suborder,  Infraorder, Superfamily, Family, Subfamily, Genus, sep = " ")) |>

  dplyr::mutate(Taxon_Path = gsub("\\ NA.*", "", Taxon_Path), # Removes the NAs
                Taxon_Path = gsub("\\s+"," ", Taxon_Path), # Removes extra spaces
                Taxon_Path = gsub(" ", " / ", Taxon_Path), # Replaces single space with " / "
                Taxon_Path = sub(" / $", "", Taxon_Path)) |> # Removes the / when it is the last character in the string

  dplyr::select(Cruise_Name, cruise, dive_number, Dive_ID, start_date_time_UTC, end_date_time_UTC,
                Annotation_ID, Annotation_Source, creator, modifier, modified_date_UTC,
                Comment, To_Be_Reviewed, positive_reviews, total_reviews,
                Geoform, Geoform_Origin, Geoform_Type, annotation_type,
                Taxon_Path, Taxonomy, Taxon, Kingdom, Phylum, Subphylum, Superfamily, Family, Subfamily, Gigaclass,
                Superclass, Class, Subclass, Superorder, Order, Infraorder, Suborder, Genus, Species,
                ROV1_tempC,
                ROV1_tempC_time,
                ROV1_tempC_SBE,
                ROV1_tempC_SBE_time,
                ROV1_lat_dd,
                ROV1_lon_dd,
                ROV1_O2_mgL,
                ROV1_O2_mgL_time,
                ROV1_S_psu_time,
                ROV1_S_psu,
                ROV1_t_FTU,
                ROV1_t_FTU_time,
                ROV1_depth_m,
                ROV1_depth_m_time)


#write.csv(EX1806_clean, "EX1806_corrected.csv", row.names = F)
#compare <- EX1806_clean |> dplyr::select(Comment, Taxon, Taxon_Path)
#write.csv(compare, "EX1806_comparison.csv", row.names = F)

#tmp <- EX1806_clean |> dplyr::group_by(dive_number) |> dplyr::summarise(n_ann = length(Taxon))

return(EX1806_clean)
}
