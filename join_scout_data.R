# =============================================================================
# CROSSWALK V4: STATSBOMB <-> SKILLCORNER
# Improvements over v3:
#   1. Spanish/Portuguese nickname expansion (Javier->Javi, Daniel->Dani, etc.)
#   2. Expanded team alias table (Athletic Bilbao, Espanyol, CyD Leonesa, etc.)
#   3. 42 hardcoded manual matches for same-team cases too divergent for algos
#   4. Pass 3b: nickname-expanded token matching
# =============================================================================

library(tidyverse)
library(stringdist)
library(stringi)

scout_data <- readRDS("data/scout_data.rds")
sc_cache   <- readRDS("data/scout_skill_corner.rds")
fis_data   <- sc_cache$fis_data

# =============================================================================
# 1. NORMALIZATION
# =============================================================================

normalize_name <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[-.'ʼ´`]", " ") |>  # separators → space (fixes "Salles-Lamonge", "S.Sandoval", "O'Brien")
    str_remove_all("[^a-z0-9 ]") |>
    str_squish()
}

NAME_STOPWORDS <- c("de", "del", "la", "los", "las", "el", "da", "do", "dos",
                    "das", "di", "du", "van", "von", "le", "les", "bin", "binte",
                    "e", "y", "i")

name_tokens <- function(x) {
  tokens <- str_split(x, " ")[[1]]
  tokens[!tokens %in% NAME_STOPWORDS & nchar(tokens) > 1]
}

tokens_contained <- function(short_norm, long_norm) {
  short_tok <- name_tokens(short_norm)
  long_tok  <- name_tokens(long_norm)
  if (length(short_tok) == 0) return(FALSE)
  all(short_tok %in% long_tok)
}

# =============================================================================
# 2. NICKNAME EXPANSION TABLE
# Maps common Spanish/Portuguese/Catalan nicknames to full forms
# Used to expand SB short names before matching against SC full names
# =============================================================================

nickname_map <- c(
  # Spanish diminutives
  "dani"      = "daniel",
  "javi"      = "javier",
  "sergi"     = "sergio",
  "santi"     = "santiago",
  "nacho"     = "ignacio",
  "fran"      = "francisco",
  "rafa"      = "rafael",
  "toni"      = "antonio",
  "xavi"      = "xavier",
  "manu"      = "manuel",
  "guille"    = "guillermo",
  "rodri"     = "rodrigo",
  "diego"     = "diego",
  "juanmi"    = "juan miguel",
  "juanpe"    = "juan pedro",
  "juanlu"    = "juan luis",
  "grego"     = "gregorio",
  "yusi"      = "youssef",
  "isco"      = "francisco",
  "moi"       = "moises",
  "kike"      = "enrique",
  "piti"      = "pedro",
  "cesc"      = "francesc",
  "nico"      = "nicolas",
  "alex"      = "alejandro",
  "rober"     = "roberto",
  "edu"       = "eduardo",
  "joselu"    = "jose luis",
  "juanfran"  = "juan francisco",
  "aitor"     = "aitor",
  "oier"      = "oier",
  # Portuguese diminutives / nicknames
  "fred"      = "frederico",
  "eder"      = "ederson",
  "joao"      = "joao",
  "rony"      = "ronielson",
  "gabi"      = "gabriel",
  "vini"      = "vinicius",
  "neto"      = "jose",
  "juninho"   = "junior",
  "cacau"     = "claudemir",
  "hulk"      = "givanildo",
  "jorginho"  = "jorge",
  "firmino"   = "roberto",
  "luisao"    = "luiz",
  # English
  "matt"      = "matthew",
  "rob"       = "robert",
  "mike"      = "michael",
  "chris"     = "christopher",
  "archie"    = "archibald",
  "shapi"     = "magomedshapi",
  "nico"      = "nicholas",
  # Other
  "morad"     = "mourad",
  "marvel"    = "marvelous",
  "dilan"     = "dylan",
  # Football mononyms (SB common name → first token of SC legal name)
  "koke"      = "jorge",
  "raphinha"  = "raphael",
  "pedri"     = "pedro",
  "leo"       = "leonardo",
  "johnny"    = "joao",
  "isi"       = "isaac",
  "pep"       = "jose",
  "tony"      = "antonio",
  "ricardinho" = "ricardo",
  "marcao"    = "marcos",
  "talisca"   = "anderson",
  "billy"     = "guillermo",
  "lucho"     = "luis",
  "pacho"     = "francisco",
  "pipe"      = "felipe",
  "juanfer"   = "juan"
)

# Expand a normalized name: if first token is a known nickname, prepend full form
expand_nickname <- function(name_norm) {
  toks <- str_split(name_norm, " ")[[1]]
  if (length(toks) == 0) return(name_norm)
  first <- toks[1]
  if (first %in% names(nickname_map)) {
    expanded <- nickname_map[[first]]
    # Return both original and expanded version concatenated for matching
    paste(name_norm, paste(c(expanded, toks[-1]), collapse = " "), sep = " | ")
  } else {
    name_norm
  }
}

# Check if any version of name_a matches name_b (handles nickname expansion)
nickname_tokens_match <- function(name_a, name_b) {
  # Try all combinations of original/expanded
  versions_a <- str_split(expand_nickname(name_a), " \\| ")[[1]]
  versions_b <- str_split(expand_nickname(name_b), " \\| ")[[1]]
  any(outer(versions_a, versions_b, Vectorize(function(a, b) {
    tokens_contained(a, b) | tokens_contained(b, a)
  })))
}

# =============================================================================
# 3. TEAM ALIAS TABLE (expanded)
# =============================================================================

team_aliases <- tribble(
  ~raw_name,                                        ~canonical,
  # Liga MX
  "club universidad nacional",                      "pumas",
  "pumas unam",                                     "pumas",
  "club america",                                   "america",
  "america",                                        "america",
  "club de futbol tigres de la unal",               "tigres",
  "tigres uanl",                                    "tigres",
  "club de futbol monterrey",                       "monterrey",
  "monterrey",                                      "monterrey",
  "club santos laguna",                             "santos laguna",
  "santos laguna",                                  "santos laguna",
  "queretaro futbol club",                          "queretaro",
  "queretaro",                                      "queretaro",
  "club de futbol pachuca",                         "pachuca",
  "pachuca",                                        "pachuca",
  "deportivo toluca futbol club",                   "toluca",
  "toluca",                                         "toluca",
  "club social y deportivo leon",                   "leon",
  "leon",                                           "leon",
  "club deportivo guadalajara",                     "guadalajara",
  "guadalajara",                                    "guadalajara",
  "atletico de san luis",                           "atletico san luis",
  "atletico san luis",                              "atletico san luis",
  "xoloitcuintles club tijuana",                    "tijuana",
  "tijuana",                                        "tijuana",
  "club de futbol atlas",                           "atlas",
  "atlas",                                          "atlas",
  "mazatlan fc",                                    "mazatlan",
  "mazatlan",                                       "mazatlan",
  "fc juarez",                                      "juarez",
  "juarez",                                         "juarez",
  "club de futbol puebla de la franja",             "puebla",
  "puebla",                                         "puebla",
  "cruz azul futbol club",                          "cruz azul",
  "cruz azul",                                      "cruz azul",
  "rayos de necaxa",                                "necaxa",
  "necaxa",                                         "necaxa",
  # Argentina
  "club atletico boca juniors",                     "boca juniors",
  "boca juniors",                                   "boca juniors",
  "club atletico river plate",                      "river plate",
  "river plate",                                    "river plate",
  "racing club",                                    "racing club",
  "club atletico racing",                           "racing club",
  "club atletico independiente",                    "independiente",
  "independiente",                                  "independiente",
  "club atletico san lorenzo de almagro",           "san lorenzo",
  "san lorenzo",                                    "san lorenzo",
  "club atletico huracan",                          "huracan",
  "huracan",                                        "huracan",
  "estudiantes de la plata",                        "estudiantes",
  "estudiantes",                                    "estudiantes",
  "club atletico tigre",                            "tigre",
  "tigre",                                          "tigre",
  "velez sarsfield",                                "velez sarsfield",
  "club atletico velez sarsfield",                  "velez sarsfield",
  "godoy cruz antonio tomba",                       "godoy cruz",
  "club deportivo godoy cruz antonio tomba",        "godoy cruz",
  "godoy cruz",                                     "godoy cruz",
  "talleres de cordoba",                            "talleres",
  "talleres",                                       "talleres",
  "club atletico lanus",                            "lanus",
  "lanus",                                          "lanus",
  "defensa y justicia",                             "defensa y justicia",
  "club atletico platense",                         "platense",
  "platense",                                       "platense",
  "club atletico banfield",                         "banfield",
  "banfield",                                       "banfield",
  "ca banfield",                                    "banfield",
  "club atletico sarmiento",                        "sarmiento",
  "sarmiento",                                      "sarmiento",
  "club atletico union",                            "union santa fe",
  "union santa fe",                                 "union santa fe",
  "club atletico colon",                            "colon",
  "colon",                                          "colon",
  "rosario central",                                "rosario central",
  "club atletico rosario central",                  "rosario central",
  "newells old boys",                               "newells",
  "newell's old boys",                              "newells",
  "club atletico newells old boys",                 "newells",
  "arsenal de sarandi",                             "arsenal sarandi",
  "arsenal sarandi",                                "arsenal sarandi",
  "club atletico belgrano",                         "belgrano",
  "belgrano",                                       "belgrano",
  "club atletico central cordoba",                  "central cordoba",
  "central cordoba",                                "central cordoba",
  "club atletico gimnasia y esgrima",               "gimnasia",
  "gimnasia la plata",                              "gimnasia",
  "instituto atletico central cordoba",             "instituto",
  "instituto",                                      "instituto",
  "asociacion atletica argentinos juniors",         "argentinos juniors",
  "argentinos juniors",                             "argentinos juniors",
  "cs independiente rivadavia",                     "independiente rivadavia",
  "independiente rivadavia",                        "independiente rivadavia",
  "ca san martin de san juan",                      "san martin san juan",
  "san martin san juan",                            "san martin san juan",
  # Brasil
  "clube de regatas do flamengo",                   "flamengo",
  "cr flamengo",                                    "flamengo",
  "flamengo",                                       "flamengo",
  "sport club corinthians paulista",                "corinthians",
  "corinthians",                                    "corinthians",
  "sociedade esportiva palmeiras",                  "palmeiras",
  "palmeiras",                                      "palmeiras",
  "sao paulo futebol clube",                        "sao paulo",
  "sao paulo fc",                                   "sao paulo",
  "sao paulo",                                      "sao paulo",
  "clube atletico mineiro",                         "atletico mineiro",
  "atletico mineiro",                               "atletico mineiro",
  "sport club internacional",                       "internacional",
  "sc internacional",                               "internacional",
  "internacional",                                  "internacional",
  "cruzeiro esporte clube",                         "cruzeiro",
  "cruzeiro ec",                                    "cruzeiro",
  "cruzeiro",                                       "cruzeiro",
  "fluminense football club",                       "fluminense",
  "fluminense fc",                                  "fluminense",
  "fluminense",                                     "fluminense",
  "clube de regatas vasco da gama",                 "vasco da gama",
  "cr vasco da gama",                               "vasco da gama",
  "vasco da gama",                                  "vasco da gama",
  "botafogo de futebol e regatas",                  "botafogo",
  "botafogo",                                       "botafogo",
  "gremio foot-ball porto alegrense",               "gremio",
  "gremio fb porto alegrense",                      "gremio",
  "gremio",                                         "gremio",
  "sport club do recife",                           "sport recife",
  "sc do recife",                                   "sport recife",
  "sport recife",                                   "sport recife",
  "america futebol clube",                          "america mg",
  "america mg",                                     "america mg",
  "red bull bragantino",                            "bragantino",
  "rb bragantino",                                  "bragantino",
  "bragantino",                                     "bragantino",
  "athletico paranaense",                           "athletico pr",
  "club athletico paranaense",                      "athletico pr",
  "athletico pr",                                   "athletico pr",
  "fortaleza esporte clube",                        "fortaleza",
  "fortaleza ec",                                   "fortaleza",
  "fortaleza",                                      "fortaleza",
  "ceara sporting club",                            "ceara",
  "ceara",                                          "ceara",
  "esporte clube bahia",                            "bahia",
  "bahia",                                          "bahia",
  "coritiba foot ball club",                        "coritiba",
  "coritiba",                                       "coritiba",
  "goias esporte clube",                            "goias",
  "goias",                                          "goias",
  "cuiaba esporte clube",                           "cuiaba",
  "cuiaba",                                         "cuiaba",
  "santos futebol clube",                           "santos",
  "santos",                                         "santos",
  "ec vitoria",                                     "vitoria",
  "vitoria",                                        "vitoria",
  "mirassol futebol clube",                         "mirassol",
  "mirassol",                                       "mirassol",
  "juventude rs",                                   "juventude",
  "ec juventude",                                   "juventude",
  "juventude",                                      "juventude",
  # Colombia
  "atletico nacional",                              "atletico nacional",
  "club atletico nacional",                         "atletico nacional",
  "atletico nacional medellin",                     "atletico nacional",
  "millonarios futbol club",                        "millonarios",
  "millonarios fc",                                 "millonarios",
  "millonarios",                                    "millonarios",
  "independiente santa fe",                         "santa fe",
  "santa fe",                                       "santa fe",
  "deportivo independiente medellin",               "medellin",
  "medellin",                                       "medellin",
  "deportes tolima",                                "tolima",
  "tolima",                                         "tolima",
  "junior de barranquilla",                         "junior",
  "junior",                                         "junior",
  "club deportivo junior fc",                       "junior",             # SC name for Junior
  "deportivo cali",                                 "deportivo cali",
  "once caldas",                                    "once caldas",
  "envigado fc",                                    "envigado",
  "envigado",                                       "envigado",
  "club deportivo la equidad",                      "la equidad",
  "cd la equidad",                                  "la equidad",
  "la equidad",                                     "la equidad",
  "boyaca chico fc",                                "boyaca chico",
  "boyaca chico",                                   "boyaca chico",
  "ad pasto",                                       "pasto",              # SC name for Deportivo Pasto
  "deportivo pasto",                                "pasto",              # SB name for Deportivo Pasto
  "pasto",                                          "pasto",
  "fortaleza ceif fc",                              "fortaleza ceif",     # SC name (FC suffix)
  "fortaleza ceif",                                 "fortaleza ceif",     # SB name
  "ad union magdalena",                             "union magdalena",    # SC name
  "union magdalena",                                "union magdalena",    # SB name
  "deportivo pereira",                              "deportivo pereira",
  "atletico bucaramanga",                           "bucaramanga",
  "america de cali",                                "america cali",       # SC/SB Colombia club
  "club america de cali",                           "america cali",
  "independiente medellin",                         "medellin",
  "independiente medellin fc",                      "medellin",
  "aguilas doradas rionegro",                       "aguilas doradas",
  "rionegro",                                       "aguilas doradas",   # SB alternate name
  "rionegro aguilas",                               "aguilas doradas",   # SC name for Rionegro
  "aguilas doradas",                                "aguilas doradas",
  "ca bucaramanga",                                 "bucaramanga",
  "atletico bucaramanga",                           "bucaramanga",
  "bucaramanga",                                    "bucaramanga",
  "cd alianza petrolera",                           "alianza petrolera",
  "alianza petrolera",                              "alianza petrolera",
  "club llaneros sa",                               "llaneros",
  "llaneros",                                       "llaneros",
  # Chile
  "club universidad de chile",                      "universidad de chile",
  "universidad de chile",                           "universidad de chile",
  "club deportivo universidad catolica",            "universidad catolica",
  "universidad catolica",                           "universidad catolica",
  "club social y deportivo colo-colo",              "colo colo",
  "colo-colo",                                      "colo colo",
  "colo colo",                                      "colo colo",
  "club deportivo huachipato",                      "huachipato",
  "huachipato",                                     "huachipato",
  "club deportivo palestino",                       "palestino",
  "palestino",                                      "palestino",
  "club deportivo ohiggins",                        "ohiggins",
  "ohiggins",                                       "ohiggins",
  "o'higgins",                                      "ohiggins",
  "everton de vina del mar",                        "everton chile",
  "everton de vina",                                "everton chile",
  "club deportivo everton de vina del mar",         "everton chile",
  "club de deportes union espanola",                "union espanola",
  "union espanola",                                 "union espanola",
  "audax italiano la florida",                      "audax italiano",
  "audax italiano",                                 "audax italiano",
  "club de deportes la serena",                     "la serena",
  "la serena",                                      "la serena",
  "nublense",                                       "nublense",
  "club de deportes antofagasta",                   "antofagasta",
  "antofagasta",                                    "antofagasta",
  # MLS
  "inter miami cf",                                 "inter miami",
  "inter miami",                                    "inter miami",
  "los angeles fc",                                 "lafc",
  "lafc",                                           "lafc",
  "los angeles galaxy",                             "la galaxy",
  "la galaxy",                                      "la galaxy",
  "new york city fc",                               "new york city",
  "new york city",                                  "new york city",
  "new york red bulls",                             "red bulls",
  "red bulls",                                      "red bulls",
  "seattle sounders fc",                            "seattle sounders",
  "seattle sounders",                               "seattle sounders",
  "atlanta united fc",                              "atlanta united",
  "atlanta united",                                 "atlanta united",
  "portland timbers",                               "portland timbers",
  "sporting kansas city",                           "sporting kc",
  "sporting kc",                                    "sporting kc",
  "fc cincinnati",                                  "cincinnati",
  "cincinnati",                                     "cincinnati",
  "columbus crew",                                  "columbus crew",
  "new england revolution",                         "new england",
  "new england",                                    "new england",
  "cf montreal",                                    "montreal",
  "montreal",                                       "montreal",
  "toronto fc",                                     "toronto",
  "toronto",                                        "toronto",
  "orlando city sc",                                "orlando city",
  "orlando city",                                   "orlando city",
  "philadelphia union",                             "philadelphia union",
  "dc united",                                      "dc united",
  "chicago fire fc",                                "chicago fire",
  "chicago fire",                                   "chicago fire",
  "minnesota united fc",                            "minnesota united",
  "minnesota united",                               "minnesota united",
  "real salt lake",                                 "real salt lake",
  "houston dynamo fc",                              "houston dynamo",
  "houston dynamo",                                 "houston dynamo",
  "vancouver whitecaps fc",                         "vancouver whitecaps",
  "vancouver whitecaps",                            "vancouver whitecaps",
  "fc dallas",                                      "dallas",
  "dallas",                                         "dallas",
  "san jose earthquakes",                           "san jose",
  "san jose",                                       "san jose",
  "colorado rapids",                                "colorado rapids",
  "nashville sc",                                   "nashville",
  "nashville",                                      "nashville",
  "austin fc",                                      "austin",
  "austin",                                         "austin",
  "charlotte fc",                                   "charlotte",
  "charlotte",                                      "charlotte",
  "st louis city sc",                               "st louis city",
  "st louis city",                                  "st louis city",
  # Premier League
  "manchester city fc",                             "manchester city",
  "manchester city",                                "manchester city",
  "manchester united fc",                           "manchester united",
  "manchester united",                              "manchester united",
  "arsenal fc",                                     "arsenal",
  "arsenal",                                        "arsenal",
  "chelsea fc",                                     "chelsea",
  "chelsea",                                        "chelsea",
  "liverpool fc",                                   "liverpool",
  "liverpool",                                      "liverpool",
  "tottenham hotspur fc",                           "tottenham",
  "tottenham hotspur",                              "tottenham",
  "newcastle united fc",                            "newcastle",
  "newcastle united",                               "newcastle",
  "aston villa fc",                                 "aston villa",
  "aston villa",                                    "aston villa",
  "west ham united fc",                             "west ham",
  "west ham united",                                "west ham",
  "brighton and hove albion fc",                    "brighton",
  "brighton & hove albion",                         "brighton",
  "brighton and hove albion",                       "brighton",
  "wolverhampton wanderers fc",                     "wolves",
  "wolverhampton wanderers",                        "wolves",
  "brentford fc",                                   "brentford",
  "brentford",                                      "brentford",
  "fulham fc",                                      "fulham",
  "fulham",                                         "fulham",
  "crystal palace fc",                              "crystal palace",
  "crystal palace",                                 "crystal palace",
  "everton fc",                                     "everton",
  "everton",                                        "everton",
  "nottingham forest fc",                           "nottingham forest",
  "nottingham forest",                              "nottingham forest",
  "luton town fc",                                  "luton",
  "luton town",                                     "luton",
  "burnley fc",                                     "burnley",
  "burnley",                                        "burnley",
  "sheffield united fc",                            "sheffield united",
  "sheffield united",                               "sheffield united",
  "afc bournemouth",                                "bournemouth",
  "bournemouth",                                    "bournemouth",
  # LaLiga
  "real madrid cf",                                 "real madrid",
  "real madrid",                                    "real madrid",
  "fc barcelona",                                   "barcelona",
  "barcelona",                                      "barcelona",
  "atletico de madrid",                             "atletico madrid",
  "atletico madrid",                                "atletico madrid",
  "sevilla fc",                                     "sevilla",
  "sevilla",                                        "sevilla",
  "real betis balompie",                            "real betis",
  "real betis",                                     "real betis",
  "real sociedad de futbol",                        "real sociedad",
  "real sociedad",                                  "real sociedad",
  "villarreal cf",                                  "villarreal",
  "villarreal",                                     "villarreal",
  "athletic club",                                  "athletic bilbao",
  "athletic club de bilbao",                        "athletic bilbao",
  "athletic bilbao",                                "athletic bilbao",
  "rc celta de vigo",                               "celta vigo",
  "celta vigo",                                     "celta vigo",
  "rcd espanyol de barcelona",                      "espanyol",
  "rcd espanyol",                                   "espanyol",
  "espanyol",                                       "espanyol",
  "getafe cf",                                      "getafe",
  "getafe",                                         "getafe",
  "rayo vallecano de madrid",                       "rayo vallecano",
  "rayo vallecano",                                 "rayo vallecano",
  "ud almeria",                                     "almeria",
  "almeria",                                        "almeria",
  "ca osasuna",                                     "osasuna",
  "osasuna",                                        "osasuna",
  "cadiz cf",                                       "cadiz",
  "cadiz",                                          "cadiz",
  "ud las palmas",                                  "las palmas",
  "las palmas",                                     "las palmas",
  "girona fc",                                      "girona",
  "girona",                                         "girona",
  "granada cf",                                     "granada",
  "granada",                                        "granada",
  "deportivo alaves",                               "alaves",
  "alaves",                                         "alaves",
  "real valladolid cf",                             "valladolid",
  "real valladolid",                                "valladolid",
  "valladolid",                                     "valladolid",
  "ud levante",                                     "levante",
  "levante ud",                                     "levante",
  "levante",                                        "levante",
  "real mallorca",                                  "mallorca",
  "rcd mallorca",                                   "mallorca",
  "mallorca",                                       "mallorca",
  "valencia cf",                                    "valencia",
  "valencia",                                       "valencia",
  "elche cf",                                       "elche",
  "elche",                                          "elche",
  "real oviedo",                                    "oviedo",
  "oviedo",                                         "oviedo",
  # LaLiga 2
  "sd eibar",                                       "eibar",
  "eibar",                                          "eibar",
  "real sporting de gijon",                         "sporting gijon",
  "sporting gijon",                                 "sporting gijon",
  "sd huesca",                                      "huesca",
  "sociedad deportiva huesca",                      "huesca",
  "huesca",                                         "huesca",
  "real oviedo",                                    "oviedo",
  "oviedo",                                         "oviedo",
  "cd leganes",                                     "leganes",
  "leganes",                                        "leganes",
  "fc cartagena",                                   "cartagena",
  "cartagena",                                      "cartagena",
  "albacete balompie",                              "albacete",
  "albacete",                                       "albacete",
  "cultural y deportiva leonesa",                   "cyd leonesa",
  "cyd leonesa",                                    "cyd leonesa",
  "burgos cf",                                      "burgos",
  "burgos",                                         "burgos",
  "cd mirandes",                                    "mirandes",
  "mirandes",                                       "mirandes",
  "racing santander",                               "racing santander",
  "real zaragoza",                                  "zaragoza",
  "zaragoza",                                       "zaragoza",
  "cordoba cf",                                     "cordoba",
  "cordoba",                                        "cordoba",
  "fc andorra",                                     "fc andorra",
  "cd castellon",                                   "castellon",
  "castellon",                                      "castellon",
  "malaga cf",                                      "malaga",
  "malaga",                                         "malaga",
  "rc deportivo de la coruna",                      "deportivo la coruna",
  "rc deportivo la coruna",                         "deportivo la coruna",
  "deportivo la coruna",                            "deportivo la coruna",
  "real sociedad san sebastian b",                  "real sociedad b",
  "real sociedad b",                                "real sociedad b",
  "agrupacion deportiva ceuta fc",                  "ceuta",
  "ad ceuta fc",                                    "ceuta",              # SB alternate name
  "ceuta",                                          "ceuta",
  "ud las palmas",                                  "las palmas",
  # Serie A
  "juventus fc",                                    "juventus",
  "juventus",                                       "juventus",
  "ac milan",                                       "milan",
  "milan",                                          "milan",
  "fc internazionale milano",                       "inter",
  "inter milan",                                    "inter",
  "internazionale",                                 "inter",
  "as roma",                                        "roma",
  "roma",                                           "roma",
  "ss lazio",                                       "lazio",
  "lazio",                                          "lazio",
  "atalanta bc",                                    "atalanta",
  "atalanta",                                       "atalanta",
  "acf fiorentina",                                 "fiorentina",
  "fiorentina",                                     "fiorentina",
  "ssc napoli",                                     "napoli",
  "napoli",                                         "napoli",
  "torino fc",                                      "torino",
  "torino",                                         "torino",
  "udinese calcio",                                 "udinese",
  "udinese",                                        "udinese",
  "bologna fc",                                     "bologna",
  "bologna",                                        "bologna",
  "us sassuolo calcio",                             "sassuolo",
  "sassuolo",                                       "sassuolo",
  "hellas verona fc",                               "verona",
  "hellas verona",                                  "verona",
  "verona",                                         "verona",
  "cagliari calcio",                                "cagliari",
  "cagliari",                                       "cagliari",
  "us salernitana",                                 "salernitana",
  "salernitana",                                    "salernitana",
  "empoli fc",                                      "empoli",
  "empoli",                                         "empoli",
  "frosinone calcio",                               "frosinone",
  "frosinone",                                      "frosinone",
  "genoa cfc",                                      "genoa",
  "genoa",                                          "genoa",
  "us lecce",                                       "lecce",
  "lecce",                                          "lecce",
  "ac monza",                                       "monza",
  "monza",                                          "monza",
  # Turquía
  "galatasaray sk",                                 "galatasaray",
  "galatasaray",                                    "galatasaray",
  "fenerbahce sk",                                  "fenerbahce",
  "fenerbahce",                                     "fenerbahce",
  "fenerbahce",                                     "fenerbahce",
  "besiktas jk",                                    "besiktas",
  "besiktas",                                       "besiktas",
  "trabzonspor",                                    "trabzonspor",
  "istanbul basaksehir fk",                         "basaksehir",
  "basaksehir",                                     "basaksehir",
  "sivasspor",                                      "sivasspor",
  "kayserispor",                                    "kayserispor",
  "antalyaspor",                                    "antalyaspor",
  "adana demirspor",                                "adana demirspor",
  "kasimpasa sk",                                   "kasimpasa",
  "kasimpasa",                                      "kasimpasa",
  "konyaspor",                                      "konyaspor",
  "alanyaspor",                                     "alanyaspor",
  "gaziantep fk",                                   "gaziantep",
  "gaziantep",                                      "gaziantep",
  "mke ankaragucu",                                 "ankaragucu",
  "ankaragucu",                                     "ankaragucu",
  # UCL / UEL
  "fc bayern munchen",                              "bayern munich",
  "fc bayern munich",                               "bayern munich",
  "bayern munich",                                  "bayern munich",
  "borussia dortmund",                              "dortmund",
  "bv borussia 09 dortmund",                        "dortmund",
  "paris saint-germain fc",                         "psg",
  "paris saint-germain",                            "psg",
  "paris saint germain",                            "psg",
  "psg",                                            "psg",
  "rb leipzig",                                     "rb leipzig",
  "rasenballsport leipzig",                         "rb leipzig",
  "afc ajax",                                       "ajax",
  "ajax",                                           "ajax",
  "fc porto",                                       "porto",
  "porto",                                          "porto",
  "sl benfica",                                     "benfica",
  "benfica",                                        "benfica",
  "sporting clube de portugal",                     "sporting cp",
  "sporting cp",                                    "sporting cp",
  "psv eindhoven",                                  "psv",
  "psv",                                            "psv",
  "fk shakhtar donetsk",                            "shakhtar",
  "shakhtar donetsk",                               "shakhtar",
  "celtic fc",                                      "celtic",
  "celtic",                                         "celtic",
  "rangers fc",                                     "rangers",
  "club brugge kv",                                 "club brugge",
  "club brugge",                                    "club brugge",
  "bayer 04 leverkusen",                            "leverkusen",
  "bayer leverkusen",                               "leverkusen"
)

canonicalize_team <- function(x) {
  norm <- normalize_name(x)
  ifelse(norm %in% team_aliases$raw_name,
         team_aliases$canonical[match(norm, team_aliases$raw_name)],
         norm)
}

# =============================================================================
# 4. HARDCODED MANUAL MATCHES
# These are verified same-team cases where names are too divergent for algos
# Format: sc_player_id, sb_player_id, league, match_type="manual"
# =============================================================================

manual_matches <- tribble(
  ~sc_player_id, ~sb_player_id, ~league,
  # Liga MX
  1028129,  43478,   "Liga MX",          # Edgar Ivan Lopez Rodriguez -> Edgar López
  5078,     8283,    "Liga MX",          # João Pedro Geraldino -> João Pedro
  11482,    6572,    "Liga MX",          # Juan Pedro Ramírez López -> Juanpe
  24584,    23849,   "Liga MX",          # Francisco José Villalba -> Fran Villalba
  25644,    33360,   "Liga MX",          # Mourad Daoudi El Ghezouani -> Morad
  1018107,  482805,  "Liga MX",          # Dennis Yael Ramírez Castillo -> D.Ramírez Castillo
  # LaLiga
  2190,     6697,    "LaLiga",           # Daniel Ceballos Fernández -> Dani Ceballos
  13881,    12194,   "LaLiga",           # Santiago Comesaña Veiga -> Santi Comesaña
  5398,     6596,    "LaLiga",           # José Luis Gayá Peña -> José Gayá (Valencia) [fixed: was wrong sc_id 29176]
  69308,    41084,   "LaLiga",           # Juan Luis Sánchez Velasco -> Juanlu (Sevilla)
  120550,   255811,  "LaLiga",           # Youssef Enriquez Lekhedim -> Yusi (Alavés)
  5942,     5199,    "LaLiga",           # Jorge Resurrección Merodio -> Koke (Atlético Madrid)
  16062,    10595,   "LaLiga",           # Raphael Dias Belloli -> Raphinha (Barcelona)
  25738,    30486,   "LaLiga",           # Pedro González López -> Pedri (Barcelona)
  35342,    68574,   "LaLiga",           # Nicholas Williams Arthuer -> Nico Williams (Athletic Club)
  31814,    32122,   "LaLiga",           # João Lucas De Souza Cardoso -> Johnny (Atlético Madrid)
  16261,    11166,   "LaLiga",           # Marcos do Nascimento Teixeira -> Marcão (Sevilla)
  35630,    39073,   "LaLiga",           # Moriba Kourouma Kourouma -> Ilaix Moriba (Celta Vigo)
  12242,    6802,    "LaLiga",           # Jonathan Castro Otto -> Jonny Castro (Alavés)
  24084,    36728,   "LaLiga",           # Mateu Jaume Morey Bauzà -> Mate Morey (Mallorca)
  32127,    49303,   "LaLiga",           # Josep María Chavarría Pérez -> Pep Chavarría (Rayo)
  142458,   195724,  "LaLiga",           # Alexandre Zurawski -> Alemão (Rayo Vallecano)
  # LaLiga 2
  647233,   394211,  "LaLiga 2",         # Thiago Helguera Merello Volante -> Thiago Emanuel Helguera Merello
  26157,    41603,   "LaLiga 2",         # Daniel Villahermosa Martínez -> Dani Villahermosa
  11612,    25830,   "LaLiga 2",         # Gregorio Sierra Pérez -> Grego Sierra
  13970,    17027,   "LaLiga 2",         # Sergio Guardiola Navarro -> Sergi Guardiola
  23204,    20063,   "LaLiga 2",         # Javier Ontiveros Parra -> Javi Ontiveros
  41327,    39344,   "LaLiga 2",         # Xavier Sintes Egea -> Xavi Sintes
  66228,    136328,  "LaLiga 2",         # Guillermo Bueno López -> Guille Bueno
  41319,    39637,   "LaLiga 2",         # Juan Miguel Latasa Fernández -> Juanmi Latasa
  14001,    11669,   "LaLiga 2",         # Gonzalo Julián Melero Manzanares -> Gonzalo Melero (Leganés) [fixed]
  69346,    406828,  "LaLiga 2",         # Alejandro Meléndez Ruiz -> Alejandro Roldán (Albacete)
  70523,    140421,  "LaLiga 2",         # Marvelous Antolín Garzón -> Marvel (Leganés)
  70530,    140431,  "LaLiga 2",         # Javier Villar del Fraile -> Javi Villar (Albacete)
  11552,    6908,    "LaLiga 2",         # Iván Alejo Peralta -> Iván Alejo (Valladolid) [fixed: was wrong sc_id 759034]
  35954,    136414,  "LaLiga 2",         # Luís Henrique de Barros Lopes -> Luis Nlavo (Leganés)
  # Brasil
  29737,    31013,   "Brasil – Série A",  # Fabrizio Germán Angileri -> Fabricio Angileri (Corinthians)
  13942,    11299,   "Brasil – Série A",  # Christopher Ramos De la Flor -> Chris Ramos (Botafogo)
  122012,   129800,  "Brasil – Série A",  # Matheus Leonardo Sales Cardoso -> Matheus Alexandre (Sport)
  24843,    44092,   "Brasil – Série A",  # Leonardo Rech Ortiz -> Léo Ortiz (Flamengo)
  # Colombia
  525235,   364863,  "Colombia",          # Geindry Steven Cuervo Holguín -> Gendry Cuervo (Envigado)
  35743,    32434,   "Colombia",          # Yílmar Andrés Velásquez -> Yilmar Velázquez (Santa Fe)
  998322,   460960,  "Colombia",          # Frey David Berrio Roqueme -> F.Berrio Roqueme (Envigado)
  # Chile
  28428,    35737,   "Chile",             # Dilan Patricio Zúñiga -> Dylan Patricio Zúñiga (Palestino)
  43535,    102512,  "Chile",             # cc León Muñoz -> Maicol Andrés León Muñoz (Huachipato)
  # MLS
  17974,    13035,   "MLS",               # Kellyn Acosta -> Kellyn Kai Perry-Acosta (Chicago Fire)
  25193,    32247,   "MLS",               # Gabriel Fortes Chaves -> Gabriel Pec (LA Galaxy)
  15869,    18920,   "MLS",               # Magomed-Shapi Suleymanov -> Shapi Suleymanov (Sporting KC)
  817415,   429014,  "MLS",               # Harbor Michael Miller -> Harbor Tarczynski-Miller (LA Galaxy)
  32940,    47159,   "MLS",               # Guilherme da Trindade Dubas -> Guilherme Biro (Austin)
  # Turquía
  31221,    39444,   "Turquía – Süper Lig", # Archibald Norman Brown -> Archie Brown (Fenerbahçe)
  12008,    9904,    "Turquía – Süper Lig", # Frederico Rodrigues de Paul Santos -> Fred (Fenerbahçe)
  35125,    43133,   "Turquía – Süper Lig", # Anderson Souza Conceição -> Talisca (Fenerbahçe)
  # Serie A
  16057,    10322,   "Serie A",            # Domilson Cordeiro dos Santos -> Dodô (Fiorentina)
  # Liga MX
  142458,   195724,  "Liga MX",            # Alexandre Zurawski -> Alemão (Pachuca)
  # UCL/UEL (duplicate for Archie Brown in UEL)
  31221,    39444,   "UEFA Europa League"   # Archibald Norman Brown -> Archie Brown
) |>
  mutate(
    sc_player_id = as.character(sc_player_id),
    sb_player_id = as.character(sb_player_id),
    match_type   = "manual"
  )

# =============================================================================
# 5. CROSSWALK BUILDER V4
# =============================================================================

build_crosswalk_v4 <- function(sb_df, sc_df, league_name,
                               fuzzy_threshold = 0.25,
                               fuzzy_method    = "jw") {
  
  message("  Building crosswalk for: ", league_name)
  
  # Get manual matches for this league, exclude from algo matching
  league_manual_sc <- manual_matches |>
    filter(league == league_name) |>
    pull(sc_player_id)
  
  league_manual_sb <- manual_matches |>
    filter(league == league_name) |>
    pull(sb_player_id)
  
  sb_prep <- sb_df |>
    select(sb_player_id = player_id, player_name_sb = player_name, team_name_sb = team_name) |>
    distinct() |>
    mutate(
      sb_player_id  = as.character(sb_player_id),
      name_norm_sb  = normalize_name(player_name_sb),
      team_canon_sb = canonicalize_team(team_name_sb)
    ) |>
    filter(!sb_player_id %in% league_manual_sb)
  
  sc_prep <- sc_df |>
    select(sc_player_id = player_id, player_name_sc = player_name, team_name_sc = team_name) |>
    distinct() |>
    mutate(
      sc_player_id  = as.character(sc_player_id),
      name_norm_sc  = normalize_name(player_name_sc),
      team_canon_sc = canonicalize_team(team_name_sc)
    ) |>
    filter(!sc_player_id %in% league_manual_sc)
  
  sb_rem <- sb_prep
  sc_rem <- sc_prep
  all_passes <- list()
  
  # ── PASS 1: Exact name + canonical team ──────────────────────────────────
  p <- inner_join(sb_rem, sc_rem,
                  by = c("name_norm_sb" = "name_norm_sc",
                         "team_canon_sb" = "team_canon_sc")) |>
    mutate(match_type = "p1_exact_name_team")
  all_passes[["p1"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P1 exact name+team:         ", nrow(p))
  
  # ── PASS 2: Exact name, unique 1:1 ───────────────────────────────────────
  p <- inner_join(sb_rem, sc_rem, by = c("name_norm_sb" = "name_norm_sc")) |>
    group_by(name_norm_sb) |> filter(n() == 1) |> ungroup() |>
    mutate(match_type = "p2_exact_name_unique")
  all_passes[["p2"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P2 exact name unique:        ", nrow(p))
  
  # ── PASS 3: Token subset, same canonical team ─────────────────────────────
  shared_teams <- intersect(sb_rem$team_canon_sb, sc_rem$team_canon_sc)
  sb_t <- sb_rem |> filter(team_canon_sb %in% shared_teams)
  sc_t <- sc_rem |> filter(team_canon_sc %in% shared_teams)
  
  if (nrow(sb_t) > 0 && nrow(sc_t) > 0) {
    p <- sb_t |>
      rename(team_canon = team_canon_sb) |>
      inner_join(sc_t |> rename(team_canon = team_canon_sc),
                 by = "team_canon", relationship = "many-to-many") |>
      rowwise() |>
      mutate(
        token_match = tokens_contained(name_norm_sb, name_norm_sc) |
          tokens_contained(name_norm_sc, name_norm_sb),
        dist = stringdist(name_norm_sb, name_norm_sc, method = fuzzy_method)
      ) |>
      ungroup() |>
      filter(token_match) |>
      group_by(sb_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      group_by(sc_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      mutate(match_type = "p3_token_same_team")
  } else { p <- tibble() }
  all_passes[["p3"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P3 token same team:          ", nrow(p))
  
  # ── PASS 3b: Nickname-expanded token subset, same canonical team ──────────
  shared_teams <- intersect(sb_rem$team_canon_sb, sc_rem$team_canon_sc)
  sb_t <- sb_rem |> filter(team_canon_sb %in% shared_teams)
  sc_t <- sc_rem |> filter(team_canon_sc %in% shared_teams)
  
  if (nrow(sb_t) > 0 && nrow(sc_t) > 0) {
    p <- sb_t |>
      rename(team_canon = team_canon_sb) |>
      inner_join(sc_t |> rename(team_canon = team_canon_sc),
                 by = "team_canon", relationship = "many-to-many") |>
      rowwise() |>
      mutate(
        nick_match = nickname_tokens_match(name_norm_sb, name_norm_sc),
        dist       = stringdist(name_norm_sb, name_norm_sc, method = fuzzy_method)
      ) |>
      ungroup() |>
      filter(nick_match) |>
      group_by(sb_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      group_by(sc_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      mutate(match_type = "p3b_nickname_same_team")
  } else { p <- tibble() }
  all_passes[["p3b"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P3b nickname same team:      ", nrow(p))
  
  # ── PASS 4: Fuzzy match, same canonical team, relaxed threshold ───────────
  shared_teams <- intersect(sb_rem$team_canon_sb, sc_rem$team_canon_sc)
  sb_f <- sb_rem |> filter(team_canon_sb %in% shared_teams)
  sc_f <- sc_rem |> filter(team_canon_sc %in% shared_teams)
  
  if (nrow(sb_f) > 0 && nrow(sc_f) > 0) {
    p <- sb_f |>
      rename(team_canon = team_canon_sb) |>
      inner_join(sc_f |> rename(team_canon = team_canon_sc),
                 by = "team_canon", relationship = "many-to-many") |>
      mutate(dist = stringdist(name_norm_sb, name_norm_sc, method = fuzzy_method)) |>
      filter(dist < fuzzy_threshold) |>
      group_by(sb_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      group_by(sc_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      mutate(match_type = paste0("p4_fuzzy_", round(dist, 3)))
  } else { p <- tibble() }
  all_passes[["p4"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P4 fuzzy same team:          ", nrow(p))
  
  # ── PASS 5: Token subset, no team restriction ─────────────────────────────
  if (nrow(sb_rem) > 0 && nrow(sc_rem) > 0) {
    p <- sb_rem |>
      cross_join(sc_rem) |>
      rowwise() |>
      mutate(
        common_tok = length(intersect(name_tokens(name_norm_sb), name_tokens(name_norm_sc))),
        sb_in_sc   = tokens_contained(name_norm_sb, name_norm_sc),
        sc_in_sb   = tokens_contained(name_norm_sc, name_norm_sb),
        dist       = stringdist(name_norm_sb, name_norm_sc, method = fuzzy_method)
      ) |>
      ungroup() |>
      filter((sb_in_sc | sc_in_sb) | (common_tok >= 2 & dist < 0.15)) |>
      group_by(sb_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      group_by(sc_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      mutate(match_type = "p5_token_no_team")
  } else { p <- tibble() }
  all_passes[["p5"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P5 token no team:            ", nrow(p))
  
  # ── PASS 5b: Nickname-expanded token, no team ─────────────────────────────
  if (nrow(sb_rem) > 0 && nrow(sc_rem) > 0) {
    p <- sb_rem |>
      cross_join(sc_rem) |>
      rowwise() |>
      mutate(
        nick_match = nickname_tokens_match(name_norm_sb, name_norm_sc),
        dist       = stringdist(name_norm_sb, name_norm_sc, method = fuzzy_method)
      ) |>
      ungroup() |>
      filter(nick_match) |>
      group_by(sb_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      group_by(sc_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      mutate(match_type = "p5b_nickname_no_team")
  } else { p <- tibble() }
  all_passes[["p5b"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P5b nickname no team:        ", nrow(p))
  
  # ── PASS 6: Fuzzy, no team, very strict ───────────────────────────────────
  if (nrow(sb_rem) > 0 && nrow(sc_rem) > 0) {
    p <- sb_rem |>
      cross_join(sc_rem) |>
      mutate(dist = stringdist(name_norm_sb, name_norm_sc, method = fuzzy_method)) |>
      filter(dist < 0.08) |>
      group_by(sb_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      group_by(sc_player_id) |> slice_min(dist, n = 1, with_ties = FALSE) |> ungroup() |>
      mutate(match_type = paste0("p6_fuzzy_noTeam_", round(dist, 3)))
  } else { p <- tibble() }
  all_passes[["p6"]] <- p
  sb_rem <- sb_rem |> filter(!sb_player_id %in% p$sb_player_id)
  sc_rem <- sc_rem |> filter(!sc_player_id %in% p$sc_player_id)
  message("    P6 fuzzy no team:            ", nrow(p))
  message("    Unmatched SB: ", nrow(sb_rem), " | Unmatched SC: ", nrow(sc_rem))
  
  # ── Combine algo passes ───────────────────────────────────────────────────
  algo_crosswalk <- bind_rows(all_passes) |>
    select(sb_player_id, player_name_sb, team_name_sb,
           sc_player_id, player_name_sc, team_name_sc,
           match_type) |>
    mutate(league = league_name)
  
  # ── Add manual matches for this league ───────────────────────────────────
  league_manual <- manual_matches |>
    filter(league == league_name) |>
    left_join(
      sb_df |> select(sb_player_id = player_id, player_name_sb = player_name,
                      team_name_sb = team_name) |>
        mutate(sb_player_id = as.character(sb_player_id)),
      by = "sb_player_id"
    ) |>
    left_join(
      sc_df |> select(sc_player_id = player_id, player_name_sc = player_name,
                      team_name_sc = team_name) |>
        mutate(sc_player_id = as.character(sc_player_id)),
      by = "sc_player_id"
    )
  
  crosswalk <- bind_rows(algo_crosswalk, league_manual)
  
  unmatched_sb <- sb_rem |>
    mutate(league = league_name, side = "statsbomb") |>
    select(sb_player_id, player_name_sb, team_name_sb, league, side)
  
  unmatched_sc <- sc_rem |>
    mutate(league = league_name, side = "skillcorner") |>
    select(sc_player_id, player_name_sc, team_name_sc, league, side)
  
  list(crosswalk = crosswalk, unmatched_sb = unmatched_sb, unmatched_sc = unmatched_sc)
}

# =============================================================================
# 6. RUN ACROSS ALL LEAGUES
# =============================================================================

shared_leagues <- list(
  "Liga MX"               = list(sb = scout_data$jugs_ligamx,   sc = fis_data$liga_mx_full),
  "Argentina"             = list(sb = scout_data$jugs_arg,       sc = fis_data$arg_fisico),
  "Brasil – Série A"      = list(sb = scout_data$jugs_brasil,    sc = fis_data$bra_fisico),
  "Colombia"              = list(sb = scout_data$jugs_colombia,  sc = fis_data$col_fisico),
  "Chile"                 = list(sb = scout_data$jugs_chile,     sc = fis_data$chile_fisico),
  "MLS"                   = list(sb = scout_data$jugs_mls,       sc = fis_data$mls_fisico),
  "Premier League"        = list(sb = scout_data$jugs_premier,   sc = fis_data$premier_fisico),
  "LaLiga"                = list(sb = scout_data$jugs_laliga,    sc = fis_data$laliga_fisico),
  "LaLiga 2"              = list(sb = scout_data$jugs_laliga_2,  sc = fis_data$laliga2_fisico),
  "Serie A"               = list(sb = scout_data$jugs_serie_a,   sc = fis_data$serie_a_fisico),
  "Turquía – Süper Lig"   = list(sb = scout_data$jugs_turquia,   sc = fis_data$tur_fisico),
  "UEFA Champions League" = list(sb = scout_data$jugs_champions, sc = fis_data$champions_fisico),
  "UEFA Europa League"    = list(sb = scout_data$jugs_uel,       sc = fis_data$uel_fisico)
)

message("=== Building v4 crosswalks ===\n")

results <- imap(shared_leagues, function(pair, league_name) {
  build_crosswalk_v4(
    sb_df           = pair$sb,
    sc_df           = pair$sc,
    league_name     = league_name,
    fuzzy_threshold = 0.25,
    fuzzy_method    = "jw"
  )
})

full_crosswalk_auto <- map_dfr(results, "crosswalk")
all_unmatched_sb    <- map_dfr(results, "unmatched_sb")
all_unmatched_sc    <- map_dfr(results, "unmatched_sc")

message("\n=== SUMMARY ===")
message("Total matched pairs:  ", nrow(full_crosswalk_auto))
message("Total unmatched SB:   ", nrow(all_unmatched_sb))
message("Total unmatched SC:   ", nrow(all_unmatched_sc))
message("\nBy pass type:")
print(count(full_crosswalk_auto, match_type, sort = TRUE))
message("\nBy league:")
print(count(full_crosswalk_auto, league, sort = TRUE))
message("\nUnmatched SC by league:")
print(count(all_unmatched_sc, league, sort = TRUE))

# Cross-reference: for each unmatched SC player, show same-team unmatched SB players
# This makes it easy to spot new manual matches
message("\n=== UNMATCHED SC + SAME-TEAM SB CANDIDATES (for manual matching) ===")
cross_ref <- all_unmatched_sc |>
  mutate(team_canon_sc = canonicalize_team(team_name_sc)) |>
  left_join(
    all_unmatched_sb |>
      mutate(team_canon_sb = canonicalize_team(team_name_sb)) |>
      select(sb_player_id, player_name_sb, team_name_sb, league, team_canon_sb),
    by = c("league", "team_canon_sc" = "team_canon_sb")
  ) |>
  filter(!is.na(player_name_sb), player_name_sb != "NA") |>
  select(league, sc_player_id, player_name_sc, player_name_sb, sb_player_id, team_name_sc)

if (nrow(cross_ref) > 0) {
  write_csv(cross_ref, "data/unmatched_sc_candidates.csv")
  message("Saved ", nrow(cross_ref), " candidate pairs → data/unmatched_sc_candidates.csv")
} else {
  message("No cross-reference candidates found.")
}

# SC match rate by league
sc_totals <- imap_dfr(shared_leagues, function(pair, league_name) {
  tibble(league = league_name,
         total_sc = pair$sc |> distinct(player_id) |> nrow())
})
sc_unmatched_counts <- count(all_unmatched_sc, league, name = "unmatched_sc")
sc_rate <- sc_totals |>
  left_join(sc_unmatched_counts, by = "league") |>
  mutate(
    unmatched_sc = replace_na(unmatched_sc, 0),
    matched_sc   = total_sc - unmatched_sc,
    pct_matched  = round(100 * matched_sc / total_sc, 1)
  ) |>
  arrange(pct_matched)
message("\n=== SC MATCH RATE BY LEAGUE ===")
print(sc_rate)
message(sprintf(
  "\nOVERALL SC MATCH RATE: %.1f%% (%d matched / %d total)",
  100 * sum(sc_rate$matched_sc) / sum(sc_rate$total_sc),
  sum(sc_rate$matched_sc), sum(sc_rate$total_sc)
))

# =============================================================================
# 7. SAVE
# =============================================================================

write_csv(full_crosswalk_auto, "data/crosswalk_auto.csv")
write_csv(all_unmatched_sb,    "data/unmatched_sb.csv")
write_csv(all_unmatched_sc,    "data/unmatched_sc.csv")

# Merge with any existing manual CSV if present
if (file.exists("data/crosswalk_manual.csv")) {
  manual_csv  <- read_csv("data/crosswalk_manual.csv")
  final_crosswalk <- full_crosswalk_auto |>
    filter(!sb_player_id %in% manual_csv$sb_player_id) |>
    bind_rows(manual_csv)
  message("\nExternal manual CSV applied: ", nrow(manual_csv), " rows")
} else {
  final_crosswalk <- full_crosswalk_auto
}

saveRDS(final_crosswalk, "data/crosswalk_final.rds")
write_csv(final_crosswalk, "data/crosswalk_final.csv")
message("Saved crosswalk_final.rds with ", nrow(final_crosswalk), " total pairs")

# =============================================================================
# 8. REBUILD scout_joined.rds
# =============================================================================

join_sb_sc_fixed <- function(sb_df, sc_df, crosswalk, league_name) {
  cw <- crosswalk |>
    filter(league == league_name) |>
    select(sb_player_id, sc_player_id, match_type) |>
    mutate(sb_player_id = as.character(sb_player_id),
           sc_player_id = as.character(sc_player_id))
  
  sb_cols         <- names(sb_df)
  sc_cols         <- names(sc_df)
  sc_cols_to_keep <- sc_cols[!sc_cols %in% c(sb_cols, "player_id", "player_name", "team_name")]
  sc_slim <- sc_df |>
    mutate(player_id = as.character(player_id)) |>
    select(player_id, all_of(sc_cols_to_keep))
  
  sb_df |>
    mutate(player_id = as.character(player_id)) |>
    left_join(cw, by = c("player_id" = "sb_player_id")) |>
    left_join(sc_slim, by = c("sc_player_id" = "player_id"))
}

message("\n=== Rebuilding scout_joined.rds ===")

joined_leagues <- imap(shared_leagues, function(pair, league_name) {
  message("  Joining: ", league_name)
  join_sb_sc_fixed(pair$sb, pair$sc, final_crosswalk, league_name)
})

sb_only_leagues <- list(
  "Concacaf Champions Cup"   = scout_data$jugs_ccl,
  "Ecuador"                  = scout_data$jugs_ecuador,
  "Paraguay"                 = scout_data$jugs_paraguay,
  "Uruguay"                  = scout_data$jugs_uruguay,
  "Championship (EFL)"       = scout_data$jugs_championship,
  "Serie B"                  = scout_data$jugs_serie_b,
  "Bundesliga"               = scout_data$jugs_bundesliga,
  "2. Bundesliga"            = scout_data$jugs_bundesliga_2,
  "Ligue 1"                  = scout_data$jugs_ligue_1,
  "Eredivisie"               = scout_data$jugs_eredivisie,
  "Bélgica"                  = scout_data$jugs_belgica,
  "Portugal – Primeira Liga" = scout_data$jugs_portugal,
  "Escocia – Premiership"    = scout_data$jugs_escocia,
  "CONMEBOL Libertadores"    = scout_data$jugs_libertadores
)

LEAGUE_CATALOG_JOINED <- c(joined_leagues, sb_only_leagues)

saveRDS(
  list(
    joined_leagues  = joined_leagues,
    sb_only_leagues = sb_only_leagues,
    league_catalog  = LEAGUE_CATALOG_JOINED,
    final_crosswalk = final_crosswalk
  ),
  file     = "data/scout_joined.rds",
  compress = "gzip"
)

message(sprintf("Wrote scout_joined.rds (%.1f MB)",
                file.info("data/scout_joined.rds")$size / 1024^2))
message("Done.")

# final <- readRDS("/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/data/scout_joined.rds")
# 
# final <- final$joined_leagues$`Liga MX`

############# ------------------- ############# ------------------- #############
############# ------------------- ############# ------------------- #############
############# ------------------- ############# ------------------- #############
############# ------------------- ############# ------------------- #############

# suppressPackageStartupMessages({
#   library(dplyr)
#   library(stringr)
#   library(stringi)
#   library(tidyr)
#   library(fuzzyjoin)
#   library(purrr)
#   library(tibble)
#   library(readr)
# })
# 
# # ----------------------------
# # File paths (YOUR PATHS)
# # ----------------------------
# BASE_PATH <- "/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/data"
# 
# path_scout <- file.path(BASE_PATH, "scout_data.rds")
# path_sc    <- file.path(BASE_PATH, "scout_skill_corner.rds")
# 
# out_final_list     <- file.path(BASE_PATH, "final/scout_joined_by_league.rds")
# out_unmatched_list <- file.path(BASE_PATH, "debug/unmatched_by_league.rds")
# out_ambiguous_list <- file.path(BASE_PATH, "debug/ambiguous_by_league.rds")
# out_summary        <- file.path(BASE_PATH, "debug/league_summary_join.rds")
# 
# dir.create(file.path(BASE_PATH, "final"), recursive = TRUE, showWarnings = FALSE)
# dir.create(file.path(BASE_PATH, "debug"), recursive = TRUE, showWarnings = FALSE)
# 
# # ----------------------------
# # Fuzzy parameters
# # ----------------------------
# METHOD    <- "jw"    # Jaro–Winkler
# MAX_DIST  <- 0.08    # smaller = stricter (0.06–0.12 typical)
# AMBIG_GAP <- 0.01    # if 2nd_best - best < AMBIG_GAP => ambiguous
# 
# # ----------------------------
# # Helpers
# # ----------------------------
# as_df <- function(x) {
#   if (is.null(x)) return(NULL)
#   if (inherits(x, "data.frame")) return(x)
#   tibble::as_tibble(x)
# }
# 
# clean_name <- function(x) {
#   x |>
#     tidyr::replace_na("") |>
#     str_to_lower() |>
#     stringi::stri_trans_general("Latin-ASCII") |>
#     str_replace_all("[^a-z\\s]", " ") |>
#     str_squish() |>
#     str_replace_all("\\b(jr|sr|ii|iii|iv)\\b", "") |>
#     str_squish() |>
#     str_replace_all("\\b(de|del|la|las|los|da|do|dos|das|van|von)\\b", " ") |>
#     str_squish()
# }
# 
# # Key that handles "missing middle / extra surname" well for LatAm names:
# # first name + first surname
# name_first_surname <- function(x) {
#   y <- clean_name(x)
#   parts <- str_split(y, "\\s+", simplify = TRUE)
#   out <- apply(parts, 1, function(row) {
#     row <- row[row != ""]
#     if (length(row) == 0) return("")
#     if (length(row) == 1) return(row[1])
#     paste(row[1], row[2])
#   })
#   out
# }
# 
# clean_league <- function(x) {
#   x |>
#     tidyr::replace_na("") |>
#     stringi::stri_trans_general("Latin-ASCII") |>
#     tolower() |>
#     str_replace_all("[–—−]", "-") |>
#     str_replace_all("[^a-z0-9]+", " ") |>
#     str_squish()
# }
# 
# # Extract SC leagues from nested structure
# extract_skillcorner_leagues <- function(obj) {
#   if (is.list(obj) && "sc_prueba" %in% names(obj)) {
#     obj <- obj$sc_prueba
#   }
#   
#   if (!is.list(obj)) stop("SkillCorner RDS: unexpected structure (not a list).")
#   if (!("fis_data" %in% names(obj))) stop("SkillCorner RDS: missing $fis_data.")
#   if (!("league_catalog_fis" %in% names(obj))) stop("SkillCorner RDS: missing $league_catalog_fis.")
#   
#   fis_data <- obj$fis_data
#   leagues  <- obj$league_catalog_fis
#   
#   if (!is.list(fis_data)) stop("SkillCorner RDS: fis_data is not a list.")
#   if (!is.character(leagues)) stop("SkillCorner RDS: league_catalog_fis is not character.")
#   if (length(fis_data) != length(leagues)) stop("SkillCorner RDS: fis_data and league_catalog_fis length mismatch.")
#   
#   fis_data <- lapply(fis_data, as_df)
#   names(fis_data) <- leagues
#   fis_data
# }
# 
# # Prefix ALL SkillCorner columns except the join key column name_key
# prefix_skillcorner_cols <- function(sc_df, name_key = "name_full") {
#   sc_df <- as_df(sc_df)
#   keep <- c(name_key)
#   rename_cols <- setdiff(names(sc_df), keep)
#   
#   sc_df |>
#     rename_with(~ paste0("sc_", .x), .cols = all_of(rename_cols))
# }
# 
# # Pick an indicator column to detect if a row got SkillCorner data
# pick_indicator_col <- function(sc_prefixed_df, name_key) {
#   candidates <- setdiff(names(sc_prefixed_df), name_key)
#   if (length(candidates) == 0) stop("No indicator column found in prefixed SkillCorner df.")
#   candidates[1]
# }
# 
# # Robustly get column name from fuzzyjoin output:
# # It might be "col", or "col.x", or "col.y"
# pick_existing <- function(df, base) {
#   opts <- c(base, paste0(base, ".x"), paste0(base, ".y"))
#   hit <- opts[opts %in% names(df)]
#   if (length(hit) == 0) return(NA_character_)
#   hit[1]
# }
# 
# # ----------------------------
# # League mapping (SCOUT list key -> intended SkillCorner league name)
# # ----------------------------
# league_map <- c(
#   jugs_ligamx    = "Liga MX",
#   jugs_arg       = "Argentina",
#   jugs_brasil    = "Brasil – Série A",
#   jugs_colombia  = "Colombia",
#   jugs_chile     = "Chile",
#   jugs_mls       = "MLS",
#   jugs_premier   = "Premier League",
#   jugs_laliga    = "LaLiga",
#   jugs_laliga_2  = "LaLiga 2",
#   jugs_serie_a   = "Serie A",
#   jugs_turquia   = "Turquía – Süper Lig",
#   jugs_champions = "UEFA Champions League",
#   jugs_uel       = "UEFA Europa League"
# )
# 
# # ----------------------------
# # Core join for ONE league (NAMES ONLY; FS -> FULL -> FUZZY FULL)
# # ----------------------------
# join_one_league <- function(scout_df, sc_df) {
#   
#   scout_df <- as_df(scout_df)
#   sc_df    <- as_df(sc_df)
#   
#   if (is.null(scout_df) || nrow(scout_df) == 0) {
#     return(list(
#       final = scout_df, unmatched = scout_df, ambiguous = NULL,
#       stats = tibble(exact_fs = 0L, exact_full = 0L, fuzzy = 0L, still_unmatched = nrow(scout_df)),
#       status = "empty_scout"
#     ))
#   }
#   
#   if (is.null(sc_df) || nrow(sc_df) == 0) {
#     return(list(
#       final = scout_df, unmatched = scout_df, ambiguous = NULL,
#       stats = tibble(exact_fs = 0L, exact_full = 0L, fuzzy = 0L, still_unmatched = nrow(scout_df)),
#       status = "empty_skillcorner"
#     ))
#   }
#   
#   if (!("player_known_name" %in% names(scout_df))) stop("Scout df must contain player_known_name")
#   if (!("player_name" %in% names(sc_df))) stop("SkillCorner df must contain player_name")
#   
#   # Build BOTH keys in both sources
#   scout_df <- scout_df |>
#     mutate(
#       name_fs   = name_first_surname(player_known_name),
#       name_full = clean_name(player_known_name)
#     )
#   
#   sc_df <- sc_df |>
#     mutate(
#       name_fs   = name_first_surname(player_name),
#       name_full = clean_name(player_name)
#     )
#   
#   # ----------------------------------------------------------
#   # A) Exact join on name_fs (first + first surname)
#   # ----------------------------------------------------------
#   sc_fs_pref <- sc_df |>
#     select(-player_name) |>
#     prefix_skillcorner_cols(name_key = "name_fs")
#   
#   ind_fs <- pick_indicator_col(sc_fs_pref, "name_fs")
#   
#   joined_fs <- scout_df |>
#     left_join(sc_fs_pref, by = "name_fs")
#   
#   matched_fs <- joined_fs |> filter(!is.na(.data[[ind_fs]]))
#   unmatched_after_fs <- joined_fs |> filter(is.na(.data[[ind_fs]])) |> select(all_of(names(scout_df)))
#   
#   # ----------------------------------------------------------
#   # B) Exact join on name_full for remaining
#   # ----------------------------------------------------------
#   sc_full_pref <- sc_df |>
#     select(-player_name) |>
#     prefix_skillcorner_cols(name_key = "name_full")
#   
#   ind_full <- pick_indicator_col(sc_full_pref, "name_full")
#   
#   joined_full <- unmatched_after_fs |>
#     left_join(sc_full_pref, by = "name_full")
#   
#   matched_full <- joined_full |> filter(!is.na(.data[[ind_full]]))
#   unmatched_after_full <- joined_full |> filter(is.na(.data[[ind_full]])) |> select(all_of(names(scout_df)))
#   
#   # If everything matched after exacts, we’re done
#   if (nrow(unmatched_after_full) == 0) {
#     final_exact <- bind_rows(matched_fs, matched_full)
#     return(list(
#       final = final_exact,
#       unmatched = unmatched_after_full,
#       ambiguous = NULL,
#       stats = tibble(
#         exact_fs = nrow(matched_fs),
#         exact_full = nrow(matched_full),
#         fuzzy = 0L,
#         still_unmatched = 0L
#       ),
#       status = "joined_exact_fs_plus_full"
#     ))
#   }
#   
#   # ----------------------------------------------------------
#   # C) Fuzzy (Jaro–Winkler) on name_full for remaining
#   # ----------------------------------------------------------
#   fuzzy_candidates <- fuzzyjoin::stringdist_left_join(
#     unmatched_after_full,
#     sc_full_pref,
#     by = c("name_full" = "name_full"),
#     method = METHOD,
#     max_dist = MAX_DIST,
#     distance_col = "name_dist"
#   )
#   
#   if (nrow(fuzzy_candidates) == 0) {
#     final_exact <- bind_rows(matched_fs, matched_full, unmatched_after_full)
#     return(list(
#       final = final_exact,
#       unmatched = unmatched_after_full,
#       ambiguous = NULL,
#       stats = tibble(
#         exact_fs = nrow(matched_fs),
#         exact_full = nrow(matched_full),
#         fuzzy = 0L,
#         still_unmatched = nrow(unmatched_after_full)
#       ),
#       status = "joined_exact_only_no_fuzzy_hits"
#     ))
#   }
#   
#   key_col <- pick_existing(fuzzy_candidates, "name_full")
#   if (is.na(key_col)) stop("Could not find name_full in fuzzy_candidates output.")
#   
#   ambiguous_summary <- fuzzy_candidates |>
#     group_by(.data[[key_col]]) |>
#     summarise(
#       best_dist = suppressWarnings(min(name_dist, na.rm = TRUE)),
#       second_best = suppressWarnings(sort(name_dist)[2]),
#       n_candidates = n(),
#       ambiguous = is.finite(second_best) & (second_best - best_dist) < AMBIG_GAP,
#       .groups = "drop"
#     )
#   
#   ambiguous_keys <- ambiguous_summary |>
#     filter(ambiguous) |>
#     pull(.data[[key_col]])
#   
#   ambiguous_dump <- fuzzy_candidates |>
#     filter(.data[[key_col]] %in% ambiguous_keys) |>
#     arrange(.data[[key_col]], name_dist)
#   
#   fuzzy_best <- fuzzy_candidates |>
#     group_by(.data[[key_col]]) |>
#     arrange(name_dist) |>
#     slice(1) |>
#     ungroup()
#   
#   # Build resolved rows robustly (do NOT assume .x/.y exist)
#   scout_cols <- names(unmatched_after_full)
#   sc_cols    <- setdiff(names(sc_full_pref), "name_full")
#   
#   fuzzy_resolved <- fuzzy_best
#   
#   # Copy scout columns from whichever form exists
#   for (nm in scout_cols) {
#     src <- pick_existing(fuzzy_resolved, nm)
#     if (is.na(src)) fuzzy_resolved[[nm]] <- NA else fuzzy_resolved[[nm]] <- fuzzy_resolved[[src]]
#   }
#   
#   # Copy skillcorner prefixed columns (sc_*) from whichever form exists
#   for (nm in sc_cols) {
#     src <- pick_existing(fuzzy_resolved, nm)
#     if (is.na(src)) fuzzy_resolved[[nm]] <- NA else fuzzy_resolved[[nm]] <- fuzzy_resolved[[src]]
#   }
#   
#   fuzzy_resolved <- fuzzy_resolved |>
#     select(all_of(c(scout_cols, sc_cols)))
#   
#   still_unmatched <- unmatched_after_full |>
#     anti_join(fuzzy_resolved |> select(name_full), by = "name_full")
#   
#   final_joined <- bind_rows(matched_fs, matched_full, fuzzy_resolved, still_unmatched)
#   
#   list(
#     final = final_joined,
#     unmatched = still_unmatched,
#     ambiguous = ambiguous_dump,
#     stats = tibble(
#       exact_fs = nrow(matched_fs),
#       exact_full = nrow(matched_full),
#       fuzzy = nrow(fuzzy_resolved),
#       still_unmatched = nrow(still_unmatched)
#     ),
#     status = "joined_exact_fs_full_plus_fuzzy"
#   )
# }
# 
# # ----------------------------
# # Read data
# # ----------------------------
# scout_list_raw <- readRDS(path_scout)
# sc_raw         <- readRDS(path_sc)
# 
# if (!is.list(scout_list_raw)) stop("scout_data.rds must be a LIST of data frames")
# 
# sc_list <- extract_skillcorner_leagues(sc_raw)
# 
# # Lookup: normalized league name -> raw league name in sc_list
# sc_keys_raw  <- names(sc_list)
# sc_keys_norm <- clean_league(sc_keys_raw)
# sc_lookup <- setNames(sc_keys_raw, sc_keys_norm)
# 
# # ----------------------------
# # Join over ALL scout leagues
# # ----------------------------
# joined_by_league    <- list()
# unmatched_by_league <- list()
# ambiguous_by_league <- list()
# 
# summary_df <- imap_dfr(scout_list_raw, function(scout_df, scout_key) {
#   
#   scout_df <- as_df(scout_df)
#   
#   intended_sc_raw  <- unname(league_map[scout_key])
#   intended_sc_norm <- if (!is.na(intended_sc_raw)) clean_league(intended_sc_raw) else NA_character_
#   
#   sc_key <- if (!is.na(intended_sc_norm) && intended_sc_norm %in% names(sc_lookup)) {
#     unname(sc_lookup[intended_sc_norm])
#   } else {
#     NA_character_
#   }
#   
#   has_mapping <- scout_key %in% names(league_map)
#   has_sc      <- !is.na(sc_key) && sc_key %in% names(sc_list)
#   
#   if (has_mapping && has_sc) {
#     cat("Joining:", scout_key, "<->", sc_key, "\n")
#     
#     res <- join_one_league(
#       scout_df = scout_df,
#       sc_df    = sc_list[[sc_key]]
#     )
#     
#     joined_by_league[[scout_key]]    <<- res$final
#     unmatched_by_league[[scout_key]] <<- res$unmatched
#     ambiguous_by_league[[scout_key]] <<- res$ambiguous
#     
#     res$stats |>
#       mutate(
#         scout_list_name = scout_key,
#         skillcorner_list_name = sc_key,
#         status = res$status,
#         scout_n = nrow(scout_df),
#         skillcorner_n = nrow(as_df(sc_list[[sc_key]]))
#       )
#     
#   } else {
#     cat("Keeping (no skillcorner):", scout_key, "\n")
#     
#     joined_by_league[[scout_key]] <<- scout_df
#     unmatched_by_league[[scout_key]] <<- NULL
#     ambiguous_by_league[[scout_key]] <<- NULL
#     
#     tibble(
#       exact_fs = NA_integer_,
#       exact_full = NA_integer_,
#       fuzzy = NA_integer_,
#       still_unmatched = NA_integer_,
#       scout_list_name = scout_key,
#       skillcorner_list_name = ifelse(is.na(intended_sc_raw), NA_character_, intended_sc_raw),
#       status = "no_skillcorner",
#       scout_n = nrow(scout_df),
#       skillcorner_n = NA_integer_
#     )
#   }
# })
# 
# # ----------------------------
# # Save outputs
# # ----------------------------
# saveRDS(joined_by_league, out_final_list)
# saveRDS(unmatched_by_league, out_unmatched_list)
# saveRDS(ambiguous_by_league, out_ambiguous_list)
# saveRDS(summary_df, out_summary)
# 
# cat("\nSaved:\n")
# cat(" - Joined list (ALL scout leagues):", out_final_list, "\n")
# cat(" - Unmatched list:", out_unmatched_list, "\n")
# cat(" - Ambiguous list:", out_ambiguous_list, "\n")
# cat(" - Summary:", out_summary, "\n\n")
# 
# print(summary_df)
#
# final_list <- read_rds("/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/data/final/scout_joined_by_league.rds")
# 
# final_list <- final_list[[11]] |>
#   distinct(player_id, .keep_all = TRUE)
# 
# glimpse(final_list)
# 
# final_list$sc_short_name[394]
# 
# names <- final_list |>
#   select(player_first_name, player_last_name, player_known_name, sc_short_name, sc_player_name)
# 
# arg_list_fin <- fis_data[[2]]
# 
# unique(arg_list_fin$position_group)
