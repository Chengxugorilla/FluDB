GISAID_sub_H1N1 <-
data.frame(String = c("A/North_", "A/St._", "A/St_", "A/South_", "A/New_", "A/Dominican_",
                      "A/Hong_Kong", "A/Solomon_Islands", "A/swine/Iowa/15/1930",
                      "A/Lyon/CHU/","A/Lyon/CHU_","A/Algeria/G0282/16",
                      "A/Algeria/G0283/16","A/Algeria/G0281/16","A/Henan/Jinshui","/2008#",
                      "A/Nosy_","A/CastillaLaMancha/","A/SaudiArabia/","A/BosniaAndHerzegovina/",
                      "A/Champagne_Ardenne/","A/Franche_Comte/","A/LaReunion/",
                      "A/Al_","A/Ile_de_France/","A/Le_","A/Clermont_Ferrand/","A/Haute_Normandie/",
                      "A/Central_African_Republic/","A/Haute_","A/Basse_","A/Nord_Pas_de_Calais/",
                      "A/Friuli_Venezia_Giulia/","A/Bosnia_and_Herzegovia/","A/BosniaAndHerzegovina/",
                      "A/Kyrgyzstan_","A/Kyrgyzstan,_"),
           Substitution = c("A/North-", "A/St.", "A/St.", "A/South-", "A/New-", "A/Dominican-",
                            "A/HongKong", "A/Solomon-Islands", "A/Iowa/15/1930",
                            "A/Lyon/CHU-","A/Lyon/CHU-","A/Algeria/G0282-16",
                            "A/Algeria/G0283-16","A/Algeria/G0281-16","A/Henan-Jinshui","/2008",
                            "A/Nosy-","A/Castilla-La-Mancha/","A/Saudi-Arabia/","A/Bosnia-and-Herzegovina/",
                            "A/Champagne-Ardenne/","A/Franche-Comte/","A/La-Reunion/",
                            "A/Al-","A/Ile-de-France/","A/Le-","A/Clermont-Ferrand/","A/Haute-Normandie/",
                            "A/Central-African-Republic/","A/Haute-","A/Basse-","A/Nord-Pas-de-Calais/",
                            "A/Friuli-Venezia-Giulia/","A/Bosnia-and-Herzegovia/","A/Bosnia-and-Herzegovia/",
                            "A/Kyrgyzstan-","A/Kyrgyzstan-"),
           stringsAsFactors = FALSE)

WHO_sub_H1N1 <-
data.frame(String = c("A/Dominican/Republic", "A/Heilongjiang/Xiangfang", "A/New.Jersey",
                      "A/Guangdong/Maonan", "A/North/Carolina", "A/New.Caledonia", "A/South/Africa",
                      "A/Israel/Q/504", "A/Solomon/Islands", "A/New-Jersey/2/8/76",
                      "A/Solomon/Islands", "A/California/4/2009","A/New-Jersey^2/8/76",
                      "A/Ohio/2/2007", "A/SI/", "A/Beijing/262/96", "A/Beijing/262/1996",
                      "A/Lyon/CHU/","A/Lyon/CHU_","A/Sachsen//","A/Algeria/G0282/16",
                      "A/Algeria/G0283/16","A/Algeria/G0281/16","A/swine/Northern-Ireland","A/Lisboa/29/20015",
                      "A/Lisboa/30/20015","A/Lisboa/31/20015","20015","/20015","/20102","/20103",
                      "/20093","A/swine/","/2008#"),
           Substitution = c("A/Dominican-Republic", "A/Heilongjiang-Xiangfang", "A/New-Jersey",
                            "A/Guangdong-Maonan", "A/North-Carolina", "A/New-Caledonia", "A/South-Africa",
                            "A/Israel/Q-504", "A/Solomon-Islands", "A/New-Jersey/8/1976",
                            "A/Solomon-Islands", "A/California/04/2009", "A/New-Jersey/8/1976",
                            "A/Ohio/02/2007", "A/Solomon-Islands/", "A/Beijing/262/1995", "A/Beijing/262/1995",
                            "A/Lyon/CHU-","A/Lyon/CHU-","A/Sachsen/","A/Algeria/G0282-16",
                            "A/Algeria/G0283-16","A/Algeria/G0281-16","A/Northern-Ireland","A/Lisboa/29/2015",
                            "A/Lisboa/30/2015","A/Lisboa/31/2015","2015","/2015","/2010","/2010",
                            "/2009","A/","/2008"),
           stringsAsFactors = FALSE)

GISAID_sub_H3N2 <-
  data.frame(String = c("A/North_", "A/St._", "A/St_", "A/South_", "A/New_", "A/Dominican_",
                        "A/Hong_Kong", "A/Solomon_Islands", "A/swine/Iowa/15/1930",
                        "A/Lyon/CHU/","A/Lyon/CHU_","A/Algeria/G0282/16",
                        "A/Algeria/G0283/16","A/Algeria/G0281/16","A/Henan/Jinshui",
                        "A/Hong_Kong_","A/Stock/6/2014","A/Switz/","A/Athens_GR/",
                        "A/Athens/112","A/Rhode_","A/LYON/","A/SENDAI/","A/SHANTOU/",
                        "A/ANHUI/","A/United_Kingdom/","A/Baden_"),
             Substitution = c("A/North-", "A/St.", "A/St.", "A/South-", "A/New-", "A/Dominican-",
                              "A/HongKong", "A/Solomon-Islands", "A/Iowa/15/1930",
                              "A/Lyon/CHU-","A/Lyon/CHU-","A/Algeria/G0282-16",
                              "A/Algeria/G0283-16","A/Algeria/G0281-16","A/Henan-Jinshui",
                              "A/HongKong","A/Stockholm/6/2014","A/Switzerland/","A/Athens/GR",
                              "A/Athens/GR112","A/Rhode-","A/Lyon/","A/Sendai/","A/Shantou/",
                              "A/Anhui/","A/United-Kingdom/","A/Baden-"),
             stringsAsFactors = FALSE)
GISAID_sub_H3N2_reg <-
  data.frame(String = c("A/Iran/(\\d+)/Ab/(\\d+A)/(\\d{4})"),
             Substitution = c("A/Iran/\\1-Ab-\\2/\\3"),
             stringsAsFactors = FALSE)


WHO_sub_H3N2 <-
  data.frame(String = c("A/Nantes/1441","A/Henan/Jinshui","A/La-Rioja/2202","A/South_Australia",
                        "A/Stock/","A/Switz/","A/Sing/","A/Bret/",
                        "A/Slov/","A/NewYork/","A/Singapore/0019/",
                        "A/UK/","A/NorthCarolina/","A/NewHampshire/","A/NorthDakota/",
                        "A/Baden-Wurttemburg/","A/Switzerland/34074//2022","/2009*","/2005`",
                        "A/CastillaLaMancha/","A/NorthLebanon/","HongKong","Hong Kong"),
             Substitution = c("A/Nantes/1441/2017","A/Henan-Jinshui","A/LaRioja/2202","A/South-Australia",
                              "A/Stockholm/","A/Switzerland/","A/Singapore/","A/Bretagne/",
                              "A/Slovenia/","A/New-York/","A/Singapore/INFIMH-16-0019/",
                              "A/United-Kingdom/","A/North-Carolina/","A/New-Hampshire/","A/North-Dakota/",
                              "A/Baden-Wurttemberg/","A/Switzerland/34074/2022","/2009","/2005",
                              "A/Castilla-La-Mancha/","A/North-Lebanon/","Hong Kong","HongKong"),
             stringsAsFactors = FALSE)
WHO_sub_H3N2_reg <-
  data.frame(String = c("A/Iran/(\\d+)/Ab/(\\d+A)/(\\d{4})"),
             Substitution = c("A/Iran/\\1-Ab-\\2/\\3"),
             stringsAsFactors = FALSE)
