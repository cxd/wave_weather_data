
getConfig <- function() {
  
  list(
    climateDir="data/climate_stations",
    waveData="data/wave",
    waveUrl="https://data.qld.gov.au/api/action/datastore_search",
    climateColumns=c(
      "Station",
      "Date",
      "Evapotranspiration_mm",
      "Rain_mm",
      "BLANK",
      "PanEvaporation_mm",
      "MaximumTemperature_C",
      "MaxRelativeHumidity_pc",
      "MinRelativeHumidity_pc",
      "Avg10mWindSpeed_m_sec",
      "SolarRadiation_MJ_sqm"
    ),
    waveColumns=c(
      "DateTime",
      "Hs",
      "Hmax",
      "Tz",
      "Tp",
      "DirTpTRUE",
      "SST"
    ),
    sites=data.frame(
      year=c(
        "2019",
        "2018",
        "2017",
        "2016",
        "2015",
        
        "2018",
        "2017",
        "2016",
        "2015",
        "2014",
        "2013",
        "2012",
        "2011",
        
        "2019",
        "2018",
        "2017",
        "2016",
        "2015",
        "2014",
        
        "2019",
        "2018",
        "2017",
        "2016",
        "2015",
        
        "2019",
        "2018",
        "2017",
        "2016",
        "2015",
        "2014",
        "2013"
        
      ),
      site=c("caloundra",
             "caloundra",
             "caloundra",
             "caloundra",
             "caloundra",
             
             "brisbane",
             "brisbane",
             "brisbane",
             "brisbane",
             "brisbane",
             "brisbane",
             "brisbane",
             "brisbane",
             
             "mooloolaba",
             "mooloolaba",
             "mooloolaba",
             "mooloolaba",
             "mooloolaba",
             "mooloolaba",
             
             "northmoretonbay",
             "northmoretonbay",
             "northmoretonbay",
             "northmoretonbay",
             "northmoretonbay",
             
             "goldcoast",
             "goldcoast",
             "goldcoast",
             "goldcoast",
             "goldcoast",
             "goldcoast",
             "goldcoast"
             
             ),
      resourceId=c("6603ad4e-98cd-4215-877c-61b9759cbbd0",
                   "e302c519-77ec-46e5-b819-767ac177f37a",
                   "80481a30-ac2d-4e67-82e9-d5fcb282f58b",
                   "52689eca-688c-4043-8a8d-ff75e8022952",
                   "fe51b69b-df4e-4eb2-981f-3e18707bd09f",
                   
                   "de0ddbda-84e5-4583-a85b-bea48bd875d5",
                   "0a54151e-5ef7-4257-85c7-84ba4662f03f",
                   "19b441dd-2539-497b-b11c-78a85def64c9",
                   "53cfe709-5b7f-4339-b8c7-919cdcdb79ae",
                   "b328dc90-4f16-4c63-a577-ad954b5e898c",
                   "5e648c66-e67f-4b76-9ba3-5a281bec7ccf",
                   "cf594247-4da2-4802-8a86-ea7b514df3e7",
                   "dc366f6a-957c-4d47-b582-05768f3c02b9",
                   
                   "d80243cd-5bca-41a0-b0f8-4d4fac168d94",
                   "aafb52d6-7bb8-4601-966b-5153bd35d4f8",
                   "aafb52d6-7bb8-4601-966b-5153bd35d4f8",
                   "8e7fcf48-ac17-45ea-b4f3-b30cd1739658",
                   "81df149b-67fc-4e5c-8ab8-b479001e04eb",
                   "ead75011-c55d-456b-b2f4-0845eb3aa128",
                   
                   "9a5b699a-58df-4ad2-b65f-81229427c25d",
                   "b1386a01-e4d5-462f-bf84-a77e519f0cc6",
                   "32838f8b-496a-4056-9c3f-b6a52932f246",
                   "4c24fddd-af86-41ea-8a03-9c33941f8f10",
                   "6ab26f13-ee41-4ef1-ba22-846aaeaaee6f",
                   
                   "ee5859f9-e55c-434f-b20c-7da30b1e53e1",
                   "d1049f97-45a9-4b3d-80be-7fce8cd3ec29",
                   "f85931b4-926a-49e3-9e56-d65bd49a9f14",
                   "c5e598d5-5a9a-45be-9da6-a9f47042e006",
                   "30cdfd68-52e9-4c5c-933c-03c2fed5a11a",
                   "9c1270fd-6626-4ed4-b629-73f687dbb04f",
                   "05f38c26-1420-48d1-a560-e9b15a0d51d7"
                   ),
      url=c("https://data.qld.gov.au/dataset/coastal-data-system-waves-caloundra/resource/6603ad4e-98cd-4215-877c-61b9759cbbd0",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-caloundra/resource/e302c519-77ec-46e5-b819-767ac177f37a",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-caloundra/resource/80481a30-ac2d-4e67-82e9-d5fcb282f58b",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-caloundra/resource/be62134c-15a2-4736-8b2a-bbfd323f10ce",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-caloundra/resource/fe51b69b-df4e-4eb2-981f-3e18707bd09f",
            
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/de0ddbda-84e5-4583-a85b-bea48bd875d5",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/3a833ec3-2685-4999-af4f-f10d304042f6",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/19b441dd-2539-497b-b11c-78a85def64c9",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/53cfe709-5b7f-4339-b8c7-919cdcdb79ae",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/b328dc90-4f16-4c63-a577-ad954b5e898c",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/5e648c66-e67f-4b76-9ba3-5a281bec7ccf",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/cf594247-4da2-4802-8a86-ea7b514df3e7",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-brisbane/resource/dc366f6a-957c-4d47-b582-05768f3c02b9",
            
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-mooloolaba/resource/d80243cd-5bca-41a0-b0f8-4d4fac168d94",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-mooloolaba/resource/b92629f7-a79f-45a2-857d-2217b2f11e63",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-mooloolaba/resource/61faa02e-a4e5-400e-98ff-3266b255da1b",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-mooloolaba/resource/8e7fcf48-ac17-45ea-b4f3-b30cd1739658",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-mooloolaba/resource/81df149b-67fc-4e5c-8ab8-b479001e04eb",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-mooloolaba/resource/ead75011-c55d-456b-b2f4-0845eb3aa128",
            
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-north-moreton/resource/b9fda7ae-c24e-4bb9-b2f4-83487327300d",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-north-moreton/resource/b1386a01-e4d5-462f-bf84-a77e519f0cc6",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-north-moreton/resource/32838f8b-496a-4056-9c3f-b6a52932f246",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-north-moreton/resource/4c24fddd-af86-41ea-8a03-9c33941f8f10",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-north-moreton/resource/6ab26f13-ee41-4ef1-ba22-846aaeaaee6f",
            
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-gold-coast/resource/ee5859f9-e55c-434f-b20c-7da30b1e53e1",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-gold-coast/resource/d1049f97-45a9-4b3d-80be-7fce8cd3ec29",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-gold-coast/resource/f85931b4-926a-49e3-9e56-d65bd49a9f14",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-gold-coast/resource/c5e598d5-5a9a-45be-9da6-a9f47042e006",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-gold-coast/resource/30cdfd68-52e9-4c5c-933c-03c2fed5a11a",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-gold-coast/resource/9c1270fd-6626-4ed4-b629-73f687dbb04f",
            "https://data.qld.gov.au/dataset/coastal-data-system-waves-gold-coast/resource/05f38c26-1420-48d1-a560-e9b15a0d51d7"
            )),
      siteMapping=data.frame(
        waveSite=c(
          "caloundra",
          "brisbane",
          "mooloolaba",
          "northmoretonbay",
          "goldcoast"
        ),
        climateSite=c(
          "redcliffe",
          "brisbane",
          "sunshine_coast_airport",
          "cape_moreton_lighthouse",
          "gold_coast_seaway"
        )
      ))
}