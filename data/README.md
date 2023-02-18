
# RFMO_data:

## Western and Central Pacific Fisheries Comission (WCPFC)

- Data are available [here](https://www.wcpfc.int/public-domain)
- Purse seine are quarterly flag-level data at 1 X 1 from 1967 - 2021
- Longline are quarterly flag-level data at 5 X 5 from 1950 - 2020
- Pole and line are quarterly flag-level data at 1 X 1 from 1950 - 2020
- Known issues include:
  - When flag id is not identified (i.e. flag_id = NA), effort is 0. This happens 92018 times for purse seine, 46,636 times for longline, and 150,558 times for pole and line. In the case of pole and line this also happens for the following iso2 codes: "JP" "PG" "SB" "FJ" "AU" "NC" "TV" "KI" "US" "NZ" "PF" "VU"



## Inter American Tropical Tuna Comission (IATTC)

- data from John
- Purse seine are monthly flag-level data at 1 X 1 from 1958 - 2021
- Longline are monthly flag-level data at 5 X 5 from 1954 - 2021
- Pole and line are monthly flag-level data at 1 X 1 1978 - 2015
- Known issues include:
    - 9 longline records have a no effort reported and make CPUE indefinite


## Indian Ocean Tuna Comission (IOTC)

- Data are available [here](https://iotc.org/data/datasets)
- Purse seine are monthly flag-level data at 1 X 1 from 1952 - 2021
- Longline are monthly flag-level data at 5 X 5 from 1952 - 2021

## International Commission for the Conservation of Atlantic Tunas (ICCAT)

- Data are available [here](https://www.iccat.int/en/accesingdb.html)

# Tuna species that we include

Multiple species are reported in each data-set. We keep species-level for each of the following tuna species (ans skipjack):

- **T. albacares**: Yellowfin
- **T. obesus**: Bigeye
- **T. tonggol**: Northern Bleufien Tuna / longtail tuna
- **T. altanticus**: Blackfin
- **T. maccoyii**: Southern Bluefin Tuna
- **T. thynnus**: Atlantic Blue Fin
- **T. orientalis**: Pacific Bluefin
- **T. alalunga**: Albacore
- **K. pelamis**: Skipjack


# Protected planet data (MPA shapefiles)

- Data are available [here](https://www.protectedplanet.net/en/thematic-areas/marine-protected-areas)

