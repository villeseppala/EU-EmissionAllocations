# g
# d
# https://pxnet2.stat.fi:443/PXWeb/api/v1/fi/StatFin/ymp/khki/statfin_khki_pxt_111k.px
# https://pxnet2.stat.fi:443/PXWeb/api/v1/fi/StatFin/ymp/khki/statfin_khki_pxt_111k.px



library(pxweb)
library(data.table)
library(ggplot2)
library(tibble)
library(tidyr)
library(ggtext)
# install.packages("ggtext")

# url <- "https://pxnet2.stat.fi:443/PXWeb/api/v1/fi/StatFin/ymp/khki/statfin_khki_pxt_111k.px"
url <- "https://pxweb2.stat.fi:443/PxWeb/api/v1/fi/StatFin/khki/statfin_khki_pxt_138v.px"


oyear = 2023
# dims = list(Päästöluokka = c('*'),
#             Kasvihuonekaasu = c('SS'),
#             Vuosi = c('*'),
#             Tiedot = c('emission'))

dims = list(Päästöluokka = c('*'),
            Kasvihuonekaasu = c('SS'),
            Vuosi = c('*'),
            Tiedot = c('emission_ar5'))


df <- pxweb_get_data(url = url, query = dims)

#df <- pxweb_get_data(url = url, query = query)


# d <-
#   pxweb_get_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/ymp/khki/statfin_khki_pxt_111k.px",
#                  dims = list(Päästöluokka = c('*'),
#                              Kasvihuonekaasu = c('00'),
#                              Vuosi = c('*'),
#                              Tiedot = c('emission')),
#                  clean = FALSE)




#?pxweb
d= as.data.table(df)

# 4 Maankäyttö, maankäytön muutokset ja metsätalous (LULUCF)
# Päästöt yhteensä ilman LULUCF-sektoria

d2 = d[,c(1,2,4)]
colnames(d2)=c("year", "sector", "maara")

d2[year == "2020*", year := 2020]
d2[year == "2021*", year := 2021]
d2[year == "2022*", year := 2022]
d2[year == "2023*", year := 2023]
d2[year == "2024*", year := 2024]


dh = d2[year ==oyear,]
#Päästöt yhteensä pl. LULUCF-sektori
#4 Maankäyttö, maankäytön muutokset ja metsätalous (LULUCF)	
#

# d2 = d2[sector %in% c("Emissions without LULUCF", "4 Land use, land-use change and forestry (LULUCF)"),]
# 
# d2[sector == "Emissions without LULUCF", sector := "ghg"]
# d2[sector == "4 Land use, land-use change and forestry (LULUCF)", sector := "lulucf"]


d3 = d2[sector %in% c("Päästöt yhteensä ilman LULUCF-sektoria", "4 Maankäyttö, maankäytön muutokset ja metsätalous (LULUCF)"),]

d3[sector == "Päästöt yhteensä ilman LULUCF-sektoria", sector := "ghg"]
d3[sector == "4 Maankäyttö, maankäytön muutokset ja metsätalous (LULUCF)", sector := "lulucf"]
#d3= copy(d2)
d3b = copy(d3)
d3b[sector == "ghg", sector := "net"]
d3b[,maara:=sum(maara, na.rm=TRUE), by=.(year)]
d3b = d3b[sector == "net", ]
d4= rbind(d3, d3b)
d4$year = as.numeric(d4$year)
d4$maara = d4$maara/1000
suomie = copy(d4)







e2 = copy(d2)

e2 = e2[sector %in% c("1A1 Energiateollisuus", "1A2 Teollisuus ja rakentaminen (polttoperäiset päästöt)", 
                      "1A3 Kotimaan liikenne", "1A4 Muut sektorit", "1A5 Muu erittelemätön polttoainekäyttö",
                      "1B Polttoaineiden haihtumapäästöt", "2 Teollisuusprosessit ja tuotteiden käyttö", "3 Maatalous",
                      "4 Maankäyttö, maankäytön muutokset ja metsätalous (LULUCF)", "5 Jätteiden käsittely"),]

e2[sector == "1A1 Energiateollisuus", sector:="Energiateollisuus"]
e2[sector == "1A2 Teollisuus ja rakentaminen (polttoperäiset päästöt)", sector:="Teoll. ja rakent. polttoperäiset"]
e2[sector == "1A3 Kotimaan liikenne", sector:="Kotimaan liikenne"]
e2[sector == "1A4 Muut sektorit", sector:="Muut energiasektorit"]
e2[sector == "2 Teollisuusprosessit ja tuotteiden käyttö", sector:="Teoll.proses. ja tuotteiden käyttö"]
e2[sector == "3 Maatalous", sector:="Maatalous"]
e2[sector == "4 Maankäyttö, maankäytön muutokset ja metsätalous (LULUCF)", sector:="Maankäyttösektori (LULUCF)"]
e2[sector == "5 Jätteiden käsittely", sector:="Jätteiden käyttö"]
# e2[sector == "1A5 Muu erittelemätön polttoainekäyttö", sector :="Muu polttoainekäyttö"]
e2[sector == "1B Polttoaineiden haihtumapäästöt", sector :="Polttoaineiden haihtumapäästöt"]

e2$year = as.numeric(e2$year)

e3 = copy(e2)

setkey(e3, maara,year)

e4 = e3[sector == "Muut energiasektorit",
        maara := maara + e3[sector == "1A5 Muu erittelemätön polttoainekäyttö", maara]
        # + e3[sector == "1B Polttoaineiden haihtumapäästöt", maara]
]

e4$maara = e4$maara/1000
e4 = e4[!sector %in% c("1A5 Muu erittelemätön polttoainekäyttö")]

e4=e4[year > 2012,]
e4 = e4[!sector == "Maankäyttösektori (LULUCF)",]
e4[sector=="Muut energiasektorit", pos :="H"]

e4[sector=="Teoll. ja rakent. polttoperäiset", pos :="G"]

e4[sector=="Kotimaan liikenne", pos :="F"]
e4[sector=="Energiateollisuus", pos :="E"]

e4[sector=="Teoll.proses. ja tuotteiden käyttö", pos :="C"]

e4[sector=="Polttoaineiden haihtumapäästöt", pos :="B"]

e4[sector=="Jätteiden käyttö", pos :="A"]

sek1 = copy(e4)

sek1$coll = "black"

caus = "white"

caus = "orange"
caus = "yellow"

sek1[pos %in% c("H", "G", "F", "E"), coll := caus]









l2 = copy(d2)

l2 = l2[sector %in% c("4A Metsämaa", "4B  Viljelysmaa", 
                      "4C Ruohikkoalueet", "4D Kosteikot", "4E Rakennetut alueet",
                      "4G Puutuotteet"),]

l2[sector =="4A Metsämaa", sector2 := "Metsämaa"]
l2[sector =="4B  Viljelysmaa", sector2 := "Viljelysmaa"]
l2[sector =="4C Ruohikkoalueet", sector2 := "Ruohikkoalueet"]
l2[sector =="4D Kosteikot", sector2 := "Kosteikot"]
l2[sector =="4E Rakennetut alueet", sector2 := "Rakennetut alueet"]
l2[sector =="4G Puutuotteet", sector2 := "Puutuotteet"]

l2[sector =="4A Metsämaa", sector := "metsa"]
l2[sector =="4B  Viljelysmaa", sector := "Viljelys"]
l2[sector =="4C Ruohikkoalueet", sector := "ruohikko"]
l2[sector =="4D Kosteikot", sector := "kosteikko"]
l2[sector =="4E Rakennetut alueet", sector := "rakennettu"]
l2[sector =="4G Puutuotteet", sector := "puutuote"]

l2$year = as.numeric(l2$year)
l2$maara = l2$maara/1000
setkey(l2, maara,year)

l3 = l2[sector == "ruohikko",
        maara := maara + l2[sector == "kosteikko", maara]+ l2[sector == "rakennettu", maara]
        # + e3[sector == "1B Polttoaineiden haihtumapäästöt", maara]
]

l3 = l3[sector %in% c("ruohikko","metsa", "viljelys","puutuote")]
l3[sector =="ruohikko", sector := "muu"]
l3[sector =="muu", sector := "Muut"]

# l3[sector=="metsa", maara :=c()]



l3b = copy(l3)
l3b = l3b[sector %in% c("metsa", "puutuote")]

# mat = c(-28.94, -42.94,-36.68,-35.27,-27.34,-26.34,-34.44,-28.02,-25.71,-26.64   ,-25.93,-29.05,-29.61,-29.65,-31.09,-34.73,-36.12,-25.06,-31.94,-47.54  ,-32.42,-31.55,-33.68,-26.02,-26.20,-21.68,-17.59,-14.24,-5.29,-12.23,  -16.26,-4.84,-4.62,0)
# mata = c(-2.95, -1.9,-1.5,-3.85,-5.43,-4.90,-4.12,-6.78,-6.73,-6.48   ,-6.61,-4.2,-4.55,-5.1,-5.4,-1.97,-4.76,-5.61,-1.79,1.65  ,-2.2,-2.17,-1.67,-2.37,-3.03,-2.91,-3.82,-4.5,-4.58,-3.38,  -1.29,-3.72,-3.25,0)
# l3b[sector =="metsa", maara := mat]
# l3b[sector =="puutuote", maara := mata]


l3b[sector == "metsa", sector := "metsajapuu"]
l3b[sector == "metsajapuu", sector2 := "Metsämaa ja puutuotteet"]



l3b[,maara:=sum(maara, na.rm=TRUE), by=.(year)]
l3b = l3b[sector == "metsajapuu", ]
l3= rbind(l3, l3b)

#l3 =l3[year > 2012,]

#sek1$emissions = sek1$emissions/1000
#sek1 = as.data.table(sek1)
#sek1$seka = str_wrap(sek1$sek, width = 45)

#sek1 = sek1[!sek %in% c("Muu erittelem?t?n polttoainek?ytt?","Polttoaineiden haihtumap??st?t")]




# f4 = f3[sector == "Fluorihiilivedyt (HFCt)",
#         maara := maara + f3[sector == "Perfluorihiilivedyt (PFCt)", maara] 
#         + f3[sector == "Rikkiheksafluoridi (SF6)", maara]
#         + f3[sector == "F-kaasut (HFCt+PFCt+SF6)", maara]]
# 
# f4 = f3[!(sector %in% c("Fluorihiilivedyt (HFCt)", "Perfluorihiilivedyt (PFCt)", "Rikkiheksafluoridi (SF6)")),]






url <- "https://pxweb2.stat.fi:443/PxWeb/api/v1/fi/StatFin/khki/statfin_khki_pxt_13qm.px"

taak = list(Päästöluokka = c('0A'),
            Kasvihuonekaasu = c('SS'),
            Vuosi = c('*'),
            Tiedot = c('ets_k', 'ets_e', 'ets_n')
)

taa <- pxweb_get_data(url = url, query = taak)

taa = as.data.table(taa)
taa = taa[,c(1,4,5,6)]



colnames(taa)=c("year", "ets", "esd", "len")

taa$ets = taa$ets+taa$len

taa$len = NULL 


taa = gather(taa, sector, maara, "ets":"esd")
taa = as.data.table(taa)

taa[year == "2020*", year := 2020]
taa[year == "2021*", year := 2021]
taa[year == "2022*", year := 2022]
taa[year == "2023*", year := 2023]

taa$year = as.numeric(taa$year)
taa$maara = taa$maara/1000
# ets = taa[sector =="ets",]
# esd = taa[sector =="ets",]
lulucf = d4[sector == "lulucf" & year %in% 1990:oyear,]

eusek = rbind(taa, lulucf)

eusek[sector =="esd", sector2 := "Taakanjakosektori"]
eusek[sector =="ets", sector2 := "Päästökauppasektori"]
eusek = eusek[!sector == "lulucf",]


eusek1 = copy(eusek)
oyear
# 
# ets = taa[,c("year", "ets")]
# esd = taa[,c("year", "esd")]
# lulucf = d4[sector == "lulucf" & year %in% 2013:2021,]
# 
# 
# 
# 
# 
# suomi2 <- gather(suomi, vuosi, kaasu, "1990":"2017")
# 
# 
# year = 2013:2022
# maara = c(31.2, 29.8, 29.5, 30.6, 29.9, 29.8, 29.4, 28.4, 27.2)
# sector = rep("esd", 9)
# 
# esd = data.table(year, maara, sector)
# 
# year = 2013:2021
# maara = c(31.6, 28.9, 25.6, 27.3, 25.3, 26.4, 23.4, 19.7, 20.5)
# sector = rep("ets", 9)
# 
# ets = data.table(year, maara, sector)
# 
# 
# #ets$maara = ets$maara+.2
# 
#lulucf$maara = lulucf$maara/1000
# 
# eusek = rbind(esd, ets, lulucf)
# 
# 
# maara = c(33.497, 32.977,32.458, 31.938, 31.771, 31.185, 30.599,30.013, 28.840, 27.970,26.626, 25.283, 23.939, NA, NA, NA, NA,17.21)
# 
# year= c(2013:2030)
# kiin = data.frame(year, maara)
# kiin = as.data.table(kiin)
# kiin$sector = "allocation"
# 
# year= c(2013:2030)
# kiin$sector2 = "Taakanjakosektorin päästökiintiö"





vert = -19.29
maara = c(vert, vert, vert,vert,vert)
year= c(2021:2025)
puu = data.frame(year, maara)
puu= as.data.table(puu)
puu$sector = "metsajapuuk"

# year= c(2013:2030)
puu$sector2 = "LULUCF-päästökiintiö (2021-2025)"








maara = c(33.497, 32.977,32.458, 31.938, 31.771, 31.185, 30.599,30.013, 28.840, 27.970,26.626, 25.283, 23.939, NA, NA, NA, NA,17.21)

year= c(2013:2030)
kiin = data.frame(year, maara)
kiin = as.data.table(kiin)
kiin$sector = "allocation"

year= c(2013:2030)
kiin$sector2 = "Taakanjakosektorin päästökiintiö"



lavg= suomie[sector=="lulucf" & year %in% c(2021:2023), mean(maara)]
lavg2 = suomie[sector=="lulucf" & year %in% c(2016:2018), mean(maara)]
mar = seq(lavg, c(lavg2-2.889),length.out = 9)
mara = mar[(5:9)]

maara = mara
year= c(2026:2030)
lal = data.frame(year, maara)
lalc = as.data.table(lal)
lalc$sector = "lallocation"
lalc$sector2 = "LULUCF-päästökiintiö (2026-2030)"






suomie[sector == "ghg", sector2:= "Kokonaispäästöt"]
suomie[sector == "net", sector2 := "Nettopäästöt"]
suomie[sector == "lulucf", sector2 := "Maankäyttösektori (LULUCF)"]



kok = rbind(eusek1, suomie, kiin, l3,puu, lalc)




lahto =  kok[sector %in% "esd" & year %in% c(2021:2023), mean(maara, na.rm=TRUE)]

#set trajectory ending point as 2030 target
loppu = kok[sector %in% "allocation" & year ==2030, mean(maara, na.rm=TRUE)]

hel = seq(lahto,loppu, length.out=(4+6*12))

helvb = hel[3:7]
m30 = hel[(4+6*12)]
m29 = hel[(4+6*12-12)]
m28 = hel[(4+6*12-12-12)]
m27 = hel[(4+6*12-12-12-12)]
m26 = hel[(4+6*12-12-12-12-12)]

kok[sector == "allocation" & year %in% c(2026:2029), maara:=c(m26,m27,m28,m29)]

lyear = kok[sector == "metsajapuu" & maara <1000000, max(year)]

koki =as.data.table(kok)
koki = koki[year > 2004,]
lyear = koki[sector == "metsajapuu" & maara <1000000, max(year)]

dif = 2030-lyear-1

koku = koki[sector %in% c("lulucf", "metsajapuu") & year %in% (lyear-dif):lyear, ]
years = (lyear+1):2030
# koku[ year %in% (lyear-dif):lyear,year := years ]
# repeat years for as many times as there are sectors, twice here
koku[ ,year := c(years, years) ]
cumc =hsv(.55,.9,.9)
debt = hsv(.85, .9,.9)
alloc = hsv(.7, .6,.8)
emis = "#4ba180"

koki = rbind(koki, koku)


koki = as.data.table(koki)


koks = koki[sector =="metsajapuu",]
koks[, sector:="diff"]
kokl = copy(koks) 
kokl[, sector:="cumu"]
kokll = copy(koks) 
kokll[, sector:="cost"]
koklll = copy(koks) 
koklll[, sector:="debt"]
kokllll = copy(koks) 
kokllll[, sector:="price"]
koks$maara = NA
kokl$maara = NA
kokll$maara = NA
koklll$maara = NA
kokllll$maara = NA

koki = rbind(koki, koks, kokl, kokll, koklll, kokllll)

koki[sector =="cumu", col :=cumc]
koki[sector =="diff",col :=hsv(.7,.9,.9)]
koki[sector =="metsajapuuk", col :=alloc]
koki[sector =="metsajapuu",col :=emis]
koki[sector =="lallocation", col :=hsv(.8, .9,.9)]
koki[sector =="lulucf", col :="#4ba180"]
koki[sector =="cost", col :=debt]
koki[sector =="debt", col :=debt]
koki[sector =="price", col :="#bc810d"]

koki$ran = 0
koki[sector =="cumu", ran :=1]
koki[sector =="diff", ran :=2]
koki[sector =="metsajapuuk", ran :=4]
koki[sector =="metsajapuu", ran :=3]
koki[sector =="lallocation", ran :=4]
koki[sector =="lulucf", ran :=3]
koki[sector =="cost", ran :=0]
koki[sector =="debt", ran :=-2]
koki[sector =="price", ran :=1]


koki[sector =="cumu", decim :=1]
koki[sector =="diff", decim :=1]
koki[sector =="metsajapuuk", decim :=1]
koki[sector =="metsajapuu", decim :=1]
koki[sector =="lallocation", decim :=1]
koki[sector =="lulucf", decim :=1]
koki[sector =="cost", decim :=0]
koki[sector =="debt", decim :=0]
koki[sector =="price", decim :=0]

koki[sector =="cumu", si :=.9]
koki[sector =="diff", si :=.45]
koki[sector =="metsajapuuk",si :=.9]
koki[sector =="metsajapuu", si :=.6]
koki[sector =="lallocation", si :=.9]
koki[sector =="lulucf",si :=.6]
koki[sector =="cost", si :=3]
koki[sector =="debt",si :=3]
koki[sector =="price", si :=3]



koki[sector =="cumu", lab :="Ylitysten/alitusten kertymä, Mt"]
koki[sector =="diff", lab :="Kiintiön ylitys/alitus, Mt"]
koki[sector =="metsajapuuk", lab :="Nielukiintiö, Mt"]
koki[sector =="metsajapuu", lab :="Nettonielu, Mt"]
koki[sector =="lallocation",lab :="EU-nielukiintiö, Mt"]
koki[sector =="lulucf", lab :="Nettonielu, Mt"]
koki[sector =="cost",lab :="Kustannus yksiköistä, M€"]
koki[sector =="debt", lab :="Kustannusten kertymä, M€"]
koki[sector =="price", lab :="Yksiköiden keskihinta, €/t"]

koki[sector=="lulucf" & year %in% c(2024:2025), maara :=NA]


write.csv(koki,"data/koki.csv", row.names = FALSE)

