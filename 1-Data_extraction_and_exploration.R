####Script 1####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Data extaction and exploration###

#clear environment
rm(list=ls())

#install packages - unhash if ever unistalled
#install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
#install.packages("rredlist")
#devtools::install_github("ropenscilabs/rredlist")

#load packages
library(plyr)
library(dplyr)
require(stringr)
require(naniar)
require(devtools)
require(rlpi)
require(rredlist)
require(tidyverse)
require(car)

#set working direcotry (left blank for reader to include their own directory).
setwd()

#check working directory
getwd()
dir()

###IMPORT REDLIST DATA###

##store key as enviornment variable - here xxx is used to represent the key as the key is confidential
#Sys.setenv(IUCN_REDLIST_KEY = "xxx")
#check key is stored
#Sys.getenv("IUCN_REDLIST_KEY")

#Can either import red list data from red list package - takes time, or import from working dirtory as it has already been saved there
#unhash as appropriate

##rl species as a data frame
#out <- rl_sp(all = TRUE)
#vapply(out, "[[", 1, "count")
#RL <- do.call(rbind, lapply(out, "[[", "result"))

#write.csv(RL, "RedListDataframe.csv")

###import red list dataframe from working directory###
dir()
RL <- read.csv("RedListDataframe.csv", header = TRUE)

##tidy RLdata

#select useful columns
RLdata <- select(RL, scientific_name, class_name, category)
colnames(RLdata) <- c("Scientific_Name", "Class", "IUCN_Category")

#replace class to lower case
RLdata$Class <- tolower(RLdata$Class)

#filter to just vertibrate species
RLdata <- filter(RLdata, Class %in% c("actinopterygii", "amphibia", "cephalaspidomorphi", "chondrichthyes", "mammalia", "reptilia", "sarcopterygii", "aves", "elasmobranchii", "holocephali", "myxini", "sarcopterygii"))

#change class to a factor and scientific name to character
str(RLdata)
RLdata$Class <- as.factor(RLdata$Class)
RLdata$Scientific_Name <- as.character(RLdata$Scientific_Name)
str(RLdata)

###IMPORT PUBLIC LPI DATA###

##import public LPI csv file from directory
dir()
LPIdata <- read.csv("LPI_LPR2016data_public.csv", header = TRUE)

##tidy LPIdata
#select useful columns
LPIdata <- select(LPIdata, Binomial, Class, Common_name)
colnames(LPIdata) <- c("Scientific_Name", "Class", "Common_Name")

#replace _ with space in red list data to ensure all scientific names written the same way (useful for when merging etc.)
LPIdata$Scientific_Name <- sub("_", " ", LPIdata$Scientific_Name)

#replace class to lower case (again so they are the same between dataframes)
LPIdata$Class <- tolower(LPIdata$Class)

#remove repeats in LPI as we are currently comparing species between the two dataframes so populations not needed
LPIdata <- distinct(LPIdata)

#Correct elasmobrachii and holocephali classes 
LPIdata$Class[LPIdata$Class %in% 'elasmobranchii'] = 'chondrichthyes' 
LPIdata$Class[LPIdata$Class %in% 'holocephali'] = 'chondrichthyes' 

#change LPI variables to characters and factors
str(LPIdata)
LPIdata$Scientific_Name <- as.character(LPIdata$Scientific_Name)
LPIdata$Common_Name <- as.character(LPIdata$Common_Name)
LPIdata$Class <- as.factor(LPIdata$Class)
str(LPIdata)

#correct any taxonomic name differences between the LPI and RL species - change LPI names to equal RL names 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Alcelaphus caama'] = 'Alcelaphus buselaphus ssp. caama' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Acanthodactylus scutellatus'] = 'Acanthodactylus aureus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Alces americanus'] = 'Alces alces' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus bairdii'] = 'Passerculus bairdii' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus henslowii'] = 'Passerculus henslowii' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus leconteii'] = 'Ammospiza leconteii' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus maritimus'] = 'Ammospiza maritima' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus nelsoni'] = 'Ammospiza nelsoni' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Amphispiza belli'] = 'Artemisiospiza belli' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Anolis limifrons'] = 'Anolis microlepis' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Aspitrigla cuculus'] = 'Chelidonichthys cuculus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Aulonocara brevirostre'] = 'Trematocranus brevirostris' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus afrohamiltoni'] = 'Enteromius afrohamiltoni' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus erubescens'] = 'Pseudobarbus erubescens' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus paludinosus'] = 'Enteromius paludinosus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus trimaculatus'] = 'Enteromius trimaculatus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Basileuterus flaveolus'] = 'Myiothlypis flaveola' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Basileuterus leucoblepharus'] = 'Myiothlypis leucoblephara' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bitis nasicornis'] = 'Bitis rhinoceros' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Boa constrictor'] = 'Boa imperator' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bos frontalis'] = 'Bos gaurus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bos grunniens'] = 'Bos mutus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bubalus bubalis'] = 'Bubalus arnee' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bunopithecus hoolock'] = 'Hoolock hoolock' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis cannabina'] = 'Linaria cannabina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis chloris'] = 'Chloris chloris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis flammea'] = 'Acanthis flammea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis flavirostris'] = 'Linaria flavirostris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis lawrencei'] = 'Spinus lawrencei'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis pinus'] = 'Spinus pinus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis psaltria'] = 'Spinus psaltria'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis tristis'] = 'Spinus tristis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carpodacus cassinii'] = 'Haemorhous cassinii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carpodacus mexicanus'] = 'Haemorhous mexicanus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carpodacus purpureus'] = 'Haemorhous purpureus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cathorops spixii'] = 'Cathorops higuchii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cebus capucinus'] = 'Cebus capucinus ssp. curtus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Centropyge loriculus'] = 'Centropyge loricula'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cercopithecus kandti'] = 'Cercopithecus mitis ssp. kandti'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Chelon macrolepis'] = 'Planiliza macrolepis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Chlorospingus ophthalmicus'] = 'Chlorospingus flavopectus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Chthonicola sagittatus'] = 'Pyrrholaemus sagittatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cichlasoma urophthalmum'] = 'Mayaheros urophthalmus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Clupea bentincki'] = 'Strangomera bentincki'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cnemidophorus ocellifer'] = 'Ameivula ocellifera'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coccothraustes vespertinus'] = 'Hesperiphona vespertina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cololabis saira'] = 'Scomberesox saurus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coluber flagellum'] = 'Masticophis flagellum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coluber lateralis'] = 'Masticophis lateralis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coregonus clupeaformis'] = 'Coregonus huntsmani'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Crocodylus johnsoni'] = 'Crocodylus johnstoni'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cyanocompsa cyanoides'] = 'Cyanoloxia Cyanoides'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica caerulescens'] = 'Setophaga caerulescens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica castanea'] = 'Setophaga castanea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica cerulea'] = 'Setophaga cerulea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica coronata'] = 'Setophaga coronata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica discolor'] = 'Setophaga discolor'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica dominica'] = 'Setophaga dominica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica fusca'] = 'Setophaga fusca'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica graciae'] = 'Setophaga graciae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica kirtlandii'] = 'Setophaga kirtlandii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica magnolia'] = 'Setophaga magnolia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica nigrescens'] = 'Setophaga nigrescens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica occidentalis'] = 'Setophaga occidentalis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica palmarum'] = 'Setophaga palmarum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica pensylvanica'] = 'Setophaga pensylvanica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica petechia'] = 'Setophaga petechia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica pinus'] = 'Setophaga pinus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica striata'] = 'Setophaga striata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica tigrina'] = 'Setophaga tigrina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica townsendi'] = 'Setophaga townsendi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica virens'] = 'Setophaga virens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Equus asinus'] = 'Equus africanus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Equus burchellii'] = 'Equus quagga'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Erethizon dorsata'] = 'Erethizon dorsatum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Eulemur albocollaris'] = 'Eulemur cinereiceps'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Galerella sanguinea'] = 'Herpestes sanguineus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hemignathus munroi'] = 'Hemignathus wilsoni'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hemignathus virens'] = 'Chlorodrepanis virens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hemitragus hylocrius'] = 'Nilgiritragus hylocrius'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hirundo daurica'] = 'Cecropis daurica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hirundo nigricans'] = 'Petrochelidon nigricans'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hirundo rupestris'] = 'Ptyonoprogne rupestris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hoplodactylus stephensi'] = 'Toropuku stephensi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyaena brunnea'] = 'Parahyaena brunnea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hydrochoeris hydrochaeris'] = 'Hydrochoerus hydrochaeris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyla chrysoscelis'] = 'Dryophytes chrysoscelis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyla femoralis'] = 'Dryophytes femoralis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyla versicolor'] = 'Dryophytes versicolor'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hylarana chalconota'] = 'Chalcorana chalconota'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hylarana signata'] = 'Pulchrana signata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hypsiboas cinerascens'] = 'Boana cinerascens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hypsiboas fasciatus'] = 'Boana fasciata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Icterus bullockii'] = 'Icterus bullockiorum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Lama glama'] = 'Lama guanicoe'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Lichenostomus penicillatus'] = 'Ptilotula penicillata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Limanda ferruginea'] = 'Pleuronectes ferrugineus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liomys irroratus'] = 'Heteromys irroratus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza alata'] = 'Planiliza alata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza aurata'] = 'Chelon auratus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza dumerili'] = 'Chelon dumerili'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza ramada'] = 'Chelon ramada'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Luscinia svecica'] = 'Cyanecula svecica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Manta alfredi'] = 'Mobula alfredi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Maylandia elegans'] = 'Pseudotropheus elegans'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Maylandia livingstonii'] = 'Pseudotropheus livingstonii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Melozone fuscus'] = 'Melozone fusca'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Micoureus demerarae'] = 'Marmosa demerarae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Miliaria calandra'] = 'Emberiza calandra'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Monachus schauinslandi'] = 'Neomonachus schauinslandi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Monarcha trivirgatus'] = 'Symposiachrus trivirgatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Mugil platanus'] = 'Mugil liza'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Myotomys unisulcatus'] = 'Otomys unisulcatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Myrmotherula hauxwelli'] = 'Isleria hauxwelli'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ocadia sinensis'] = 'Mauremys sinensis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oporornis formosus'] = 'Geothlypis formosa'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oporornis philadelphia'] = 'Geothlypis philadelphia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oporornis tolmiei'] = 'Geothlypis tolmiei'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oreomystis mana'] = 'Manucerthia mana'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oryzomys angouya'] = 'Sooretamys angouya'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oryzomys russatus'] = 'Euryoryzomys russatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Otaria flavescens'] = 'Otaria byronia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parula americana'] = 'Setophaga americana'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus ater'] = 'Periparus ater'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus atricapillus'] = 'Poecile atricapillus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus caeruleus'] = 'Cyanistes caeruleus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus carolinensis'] = 'Poecile carolinensis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus cristatus'] = 'Lophophanes cristatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus gambeli'] = 'Poecile gambeli'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus hudsonicus'] = 'Poecile hudsonicus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus montanus'] = 'Poecile montanus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus rufescens'] = 'Poecile rufescens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pedostibes hosii'] = 'Rentapia hosii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax aristotelis'] = 'Gulosus aristotelis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax atriceps'] = 'Leucocarbo atriceps'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax auritus'] = 'Nannopterum auritus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax brasilianus'] = 'Nannopterum brasilianus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax pelagicus'] = 'Urile pelagicus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax penicillatus'] = 'Urile penicillatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax urile'] = 'Urile urile'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phrynoidis aspera'] = 'Phrynoidis asper'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phylidonyris melanops'] = 'Gliciphila melanops'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Physeter catodon'] = 'Physeter macrocephalus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pica nuttalli'] = 'Pica nutalli'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pipra rubrocapilla'] = 'Ceratopipra rubrocapilla'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Platanista minor'] = 'Platanista gangetica ssp. minor'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pseudolabrus rubicundus'] = 'Pseudolabrus mortonii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ptiloris victoriae'] = 'Lophorina victoriae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Raja binoculata'] = 'Beringraja binoculata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Raja rhina'] = 'Beringraja rhina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Rena dissectus'] = 'Rena dissecta'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Rhinobatos annulatus'] = 'Acroteriobatus annulatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Saiga borealis'] = 'Saiga tatarica ssp. mongolica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Sebastes paucispinis'] = 'Sebastes paucispinus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Selenotoca multifasciata'] = 'Scatophagus tetracanthus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Seminatrix pygaea'] = 'Liodytes pygaea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Sillago analis'] = 'Sillago ciliata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus columbianus'] = 'Urocitellus columbianus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus franklinii'] = 'Poliocitellus franklinii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus parryii'] = 'Urocitellus parryii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus tridecemlineatus'] = 'Ictidomys tridecemlineatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spizella arborea'] = 'Passerella arborea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Squalius alburnoides'] = 'Iberocypris alburnoides'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Stegastes variabilis'] = 'Stegastes xanthurus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tamias amoenus'] = 'Neotamias amoenus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tamias minimus'] = 'Neotamias minimus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Taurotragus derbianus'] = 'Tragelaphus derbianus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Taurotragus oryx'] = 'Tragelaphus oryx'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Theragra chalcogramma'] = 'Gadus chalcogrammus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Thraupis sayaca'] = 'Tangara sayaca'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tiaris fuliginosus'] = 'Asemospiza fuliginosa'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tilapia mariae'] = 'Pelmatolapia mariae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Trachycephalus venulosus'] = 'Trachycephalus typhonius'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Triglopsis quadricornis'] = 'Myoxocephalus quadricornis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Uncia uncia'] = 'Panthera uncia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora celata'] = 'Leiothlypis celata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora luciae'] = 'Leiothlypis luciae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora peregrina'] = 'Leiothlypis peregrina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora ruficapilla'] = 'Leiothlypis ruficapilla'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora virginiae'] = 'Leiothlypis virginiae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vestiaria coccinea'] = 'Drepanis coccinea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Wilsonia canadensis'] = 'Cardellina canadensis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Wilsonia citrina'] = 'Setophaga citrina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Wilsonia pusilla'] = 'Cardellina pusilla'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Xenops rutilans'] = 'Xenops rutilus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Zoothera naevia'] = 'Ixoreus naevius'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Damaliscus korrigum'] = 'Damaliscus lunatus ssp. korrigum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hydromedusa tectifera'] = 'Hydromedusa maximiliani'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Trichosurus johnstonii'] = 'Trichosurus vulpecula'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Callicebus moloch'] = 'Plecturocebus moloch'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Celeus grammicus'] = 'Celeus undatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cyanoloxia Cyanoides'] = 'Cyanoloxia cyanoides'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Gallinago_megala'] = 'Gallinago megala'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Marmosa paraguayanus'] = 'Marmosa paraguayana'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax gaimardi'] = 'Poikilocarbo gaimardi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phelsuma ornata'] = 'Cnemaspis ornata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pipistrellus subflavus'] = 'Perimyotis subflavus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus undulatus'] = 'Urocitellus undulatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Rhinoraja murrayi'] = 'Bathyraja murrayi'


###IMPORT CONFIDENTIAL LPI SPECIES 

##import csv file for list of LPI species for which the trends are confidential 
ConfLPIdata <- read.csv("LPI_taxonomy.csv", header = TRUE)

##tidy confidential data
#select useful columns
ConfLPIdata <- select(ConfLPIdata, rl_binomial, Class, common_name)
colnames(ConfLPIdata) <- c("Scientific_Name", "Class", "Common_Name")

#replace class to lower case
ConfLPIdata$Class <- tolower(ConfLPIdata$Class)

#remove repeats in confidential LPI 
ConfLPIdata <- distinct(ConfLPIdata)

##Correct chondrichthyes and actinopteri classes
ConfLPIdata$Class[ConfLPIdata$Class %in% 'elasmobranchii'] = 'chondrichthyes' 
ConfLPIdata$Class[ConfLPIdata$Class %in% 'holocephali'] = 'chondrichthyes' 
ConfLPIdata$Class[ConfLPIdata$Class %in% 'actinopteri'] = 'actinopterygii' 
ConfLPIdata$Class[ConfLPIdata$Class %in% 'coelacanthi'] = 'actinopterygii' 
ConfLPIdata$Class[ConfLPIdata$Class %in% 'dipneusti'] = 'sarcopterygii'
ConfLPIdata$Class[ConfLPIdata$Class %in% 'petromyzonti'] = 'actinopterygii'

#remove non-vertibrate classes
ConfLPIdata <- filter(ConfLPIdata, Class %in% c("actinopterygii", "amphibia", "cephalaspidomorphi", "chondrichthyes", "mammalia", "reptilia", "sarcopterygii", "aves", "elasmobranchii", "holocephali", "myxini", "sarcopterygii"))

#change ConfLPI variables to characters and factors
str(ConfLPIdata)
ConfLPIdata$Scientific_Name <- as.character(ConfLPIdata$Scientific_Name)
ConfLPIdata$Common_Name <- as.character(ConfLPIdata$Common_Name)
ConfLPIdata$Class <- as.factor(ConfLPIdata$Class)
str(ConfLPIdata)

#for confidential species with scientific name as NULL, use the common name to asign scientific names where possible 
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Sooty oystercatcher'] = 'Haematopus fuliginosus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Red-kneed dotterel'] = 'Erythrogonys cinctus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Monk saki / Miller's saki"] = 'Pithecia monachus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Red howler monkey'] = 'Alouatta seniculus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Orange-naped snake'] = 'Furina ornata'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Jameson's mamba"] = 'Dendroaspis jamesoni'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Nidua fringe-fingered lizard'] = 'Acanthodactylus aureus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'White-faced capuchin'] = 'Cebus versicolor'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'White-fronted capuchin'] = 'Cebus capucinus ssp. curtus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Rainbow skink / Grass skink / Delicate skink'] = 'Trachylepis margaritifer'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'European adder'] = 'Vipera berus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Spix's whiptail"] = 'Ameivula ocellifera'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Redtail tegu'] = 'Vanzosaura rubricauda'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% 'Burrowing night snake'] = 'Philodryas agassizii'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Mitchell's water monitor"] = 'Varanus mitchelli'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Merten's water monitor"] = 'Varanus mertensi'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Stripefoot anole"] = 'Anolis lineatopus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Cuban brown anole"] = 'Anolis sagrei'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Eastern water dragon"] = 'Intellagama lesueurii'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Common New Zealand skink"] = 'Oligosoma polychroma'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Tiger keelback"] = 'Rhabdophis tigrinus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Andamans bent-toed gecko"] = 'Cyrtodactylus rubidus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Yellow-chinned anole"] = 'Anolis gundlachi'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Emerald anole"] = 'Anolis evermanni'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Olive bush anole"] = 'Anolis krugi'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Puerto Rican racer"] = 'Borikenophis variegatus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "South-American snake-headed turtle"] = 'Hydromedusa tectifera'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Anchieta’s dune / Shovel-snouted lizard"] = 'Meroles anchietae'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Brown catsnake / Brown tree snake"] = 'Boiga irregularis'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Pale-rumped ctenotus"] = 'Ctenotus regius'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Northern death adder"] = 'Acanthophis praelongus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Short-crested Bay Island forest lizard"] = 'Coryphophylax subcristatus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Jacky lashtail / Jacky lizard / Jacky dragon"] = 'Amphibolurus muricatus'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Lesueur's velvet gecko"] = 'Amalosia lesueurii'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Etheridge's lava lizard"] = 'Tropidurus etheridgei'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Slow worm"] = 'Anguis fragilis'
ConfLPIdata$Scientific_Name[ConfLPIdata$Common_Name %in% "Marbled lungfish"] = 'Protopterus aethiopicus ssp. aethiopicus'

#remove rows where both the scientific name and common name were NULL
ConfLPIdata <-ConfLPIdata[-c(1555 , 2646, 4032), ]

#merge lists together to get full list of LPI species - preliminory investigations showed that  
#both databases have some unique species, so to get the most comprehensive list, must merge them both
LPIdata <-  merge(LPIdata, ConfLPIdata, all = TRUE, by = c("Scientific_Name", "Class"))
LPIdata<- arrange(LPIdata, Scientific_Name)

#check structure of full LPI dataset, ensure common names are characters so they can be merged
str(LPIdata)
LPIdata$Common_Name.x <- as.character(LPIdata$Common_Name.x)
LPIdata$Common_Name.y <- as.character(LPIdata$Common_Name.y)
str(LPIdata)

#merge common names together 
LPIdata <- LPIdata%>%mutate(Common_Name = coalesce(Common_Name.y, Common_Name.x))
LPIdata <- select(LPIdata, -Common_Name.x, -Common_Name.y)

#As some confLPI names were also out of sync with RL names, re-run this code then remove the repeats
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Alcelaphus caama'] = 'Alcelaphus buselaphus ssp. caama' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Acanthodactylus scutellatus'] = 'Acanthodactylus aureus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Alces americanus'] = 'Alces alces' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus bairdii'] = 'Passerculus bairdii' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus henslowii'] = 'Passerculus henslowii' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus leconteii'] = 'Ammospiza leconteii' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus maritimus'] = 'Ammospiza maritima' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ammodramus nelsoni'] = 'Ammospiza nelsoni' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Amphispiza belli'] = 'Artemisiospiza belli' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Anolis limifrons'] = 'Anolis microlepis' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Aspitrigla cuculus'] = 'Chelidonichthys cuculus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Aulonocara brevirostre'] = 'Trematocranus brevirostris' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus afrohamiltoni'] = 'Enteromius afrohamiltoni' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus erubescens'] = 'Pseudobarbus erubescens' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus paludinosus'] = 'Enteromius paludinosus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Barbus trimaculatus'] = 'Enteromius trimaculatus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Basileuterus flaveolus'] = 'Myiothlypis flaveola' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Basileuterus leucoblepharus'] = 'Myiothlypis leucoblephara' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bitis nasicornis'] = 'Bitis rhinoceros' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Boa constrictor'] = 'Boa imperator' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bos frontalis'] = 'Bos gaurus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bos grunniens'] = 'Bos mutus' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bubalus bubalis'] = 'Bubalus arnee' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Bunopithecus hoolock'] = 'Hoolock hoolock' 
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis cannabina'] = 'Linaria cannabina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis chloris'] = 'Chloris chloris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis flammea'] = 'Acanthis flammea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis flavirostris'] = 'Linaria flavirostris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis lawrencei'] = 'Spinus lawrencei'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis pinus'] = 'Spinus pinus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis psaltria'] = 'Spinus psaltria'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carduelis tristis'] = 'Spinus tristis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carpodacus cassinii'] = 'Haemorhous cassinii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carpodacus mexicanus'] = 'Haemorhous mexicanus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Carpodacus purpureus'] = 'Haemorhous purpureus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cathorops spixii'] = 'Cathorops higuchii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cebus capucinus'] = 'Cebus capucinus ssp. curtus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Centropyge loriculus'] = 'Centropyge loricula'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cercopithecus kandti'] = 'Cercopithecus mitis ssp. kandti'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Chelon macrolepis'] = 'Planiliza macrolepis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Chlorospingus ophthalmicus'] = 'Chlorospingus flavopectus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Chthonicola sagittatus'] = 'Pyrrholaemus sagittatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cichlasoma urophthalmum'] = 'Mayaheros urophthalmus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Clupea bentincki'] = 'Strangomera bentincki'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cnemidophorus ocellifer'] = 'Ameivula ocellifera'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coccothraustes vespertinus'] = 'Hesperiphona vespertina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cololabis saira'] = 'Scomberesox saurus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coluber flagellum'] = 'Masticophis flagellum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coluber lateralis'] = 'Masticophis lateralis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Coregonus clupeaformis'] = 'Coregonus huntsmani'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Crocodylus johnsoni'] = 'Crocodylus johnstoni'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cyanocompsa cyanoides'] = 'Cyanoloxia Cyanoides'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica caerulescens'] = 'Setophaga caerulescens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica castanea'] = 'Setophaga castanea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica cerulea'] = 'Setophaga cerulea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica coronata'] = 'Setophaga coronata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica discolor'] = 'Setophaga discolor'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica dominica'] = 'Setophaga dominica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica fusca'] = 'Setophaga fusca'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica graciae'] = 'Setophaga graciae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica kirtlandii'] = 'Setophaga kirtlandii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica magnolia'] = 'Setophaga magnolia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica nigrescens'] = 'Setophaga nigrescens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica occidentalis'] = 'Setophaga occidentalis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica palmarum'] = 'Setophaga palmarum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica pensylvanica'] = 'Setophaga pensylvanica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica petechia'] = 'Setophaga petechia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica pinus'] = 'Setophaga pinus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica striata'] = 'Setophaga striata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica tigrina'] = 'Setophaga tigrina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica townsendi'] = 'Setophaga townsendi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Dendroica virens'] = 'Setophaga virens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Equus asinus'] = 'Equus africanus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Equus burchellii'] = 'Equus quagga'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Erethizon dorsata'] = 'Erethizon dorsatum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Eulemur albocollaris'] = 'Eulemur cinereiceps'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Galerella sanguinea'] = 'Herpestes sanguineus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hemignathus munroi'] = 'Hemignathus wilsoni'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hemignathus virens'] = 'Chlorodrepanis virens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hemitragus hylocrius'] = 'Nilgiritragus hylocrius'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hirundo daurica'] = 'Cecropis daurica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hirundo nigricans'] = 'Petrochelidon nigricans'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hirundo rupestris'] = 'Ptyonoprogne rupestris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hoplodactylus stephensi'] = 'Toropuku stephensi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyaena brunnea'] = 'Parahyaena brunnea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hydrochoeris hydrochaeris'] = 'Hydrochoerus hydrochaeris'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyla chrysoscelis'] = 'Dryophytes chrysoscelis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyla femoralis'] = 'Dryophytes femoralis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hyla versicolor'] = 'Dryophytes versicolor'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hylarana chalconota'] = 'Chalcorana chalconota'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hylarana signata'] = 'Pulchrana signata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hypsiboas cinerascens'] = 'Boana cinerascens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hypsiboas fasciatus'] = 'Boana fasciata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Icterus bullockii'] = 'Icterus bullockiorum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Lama glama'] = 'Lama guanicoe'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Lichenostomus penicillatus'] = 'Ptilotula penicillata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Limanda ferruginea'] = 'Pleuronectes ferrugineus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liomys irroratus'] = 'Heteromys irroratus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza alata'] = 'Planiliza alata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza aurata'] = 'Chelon auratus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza dumerili'] = 'Chelon dumerili'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Liza ramada'] = 'Chelon ramada'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Luscinia svecica'] = 'Cyanecula svecica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Manta alfredi'] = 'Mobula alfredi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Maylandia elegans'] = 'Pseudotropheus elegans'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Maylandia livingstonii'] = 'Pseudotropheus livingstonii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Melozone fuscus'] = 'Melozone fusca'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Micoureus demerarae'] = 'Marmosa demerarae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Miliaria calandra'] = 'Emberiza calandra'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Monachus schauinslandi'] = 'Neomonachus schauinslandi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Monarcha trivirgatus'] = 'Symposiachrus trivirgatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Mugil platanus'] = 'Mugil liza'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Myotomys unisulcatus'] = 'Otomys unisulcatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Myrmotherula hauxwelli'] = 'Isleria hauxwelli'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ocadia sinensis'] = 'Mauremys sinensis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oporornis formosus'] = 'Geothlypis formosa'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oporornis philadelphia'] = 'Geothlypis philadelphia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oporornis tolmiei'] = 'Geothlypis tolmiei'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oreomystis mana'] = 'Manucerthia mana'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oryzomys angouya'] = 'Sooretamys angouya'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Oryzomys russatus'] = 'Euryoryzomys russatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Otaria flavescens'] = 'Otaria byronia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parula americana'] = 'Setophaga americana'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus ater'] = 'Periparus ater'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus atricapillus'] = 'Poecile atricapillus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus caeruleus'] = 'Cyanistes caeruleus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus carolinensis'] = 'Poecile carolinensis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus cristatus'] = 'Lophophanes cristatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus gambeli'] = 'Poecile gambeli'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus hudsonicus'] = 'Poecile hudsonicus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus montanus'] = 'Poecile montanus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Parus rufescens'] = 'Poecile rufescens'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pedostibes hosii'] = 'Rentapia hosii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax aristotelis'] = 'Gulosus aristotelis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax atriceps'] = 'Leucocarbo atriceps'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax auritus'] = 'Nannopterum auritus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax brasilianus'] = 'Nannopterum brasilianus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax pelagicus'] = 'Urile pelagicus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax penicillatus'] = 'Urile penicillatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax urile'] = 'Urile urile'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phrynoidis aspera'] = 'Phrynoidis asper'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phylidonyris melanops'] = 'Gliciphila melanops'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Physeter catodon'] = 'Physeter macrocephalus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pica nuttalli'] = 'Pica nutalli'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pipra rubrocapilla'] = 'Ceratopipra rubrocapilla'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Platanista minor'] = 'Platanista gangetica ssp. minor'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pseudolabrus rubicundus'] = 'Pseudolabrus mortonii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Ptiloris victoriae'] = 'Lophorina victoriae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Raja binoculata'] = 'Beringraja binoculata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Raja rhina'] = 'Beringraja rhina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Rena dissectus'] = 'Rena dissecta'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Rhinobatos annulatus'] = 'Acroteriobatus annulatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Saiga borealis'] = 'Saiga tatarica ssp. mongolica'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Sebastes paucispinis'] = 'Sebastes paucispinus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Selenotoca multifasciata'] = 'Scatophagus tetracanthus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Seminatrix pygaea'] = 'Liodytes pygaea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Sillago analis'] = 'Sillago ciliata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus columbianus'] = 'Urocitellus columbianus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus franklinii'] = 'Poliocitellus franklinii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus parryii'] = 'Urocitellus parryii'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus tridecemlineatus'] = 'Ictidomys tridecemlineatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spizella arborea'] = 'Passerella arborea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Squalius alburnoides'] = 'Iberocypris alburnoides'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Stegastes variabilis'] = 'Stegastes xanthurus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tamias amoenus'] = 'Neotamias amoenus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tamias minimus'] = 'Neotamias minimus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Taurotragus derbianus'] = 'Tragelaphus derbianus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Taurotragus oryx'] = 'Tragelaphus oryx'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Theragra chalcogramma'] = 'Gadus chalcogrammus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Thraupis sayaca'] = 'Tangara sayaca'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tiaris fuliginosus'] = 'Asemospiza fuliginosa'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Tilapia mariae'] = 'Pelmatolapia mariae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Trachycephalus venulosus'] = 'Trachycephalus typhonius'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Triglopsis quadricornis'] = 'Myoxocephalus quadricornis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Uncia uncia'] = 'Panthera uncia'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora celata'] = 'Leiothlypis celata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora luciae'] = 'Leiothlypis luciae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora peregrina'] = 'Leiothlypis peregrina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora ruficapilla'] = 'Leiothlypis ruficapilla'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vermivora virginiae'] = 'Leiothlypis virginiae'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Vestiaria coccinea'] = 'Drepanis coccinea'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Wilsonia canadensis'] = 'Cardellina canadensis'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Wilsonia citrina'] = 'Setophaga citrina'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Wilsonia pusilla'] = 'Cardellina pusilla'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Xenops rutilans'] = 'Xenops rutilus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Zoothera naevia'] = 'Ixoreus naevius'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Damaliscus korrigum'] = 'Damaliscus lunatus ssp. korrigum'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Hydromedusa tectifera'] = 'Hydromedusa maximiliani'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Trichosurus johnstonii'] = 'Trichosurus vulpecula'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Callicebus moloch'] = 'Plecturocebus moloch'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Celeus grammicus'] = 'Celeus undatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Cyanoloxia Cyanoides'] = 'Cyanoloxia cyanoides'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Gallinago_megala'] = 'Gallinago megala'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Marmosa paraguayanus'] = 'Marmosa paraguayana'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phalacrocorax gaimardi'] = 'Poikilocarbo gaimardi'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Phelsuma ornata'] = 'Cnemaspis ornata'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Pipistrellus subflavus'] = 'Perimyotis subflavus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Spermophilus undulatus'] = 'Urocitellus undulatus'
LPIdata$Scientific_Name[LPIdata$Scientific_Name %in% 'Rhinoraja murrayi'] = 'Bathyraja murrayi'

#remove repeats in complete LPI 
LPIdata <- distinct(LPIdata)

#remove first row
LPIdata <- LPIdata[-1, ]

#save comprehensive LPI dataset
write.csv(LPIdata, "Full_LPI_Species_Dataset.csv")

###Merge comprehensive LPI dataframe with Red List dataframe###

#set up a present in LPI column as TRUE
LPIdata$PresentInLPI <- TRUE
#set up a present in RL column with TRUE
RLdata$PresentInRL <- TRUE

#create new dataframe called AllData by merging LPI data and RL data
AllData <- merge(LPIdata, RLdata, all = TRUE, by = c("Scientific_Name", "Class"))
AllData <- arrange(AllData, Scientific_Name)

#convert Present from 'TRUE, NA' to 'TRUE, FALSE'
AllData$PresentInLPI <- ifelse(is.na(AllData$PresentInLPI), FALSE, TRUE)
AllData$PresentInRL <- ifelse (is.na(AllData$PresentInRL), FALSE, TRUE)

#check how many species are present in each database
with(AllData, table(PresentInLPI, PresentInRL))

#get dataframe of the species in the LPI and not the RL - useful for results section
LPInotRL <- filter(AllData, PresentInLPI %in% TRUE & PresentInRL %in% FALSE)

#same dataframe but only non-fish species - useful for results section
LPInotRLnotFish <- filter(LPInotRL, Class %in% c('reptilia', 'aves', 'mammalia', 'amphibia'))

###Create Summary Tables###
#remove plry for group_by to work properly
detach(package:plyr)

#create SumTable using counts 
SumTableCounts <- AllData%>%group_by(Class)%>%summarise(number_in_LPI = sum(PresentInLPI), number_in_RL = sum(PresentInRL), number_in_both = sum(PresentInRL&PresentInLPI))

#add a total row
SumTableCounts2 <- AllData%>%summarise(number_in_LPI = sum(PresentInLPI), number_in_RL  = sum(PresentInRL), number_in_both = sum(PresentInRL&PresentInLPI))
SumTableCounts2$Class <- "Total"
SumTableCounts <- rbind(SumTableCounts, SumTableCounts2)

#create SumTable using percentage 
SumTablePercent <- AllData%>%group_by(Class)%>%summarise(PercentageLPI = sum((PresentInLPI)/n()*100), PercentageRL = sum((PresentInRL)/n()*100), PercentageBoth = sum((PresentInRL&PresentInLPI)/n()*100))

#add a total row
SumTablePercent2 <- AllData%>%summarise(PercentageLPI = sum((PresentInLPI)/n()*100), PercentageRL = sum((PresentInRL)/n()*100), PercentageBoth = sum((PresentInRL&PresentInLPI)/n()*100))
SumTablePercent2$Class <- "Total"
SumTablePercent <- rbind(SumTablePercent, SumTablePercent2)

#round to 2 decimal places
SumTablePercent$PercentageLPI <- format(round(SumTablePercent$PercentageLPI, 2), nsmall = 2)
SumTablePercent$PercentageRL <- format(round(SumTablePercent$PercentageRL, 2), nsmall = 2)
SumTablePercent$PercentageBoth <- format(round(SumTablePercent$PercentageBoth, 2), nsmall = 2)


#reload plry
library(plyr)

###INCLUDE IUCN ASSESSMENT CRITERIA###

#access criteria through rl_search function as below
#save an example dataframe to use col names from later
Puffin <- as.data.frame(rl_search('Fratercula arctica'))

#create new dataframe to fill with rl_search output
output <- as.data.frame(matrix(ncol = ncol(Puffin)))

#give columns names
colnames(output) <- colnames(Puffin)

#create list to loop through 
species = unique(RLdata$Scientific_Name)

#loop to append data
for (i in seq(1, length(species), by=1000)) {
  
  cat("Processing species ", i, " to ", (i+999), "...\n")
  subset_species = species[i:(i+999)]
  
  for (sp in subset_species) {
    rl_result <- data.frame(name = sp, stringsAsFactors = FALSE)
    tryCatch({
      rl_result <- as.data.frame(rl_search(sp), stringsAsFactors = FALSE)
      
    }, error = function(e){
      cat("Error processing species: ", sp, "\n")
      
    })
    output <- rbind.fill(output, rl_result)
  }
  
  # Pause for 30 seconds
  cat("Waiting for 30 seconds to continue...\n")
  Sys.sleep(30)
  
}

#save output
write.csv(output, "output_finished.csv")

###merge extracted data with full datasets###

#import RL criteria
RLCriteria <- read.csv("output_finished.csv", header = TRUE, row.names = 1)

#remove first row and rows 47037-47256 which have thrown errors
RLCriteria <- RLCriteria[-c(1, 47037:47256), ]

#subset to only scienfitic name, criteria, population trend and common name columns
RLCriteria <- select(RLCriteria, result.scientific_name, result.criteria, result.main_common_name, result.population_trend)

#give columns appropriate names
colnames(RLCriteria) <- c("Scientific_Name", "IUCN_Criteria", "Common_Name", "Population_Trend")

#merge with AllData to produce full dataset 
DataWithCriteria <- merge(AllData, RLCriteria, all = TRUE, by ="Scientific_Name")

#check structure of full dataset, DataWithCriteria, ensure common names are charcters
str(DataWithCriteria)
DataWithCriteria$Common_Name.x <- as.character(DataWithCriteria$Common_Name.x)
DataWithCriteria$Common_Name.y <- as.character(DataWithCriteria$Common_Name.y)
str(DataWithCriteria)

#merge common names together 
DataWithCriteria <- DataWithCriteria%>%mutate(Common_Name = coalesce(Common_Name.y, Common_Name.x))
DataWithCriteria <- select(DataWithCriteria, -Common_Name.x, -Common_Name.y)

#remove rows 53583-54081 - empty cells
DataWithCriteria <-DataWithCriteria[-c(53583:54081), ]

#save as csv
write.csv(DataWithCriteria, "DataWithCriteria&Trend.csv")

##create summary tables for number of species categorised under each criteria##
#explore different criteria
Criteria <- unique(DataWithCriteria$IUCN_Criteria) 
Criteria
length(Criteria)

##Add TRUE FALSE collumns for  A, B, C etc.for summary table
#dataset to only species which have a criteria (only threatened species)
DataOnlyCriteria <- subset(DataWithCriteria, (!is.na(DataWithCriteria$IUCN_Criteria)))

#identify those categorised by each criterion
criterionA <-DataOnlyCriteria[grep('[A]', DataOnlyCriteria$IUCN_Criteria), ]
criterionA$CriteriaA <- "TRUE"

criterionB <-DataOnlyCriteria[grep('[B]', DataOnlyCriteria$IUCN_Criteria), ]
criterionB$CriteriaB <- "TRUE"

criterionC <- DataOnlyCriteria[grep('[C]', DataOnlyCriteria$IUCN_Criteria), ]
criterionC$CriteriaC <- "TRUE"

criterionD <- DataOnlyCriteria[grep('[D]', DataOnlyCriteria$IUCN_Criteria), ]
criterionD$CriteriaD <- "TRUE"

criterionE <- DataOnlyCriteria[grep('[E]', DataOnlyCriteria$IUCN_Criteria), ]
criterionE$CriteriaE <- "TRUE"

#join together to produce full dataset 
criterionAB <- merge(criterionA, criterionB, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Common_Name", "Class", "IUCN_Category", "PresentInRL", "PresentInLPI", "Population_Trend"))
criterionABC <- merge(criterionAB, criterionC, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Class", "IUCN_Category", "PresentInRL", "Common_Name", "PresentInLPI", "Population_Trend"))
criterionABCD <- merge(criterionABC, criterionD, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Class", "IUCN_Category", "PresentInRL", "Common_Name", "PresentInLPI", "Population_Trend"))
DataOnlyCriteria <- merge(criterionABCD, criterionE, all = TRUE, by = c("Scientific_Name", "IUCN_Criteria", "Class", "IUCN_Category", "PresentInRL", "Common_Name", "PresentInLPI", "Population_Trend"))

#Add falses ('TRUE, FALSE' used for ease of creating summary tables)
DataOnlyCriteria$CriteriaA <- ifelse(is.na(DataOnlyCriteria$CriteriaA), FALSE, TRUE)
DataOnlyCriteria$CriteriaB <- ifelse(is.na(DataOnlyCriteria$CriteriaB), FALSE, TRUE)
DataOnlyCriteria$CriteriaC <- ifelse(is.na(DataOnlyCriteria$CriteriaC), FALSE, TRUE)
DataOnlyCriteria$CriteriaD <- ifelse(is.na(DataOnlyCriteria$CriteriaD), FALSE, TRUE)
DataOnlyCriteria$CriteriaE <- ifelse(is.na(DataOnlyCriteria$CriteriaE), FALSE, TRUE)

###Create Summary Tables###

#check structure
str(DataOnlyCriteria)

##table of percentages of species categorised by each criteria
CriteriaSumTableA <- summarise(DataOnlyCriteria, PercentA = (sum(CriteriaA)/nrow(DataOnlyCriteria)*100))
CriteriaSumTableB <- summarise(DataOnlyCriteria, PercentB = (sum(CriteriaB)/nrow(DataOnlyCriteria)*100))
CriteriaSumTableC <- summarise(DataOnlyCriteria, PercentC = (sum(CriteriaC)/nrow(DataOnlyCriteria)*100))
CriteriaSumTableD <- summarise(DataOnlyCriteria, PercentD = (sum(CriteriaD)/nrow(DataOnlyCriteria)*100))
CriteriaSumTableE <- summarise(DataOnlyCriteria, PercentE = (sum(CriteriaE)/nrow(DataOnlyCriteria)*100))

#bind together
CriteriaSumTable <- cbind(CriteriaSumTableA, CriteriaSumTableB, CriteriaSumTableC, CriteriaSumTableD, CriteriaSumTableE)

#round to 2 decimal places
CriteriaSumTable <- format(round(CriteriaSumTable, 2), nsmall = 2)

##table of counts of species categorised by each criteria
CritSumTableCounts <- summarise(DataOnlyCriteria, counts_A = sum(CriteriaA), counts_B = sum(CriteriaB), counts_C = sum(CriteriaC), counts_D = sum(CriteriaD), counts_E = sum(CriteriaE))
