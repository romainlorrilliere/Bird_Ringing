library(data.table)

b <- fread("library/bague_size.csv")
sp <- fread("library/CODES_ESPECES_CRBPO.csv")

setnames(sp,c("CODE","NOM VERNACULAIRE"),c("sp_code_crbpo","french_name"))
sp <- sp[,.(sp_code_crbpo,french_name)]
bb <- merge(b,sp,by="sp_code_crbpo",all.x=TRUE)

bb <- bb[,c(2,1,9,3:8)]
fwrite(bb,"library/taille_bague_fr.csv")

library(readxl)
library(lubridate)

les_fichiers <- c("data_archive/data_raw/masque_saisie_donnees_crbpo_v3.118_RL_SPOL-JDP_2018_2019.xlsx")
les_onglets <- c("SAISIE")


d <- NULL
for(i in 1:length(les_fichiers)){
    file_name <- les_fichiers[i]
    cat(file_name,"\n")
    di <- read_xlsx(file_name, sheet = les_onglets[i])
    di$hiver <- les_hivers[i]
    cat(nrow(di),"lignes\n")
    d <- rbind(d,di)
}

d <- data.table(d)



dd <- fread("data_raw/LORRILLIERE_Romain_2019112710h55h07_RL.csv")
setnames(dd,"THEME SESSION","THEME_SESSION")

dd_date <- unique(dd[THEME %in% c("SPOL","MANGEOIRE")| THEME_SESSION == "MANGEOIRE",DATE])
dd_date <- unique(dd[THEME_SESSION == "MANGEOIRE",DATE])
dd_date
dd_date <- as.Date(dd_date,format = "%d/%m/%Y")


dd <- dd[THEME %in% c("SPOL","MANGEOIRE")| THEME_SESSION %in% c("SPOL","MANGEOIRE"),]
dd <- dd[LIEUDIT != "NOIRBREUIL"]

dd_sp <- as.data.frame(table(dd[,ESPECE]))
colnames(dd_sp) <- c("sp_code_crbpo","nb")
dd_sp
dd_sp <- rbind(dd_sp,data.frame(sp_code_crbpo = c("TURILI","TURPIL","TURVIS","COLBUS"),nb = 0))

setDT(dd_sp)

setorder(dd_sp, -nb)
dd_sp


bb <- merge(bb,dd_sp,by="sp_code_crbpo")
bb

fwrite(bb,"library/taille_bague_spol_mangeoire.csv")




