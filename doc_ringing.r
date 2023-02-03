library(data.table)
library(readxl)
library(lubridate)



b <- fread("library/bague_size.csv",encoding="Latin-1")
sp <- fread("library/CODES_ESPECES_CRBPO.csv",encoding="Latin-1")

setnames(sp,c("CODE","NOM VERNACULAIRE"),c("sp_code_crbpo","french_name"))
sp <- sp[,.(sp_code_crbpo,french_name)]
bb <- merge(b,sp,by="sp_code_crbpo",all.x=TRUE)

bb <- bb[,c(2,1,9,3:8)]
fwrite(bb,"library/taille_bague_fr.csv")


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



## SPOL

les_fichiers <- dir("saisie/")
les_fichiers <- les_fichiers[grep("SPOL",les_fichiers)]

les_fichiers <- les_fichiers[grep("xlsx",les_fichiers)]
print(les_fichiers)

les_fichiers <- paste0("saisie/",les_fichiers)



lescolonnes <- c("ACTION","CENTRE","BAGUE","DATE","HEURE", "ESPECE","SEXE","AGE","MEMO SESSION","MEMO","FS","HS","DS","NF","CS","PI","PC","LP","LT","MA","ES","MU","COND REPR","CIRC REPR","PAYS","DEPT","LOCALITE","LIEUDIT","LAT","LON","THEME SESSION","THEME","BAGUEUR","AD", "CA", "SG", "SC")

d <- NULL
for(i in 1:length(les_fichiers)){
    file_name <- les_fichiers[i]
    cat(file_name,"\n")
    di <- read_xlsx(file_name, sheet = "SAISIE")
    di <- di[,lescolonnes]
        cat(nrow(di),"lignes\n")
    d <- rbind(d,di)
}

setDT(d)
setnames(d,colnames(d),gsub(" ","_",colnames(d)))
d[,DATE:=as.Date(DATE)]
d[,HEURE := format(as.POSIXct(HEURE),"%H:%M")]
d[,HS := format(as.POSIXct(HS),"%H:%M")]
d[,DS := format(as.POSIXct(DS),"%H:%M")]



dd <- fread("data_raw/LORRILLIERE_Romain_2019112710h55h07_RL.csv",dec=",",encoding="Latin-1")
dd[,SC := NA]
dd <- dd[,lescolonnes,with= FALSE]
setnames(dd,colnames(dd),gsub(" ","_",colnames(dd)))


dd[,BAGUE := gsub("\\.","",BAGUE,perl=TRUE)]

dd <- dd[THEME %in% c("SPOL","MANGEOIRE")| THEME_SESSION %in% c("SPOL","MANGEOIRE"),]
dd <- dd[LIEUDIT != "NOIRBREUIL"]


dd[,DATE:=as.Date(DATE,format = "%d/%m/%Y")]
dd[,HEURE := substr(HEURE,1,5)]

dd[nchar(DS) == 11,DS := substr(DS,7,11)]
dd[nchar(DS) == 8,DS := substr(DS,1,5)]

dd_date <- unique(dd[THEME %in% c("SPOL","MANGEOIRE")| THEME_SESSION == "MANGEOIRE",DATE])
dd_date <- unique(dd[THEME_SESSION == "MANGEOIRE",DATE])
dd_date

dim(d)
d <- d[!(DATE %in% dd_date),]
dim(d)


d <- rbind(dd,d)

d[,`:=`(year = year(DATE), month = month(DATE))]
d <- d[month < 4 | month > 9,]

d[,hiver := ifelse(month > 9,paste0(year,"-",substr(year + 1,3,4)),paste0(year-1,"-",substr(year,3,4)))]

d[,ESPECE := substr(ESPECE,1,6)]

d[grep("ardin",LIEUDIT),LIEU := "JdP"]
d[is.na(LIEU),LIEU := ifelse(DEPT %in% c("75","91"),"IdF","Fr")]

d_ind <- d[,.(nb=.N),by = .(BAGUE,ESPECE,LIEU)]
dd_sp <- dcast(d_ind,ESPECE ~ LIEU, value.var = "nb")

dd_sp <- dd_sp[,.(ESPECE,JdP,IdF,Fr)]
d_ind_bague <- d[ACTION == "B",.(JdP_ring=.N),by = .(BAGUE,ESPECE,LIEU)]

dd_sp_all <- d[,.(N = .N),by = .(ESPECE)]
dd_sp_autre <- data.frame(ESPECE = c("TURPIL","COCTES","DENMAJ","DENMIN","MOTCIN","BOMGAR","PHOOCH","TURVIS","PICPIC","GARGLA","STUVUL","CARMEA"))
dd_sp_all <- merge(dd_sp_all,dd_sp_autre,all=TRUE)
dd_sp_all <- dd_sp_all[,.(ESPECE)]



dd_sp <- merge(dd_sp_jdp_ring,dd_sp,by="ESPECE",all=TRUE)

dd_sp <- merge(dd_sp,dd_sp_all,by="ESPECE",all=TRUE)


dd_sp[is.na(dd_sp)] <- 0


setnames(dd_sp,"ESPECE","sp_code_crbpo")

bb <- fread("library/taille_bague_fr.csv",encoding="Latin-1")
bb_sp <- merge(bb,dd_sp,by="sp_code_crbpo")
bb_sp

fwrite(bb,"library/taille_bague_spol_mangeoire.csv")


library(kableExtra)

k_bb_sp <- kable(bb_sp)
save_kable(k_bb_sp,"library/taille_bague_spol_mangeoire.pdf")


t1 <- ttheme_default(core=list(
        fg_params=list(fontface=c(rep("plain", 4), "bold.italic")),
        bg_params = list(fill=c(rep(c("grey95", "grey90"),
                                    length.out=4), "#6BAED6"),
                         alpha = rep(c(1,0.5), each=5))
        ))

t2 <- ttheme_minimal(core=list(
        fg_params=list(fontsize=5),
        bg_params = list(fill=rep(c("grey95", "grey90"))
        )))
grid.table(bb_sp,theme = t2)



mytable     <-   tableGrob(bb_sp,
                       gpar.coretext = gpar(fontsize = 12),
                       gpar.coltext  = gpar(fontsize = 12,col="white"),
                       gpar.rowtext  = gpar(fontsize = 12, fontface="bold"),
                       gpar.corefill = gpar(fill = rgb(255,255,255, maxColorValue      =255), alpha = 1, col = NA),
                       gpar.rowfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),
                       gpar.colfill  = gpar(fill = 0, alpha = 1 ,col= "white"),
                       equal.width   = TRUE,
                       show.rownames = TRUE,
                       show.rsep     = TRUE,
                       show.hlines   = TRUE,
                       show.csep     = FALSE,
                       show.vlines   = FALSE,
                       show.box      = FALSE,
                       padding.h     = unit(15, "mm"),
                       padding.v     = unit(8, "mm"),
                       core.just     = "center",
                       row.just      = "left")

mytable


library("gridExtra")
pdf("library/taille_bague_spol_mangeoire.pdf")       # Export PDF
grid.table(bb_sp,theme = t2)
dev.off()






dd <- fread("data_raw/LORRILLIERE_Romain_2019112710h55h07_RL.csv")
setnames(dd,"THEME SESSION","THEME_SESSION")

dd <- dd[(THEME == "STOC" | THEME_SESSION == "STOC" ) & LOCALITE == "SAINT-HERBLAIN",]

dd_sp <- as.data.frame(table(dd[,ESPECE]))
colnames(dd_sp) <- c("sp_code_crbpo","nb")
setDT(dd_sp)

setorder(dd_sp, -nb)
dd_sp

bb <- fread("library/taille_bague_spol_mangeoire.csv",encoding="Latin-1")
bb[,nb:=NULL]

bbb <- merge(bb,dd_sp,by="sp_code_crbpo")
bbb

bbb <- bbb[group != "PUL",.(sp_code_crbpo,update,french_name,diam,letter,nb)]
bbb

fwrite(bbb,"library/taille_bague_stoc_rieffele.csv")











