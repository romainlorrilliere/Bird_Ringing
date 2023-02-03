
doc_spol_jdp <- function(output = TRUE, write_pdf= FALSE){

#    output = TRUE; write_pdf= TRUE
    library(data.table)
    library(readxl)
    library(lubridate)


    if(length(grep("saisie",dir()))==0)
        setwd("..")
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

    d_ind_ring <- d[ACTION == "B" & LIEU == "JdP",.(nb=.N),by = .(BAGUE,ESPECE,LIEU)]
    dd_sp_ring <- d_ind_ring[,.(ring=.N),by = .(ESPECE)]

    dd_sp_all <- d[,.(N = .N),by = .(ESPECE)]
    dd_sp_autre <- data.frame(ESPECE = c("TURPIL","COCTES","DENMAJ","DENMIN","TURVIS","PICPIC","GARGLA","STUVUL","CARMEA"))
    dd_sp_all <- merge(dd_sp_all,dd_sp_autre,all=TRUE)
    dd_sp_all <- dd_sp_all[,.(ESPECE)]



    dd_sp <- merge(dd_sp_ring,dd_sp,by="ESPECE",all=TRUE)

    dd_sp <- merge(dd_sp,dd_sp_all,by="ESPECE",all=TRUE)


    dd_sp[is.na(dd_sp)] <- 0


    setnames(dd_sp,"ESPECE","sp_code_crbpo")

    bb <- fread("library/taille_bague_fr.csv",encoding="Latin-1")
    bb_sp <- merge(bb,dd_sp,by="sp_code_crbpo")
    bb_sp
    bb_sp[sp_code_crbpo == "PICPIC",diam := "6/7"]
    setnames(bb_sp,c("sp_code_crbpo","letter","material"),c("code","pref","metal"))
    bb_sp <- bb_sp[,.(code,update,french_name,group,diam,pref,metal,ring,JdP,IdF,Fr)]

    bb_sp[bb_sp == 0] <- ""
    ##   bb_sp


    fwrite(bb_sp,"library/taille_bague_spol_mangeoire.csv")


    if(write_pdf) {
        library(reporter)
        library(dplyr)

        tbl <-   titles(create_table(bb_sp),"Taille bague spol mangeoire")

        rpt <- create_report("library/taille_bague_spol_mangeoire.pdf", output_type = "PDF") %>%
            add_content( tbl) %>% options_fixed(font_size = 8)


        write_report(rpt)
    }


    if (output) return(bb_sp)


}
