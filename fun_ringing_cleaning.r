


### realise un affichage ecran et une sauvegarde du log
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title catlog
##' @param txt : CHAR chaine de caractère à ecrire
##' @param fileLog : CHAR nom du fichier
##' @return
##' @author Romain Lorrilliere
catlog <- function(txt,fileLog=paste0("log_",as.Date(Sys.time()),".txt"),pathLog="log/",append = TRUE) {
    cat(txt)
    dir.create(file.path(pathLog), showWarnings = FALSE)
    cat(txt,file=paste0("log/",fileLog),append = append)
}






 add_age <- function(d,fileLog=paste0("log_",as.Date(Sys.time()),".txt")) {
### age managing
    catlog(c("\n====================================\n\n - Checking: Age\n------------------------------------\n"),fileLog)
                                        # d$AGE correcting
    d$AGE <- ifelse(is.na(d$AGE),"VOL",ifelse(d$AGE %in% c("?","??","???","VOL"),"VOL",d$AGE))
                                        # line.juv line numbers of juveniles
    line.juv <-  union(setdiff(grep("1",d$AGE),grep("+",d$AGE,fixed="TRUE")),grep("PUL",d$AGE))
                                        # bague_year.juv vector of identifiant bague year of juveniles
    bague_year.juv <- unique(paste(d$BAGUE,d$YEAR)[line.juv])
                                        # line.ad line number of adules
    line.ad <-  union(grep("+",d$AGE,fixed=TRUE),grep("[2-9]",d$AGE,perl =TRUE))
                                        # bague_year.ad vector of identifiant bague year of adultes
    bague_year.ad <- unique(paste(d$BAGUE,d$YEAR)[line.ad])

    catlog(c("    -- add column AGE_stage (JUV, AD, VOL)\n"),fileLog)
    d$AGE_stage <- "VOL"
    d$AGE_stage[paste(d$BAGUE,d$YEAR) %in%  bague_year.juv] <- "JUV"
    d$AGE_stage[paste(d$BAGUE,d$YEAR) %in%  bague_year.ad] <- "AD"

    catlog(c("    -- add column AGE_first (JUV, AD, VOL)\n"),fileLog)
    ringing_year <- aggregate(d$YEAR,by=list(d$BAGUE),min)
    vec_ringing_year <- ringing_year[,2]
    names(vec_ringing_year) <- ringing_year[,1]
    first_catch_date<- aggregate(strptime(paste(d$DATE,d$HEURE),"%Y-%m-%d %H:%M"),by=list(d$BAGUE),min)
    colnames(first_catch_date) <- c("BAGUE","DATE_HEURE")
    table_first_age <- subset(d,paste(BAGUE,strptime(paste(d$DATE,d$HEURE),"%Y-%m-%d %H:%M")) %in% paste(first_catch_date$BAGUE,first_catch_date$DATE_HEURE),
                              select=c("BAGUE","AGE","DATE","HEURE"))
    vec_first_age <- table_first_age$AGE
    names(vec_first_age) <- table_first_age$BAGUE

    vec_first_age <- ifelse(vec_first_age=="VOL","VOL",ifelse(vec_first_age %in% c("PUL","1A","1A?"),"JUV","AD"))

    ## AGE_fist  age construit a partir de la premiere capture
    d$AGE_first <- ifelse(d$YEAR == vec_ringing_year[d$BAGUE],vec_first_age[d$BAGUE],"AD")


     return(d)
}


add_sexe <- function(d) {

    catlog(c("\n====================================\n\n - Checking: Sex\n------------------------------------\n"))


    d$SEXE <- ifelse(is.na(d$SEXE),"?",d$SEXE)
    d$SEXE_FIRST <- NA
    d$SEXE_FIRST_INCERTITUDE <- FALSE

   # d.S_ADfm <- d[-(grep("?",d$SEXE,fixed=TRUE)),]
    d.S_ADfm <- subset(d,AGE_first=="AD")

    first_catch_date<- aggregate(strptime(paste(d.S_ADfm$DATE,d.S_ADfm$HEURE),"%Y-%m-%d %H:%M"),by=list(d.S_ADfm$BAGUE),min)
    colnames(first_catch_date) <- c("BAGUE","DATE_HEURE")
    table_first_sex_ad <- subset(d.S_ADfm,paste(BAGUE,strptime(paste(d.S_ADfm$DATE,d.S_ADfm$HEURE),"%Y-%m-%d %H:%M")) %in% paste(first_catch_date$BAGUE,first_catch_date$DATE_HEURE),
                              select=c("BAGUE","SEXE","DATE","HEURE"))

    table_first_sex_ad <- subset(table_first_sex_ad,select=c("BAGUE","SEXE"))
    colnames(table_first_sex_ad)[2] <- "SEXE_FIRST_VOL"

    d <- merge(d,table_first_sex_ad,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_FIRST_VOL == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_FIRST_VOL,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_FIRST_VOL")]

    d.SJ <- subset(d,is.na(SEXE_FIRST))


    d.SJvol <- subset(d.SJ,AGE_first=="VOL")


    first_catch_date<- aggregate(strptime(paste(d.SJvol$DATE,d.SJvol$HEURE),"%Y-%m-%d %H:%M"),by=list(d.SJvol$BAGUE),min)
    colnames(first_catch_date) <- c("BAGUE","DATE_HEURE")
    table_first_sex_ad <- subset(d.SJvol,paste(BAGUE,strptime(paste(d.SJvol$DATE,d.SJvol$HEURE),"%Y-%m-%d %H:%M")) %in% paste(first_catch_date$BAGUE,first_catch_date$DATE_HEURE),
                              select=c("BAGUE","SEXE","DATE","HEURE"))

    table_first_sex_ad <- subset(table_first_sex_ad,select=c("BAGUE","SEXE"))
    colnames(table_first_sex_ad)[2] <- "SEXE_FIRST_VOL"

    d <- merge(d,table_first_sex_ad,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_FIRST_VOL,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_FIRST_VOL == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_FIRST_VOL,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_FIRST_VOL")]


    d.SJ <- subset(d,is.na(SEXE_FIRST))
    d.SJ_fm <- d.SJ[-(grep("?",d.SJ$SEXE,fixed=TRUE)),]

    u.SJ_fm <- unique(subset(d.SJ_fm,select=c("BAGUE","SEXE")))
    table.SJ_fm <- table(u.SJ_fm$BAGUE)

    table.SJ <- data.frame(BAGUE=names(table.SJ_fm)[table.SJ_fm>1],SEXE_JUV = "?")

    u.SJ_fm <- subset(u.SJ_fm,!(BAGUE %in% names(table.SJ_fm)[table.SJ_fm>1]), select=c("BAGUE","SEXE"))
    colnames(u.SJ_fm)[2] <- "SEXE_JUV"

    table.SJ <- rbind(table.SJ,u.SJ_fm)

    d <- merge(d,table.SJ,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_JUV,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_JUV,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_JUV == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_JUV,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_JUV")]




   d.SJ <- subset(d,is.na(SEXE_FIRST) & SEXE != "?")

    u.SJ_fm <- unique(subset(d.SJ_fm,select=c("BAGUE","SEXE")))
    table.SJ_fm <- table(u.SJ_fm$BAGUE)

    u.SJ_fm <- subset(u.SJ_fm,!(BAGUE %in% names(table.SJ_fm)[table.SJ_fm>1]), select=c("BAGUE","SEXE"))
    colnames(u.SJ_fm)[2] <- "SEXE_JUV"

    if(length(names(table.SJ_fm)[table.SJ_fm>1])>0) {
        table.SJ <- data.frame(BAGUE=names(table.SJ_fm)[table.SJ_fm>1],SEXE_JUV = "?")
           table.SJ <- rbind(table.SJ,u.SJ_fm)

    } else {
          table.SJ <- u.SJ_fm
        }


    d <- merge(d,table.SJ,by="BAGUE",all=TRUE)

    d$SEXE_FIRST[grep("M",d$SEXE_JUV,fixed=TRUE)] <- "M"
    d$SEXE_FIRST[grep("F",d$SEXE_JUV,fixed=TRUE)] <- "F"
    d$SEXE_FIRST[d$SEXE_JUV == "?"] <- "U"
    d$SEXE_FIRST_INCERTITUDE[grep("?",d$SEXE_JUV,fixed=TRUE)] <- TRUE

    d <- d[,which(colnames(d)!="SEXE_JUV")]

    d$SEXE_FIRST_INCERTITUDE[is.na(d$SEXE_FIRST)] <- TRUE
    d$SEXE_FIRST[is.na(d$SEXE_FIRST)] <- "U"


### SEXE_UNIQUE autre methode de correction du sex

   d.Sfm <- d[-(grep("?",d$SEXE,fixed=TRUE)),]
    t.contingency.Sfm <- table(d.Sfm$BAGUE,d.Sfm$SEXE)
    t.sex <- data.frame(BAGUE = rownames(t.contingency.Sfm),
                        NEWSEX = colnames(t.contingency.Sfm)[apply(t.contingency.Sfm,1,function(X) match(max(X),X))],
                        SEX.CONFIDENCE = apply(t.contingency.Sfm,1,max)/rowSums(t.contingency.Sfm),
                        NB.DATA.FOR.SEX = rowSums(t.contingency.Sfm),
                        SEX.CONFIDENCE.CAT = "OK",stringsAsFactors=FALSE)
    t.sex$SEX.CONFIDENCE.CAT[t.sex$SEX.CONFIDENCE < 1] <- "PROB"
    d.Si <- subset(d,d$BAGUE %nin% t.sex$BAGUE)
    d.Si <- d.Si[d.Si$SEXE != "?",]
    d.Si$SEXE <- substr(d.Si$SEXE,1,1)
    t.contingency.Si <- table(d.Si$BAGUE,d.Si$SEXE)
    t.sex <- rbind(t.sex,
                   data.frame(BAGUE = rownames(t.contingency.Si),
                              NEWSEX = colnames(t.contingency.Si)[apply(t.contingency.Si,1,function(X) match(max(X),X))],
                              SEX.CONFIDENCE = apply(t.contingency.Si,1,max)/rowSums(t.contingency.Si),
                              NB.DATA.FOR.SEX = rowSums(t.contingency.Si),
                              SEX.CONFIDENCE.CAT = "UNCERTAIN",stringsAsFactors=FALSE))
    t.sex$SEX.CONFIDENCE.CAT[t.sex$SEX.CONFIDENCE < 1 & t.sex$SEX.CONFIDENCE.CAT == "UNCERTAIN"] <- "VERY_UNCERTAIN"

     d.Su <- subset(d,d$BAGUE %nin% t.sex$BAGUE)
    t.contingency.Su <- table(d.Su$BAGUE,d.Su$SEXE)
    t.sex <- rbind(t.sex,
                   data.frame(BAGUE = rownames(t.contingency.Su),
                              NEWSEX = "U",
                              NB.DATA.FOR.SEX = rowSums(t.contingency.Su),
                              SEX.CONFIDENCE = 1,
                              SEX.CONFIDENCE.CAT = "UNKNOWN",stringsAsFactors=FALSE))

    d <- merge(d,t.sex,by="BAGUE")
    lse <- length(which(t.sex$SEX.CONFIDENCE<1))

    if ( lse == 0)
    {
        cat(" --> OK\n")
    } else {
        cat(" !!! WARNING MESSAGE:",
            lse,"induvidual(s) do(es) not have a single value of sex\n",
            cat(" ==> Check warning_sex.csv in output/ directory !!\n\n"))
        t.warning.sex <- subset(d,BAGUE %in% t.sex$BAGUE[which(t.sex$SEX.CONFIDENCE<1)])
        t.warning.sex <- t.warning.sex[order(t.warning.sex$BAGUE,t.warning.sex$DATE),]
        write.csv2(t.warning.sex,"output/warning_sex.csv",row.names=FALSE,na="",quote=FALSE)
    }


### news columns SEX.CONFIDENCE.CAT and GROUP
    cat("    -- add column SEX.CONFIDENCE\n")
    cat("    -- add column SEX.CONFIDENCE.CAT (UNKNOWN, PROB, CRED)\n")
                                        #   d <- merge(d,dsp,by="SP")
    cat("    -- add column SEXE2 U if SEX.CONFIDENCE<0.8 \n")
    d$SEXE2 <- ifelse(d$SEX.CONFIDENCE<.8,"U",d$SEXE)



    return(d)

}
