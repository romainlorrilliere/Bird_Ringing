library(data.table)
library(ggplot2)

d <- fread("data/data_stoc_rieffel.csv",encoding="Latin-1")


d_jour_ois  <- d[,.(nb = .N),by=.(BAGUE,DATE,nom_sp)]

fwrite(d_jour_ois,"data/STOC_rieffel_ois_jour.csv",sep=";",dec=",")



d_jour_sp  <- d[,.(nb = .N),by=.(DATE,nom_sp)]
fwrite(d_jour_sp,"data/STOC_rieffel_sp_jour.csv",sep=";",dec=",")



d_sp <- d_jour_sp[,.(nb = .N), by= DATE]
d_sp[,num_session := 1:.N]
md <- glm(nb ~ num_session, data = d_sp, family = "poisson")
summary(md)

d_sp[, DATE := as.Date(DATE)]
d_sp[,DATE_num := as.numeric(DATE)]

md <- glm(nb ~ DATE_num, data = d_sp, family = "poisson")
summary(md)


d_sex <- d[SEXE %in% c("M","F") & ACTION %in% c("B","C"),.(nb = .N),by = .(DATE,ACTION,SEXE)]
d_all <- expand.grid(SEXE = c("M","F"),ACTION = c("B","C"),DATE = unique(d_sex[,DATE]))
d_sex <- merge(d_sex,d_all,all=TRUE)
d_sex[is.na(nb),nb := 0]


gg <- ggplot(data=d_sex,aes(x=DATE,y=nb,colour=SEXE,group=SEXE)) + geom_point() + geom_line() + facet_grid(ACTION~.)
gg <- gg + labs(title = "Variation de nombre de captures en fonction du sexe et de l'action",x="Date",y="Nombre de capture lors de la session")
gg
ggsave("output/STOC_nb_sexe.png",gg)

d_sex[,DATEfact := as.factor(DATE)]
md <- glm(nb ~ ACTION*SEXE + DATEfact, data = d_sex, family = "poisson")

print(summary(md))






d_LT <- d[,.(LTmean = median(LT,na.rm=TRUE)),by=.(BAGUE)]
d <- merge(d,d_LT)


d_LT <- d[ACTION == "B" & SEXE %in% c("M","F")& !is.na(LTmean),.(YEAR,nom_sp,SEXE,LTmean)]
d_LT[,occ_year := .N,by = .(nom_sp,YEAR)]

d_LT[,occ_year_med := median(occ_year),by = .(nom_sp)]

gg <- ggplot(d_LT[occ_year_med > 3,],aes(x=YEAR,y=LTmean,colour=SEXE,group=SEXE,fill=SEXE)) + facet_wrap(.~nom_sp,scales="free_y")
gg <- gg + geom_point()
##gg <- gg + theme(legend.position = "none")
gg <- gg + geom_smooth(method="lm")
gg
ggsave("output/STOC_LT.png",gg)

gg <- ggplot(d_LT[nom_sp == "Grimpereau des jardins",],aes(x=YEAR,y=LTmean,colour=SEXE,group=SEXE)) + facet_wrap(.~nom_sp,scales="free_y")
gg <- gg + geom_point()
#gg <- gg + theme(legend.position = "none")
gg <- gg + geom_smooth(method="lm")
gg


gg <- ggplot(d_LT[occ_year_med > 3,],aes(x=LTmean,fill=SEXE))  + facet_wrap(.~nom_sp,scales="free") + geom_histogram(alpha=0.5)
gg



md <- glm(LTmean ~ SEXE*nom_sp + YEAR*nom_sp, data= d_LT[occ_year_med > 3,])
print(summary(md))
