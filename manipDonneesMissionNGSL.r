## #####
##
## Lectures des données du relevé MPO NGSL
##
## Par Mathieu Desgagnés, novembre 2017, basé sur les travaux de Hugo Bourdage et les scripts SAS de "PACES"
##
## Entrée: NGSL_Set.csv, NGSL_CAPT.csv, NGSL_Cbio.csv, stratum.sas7bdat
##         à l'adresse: S:\Releves_Poissons_de_Fond_et_Crevette\Donnees_PACES
## Sortie: lectureReleveNgsl.RData
##
## #####

require('sas7bdat')                     #vignette('sas7bdat')
require('lubridate')                    #fonction en lien avec la date (pour séparer année, mois et jour)

lireReleveNgsl <- function(dirInput='', dirOutput=''){
    ## lit les données du relevé et les enregistre
    ## objets créé: stratum.rn, trait.rn, capt.rn, capt.trait.rn = 'capture pour chaque trait', long.trait.rn = 'taille des individus pour chaque trait'
    print(paste0(dirInput,'NGSL_Set.csv'))
    trait <- read.csv2(paste0(dirInput,'NGSL_Set.csv'), as.is=TRUE, dec='.')  #produit par Denis Bernier, caractéristique des traits
    trait.rn <- trait[trait$Resultat %in% c(1,2),] #traits 3,4 et 5 sont considérés comme non-réussi
    names(trait.rn) <- c('source','noReleve','nbpc','noStation','annee','dateDeb','typeTrait','opano','profMoy','noStrate','engin','resultat','heureMoy','minuteMoy',
                         'latitudeDeb','longitudeDeb','distanceVit','distanceCor','distancePos','ouvertureHori','tFond','saliniteFond','oxygene','cZonPec')
    trait.rn$aire <- trait.rn$distanceVit*trait.rn$ouvertureHori #aire balayé
    trait.rn$aire <- trait.rn$aire/mean(trait.rn$aire) #aire balayé par chaque trait, normalisée
    trait.rn$X <- -floor(trait.rn$longitudeDeb/100)-trait.rn$longitudeDeb%%100/60 #longitude en décimal
    trait.rn$Y <- floor(trait.rn$latitudeDeb/100)+trait.rn$latitudeDeb%%100/60 #latitude en décimal
    trait.rn$date_deb <- as.Date(trait.rn$dateDeb, origin='1960-01-01')
    trait.rn$mois <- month(trait.rn$dateDeb)
    trait.rn$jour <- day(trait.rn$dateDeb)
    print(paste0(dirInput,'NGSL_CAPT.csv'))
    capt.init <- read.csv2(paste0(dirInput,'NGSL_CAPT.csv'), as.is=TRUE, dec='.')  #produit par Denis Bernier, capture par trait et espece
    names(capt.init) <- c('source','noReleve','nbpc','noStation','espece','categ','pdsCapt','pdsEch','nbCapt','nbEch','tCar','pdsCaptCorr','pdsEchCorr','nbCaptCorr','nbEchCorr')
    print(paste0(dirInput,'NGSL_Cbio.csv'))
    dfl.init <- read.csv2(paste0(dirInput,'NGSL_Cbio.csv'), as.is=TRUE, dec='.')  #produit par Denis Bernier, caractéristiques biologiques des captures
    names(dfl.init) <- c('source','noReleve','nbpc','noStation','espece','categ','noSpecimen','typeEch','longueur','longP','sexe','maturite','pdsTot','age','etat','rayons','tCar',
                         'noEtiq','noOriSp','nb','nbCor')
    capt.rn <- subset(capt.init, espece==893) #flétan altantique
    long.rn <- subset(dfl.init, espece==893)  #flétan altantique
    capt.trait.rn <- merge(trait.rn, capt.rn, by.x=c('source','noReleve','nbpc','noStation'), by.y=c('source','noReleve','nbpc','noStation'), all=TRUE)
    capt.trait.rn$pdsFletan <- capt.trait.rn$pdsCaptCor #si le poids vient d'un bateau avant le Teleost
    capt.trait.rn$pdsFletan[is.na(capt.trait.rn$pdsFletan)] <- capt.trait.rn$pdsCapt[is.na(capt.trait.rn$pdsFletan)] #si le poids vient du Teleost
    capt.trait.rn$pdsFletan[is.na(capt.trait.rn$pdsFletan)] <- 0 #zero sinon
    capt.trait.rn$nbFletan <- capt.trait.rn$nbCaptCor        #si le nb de flétan vient d'un bateau avant le Teleost
    capt.trait.rn$nbFletan[is.na(capt.trait.rn$nbFletan)] <- capt.trait.rn$nbCapt[is.na(capt.trait.rn$nbFletan)] #si le nb de flétan vient du Teleost
    capt.trait.rn$nbFletan[is.na(capt.trait.rn$nbFletan)] <- 0 #zero sinon
    long.trait.rn <- merge(trait.rn, long.rn, by.x=c('source','noReleve','nbpc','noStation'), by.y=c('source','noReleve','nbpc','noStation'), all=TRUE)
    long.trait.rn$longueur <- long.trait.rn$longueur/10 #en centimetre
    print(paste0(dirInput,'stratum.sas7bdat'))
    stratum.rn <- read.sas7bdat(paste0(dirInput,'stratum.sas7bdat')) #pas besion de mettre à jour
    names(stratum.rn) <- c('noStrate','surfaceKm2')
    save(trait.rn,capt.rn,capt.trait.rn,long.trait.rn,stratum.rn, file=paste0(dirOutput,'lectureReleveNgsl.RData'))
}
if(FALSE){
    lireReleveNgsl(dirInput='S:/Releves_Poissons_de_Fond_et_Crevette/Donnees_PACES/', dirOutput='')
}else{
    load('lectureReleveNgsl.RData', verbose=TRUE)
}

lireReleveNgslAutre <- function(dirInput='', dirOutput='', type='LH'){
    ## lire les relevés non convertis en "NGSL_xxx.csv"
    ## relevés disponibles: AN, GA, LH, TE
    print(paste0(dirInput,type,'_Set.csv'))
    trait <- read.csv2(paste0(dirInput,type,'_Set.csv'), as.is=TRUE, dec='.', na.string='. ')  #produit par Denis Bernier
    trait.rn <- trait[trait$resultat %in% c(1,2),]
    names(trait.rn) <- c('source','noReleve','nbpc','noStation','annee','dateDeb','typeTrait','opano','profMoy','noStrate','engin','resultat','heureMoy','minuteMoy',
                         'latitudeDeb','longitudeDeb','distanceVit','distanceCor','distancePos','ouvertureHori','tFond','saliniteFond','oxygene','cZonPec')
    trait.rn$aire <- trait.rn$distanceVit*trait.rn$ouvertureHori #aire balayé
    trait.rn$aire <- trait.rn$aire/mean(trait.rn$aire) #aire balayé par chaque trait, normalisée
    trait.rn$X <- -floor(trait.rn$longitudeDeb/100)-trait.rn$longitudeDeb%%100/60 #longitude en décimal
    trait.rn$Y <- floor(trait.rn$latitudeDeb/100)+trait.rn$latitudeDeb%%100/60 #latitude en décimal
    trait.rn$date_deb <- as.Date(trait.rn$dateDeb, origin='1960-01-01')
    trait.rn$mois <- month(trait.rn$dateDeb)
    trait.rn$jour <- day(trait.rn$dateDeb)
    print(paste0(dirInput,type,'_CAPT.csv'))
    capt.init <- read.csv2(paste0(dirInput,type,'_CAPT.csv'), as.is=TRUE, dec='.', na.string='. ')  #produit par Denis Bernier
    names(capt.init) <- c('source','noReleve','nbpc','noStation','espece','categ','pdsCapt','pdsEch','nbCapt','nbEch','tCar')
    print(paste0(dirInput,'NGSL_Cbio.csv'))
    dfl.init <- read.csv2(paste0(dirInput,type,'_Cbio.csv'), as.is=TRUE, dec='.', na.string='. ')  #produit par Denis Bernier
    names(dfl.init) <- c('source','noReleve','nbpc','noStation','espece','categ','noSpecimen','typeEch','longueur','longP','sexe','maturite','pdsTot','age','etat','rayons','tCar',
                         'noEtiq','noOriSp')
    capt.rn <- subset(capt.init, espece==893) #flétan altantique
    long.rn <- subset(dfl.init, espece==893)  #flétan altantique
    capt.trait.rn <- merge(trait.rn, capt.rn, by.x=c('source','noReleve','nbpc','noStation'), by.y=c('source','noReleve','nbpc','noStation'), all=TRUE)
    capt.trait.rn$pdsFletan <- capt.trait.rn$pdsCapt
    capt.trait.rn$pdsFletan[is.na(capt.trait.rn$pdsFletan)] <- 0 #zero sinon
    capt.trait.rn$nbFletan <- capt.trait.rn$nbCapt
    capt.trait.rn$nbFletan[is.na(capt.trait.rn$nbFletan)] <- 0 #zero sinon
    long.trait.rn <- merge(trait.rn, long.rn, by.x=c('source','noReleve','nbpc','noStation'), by.y=c('source','noReleve','nbpc','noStation'), all=TRUE)
    long.trait.rn$longueur <- long.trait.rn$longueur/10 #en centimetre
    print(paste0(dirInput,'stratum.sas7bdat'))
    stratum.rn <- read.sas7bdat(paste0(dirInput,'stratum.sas7bdat')) #pas besion de mettre à jour
    if(type=='LH'){
        trait.rn.lh <- trait.rn; capt.rn.lh <- capt.rn; capt.trait.rn.lh <- capt.trait.rn; long.trait.rn.lh <- long.trait.rn; stratum.rn <- stratum.rn;
        save(trait.rn.lh,capt.rn.lh,capt.trait.rn.lh,long.trait.rn.lh,stratum.rn, file=paste0(dirOutput,'lectureReleveLH.RData'))
    }else{
        if(type=='GA'){
            trait.rn.ga <- trait.rn; capt.rn.ga <- capt.rn; capt.trait.rn.ga <- capt.trait.rn; long.trait.rn.ga <- long.trait.rn; stratum.rn <- stratum.rn;
            save(trait.rn.ga,capt.rn.ga,capt.trait.rn.ga,long.trait.rn.ga,stratum.rn, file=paste0(dirOutput,'lectureReleveGA.RData'))
        }else{                          #a mettre à jour si utilise les autres
            trait.rn.xx <- trait.rn; capt.rn.xx <- capt.rn; capt.trait.rn.xx <- capt.trait.rn; long.trait.rn.xx <- long.trait.rn; stratum.rn <- stratum.rn;
            save(trait.rn.xx,capt.rn.xx,capt.trait.rn.xx,long.trait.rn.xx,stratum.rn, file=paste0(dirOutput,'lectureReleveXX.RData'))
        }
    }
}
if(FALSE){
    lireReleveNgsl(dirInput='S:/Releves_Poissons_de_Fond_et_Crevette/Donnees_PACES/', dirOutput='')
}else{
    load('lectureReleveLH.RData', verbose=TRUE)
}

calculerIndiceAbondance <- function(annee, stratum, trait, capt, strates.init=NULL){
    ## trois étapes: 1)calcul des pue et nue 2)modèle multiplicatif 3)calcul des pue et nue total
    ## annee= vercteur des années disponible/à considérer (ex.   1990:2015 )
    ## stratum= data.frame: nom des strates et leur surface ('stratum.rn')
    ## trait= trait ('trait.rn')
    ## capt= capture ('capt.rn')
    ## strates.init= si on utilise un sous-ensemble des strates, fournir un vecteur des strates à utiliser
    ##
    ## nombre d'unités chalutables
    strates <- as.data.frame(cbind(noStrate=c(401,402,403,404,405,406,407,408,409,410,411,412,413,414,801,802,803,804,805,806,807,808,809,810,811,
                                       812,813,814,815,816,817,818,819,820,821,822,823,824,827,828,829,830,831,832,833,835,836,837,838,839,
                                       840,841,851,852,854,855),
                                   nbUnit = c(23177.13048,38628.69247,50582.11636,33672.81975,62826.66218,109618.6685,99268.75271,116178.0493,38628.69247,
                                       77257.80993,79007.09148,54517.57484,31048.89743,16471.97622,51602.53059,58161.91139,296494.722,105828.5585,
                                       244892.6164,90376.99652,100726.4873,103204.6362,65742.13142,32506.63206,63992.84988,197517.5162,168217.9003,
                                       43730.76364,187313.7988,213843.7189,154952.9402,117893.7562,61223.1541,57724.59101,54080.25445,137897.8702,
                                       23614.45087,35567.87476,137314.7763,103496.1831,114428.7678,81485.24033,51165.21021,168363.6738,23760.22433,
                                       112242.5908,133816.2132,113408.7785,143583.0352,186584.9315,32506.63206,34693.23399,19398.1851,18167.01559,
                                       19741.10973,39454.05076)))
    if(!is.null(strates.init)){    #si on choisit un sous-ensemble de strates
        strates <- subset(strates, noStrate %in% strates.init)
    }
    ## calcul des pue et nue
    pue.strate <- array(dim=c(length(annee),length(strates$noStrate),3), dimnames=list(annee=annee, strate=strates$noStrate, c('moy','var','n')))
    pue.strate[,,'n'] <- 0
    nue.strate <- array(dim=c(length(annee),length(strates$noStrate),3), dimnames=list(annee=annee, strate=strates$noStrate, c('moy','var','n')))
    nue.strate[,,'n'] <- 0
    for(i.an in seq_along(annee)){
        trait.temp <- trait[trait$annee==annee[i.an] & trait$noStrate %in% strates$noStrate,] #sait pas pkoi, problème si utilise subset()
        noRel.temp <- unique(trait.temp[,c('noReleve','nbpc')])
        dat <- NULL
        for(i in 1:nrow(noRel.temp)){
            dat <- rbind(dat, subset(capt, noReleve%in%noRel.temp[i,1]&nbpc%in%noRel.temp[i,2]))
        }
        temp <- merge(dat, trait.temp, by.x=c('source','noReleve','nbpc','noStation'), by.y=c('source','noReleve','nbpc','noStation'), all.x=TRUE, all.y=TRUE)
        temp$pdsCaptCor[is.na(temp$pdsCaptCor)] <- 0
        temp$nbCaptCor[is.na(temp$nbCaptCor)] <- 0
        dat <- subset(temp, typeTrait%in%c(1,2))
        temp <- tapply(dat$noStrate, dat$noStrate, length)
        pue.strate[i.an, names(temp), 'n'] <- temp
        nue.strate[i.an, names(temp), 'n'] <- temp
        ybar.temp <- tapply(dat$nbCaptCor * 0.75 / dat$distanceVit * 16.94 / dat$ouvertureHori, list(dat$noStrate), mean)
        nue.strate[i.an, names(ybar.temp), 'moy'] <- ybar.temp
        s2.temp <- tapply(dat$nbCaptCor * 0.75 / dat$distanceVit * 16.94 / dat$ouvertureHori, list(dat$noStrate), var)
        nue.strate[i.an, names(s2.temp), 'var'] <- s2.temp
        ybar.temp <- tapply(dat$pdsCaptCor * 0.75 / dat$distanceVit * 16.94 / dat$ouvertureHori, list(dat$noStrate), mean)
        pue.strate[i.an, names(ybar.temp), 'moy'] <- ybar.temp
        s2.temp <- tapply(dat$pdsCaptCor * 0.75 / dat$distanceVit * 16.94 / dat$ouvertureHori, list(dat$noStrate), var)
        pue.strate[i.an, names(s2.temp), 'var'] <- s2.temp
    }
    ##
    ## faire modèle multiplicatif
    nbAnneeDeRecul <- 3                     #pour un total de 4 ans de modèle
    formule <- as.formula(moy~-1+annee+strate)
    for(i.an in annee){
        ## pour les 3 premières années de la série, les années 1 à 4 années sont utilisée pour le modèle
        ## pour les années suivantes, l'année en cours et les trois précédentes sont utilisées pour le modèle
        ## les strates 85x commencent en 2008, mais il y a toujours 3 stations ou plus en 2008:2010 pour ces strates
        if(i.an %in% min(annee):2010){
            lesquelles.strates <- as.character(strates[which(!strates$noStrate%in%c('851','852','854','855')),'noStrate'])
        }else{
            lesquelles.strates <- as.character(strates[,'noStrate'])
        }
        if(any(pue.strate[as.character(i.an),lesquelles.strates,'n']<2)){ #valides si des strates ont moins de 2 échantillons pour l'année donnée
            if(i.an %in% min(annee):(min(annee)+nbAnneeDeRecul-1)){
                lesquelles.annees <- as.character(min(annee):(min(annee)+nbAnneeDeRecul))
            }else{
                lesquelles.annees <- as.character((i.an-nbAnneeDeRecul):i.an)
            }
            newdata <- expand.grid(strate=as.factor(lesquelles.strates),
                                   annee=as.factor(lesquelles.annees))
            ## pue
            data.temp <- as.data.frame(as.table(pue.strate[lesquelles.annees, lesquelles.strates, 'moy']))
            names(data.temp)[3] <- 'moy'
            model <- lm(formule, data=data.temp)
            newdata <- cbind(newdata, pred=predict(model, newdata=newdata))
            ## s'il n'y a qu'un échantillon
            unEch <- dimnames(pue.strate[,lesquelles.strates,])[[2]][which(pue.strate[as.character(i.an),lesquelles.strates,'n']==1)]
            if(length(unEch)>0) {
                temp <- (unlist(subset(newdata, annee==as.character(i.an) & strate%in%unEch, pred)) +
                         pue.strate[as.character(i.an), unEch, 'moy']) / 2
                pue.strate[as.character(i.an), unEch, 'moy'] <- mapply(FUN=function(x){max(x,0)}, temp)
            }
            ## s'il n'y a aucun échantillon
            aucunEch <- dimnames(pue.strate[,lesquelles.strates,])[[2]][which(pue.strate[as.character(i.an),lesquelles.strates,'n']==0)]
            if(length(aucunEch)>0){
                temp <- (unlist(subset(newdata, annee==as.character(i.an) & strate%in%aucunEch, pred)))
                pue.strate[as.character(i.an), aucunEch, 'moy'] <- mapply(FUN=function(x){max(x,0)}, temp)
            }
            ## nue
            data.temp <- as.data.frame(as.table(nue.strate[lesquelles.annees, lesquelles.strates,'moy']))
            names(data.temp)[3] <- 'moy'
            model <- lm(formule, data=data.temp)
            newdata <- cbind(newdata, pred=predict(model, newdata=newdata))
            ## s'il n'y a qu'une observation
            unEch <- dimnames(nue.strate[,lesquelles.strates,])[[2]][which(nue.strate[as.character(i.an),lesquelles.strates,'n']==1)]
            if(length(unEch)>0) {
                temp <- (unlist(subset(newdata, annee==as.character(i.an) & strate%in%unEch, pred)) +
                         nue.strate[as.character(i.an), unEch, 'moy']) / 2
                nue.strate[as.character(i.an), unEch, 'moy'] <- mapply(FUN=function(x){max(x,0)}, temp)
            }
            ## s'il n'y a aucune observation
            aucunEch <- dimnames(nue.strate)[[2]][which(nue.strate[as.character(i.an),lesquelles.strates,'n']==0)]
            if(length(aucunEch)>0){
                temp <- (unlist(subset(newdata, annee==as.character(i.an) & strate%in%aucunEch, pred)))
                nue.strate[as.character(i.an), aucunEch, 'moy'] <- mapply(FUN=function(x){max(x,0)}, temp)
            }
        }
    }
    ##
    ## calculer total après modèle multiplicatif
    ybarTot <- list()
    ybarTot[['pue']] <- array(dim=c(length(annee), 6), dimnames=list(annee=annee, c('moy','var','ICmin','ICmax','df','tStudent')))
    ybarTot[['nue']] <- array(dim=c(length(annee), 6), dimnames=list(annee=annee, c('moy','var','ICmin','ICmax','df','tStudent')))
    for(i.an in seq_along(annee)){
        quellesStrates <- as.character(strates$noStrate)
        if(annee[i.an]<2008) quellesStrates <- as.character(strates[which(!strates$noStrate%in%c('851','852','854','855')),'noStrate'])
        N <- strates[strates[,'noStrate']%in%quellesStrates,'nbUnit']; names(N) <- quellesStrates #nombre d'unité échantillonnables par strates
        Ntot <- sum(N)                                  #total des unités échantilllonnables dans ngsl
        f <- nue.strate[i.an,quellesStrates,'n']/N #proportion d'unités échantillonnés sur la strate
        W <- N/Ntot                                     #poids de la strate dans le ngsl
        ## pue
        ybarTot[['pue']][i.an,'moy'] <- weighted.mean(pue.strate[i.an,quellesStrates,'moy'], W)
        ybarTot[['pue']][i.an,'var'] <- sum((W[quellesStrates]^2 * pue.strate[i.an,quellesStrates,'var'] * (1-f)) /
                                            pue.strate[i.an,quellesStrates,'n'], na.rm=TRUE)
        a <- N*(N-pue.strate[i.an,quellesStrates,'n'])/pue.strate[i.an,quellesStrates,'n']
        d <- (sum(a*pue.strate[i.an,quellesStrates,'var'],na.rm=TRUE))^2 /
            sum((a*pue.strate[i.an,quellesStrates,'var'])^2/(pue.strate[i.an,quellesStrates,'n']-1),na.rm=TRUE)
        ybarTot[['pue']][i.an,c('df')] <- d
        ybarTot[['pue']][i.an,c('tStudent')] <- qt(0.975, df=d)
        ybarTot[['pue']][i.an,c('ICmin','ICmax')] <- ybarTot[['pue']][i.an,'moy'] + c(-1,1) * qt(0.975, df=d)*
            sqrt(ybarTot[['pue']][i.an,'var'])
        ## nue
        ybarTot[['nue']][i.an,'moy'] <- weighted.mean(nue.strate[i.an,quellesStrates,'moy'], W)
        ybarTot[['nue']][i.an,'var'] <- sum((W[quellesStrates]^2 * nue.strate[i.an,quellesStrates,'var'] * (1-f)) /
                                            nue.strate[i.an,quellesStrates,'n'], na.rm=TRUE)
        a <- N*(N-nue.strate[i.an,quellesStrates,'n'])/nue.strate[i.an,quellesStrates,'n']
        d <- (sum(a*nue.strate[i.an,quellesStrates,'var'],na.rm=TRUE))^2 /
            sum((a*nue.strate[i.an,quellesStrates,'var'])^2/(nue.strate[i.an,quellesStrates,'n']-1),na.rm=TRUE)
        ybarTot[['nue']][i.an,c('df')] <- d
        ybarTot[['nue']][i.an,c('tStudent')] <- qt(0.975, df=d)
        ybarTot[['nue']][i.an,c('ICmin','ICmax')] <- ybarTot[['nue']][i.an,'moy'] + c(-1,1) * qt(0.975, df=d)*
            sqrt(ybarTot[['nue']][i.an,'var'])
    }
    return(ybarTot)
}
ybarTot.rn <- calculerIndiceAbondance(annee=1990:2016, stratum=stratum.rn, trait=trait.rn, capt=capt.rn)
## par(mfrow=c(1,2))
## plot(ybarTot.rn$pue[,'moy'], type='l')
## plot(ybarTot.rn$nue[,'moy'], type='l')

