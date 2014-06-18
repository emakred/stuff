###############################################################################
##
###############################################################################
rm(list=ls(all=TRUE))
gc()
library(foreign)
library(MASS)
library(ggplot2)
library(slam)
library(Matrix)
library(leiv)

setwd('~/workspace/contributions')
source('src/utils.r')
reload()

print(load('~/workspace/crowdpac/data/cp_rc_out.rda'))
print(load('~/workspace/rcscale/data/joint_data_crowdpac.rda'))
# ADAM commented out next 2 lines
## load('/workspace/crowdpac/data/issue_positions.rda')
## iss.pos <- t(out) 

print(load('~/workspace/contributions/finished_runs/awm_79_14_current.rda'))

contributors <- awm.results$contributors
cands <- awm.results$cands
indiv <- contributors[,'indiv'] =='I'
nrc <- contributors[,'nrc']


contributions <- awm.results$contributions
nd <- gsub('nominee','',contributions[,'CandID'])
cycle <- as.numeric(substr(nd,nchar(nd)-3,nchar(nd)))
contributions <- contributions[cycle >= 2004,]


ctab <- as.data.frame(contributions[,c('amount','filer','CandID')])
ctab$amount <- as.integer(as.character(ctab[,'amount']))
ctab$amount[ctab$amount < 0] <- 0


##CONSTRUCT CONTRIB.MATRIX
ctab <- aggregate(amount ~ filer + CandID, data=ctab,sum)
ctab[as.numeric(ctab[,'amount']) > 300,'amount'] <- 300
contrib.matrix <- xtabs(amount~ filer + CandID,ctab,sparse=TRUE)

m <- match(cands[,'ICPSR'],colnames(contrib.matrix))
cands <- cands[!is.na(m),]
m <- match(cands[,'ICPSR'],colnames(contrib.matrix))
contrib.matrix <- contrib.matrix[,m]

##construct cm
m <- match(ctab[,'CandID'],cands[,'ICPSR'])
ucid <- cands[m,'ICPSR2']
ctab[,'CandID'] <- factor(ucid)
cm <- xtabs(amount ~ filer + CandID,ctab,sparse=TRUE)

mm <- match(colnames(cm),cands[,'ICPSR2'])
cm <- cm[,!is.na(mm)]
mm1 <- match(as.numeric(rownames(cm)),as.numeric(contributors[,'comid']))
contributors <- contributors[mm1,]
nrc <- nrc[mm1]
indiv <- indiv[mm1]


MM <- ceiling(cm / 1e10)
use2 <- rowSums(MM) > 1 & nrc == 0

cm <- cm[use2,]
contrib.matrix <- contrib.matrix[use2,]
contributors <- contributors[use2,]
indiv <- indiv[use2]


###############################################################################
##
###############################################################################
cands.in <- cands[!duplicated(cands[,'ICPSR2']),]
is.ip <- senout$leginfo$idealPoints
na.mat <- senout$leginfo$na.mat
is.ip[na.mat == 0] <- NA
# ADAM commented out next line
##is.ip <- t(senout$leginfo$init.issue.scores)


m <- match(as.numeric(as.character(cands.in[,'ICPSR2'])),as.numeric(rownames(is.ip)))
cands.w.ig <- is.ip[m,]
cands.w.ig[cands.w.ig ==0] <- NA
cands.in <- cands.in[!is.na(m),]
# ADAM commented out next line
## igscores <- cbind(cands.w.ig,dwnom= cands.in[,'cs.1'],cfscore=cands.in[,'oc1'])
m <- match(as.numeric(as.character(cands.in[,'ICPSR2'])),as.numeric(rownames(cands.w.ig)))
igscores <- cands.w.ig[m,]
igscores.new <- igscores


##CANDIATE ISSUE WEIGHTS
cand.iweights <- senout$leginfo$cand.iweights
m <- match(as.numeric(as.character(cands.in[,'ICPSR2'])),as.numeric(rownames(cand.iweights)))
cand.iweights <- cand.iweights[m,]
m1 <- match(colnames(igscores.new),colnames(cand.iweights))
cand.iweights <- cand.iweights[,m1]

# ERIN:  where is candPriorities generated first?
# ERIN commented out next 3 lines...not used in uncommented regions of this script
# print(load('~/workspace/crowdpac/data/candPriorities.rda'))
# m <- match(as.numeric(as.character(cands.in[,'ICPSR2'])),as.numeric(rownames(candPriorities)))
# candPriorities <- candPriorities[m,]

# ADAM commented out next 2 lines
## m <- match((as.character(cands.in[,'bonica_rid'])),(rownames(iss.pos)),incomparables=c('',NA))
## ips <- iss.pos[m,]

if(FALSE){
    ig.out <- cbind(cands.in[,c('Name','State','District','bonica_rid')],igscores.new)
    write.csv(ig.out,file='/workspace/crowdpac/data/issue_specific_vote_scores_tmp.csv')
}


# ERIN commented out next 8 lines, not used elsewhere in script
# cand.iweights.norm <- cand.iweights
# cand.iweights.norm[is.na(cand.iweights.norm)] <- 0
# cand.iweights.norm <- t(apply(cand.iweights.norm, 1,
#                             function(z){
#                                 out <- z / sum(z,na.rm=T)
#                                 print(sum(z,na.rm=T))
#                                 return(out)
#                             }))
# ERIN commented out, not used in the rest of the uncommented script
# cand.ips.start <- as.numeric(igscores[,1])


# ERIN commented out next 2 lines, not used elsewhere in script
# all.cwvect <- colMeans(cand.iweights.norm,na.rm=T)
# all.ipvect <- colMeans(igscores,na.rm=T)
MM <- ceiling(cm / 1e10)
nconts <- which(rowSums(MM) >= 4)
# ADAM commented out next line
##cm.in2 <- as.matrix(cm.in[nconts,])


cm.all <- cm[nconts,]
cand.cw.new <- matrix(NA,ncol(cm.all),ncol(cand.iweights))
colnames(cand.cw.new) <- colnames(cand.iweights)
cand.ip.new <- cand.cw.new

# ERIN commented while focusing on WEIGHTS
# m <- match(rownames(igscores),colnames(cm.all))
# ERIN commented while focusing on WEIGHTS
# cand.ip.new[m[!is.na(m)],] <- igscores[!is.na(m),]
m <- match(rownames(cand.iweights),colnames(cm.all))
cand.cw.new[m[!is.na(m)],] <- cand.iweights[!is.na(m),]

# ERIN commented while focusing on WEIGHTS
# holdout <- sample(1:nrow(cand.iweights),200)
# ERIN commented while focusing on WEIGHTS
cand.iweights.old <- cand.cw.old <- cand.cw.new
# ERIN commented while focusing on WEIGHTS
# igscores.old <- cand.ip.old <- cand.ip.new
# ADAM commented out next 2 lines
## cand.iweights[which(!is.na(m))[holdout],] <- NA
## igscores[which(!is.na(m))[holdout],] <- NA

##CONTRIBUTOR ESTIMATES
cont.ip.mat <- matrix(NA,nrow(cm.all),ncol(igscores))
colnames(cont.ip.mat) <- colnames(igscores)
cont.iw.mat <- cont.ip.mat

cm.all3 <- t(cm.all)
for(zz in 1:3){

    for(count in 1:nrow(cm.all)){
        y <- cm.all3[,count]
        use <- (y > 0)
        y1 <- y[use]
        ipos <- cand.ip.new[use,]
        cw <- cand.cw.new[use,]

        oo <- (ipos * cw) * y1
        # ipvect <- colSums(oo,na.rm=T)/colSums(cw*y1,na.rm=T)
        cwvect <- colSums(cw*y1,na.rm=T) / sum(cw * y1,na.rm=T)
        # cont.ip.mat[count,] <- ipvect
        cont.iw.mat[count,] <- cwvect
        count <- count + 1
        if(count %% 100 == 0){print(count)}
    }

    ##CANDIDATE ESTIMATES
    for(xx in 1:ncol(cm.all)){
        y <- cm.all[,xx]
        use <- (y > 0)
        y1 <- y[use]
        ipos <- cont.ip.mat[use,]
        cw <- cont.iw.mat[use,]

        oo <- (ipos * cw) * y1
        if(is.null(nrow(oo))){
            # ERIN commented while focusing on WEIGHTS
            # ipvect <- oo / (cw * y1)
            cwvect <- cw*y1 / sum(cw * y1,na.rm=T)
        }else{
            # ERIN commented while focusing on WEIGHTS
            # ipvect <- colSums(oo,na.rm=T)/colSums(cw*y1,na.rm=T)
            cwvect <- colSums(cw*y1,na.rm=T) / sum(cw * y1,na.rm=T)
        }
        cand.cw.new[xx,] <- cwvect
        # ERIN commented while focusing on WEIGHTS
        # cand.ip.new[xx,] <- ipvect

        # ADAM commented out next line
        ## print(weighted.mean(abs(lm(ipvect~igscores[xx,])$residuals),cwvect))
        if(xx %% 100 == 0){print(xx)}
    }
    # ERIN commented while focusing on WEIGHTS
    # x <- cand.ip.new[!is.na(cand.ip.old[,1]),]
    # y <- cand.ip.old[!is.na(cand.ip.old[,1]),]

    # ADAM commented out next 2 lines
    ##cand.cw.new[!is.na(cand.cw.old[,1]),] <- cand.cw.old[!is.na(cand.cw.old[,1]),]
    ##cand.ip.new[!is.na(cand.ip.old[,1]),] <- cand.ip.old[!is.na(cand.ip.old[,1]),]


    # ERIN commented while focusing on WEIGHTS
    # for(ix  in 1:ncol(cand.ip.new)){
    #     x1 <- cand.ip.new[,ix]
    #     y1 <- cand.ip.old[,ix]
    #     lm.out <- lm(y1~x1)
    #     x2 <- predict(lm.out,data.frame(x1))
    #     cand.ip.new[,ix] <- x2
    # }

    cand.cw.new[!is.na(cand.cw.old)] <- cand.cw.old[!is.na(cand.cw.old)]
    # ERIN commented while focusing on WEIGHTS
    # cand.ip.new[!is.na(cand.ip.old)] <- cand.ip.old[!is.na(cand.ip.old)]


    cand.cw.new[abs(cand.cw.new) == 1] <- NA
    # ERIN commented while focusing on WEIGHTS
    # cand.ip.new[abs(cand.ip.new) == 1] <- NA

    # ERIN commented while focusing on WEIGHTS
    # for(issue in colnames(igscores)){
    #     print(issue)
    #     z1 <- as.vector(cand.ip.new[,issue])
    #     z2 <- as.vector(igscores.old[,issue])
    #     lm.holdout <- lm(z2~z1,subset=holdout)
    #     lm.all <- lm(z2~z1)
    #     print(summary(lm.holdout))
    # }

    # ERIN: not needed for candidate_issue_weights
    # for(issue in colnames(igscores)){
    #     print(issue)
    #     z1 <- as.vector(cand.cw.new[,issue])
    #     z2 <- as.vector(cand.iweights.old[,issue])
    #     lm.holdout <- lm(z2~z1,subset=holdout)
    #     lm.all <- lm(z2~z1)
    #     print(summary(lm.holdout))
    # }

    # ERIN commented while focusing on WEIGHTS
    # rownames(cand.ip.new) <- colnames(cm.all)
    rownames(cand.cw.new) <- colnames(cm.all)
    # ERIN commented while focusing on WEIGHTS
    # save(cand.ip.new,file='/workspace/tmp/cand_ip_new_tmp.rda')
}

# ERIN commented while focusing on WEIGHTS
# cn <- rownames(cand.ip.new)
# m <- match(cn,cands[,'ICPSR2'])
# rownames(cand.ip.new) <- cands[m,'bonica_rid']
# cand.ip.new <- apply(cand.ip.new,2,
#                      function(z){
#                          z <- (z - mean(z,na.rm=T))
#                          z <- ((z / (max(abs(z),na.rm=T))))* 5
#                          z <- z + 5
#                          print(c(min(z,na.rm=T),max(z,na.rm=T)))
#                          return(z)
#                      })
# write.csv(cand.ip.new, file='/workspace/crowdpac/output/candidate_issue_positions.csv')



cn <- rownames(cand.cw.new)
m <- match(cn,cands[,'ICPSR2'])
rownames(cand.cw.new) <- cands[m,'bonica_rid']
write.csv(cand.cw.new, file='~/workspace/crowdpac/output/candidate_issue_weights.csv')



# ADAM commented out from here to end

## cands.w.ig <- cands[!is.na(cands[,'mean.lccr']) & cands[,'mean.lccr'] != '',]
## cands.in <- cands[!duplicated(cands[,'ICPSR2']),]
## m <- match(cands.in[,'bonica_rid'],cands.w.ig[,'bonica_rid'])
## cands.in[,c(61,63,64:74)] <- cands.w.ig[m,c(61,63,64:74)]
## igscores <- cbind(dwnom= cands.in[,'cs.1'],cands.in[,c(61,63,64:74)])
## igscores.new <- igscores
## ilist <- c('defense_and_national_security',
##            'taxes',
##            'finance',
##            'guns',
##            'jobs_and_economy',
##            'health_care',
##            'israel')


##for(xx in 1:ncol(igscores.new)){
## m <- match(cands.in[,'ICPSR2'],colnames(cm))
## cm.in <- cm[,m]
## cand.ips.start <- as.numeric(igscores[,xx])
## cand.ips.start <- (cand.ips.start - mean(cand.ips.start,na.rm=T))/sd(cand.ips.start,na.rm=T)
## cand.ips.orig <- cand.ips.start
## cand.ips.holdout <- cand.ips.start
## cand.ips.holdout[-holdout] <- NA
## cand.ips.start[holdout] <- NA
## cand.ips <- cand.ips.start
## cand.ips.dw <- as.numeric(cands.in[,'cs.1'])
## cand.iws <- cand.iweights[,xx]
## cand.iws <- (cand.iws - mean(cand.iws,na.rm=T))/sd(cand.iws,na.rm=T)


      ##NRA: 53553
      ##BRADY CAMPAGIN: 3817416197

      ##IMPLEMENT REGRESSION APPROACH
      ## cand.iws2 <- NULL
      ## for(issue in ilist){##colnames(igscores.new)){
      ##     ##issue <- colnames(igscores.new)[xx]
      ##     cpriorities <- unlist(apply(candPriorities[,-c(1:6)],1,
      ##                                 function(x){
      ##                                     out <- which(x == issue)
      ##                                     if(length(out) == 0){out <- length(x)}
      ##                                     return(out)
      ##                                 }))
      ##     cand.iws2 <- cbind(cand.iws2,ifelse(cpriorities <= 5,1,0))
      ## }
      ## colnames(cand.iws2) <- ilist

      ## ##
      ## cand.ips.dw <-  igscores[,1]
      ## issue <- 'abortion'
      ## ig2 <- igscores[,issue]
      ## ig2 <- ig2 - mean(ig2,na.rm=T)
      ## ##ig2 <- ips[,issue]
      ## cand.iws2 <- cand.iweights[,issue]
      ## cand.iws2 <- (cand.iws2 - mean(cand.iws2,na.rm=T))

      ## ##colnames(cand.iws2) <- colnames(igscores.new)
      ## nra <- which(as.numeric(contributors[,'comid']) ==  53553)
      ## brady <- which(as.numeric(contributors[,'comid']) ==  3817416197)
      ## rtl <-   which(as.numeric(contributors[,'comid']) == 111278)
      ## eml <- which(as.numeric(contributors[,'comid']) == 3846160984)

      ## MM <- ceiling(cm.in / 1e10)
      ## nconts <- which(rowSums(MM) > 20)
      ## oo1 <- NULL
      ## for(xx in nconts){
      ##     y <- cm.in[xx,]
      ##     y[y > 20] <- 20
      ##     use <- !(is.na(cand.ips.dw) | is.na(ig2) | is.na(cand.iws2))
      ##     dat <- data.frame(y,cand.ips.dw, ig2,cand.iws2)
      ##     ##print(summary(lm(y~ig2)))
      ##     ##print(summary(lm(y~ig2,weights=cand.iws2)))
      ##     oo <- options(digits = 20)
      ##     options(oo)
      ##     ## fit0 <- glm.nb(y~cand.ips.dw,data=dat[use,],control=glm.control(epsilon=1e-8,maxit=50))
      ##     ## fit1 <- glm.nb(y~ cand.ips.dw + ig2 *cand.iws2,data=dat[use,],control=glm.control(epsilon=1e-13,maxit=25))
      ##     ## try(print(summary(fit0)))
      ##     ## try(print(summary(fit1)))

      ##     ## print(fit0)
      ##     ## print(fit1)
      ##     ##
      ##     ## vt <- vuong(fit0,fit1)

      ##     fit0 <- lm(y~cand.ips.dw,data=dat[use,])
      ##     fit1 <- lm(y~ cand.ips.dw + ig2 *cand.iws2,data=dat[use,])
      ##     try(print(summary(fit0)))
      ##     try(print(summary(fit1)))
      ##     f.out <- (anova(fit0, fit1, test = "F"))
      ##     print(f.out)
      ##     ## print(vt)
      ##     oo <-  unlist(f.out['Pr(>F)'])[2]
      ##     oo1 <- c(oo1,oo)

      ##     ## print(summary(lm(y~ ig2 + cand.iws2)))
      ##     ## fit0 <- lm(y~cand.ips.dw)
      ##     ## fit1 <- lm(y~cand.ips.dw + ig2 + cand.iws2)
      ##     ## print(anova(fit0, fit1, test = "F"))
      ##     ## print(summary(vglm(y~ cand.ips.dw + ig3*cand.iws2,tobit(Lower=0,Upper=300))))
      ## }


      ## summary(vglm(y~ig3*cand.iws2,tobit(Lower=0,Upper=300)))##,weights=cand.iws2))



      ## contrib.iws <- get.contrib.means(cm.in,weights=NULL,cand.iws2,
      ##                                  cores=cores,upper.limit=contrib.lim)

      ## contrib.iws <- get.contrib.means(MM,weights=NULL,cand.iws2,
      ##                                  cores=cores,upper.limit=contrib.lim)



      ## nc <- rowSums(MM)
      ## contrib.iws[indiv] <- NA
      ## contrib.iws[is.na(contrib.iws)] <- NA
      ## contrib.iws[nc < 100] <- NA

      ## cct <- cbind(contrib.iws,nc,contributors)
      ## cct <- cct[!is.na(cct[,1]),]
      ## contrib.iws2 <- as.numeric(cct[,1])
      ## oo <- rev(order(abs(contrib.iws2)))
      ## cct[oo[1:10],]



      ## contrib.ips <- get.contrib.means(cm.in,weights=NULL,cand.ips,
      ##                                  cores=cores,upper.limit=contrib.lim)



##       if(zz >= 1){
##           use <- !is.na(cand.ips)
##           cm.in2 <- cm.in[,use]
##           candips2 <- cand.ips[use]

##           ##CALCULATE DEVIATIONS
##           MM <- ceiling(cm.in2/ 1e10)
##           MM <- as.simple_triplet_matrix(MM)
##           MM2 <- t(t(MM) * candips2)
##           aa <- aggregate(MM2$v,list(d=MM2$i),sd)
##           rnames <- MM2$dimnames[[1]]
##           aa[,1] <- rnames[aa[,1]]

##           MM <- ceiling(cm.in2/ 1e10)
##           ids <- as.numeric(rownames(MM))
##           m <- match(ids,as.numeric(aa[,1]))
##           aa <- aa[m,]
##           vv <- quantile(aa[,2],na.rm=T)
##           keep <- ((!is.na(aa[,2]) & aa[,2] < .75 & rowSums(MM) > 1))
##           ## if(zz == 1){
##           ##     keep <- ((!is.na(aa[,2]) & aa[,2] < .15 & rowSums(MM) > 0) | rowSums(MM) == 1)
##           ## }

##           ## if(zz == 2){
##           ##     keep <- ((!is.na(aa[,2]) & aa[,2] < vv[2] & rowSums(MM) > 0))
##           ## }
##           ## if(zz > 2){
##           ##     keep <- ((!is.na(aa[,2]) & aa[,2] < vv[2] & rowSums(MM) > 1))
##           ## }
##           contrib.ips[!keep] <- NA
##       }

##         cand.ips <- get.cand.means(cm.in,contrib.ips,weights=NULL,
##                                    dynamic.cands=dynamic.cands,cores=cores)
##         cand.ips3 <- as.numeric(cands.in[,'oc1'])
##         party <- cands.in[,'Party']
##         gender <- cands.in[,'cand_gender']

##         party[!(party %in% c(100,200))] <- 328


##         X <- cbind(cand.ips.dw,cand.ips.orig,cand.ips,cand.ips3,party,gender,state=cands.in[,'State'])
##         X <- as.data.frame(X,stringsAsFactors=FALSE)
##         X$cand.ips.orig <- as.numeric(X$cand.ips.orig)
##         X$cand.ips.dw <- as.numeric(X$cand.ips.dw)
##         X$cand.ips <- as.numeric(X$cand.ips)
##         X$cand.ips3 <- as.numeric(X$cand.ips3)
##         X$party <- as.factor(X$party)
##         X$state <- as.factor(X$state)

##         ##lm.out <- lm(cand.ips.orig~cand.ips + cand.ips2 + cand.ips3 + party,data=X)
##         lm.out <- lm(cand.ips.dw~cand.ips,data=X)
##         print(colnames(igscores)[xx])
##         print(sum(!is.na(contrib.ips)))
##         print(summary(lm.out))
##         if(FALSE){
##             cand.ips <- predict(lm.out,X)
##             ## cand.ips[cand.ips < min(cand.ips.orig,na.rm=T) - 1] <-  min(cand.ips.orig,na.rm=T) - 1
##             ## cand.ips[cand.ips > max(cand.ips.orig,na.rm=T) + 1] <-  max(cand.ips.orig,na.rm=T) + 1
##             cand.ips[!is.na(cand.ips.start)] <-  cand.ips.start[!is.na(cand.ips.start)]
##             ##plot(cand.ips[holdout],cand.ips.holdout[holdout])
##             print(summary(lm(cand.ips.holdout~cand.ips,subset=holdout)))
##         }

##     }

##   cand.ips.out <- predict(lm.out,X)

##   gg <- rbind(cbind(cand.ips.out,cand.ips.orig,'all'),
##               cbind(cand.ips.out[holdout],cand.ips.holdout[holdout],'holdout'))
##   colnames(gg) <- c('x','y','type')
##   gg <- as.data.frame(gg)
##   gg$x <- as.numeric(as.character(gg$x))
##   gg$y <- as.numeric(as.character(gg$y))
##   gg$type <- factor(as.character(gg$type))
##   q <- qplot(data=gg,x=x,y=y)
##   q <- q + facet_wrap(~type)
##   print(q)

##   print(cor(cand.ips.out,cand.ips.orig,use='complete.obs'))
##   print(cor(cand.ips.out[holdout],cand.ips.orig[holdout],use='complete.obs'))
##   ##print(cor(cand.ips.out[holdout],cand.ips.orig[holdout],use='complete.obs'))
##   igscores.new[,xx] <- cand.ips.out
## }

## browser()
## cc <- cbind(cands.in[,c('Name','District','seat','bonica_rid','oc1')],igscores.new,igscores)
## dd <- cands[,c('election','fecyear','District','bonica_rid','Name','seat','Party',
##                'oc1','cs.1',
##                'Incum_Chall','candStatus',
##                'total.disbursements','total.receipts',
##                's.elec.stat','p.elec.stat','r.elec.stat','gen.elec.stat','gen.elect.pct','winner',
##                'num.givers.total')]


## mm <- match(dd[,'bonica_rid'],cc[,'bonica_rid'])
## ig <- cc[mm,c(6:18)]
## mode(ig) <- 'numeric'
## ig <- round(ig,2)

## cfscore <- as.numeric(dd[,'oc1'])
## ig<- apply(ig,2,
##                 function(z){
##                   oo <- lm(cfscore~z)
##                   z <- coef(oo)[1] + coef(oo)[2] * z
##                   return(z)
##                 })

## dd1 <-  cbind(dd[,1:9],ig,dd[,-c(1:9)])
## ##dd1 <- dd1[grep('cand',dd1[,'bonica_rid']),]
## dd1 <- dd1[grep('2012',dd1[,'election'],ignore.case=T),]
## ##dd1 <- dd1[dd1[,'election'] == 2012 & dd1[,'fecyear'] == 2012,]
## ##dd1[dd1[,'District'] == 0,'District'] <- 'PRES'
## ##dd1 <- dd1[order(dd1[,'District']),]
## cn <- colnames(dd1)
## cn <- gsub('mean.','',cn)
## cn <- gsub('.mean','',cn)
## cn <- gsub('turbo.','',cn)
## cn[8] <- 'CFscore (Lib-Con)'
## cn[9] <- 'dwnom'
## colnames(dd1) <- cn
## dd1 <- dd1[!is.na(as.numeric(dd1[,'ada'])),]
## write.csv(dd1,file='/workspace/tmp/crowdPACestimates_all.csv')



