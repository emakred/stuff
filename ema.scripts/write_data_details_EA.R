##

# ------------- functions and libraries -------------------------------------------

library(Matrix)
library(RMySQL)

username = 'crowdpacdata'
passwd = 'crowdpacdata2014'
dbname = 'cfdb'
port = 5841
host = 'cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com'

con <- dbConnect(MySQL(), user=username, password=passwd, dbname=dbname, port=port, host=host)

most.common <- function(z){
    z <- z[z != '' | grepl('self',z)]
    if(length(z) == 0){
        return('')
    }
    tt <- table(z)
    tt1 <- tt[rev(order(tt))]
    return(names(tt1)[1])
}

##FUNCTION TO RESCALE CF-SCORES FROM -10 to 10
rescale.cf <- function(z){
    load('/scratch1/Dropbox/crowdpac/data/rescaling_params.rda' )
    qq1 <- qq1 - mean(qq1)
    z[z > qq1[2]] <- qq1[2]
    z[z < qq1[1]] <- qq1[1]
    z <- ((z / (max(abs(qq1),na.rm=T)))) * 10
    return(z)
}


# ---------------------------------------------------------------------------------


# if(!exists('aresults')){
  load('/scratch1/Dropbox/contributions/finished_runs/aresults_79_14_current.rda')
  load('~/workspace/contributions/finished_runs/cands_79_14_current.rda')
# }

cm <- aresults$cm
cm1 <- ceiling(cm/1e10)

contributors <- aresults$contributors
# cands <- aresults$cands
comid <- as.numeric(contributors[,'comid'])
party.label <- ifelse(cands[,'Party'] == 100,'DEM',
                      ifelse(cands[,'Party'] == 200,'REP',
                             ifelse(cands[,'third.party'] == 'LIB','LIB',
                                    ifelse(cands[,'third.party'] == 'GRN','LIB','IND'))))

exclude.recips <- c(cands[cands[,'cand_or_committee'] == 'comm' & 
                    cands[,'Party'] %in% c(100,200),'ICPSR2'],
                    cands[grepl('nominee',cands[,'bonica_rid']),'ICPSR2'])
cands.states <- as.character(cands[,'State'])

###############################################################################
##COLLECT LISTS OF INFORMATIVE DONORS
###############################################################################


###############################################################################
##LOOP TO COLLECT RESULTS FOR CANDIDATES
###############################################################################
df <- data.frame()
donations.from.ind.all.cands <- matrix(nrow=0,ncol=12)
donations.from.org.all.cands <- matrix(nrow=0,ncol=9)
donations.to.all.cands <- matrix(nrow=0,ncol=9)
shared.donations.all <- matrix(nrow=0,ncol=9)

# iss <- 'overall'

cc1 <- read.csv('/scratch1/Dropbox/crowdpac/output/candidate_master_to_import.csv')
unq.brid <- unique(cc1[,'bonica_rid'])

for(y in 1:length(unq.brid)) {
  i <- unq.brid[y]
  print(which(unq.brid==i))
  m <- match(i,cands[,'bonica_rid'])  

  if (!is.na(m)) {

    icpsr <- cands[m,'ICPSR2']
    print(cands[m,c('Name','oc1')])

    ###############################################################################
    ##DONORS WHO GAVE TO CANDIDATE i ALSO GAVE TO CANDIDATES x,y,z
    ###############################################################################
    # k <- match(icpsr,colnames(cm))
    
    # use <- which((cm1[,k]) > 0)
    # cm.i <- cm1[use,]
    # oo <- colSums(cm.i)
    # num.donors <- length(use)
    # num.of.shared.donors <- oo
    # oo1 <- oo /  log1p(colSums(cm1))
    # oo1[oo < 3] <- 0
    # oo <- oo1

    # if(sum(oo > 0) > 1 ){
    #   similar <- rev(order(oo))[-1]
    #   u.icpsr <- colnames(cm.i)[similar[1:min(sum(oo>0),20)]]
    #   # u.icpsr <- u.icpsr[!(u.icpsr %in% exclude.recips)]
    #   oo <- oo[similar[1:min(sum(oo>0),20)]]
    #   num.of.shared.donors <- num.of.shared.donors[similar[1:min(sum(oo>0),20)]]

    #   m <- match(u.icpsr,cands[,'ICPSR2'])
    #   sim.mat <- cbind(cands[m,c('Name','oc1','bonica_rid')],oo,round(oo/length(use),3))

    #   m <- match(as.character(sim.mat[,'bonica_rid']),as.character(cands[,'bonica_rid']))
    #   recip <- cands[m,c('Name','Party','oc1','State','District')]
    #   cf <- round(rescale.cf(as.numeric(recip[,'oc1'])),1)
    #   recip.info <- recip[,c('Name','State','District')]
    #   shared.donations <- na.omit(cbind(bonica_rid=as.character(i),
    #                                     recip.info,
    #                                     as.character(sim.mat[,'bonica_rid']),
    #                                     num.donors,
    #                                     num.of.shared.donors,
    #                                     pct.of.shared.donors= (num.of.shared.donors/num.donors),
    #                                     recipient_idscore=cf))

    #   colnames(shared.donations) <- c('bonica_rid','name','state','district',
    #                                   'recipient.id',
    #                                   'num.of.donors','num.of.shared.donors','pct.donors.shared',
    #                                   'recipient.idscore')
    #   # print(shared.donations)
    #   shared.donations.all <- rbind(shared.donations.all,shared.donations)
    # }
        
    ###############################################################################
    ##DONATIONS (FROM INDIVIDUALS) TO CANDIDATE
    ###############################################################################
    # donors <- dbGetQuery(con,
    #                      paste("select * from newconts ",
    #                            " where bonica_rid = '",i,"' and bonica_id2 > 0",sep=''))
    # if (!identical(donors,df)) {
    #   female <- ifelse(donors[,'contributor_gender'] == 'F',5e10,0)
    #   donors[,'bonica_id2'] <- donors[,'bonica_id2'] + female
    #   donors <- donors[(donors[,'transaction_type'] %in%  c('24K','15','15E','24Z',
    #                                                   '11','10','19','24I','15Z',
    #                                                '24F','24E','24C',' ','NA','')),]
    #   donors <- donors[as.numeric(donors[,'amount']) >0,]

    #   if (length(donors$amount)>0) {
    #     agg.am <- aggregate(donors$amount,by=list(donors$bonica_id2),FUN=sum,na.rm=T)
    #     agg.am <- agg.am[rev(order(agg.am[,2])),]
    #     ind <- NULL
    #     for(j in 1:min(30,nrow(agg.am))){ ## cycle through top 20
    #         pp <- donors[which(donors[,'bonica_id2'] == agg.am[j,1]),]
    #         pp <- pp[rev(order(pp[,'date'])),]
    #         if(pp[1,'contributor_type'] == 'C'){
    #             ln  <- most.common(pp[,'contributor_name'])
    #             fn <- emp <- occ <- ''
    #         }else{
    #             emp  <- most.common(pp[,'contributor_employer'])
    #             occ  <- most.common(pp[,'contributor_occupation'])
    #             ln  <- most.common(pp[,'contributor_lname'])
    #             fn  <- most.common(pp[,'contributor_ffname'])
    #             ##location and date of most recent contribution
    #         }
    #         loc <- unlist(pp[1,c('contributor_city','contributor_state','date','bonica_id2')])
    #         am <- as.numeric(pp[1,'amount'])
    #         ind <- rbind(ind,c(ln,fn,occ,emp,loc,am,round(agg.am[j,2])))
    #     }
    #     if(nrow(ind) == 1){
    #         m  <- match(as.numeric(ind['bonica_id2']),comid)
    #     }else{
    #         m <- match(as.numeric(ind[,'bonica_id2']),comid)
    #     }
    #     cf <- round(as.numeric(contributors[m,'oc1']),2)
    #     cf <- rescale.cf(cf)
    #     donations.from.ind <- cbind(bonica_rid=as.character(i),ind,cf)
    #     # need to remove any records with 'NA' in the donor.idscore (=cf)
    #     donations.from.ind <- donations.from.ind[!is.na(cf),]
    #     donations.from.ind <- donations.from.ind[1:min(10,nrow(donations.from.ind)),]
    #     colnames(donations.from.ind) <- c('bonica_rid','last.name','first.name','occ','emp','city','state',
    #                                       'date.of.most.recent.cont','donor.id',
    #                                       'amount.of.most.recent.cont','total.amount.career',
    #                                       'donor.idscore')
    #     donations.from.ind.all.cands <- rbind(donations.from.ind.all.cands,donations.from.ind)
    #   } # end: if (length(donors$amount)>0)
    # } # end: if (!identical(donors,df))

    ###############################################################################
    ##DONATIONS (FROM ORGANIZATIONS) TO CANDIDATE
    ###############################################################################

    #   if(iss =='overall'){
    #       d2 <- donors[donors[,'contributor_type'] == 'C',]
    #   }else{
    #       d2 <- donors[donors[,'bonica_id2'] %in% opl & donors[,'contributor_type'] == 'C',]
    #   }
    #   ## oo <- match(as.numeric(d2[,'bonica_id']),comid)
    #   ## d2 <- d2[!is.na(oo) & contributors[oo,'nrc'] == 0,]
    #   if (length(d2$contributor_type)>0) {
    #     agg.am <- aggregate(d2$amount,list(d2$bonica_id2),sum,na.rm=T)
    #     agg.am <- agg.am[rev(order(agg.am[,2])),]
    #     org <- NULL
    #     for(j in 1:nrow(agg.am)){
    #         pp <- donors[which(donors[,'bonica_id2'] == agg.am[j,1]),]
    #         pp <- pp[rev(order(pp[,'date'])),]
    #         ln  <- most.common(pp[,'contributor_name'])
    #         ##location and date of most recent contribution
    #         loc <- unlist(pp[1,c('contributor_city','contributor_state','date','bonica_id2')])
    #         am <- as.numeric(pp[1,'amount'])
    #         org <- rbind(org,c(ln,loc,am,round(agg.am[j,2])))
    #     }
    #     org <- org[rev(order(as.numeric(org[,7]))),]
    #     if (nrow(agg.am) == 1) {
    #       m <- match(as.numeric(org[5]),comid)  
    #       org <- t(org)
    #     } else {
    #       m  <- match(as.numeric(org[,'bonica_id2']),comid)
    #     }
    #     cf <- round(as.numeric(contributors[m,'oc1']),2)
    #     cf <- rescale.cf(cf)

    #     # need to remove any records with 'NA' in the donor.idscore (=cf)
    #     donations.from.org <- na.omit(cbind(bonica_rid=as.character(i),org,cf))

    #     # pick top donations HERE (top of total.amount.career)
    #     top.org <- 20
    #     if (nrow(donations.from.org)>top.org) {
    #       donations.from.org <- donations.from.org[1:top.org,]
    #     }
    #     colnames(donations.from.org) <- c('bonica_rid','org.name','city','state',
    #                                       'date.of.most.recent.cont','donor.id',
    #                                       'amount.of.most.recent.cont','total.amount.career',
    #                                       'donor.idscore')
    #     # ----->>>>  change this to a variable that gets written to then save everyone's donation info together  <<<<----------   !!!!!!!!
    #     donations.from.org.all.cands <- rbind(donations.from.org.all.cands,donations.from.org)
    #   } # end: if (length(d2$contributor_type)>0)

    # } # end: if (!identical(donors,df)) 

    ###############################################################################
    ##DONATIONS FROM CANDIDATE
    ###############################################################################






    # ------------------------------- start of just-for-now commenting ------------------


    #CONTRIBUTOR ID FOR PERSONAL CONTRIBUTIONS MADE BY THE CANDIDATE
    # !!!   ------>>>>   THIS QUERY RETURNS MANY MORE THAN 1 cong.i   <<<<<------  
    # cong.i <- dbGetQuery(con, paste("select ContribID from cand_cont_db ",
    #                            " where bonica_rid = '",i,"'",sep=''))
    # if(!identical(cong.i,df)) {
    #   if (!is.na(cong.i)){
    #     cong.i <- cong.i$ContribID[1]
    #     conts <- dbGetQuery(con,
    #                       paste("select * from newconts ",
    #                             " where bonica_id2 = ",cong.i,sep=''))
    #     if (length(conts) > 0) {
    #       conts <- conts[!(conts[,'transaction_type'] %in% c('15C','24Y','22Y')),]
    #       ##ORDER DATE ASCENDING
    #       conts <- conts[rev(order(conts[,'date'])),]

    #       ##SCREENING SELF-CONTRIBS
    #       conts <- conts[conts[,'bonica_rid'] != i,]

    #       m <- match(as.character(conts[,'bonica_rid']),as.character(cands[,'bonica_rid']))
    #       recip <- cands[m[!is.na(m)],c('Name','Party','oc1','State','District')]
    #       conts <- conts[!is.na(m),]

    #       if (length(conts[,'amount'])>0) {
    #         agg.am <- aggregate(as.numeric(conts[,'amount']),list(conts[,'bonica_rid']),sum,na.rm=T)

    #         pp1 <- conts[,'bonica_rid']
    #         conts <- conts[!duplicated(pp1),]
           
    #         m <- match(conts[,'bonica_rid'],agg.am[,1])
    #         amount.total.career <- agg.am[m,2]
           

    #         if (nrow(agg.am) == 1) {
    #           recip <- as.data.frame(recip)
    #         }

    #         recip <- recip[!duplicated(pp1),]
    #         if (nrow(agg.am) ==1) {
    #           cf <- round(as.numeric(recip[3]),2)
    #           recip.info <- as.vector(recip[c(1,4,5)])
    #           if (class(recip)!='data.frame'){
    #             recip.info <- t(recip.info)
    #           }
    #         } else {
    #           cf <- round(as.numeric(recip[,'oc1']),2)
    #           recip.info <- recip[,c('Name','State','District')]
    #         }

    #         cf <- rescale.cf(cf)

    #         # need to remove any records with 'NA' in the recipient_idscore (=cf)
    #         donations.to <- na.omit(cbind(bonica_rid=as.character(i),
    #                         recip.info,
    #                         conts[,c('date','bonica_rid','amount')],
    #                         amount.total.career,recipient_idscore=cf))

    #         donations.to <- donations.to[rev(order(as.numeric(donations.to[,'amount.total.career']))),]

    #         top.donations.to <- 20
    #         if (nrow(donations.to)>top.donations.to) {
    #           donations.to <- donations.to[1:top.donations.to,]
    #         }
    #         colnames(donations.to) <- c('bonica_rid','name','state','district',
    #                                   'date.of.most.recent.cont',
    #                                   'recipient.id',
    #                                   'amount.of.most.recent.cont','total.amount.career',
    #                                   'recipient.idscore')
    #         donations.to.all.cands <- rbind(donations.to.all.cands,donations.to)
    #       }  # end if (length(conts[,'amount'])>0) 
    #     }  # end if (length(conts) > 0) 
    #   } # end if (!is.na(cong.i))
    # } # end if(!identical(cong.i,df)) 

    # --------------------------------- end of just-for-now commenting ------------------



    ##ISSUE
    iss_vec <- c('jobs_and_the_economy','immigration','abortion',
                 'guns','education','energy_and_environment',
                 'social_security_and_medicare','defense_and_national_security',
                 'budget_and_taxes','welfare_and_poverty','banking_and_finance',
                 'healthcare','guns','labor','foreign_policy')

    # if (iss != 'overall') {
      for (iss in iss_vec) {

        ###############################################################################
        ##TEXT
        ###############################################################################
        if(!exists('floortext')){
          floortext <- read.csv('/scratch1/Dropbox/crowdpac/data/floor.csv',sep='|',as.is=T)
        }
        if(!exists('lda.results')){
          load('/scratch1/Dropbox/crowdpac/data/lda_results.rda')
        }
        sponsors <- lda.results$sponsors
        spb <- sponsors[,which(colnames(sponsors) == icpsr)]
        bln <- paste(names(spb[spb == 1]),'_',sep='')

        docs <- lda.results$dmat[which(lda.results$leg.id == icpsr),]
        iw <- docs[,iss]
        docs2 <- docs[rev(order(iw))[1:25],]

        bills <- lda.results$dmat[which(rownames(lda.results$dmat) %in%  c(bln)),]
        iw <- bills[,iss]
        bills2 <- bills[rev(order(iw))[1:10],]


        fl2 <- floortext[which(floortext[,'bonica_rid'] == i),]
        pp <- paste(fl2[,2],'_',fl2[,'bval'],sep='')
        speech <- NULL
        for(doc in rownames(docs2)){
         	doc <- gsub('_[0-9]$|_$','',doc,perl=T)
         	txt <- fl2[grep( doc,pp),]
         	txt <- txt[txt[,18] == 'speech',]
         	txt <- txt[,c(14,6,8,18,19)]
         	speech <- rbind(speech,txt)
        }
        write.csv(speech,file=paste('/scratch1/Dropbox/crowdpac/output/data_details/',i,'_',iss,'_text_speech.csv',sep=''))


        fl2 <- floortext[which(floortext[,'bonica_rid'] == i),]
        pp <- paste(fl2[,2],'_',fl2[,'bval'],sep='')
        billtext <- NULL
        for(doc in rownames(bills2)){
          doc <- gsub('_[0-9]$|_$','',doc,perl=T)
          txt <- fl2[grep( doc,pp),]
          txt <- txt[txt[,18] == 'bill',]
          txt <- txt[,c(14,6,8,18,19)]
          billtext <- rbind(billtext,txt)
        }
        write.csv(billtext,file=paste('/scratch1/Dropbox/crowdpac/output/data_details/',i,'_',iss,'_text_bills.csv',sep=''))


  #       ###############################################################################
  #       ##VOTES
  #       ###############################################################################

  #       ##LOAD FILE WITH OUTPUT FROM ISSUE SPECIFIC ROLL CALL MODEL
  #       load('/scratch1/Dropbox/crowdpac/data/cp_rc_out.rda')
  #       ##LOAD BILL ISSUE WEIGHTS
  #       biw <- read.csv('/scratch1/Dropbox/crowdpac/output/bill_issue_weights.csv')

  #       bill.iweights <- senout$leginfo$bill.iweights
  #       sponsors <- senout$leginfo$sponsors
  #       ipmat <- senout$idealPoints.out[,iss]
  #  	    sponsor.ip <- apply(sponsors,1,
  #                        function(z){mean(ipmat[which(z == 1)])})
  #       votes <- senout$votes[[paste('id',icpsr,sep='')]]
  #       session <- senout$votes[[paste('sessions',icpsr,sep='')]]
  #       record <- senout$record

  #       iw <- biw[,iss]
  #       o <- which(iw > .2 & votes %in% c(1,6) & biw[,'X.2'] == 'On Passage')



  #       biw2 <- cbind(bonica_rid=i,issue=iss,votes=votes[o],rcnum= record[o,'rc'],bill.desc=senout$leginfo$bill.desc[o],
  #                      session=session[o],biw[o,],sponsor.ip[o])
  #       biw2 <- biw2[nrow(biw2):1,]
  #       biw3 <- biw2[!duplicated(biw2[,'X.1']),]

  #       biw3[,'votes'] <- ifelse(biw3[,'votes'] == 1,'Y','N')
  #       biw3 <- biw3[,c('bonica_rid','issue',iss,'votes','X.1','X.3','X.4')]
  #       colnames(biw3) <- c('bonica_rid','issue.name','issue.weight','leg.vote',
  #                            'bill.title','bill.name','date')
  #       write.csv(biw3,file=paste('/scratch1/Dropbox/crowdpac/output/data_details/',i,'_votes.csv',sep=''))

  #       ## for(x in 1:nrow(biw3)){
  #          ##     sess <- biw3[x,'session']
  #          ##     rcnum <- biw3[x,'rcnum']
  #          ##     bn <- biw3[x,'bill.desc']
  #          ##     bns <- senout$legislature[[paste('sen',sess,sep='')]]$bill.desc
  #          ##     rcsel <- senout$legislature[[paste('sen',sess,sep='')]]$rcSel
  #       ## }

      } # end : for (iss in iss_vec)
    # } # end: if (iss != 'overall') 
  } # end: if (!is.na(m)) 
} # end: for(y in 1:length(unq.brid)) 

if(FALSE) {
  write.csv(donations.from.ind.all.cands,file=paste('/scratch1/Dropbox/crowdpac/output/data_details/donations_from_ind_to_import.csv',sep=''),row.names=FALSE)
  write.csv(donations.from.org.all.cands,file=paste('/scratch1/Dropbox/crowdpac/output/data_details/donations_from_org_to_import.csv',sep=''),row.names=FALSE)
  write.csv(donations.to.all.cands,file=paste('/scratch1/Dropbox/crowdpac/output/data_details/personal_donations_to_import.csv',sep=''),row.names=FALSE)
  write.csv(shared.donations.all,file=paste('/workspace/crowdpac/output/data_details/shared_donations_all.csv',sep=''),row.names=FALSE)
}
