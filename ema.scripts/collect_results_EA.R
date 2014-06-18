library(RMySQL)
con <- dbConnect(MySQL(), user="crowdpacdata", password="crowdpacdata2014", dbname='cfdb',
                 host='cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com', port=5841)

# ADAM was using a difference source data file
# load('~/workspace/contributions/finished_runs/scaling_for_mapping_paper/aresults_79_14_11_11.rda')
print(load('~/workspace/contributions/finished_runs/aresults_79_14_current.rda'))

cands <- aresults$cands

cand.master <- read.csv('~/workspace/crowdpac/states/2014/CA/candidate_master_to_import.csv',as.is=T)
m <- match(cand.master[,'bonica_rid'],cands[,'bonica_rid'])
pids <- cands[m,'ICPSR2']
pids <- pids[!is.na(pids)]

get.pout <- function(pids, opl,opc,issue='none'){

    ccpc <- opc[!is.na(opc) & opc >0]
    ccpl <-  opl[!is.na(opl) & opl >0]

    rm1 <- ccpc %in% ccpl
    rm2 <- ccpl %in% ccpc

    ccpc <- ccpc[!rm1]
    ccpl <- ccpl[!rm2]

    presout <- NULL
    for(p in pids){
        cc1 <- dbGetQuery(con,
                          paste("select recipient_ext_id, bonica_id2, amount,",
                                " transaction_type,contributor_state, contributor_lname",
                                " from newconts",
                                " where recipient_ext_id like '",p,"%'",
                                " and bonica_id2 > 0",
                                " and transaction_type in ('15','15Z','24E','24K','24I','24Z','15J',' ','')",
                                sep=''))
        if(length(cc1) > 0){
            a1 <- as.numeric(cc1[cc1[,'bonica_id2'] %in% ccpc,'amount'])
            a2 <- as.numeric(cc1[cc1[,'bonica_id2'] %in% ccpl,'amount'])
            a3 <- as.numeric(cc1[,'amount'])
            if(sum(a3) > 1000){
                q1 <- sum(a1)##sum(a1[a1 > 0])
                q2 <- sum(a2)##sum(a2[a2 > 0])
                q3 <- sum(a3)##sum(a3[a3 > 0])

                ubid <- unique(cc1[,'bonica_id2'])
                qq1 <- sum(ubid %in% ccpc)
                qq2 <- sum(ubid %in% ccpl)
                qq3 <- length(ubid)

                pp <- c(p,q3,q1,q2,q2/(q1+q2),(q2/q3 - q1/q3))
                presout <- rbind(presout,pp)
            }
        }
    }
    m <- match(presout[,1],cands[,'ICPSR2'])
    rownames(presout) <- cands[m,'bonica_rid']
    presout[is.na(as.numeric(presout[,5]))] <- 0
    presout[is.na(as.numeric(presout[,6]))] <- 0
    presout <- cbind(presout,
                     rank(as.numeric(presout[,5]),na.last=NA),
                     rank(as.numeric(presout[,6]),na.last=NA))
    reps <- cands[m,'Party'] == "200"
    rrep <- cbind(rank(as.numeric(presout[reps,5]),na.last=NA),
                  rank(as.numeric(presout[reps,6]),na.last=NA))
    rr <- matrix(NA,nrow(presout),2)
    rr[reps,] <- rrep
    presout <- cbind(presout,rr)
    return(presout)
}



##
##SAME SEX MARRIAGE
##
print(load('~/workspace/tmp/profamily_bids2.rda'))
print(load('~/workspace/tmp/promarriage_bids2.rda'))
opl <- opl2
opc <- opc2
ssm <- get.pout(pids,opl,opc)
write.csv(ssm,file='~/workspace/contributions/analysis/nytimes/output/ssm.csv')


##
##ABORTION
##
print(load('~/workspace/tmp/prolife_bids2.rda'))
print(load('~/workspace/tmp/prochoice_bids2.rda'))
opc <- opc2
opl <- opl2
abortion <- get.pout(pids,opl,opc)
write.csv(abortion,file='~/workspace/contributions/analysis/nytimes/output/abortion.csv')


##
##ENERGY
##
print(load('~/workspace/tmp/prooil_bids2.rda'))
print(load('~/workspace/tmp/proenv_bids2.rda'))
opc <- opc2
opl <- opl2
energy <- get.pout(pids,opl,opc)
write.csv(energy,file='~/workspace/contributions/analysis/nytimes/output/energy.csv')


# ADAM commented out next 2 lines
## cands <- ps.out$cands
## contributors <- ps.out$contributions
# ERIN: this file doesn't exist, so using closest we can find
# load("~/workspace/contributions/finished_runs/aresults_79_12_with_par_individuals_states_ctc_ndups_2.rda")
load("~/workspace/contributions/finished_runs/aresults_79_14_current.rda")
cfcands <- aresults$cands

m <- match(abortion[,1],cfcands[,'ICPSR2'])

# newcands <- cfcands[m[!is.na(m)],]
newcands <- cfcands[m,]

oc.dyn <- as.numeric(as.character(newcands[,'cfscores.dyn']),na.last=NA)
roc <- rank(oc.dyn)
rep <- newcands[,'Party'] == 200
rroc <- rank(as.numeric(newcands[rep,'cfscores.dyn']),na.last=NA)
# seeding reproc vector size
reproc <- roc * NA
reproc[which(rep)] <- rroc
ocout <- cbind(newcands[,'ICPSR2'],roc,reproc)


# ADAM's version uses ssm*p*, abortion*p*, energy*p* but thats just subset of all candidates
# so not needed for our purposes...updated in this (ratio) and next (magnitude) block
# rankratio <- cbind(ssmp[,c(7,9)],abortionp[,c(7,9)],energyp[,c(7,9)],ocp[,2:3])
rankratio <- cbind(ssm[,c(7,9)],abortion[,c(7,9)],energy[,c(7,9)],ocout[,2:3])
mode(rankratio) <- 'numeric'
colnames(rankratio) <- c('Rank Same-sex Marriage All','Rank Same-sex Marriage Reps',
                         'Rank Abortion All','Rank Abortion Reps',
                         'Rank Environment All','Rank Environment Reps',
                         'Rank CFscores All', 'Rank CFscores Reps')
rownames(rankratio) <- rownames(abortion)

# rankmag <- cbind(ssmp[,c(8,10)],abortionp[,c(8,10)],energyp[,c(8,10)],ocp[,2:3])
rankmag <- cbind(ssm[,c(8,10)],abortion[,c(8,10)],energy[,c(8,10)],ocout[,2:3])
mode(rankmag) <- 'numeric'
colnames(rankmag) <- c('Rank Same-sex Marriage All','Rank Same-sex Marriage Reps',
                         'Rank Abortion All','Rank Abortion Reps',
                         'Rank Environment All','Rank Environment Reps',
                         'Rank CFscores All', 'Rank CFscores Reps')
rownames(rankmag) <- rownames(abortion)

write.csv(rankratio,file='~/workspace/contributions/analysis/nytimes/output/fedratio.csv')
write.csv(rankmag,file='~/workspace/contributions/analysis/nytimes/output/fedmag.csv')


m <- match(abortion[,1],ocout[,1])
rankmag2 <- cbind(ssm[,c(8)],abortion[,c(8)],energy[,c(8)],
                  ssm[,c(7)],abortion[,c(7)],energy[,c(7)],
                  ocout[m,2],
                  ssm[,c(6)],abortion[,c(6)],energy[,c(6)],
                  ssm[,c(5)],abortion[,c(5)],energy[,c(5)],
                  oc.dyn[m],
                  ocout[m,1])
m <- match(ocout[m,1],cfcands[,'ICPSR2'])
rankmag2 <- cbind(rankmag2,cfcands[m,'Party'])
# mode(rankmag2) <- 'numeric'  # this is turning ICPSR2 ids into NA's
rankmag2 <- cbind(newcands[,'bonica_rid'],rankmag2)
colnames(rankmag2) <- c('bonica_rid',
                        'Rank Mag Same-sex Marriage',
                        'Rank Mag Abortion',
                        'Rank Mag Environment',
                        'Rank Ratio Same-sex Marriage',
                        'Rank Ratio Abortion',
                        'Rank Ratio Environment',
                        'Rank CFscores',
                        'Mag Same-sex Marriage',
                        'Mag Abortion',
                        'Mag Environment',
                        'Ratio Same-sex Marriage',
                        'Ratio Abortion',
                        'Ratio Environment',
                        'CFscores',
                        'CandID',
                        'Party')


rankmag2 = rankmag2[!is.na(rankmag2[,15]),]
rankmag2[,2] <- rank(rankmag2[,2])
rankmag2[,3] <- rank(rankmag2[,3])
rankmag2[,4] <- rank(rankmag2[,4])
rankmag2[,5] <- rank(rankmag2[,5])
rankmag2[,6] <- rank(rankmag2[,6])
rankmag2[,7] <- rank(rankmag2[,7])
rankmag2[,8] <- rank(rankmag2[,8])
write.csv(rankmag2,file='~/workspace/contributions/analysis/nytimes/output/rankmag3.csv')
