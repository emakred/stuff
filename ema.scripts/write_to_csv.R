library(RMySQL)

username = 'crowdpacdata'
passwd = 'crowdpacdata2014'
dbname = 'cfdb'
port = 5841
host = 'cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com'

con <- dbConnect(MySQL(), user=username, password=passwd, dbname=dbname, port=port, host=host)


## 

# functions included in this script
#
#
# GET.CURRENT.MCS
#  reads:  Dropbox/contributions/finished_runs/cands_79_14_current.rda
#  returns: c.cands
#
# GENERATE.RESCALING.PARAMS
#  calls: get.current.mcs
#  writes: Dropbox/crowdpac/data/rescaling_params.rda
#  returns: qq1
#
# RESCALE.CF
#  inputs: cf score unscaled, qq1
#  returns: cf score scaled
#
# GET.CANDS
#  inputs: year, state
#  sources: IL_remove.R
#  reads: Dropbox/contributions/data/cand_all_1979_2014.rda
#  reads: Dropbox/contributions/finished_runs/cands_79_14_current.rda
#  calls: generate.rescaling.params
#  calls: rescale.cf
#  calls: convert_districts
#  writes: candidate_master_to_import.csv
#  # returns: bonica_id2 (=ContribID)
#  returns: cand.master
#
# FIND.BATTLEGROUND.DISTRICTS
#  reads: Dropbox/contributions/data/cand_all_1979_2014.rda
#  writes: Dropbox/crowdpac/output/battleground_by_district.csv
#  returns: oo
#
# GET.ISSUE.ORDER
#  inputs: cand.master
#  calls: find.battleground.districts
#  reads: Dropbox/crowdpac/data/lda_results.rda
#  writes: Dropbox/crowdpac/output/top20_by_issue.rda
#  writes: Dropbox/crowdpac/output/top20_by_issue.csv
#  writes: Dropbox/crowdpac/data/candPriorities.rda
#  writes: Dropbox/crowdpac/output/candidate_issue_order_to_import.csv
#
# GET.ISSUE.POSITIONS
#  inputs: cand.master
#  calls: get.current.mcs
#  calls: rescale.cf
#  reads: Dropbox/crowdpac/data/cp_rc_out.rda
#  writes: Dropbox/crowdpac/output/candidate_issue_positions_to_import.csv
#
# GET.ISSUE.WEIGHTS
#  inputs: cand.master
#  reads: Dropbox/crowdpac/output/candidate_issue_weights.csv
#  writes: Dropbox/crowdpac/output/candidate_issue_weights_to_import.csv
#
# GET.ISSUE.MEANS
#  inputs: cand.master
#  reads: Dropbox/crowdpac/output/candidate_issue_positions_to_import.csv
#  writes: Dropbox/crowdpac/output/party_means_to_import.csv'




get.current.mcs <- function(){
  # calibrating the range of cf-scores between given quantile for a given subset 
  # of candidates: those that served in congress between 2004 and 2014. The rational 
  # for this period rather than current members of the 113th Congress is that the 
  # scores are identified based on a distributional assumpation. Given Republican 
  # majority control of the 113th, the median and quantiles skew to the
  # right. The 2004 to 2014 period gives a more balanced partisan distribution.
  print(load('~/workspace/contributions/finished_runs/cands_79_14_current.rda'))
  current.mcs <- ((cands[,'election'] %in% seq(2004,2014,2)) &
                  (!is.na(as.numeric(cands[,'ICPSR2']))) &
                  # grepl('fed',cands[,'seat']) &
                  grepl('federal:senate|federal:house',cands[,'seat']) &
                  cands[,'Party'] %in% c(100,328,200))
  current.mcs[is.na(current.mcs)] <- FALSE
  # pick off the rows for candidates in the above defined subset
  c.cands <- cands[current.mcs,]
  c.cands <- c.cands[!duplicated(c.cands[,'bonica_rid']),]
  return(c.cands)
}



generate.rescaling.params <- function() {
  # calculating boundaries for 0.005 - 0.99 quantile cf-scores
  c.cands <- get.current.mcs()
  # pick column 'oc1' for the *un*scaled cf-score
  z <- as.numeric(as.character(c.cands[,'oc1']))
  # cut-offs for raw cf-scores  (na.rm=T to ignore NA's) 
  qq1 <- quantile(z,pr=c(.005,.995),na.rm=T)
  # save p(0.005) and p(0.99) for cand[,'oc1']
  save(qq1,file='~/workspace/crowdpac/data/rescaling_params.rda' )
  return(qq1)
}



rescale.cf <- function(z,qq1){
    z[z > qq1[2]] <- qq1[2]
    z[z < qq1[1]] <- qq1[1]
    z <- z - mean(qq1)
    z <- ((z / (max(abs(qq1-mean(qq1)),na.rm=T)))) * 10
    return(z)
}




get.cands <- function(cand_file,year,election_date) {
  
  # cand_file<-'~/workspace/crowdpac/states/2014/general/list.csv'
  # year<-2014
  # election_date<-'2014-11-04'
  

  # also make it so that if ia new score exists the old one is overwritten
  # did the above, but commented out temporarily due to no time for anomaly checking of overwrites


  candidate_list <- read.csv(cand_file,as.is=T)

  candidate_list[is.na(candidate_list[,'district']),'district'] <- ''
  candidate_list[is.na(candidate_list[,'bonica_id2']),'bonica_id2'] <- ''
  candidate_list[is.na(candidate_list[,'cf.score']),'cf.score'] <- ''



  candidate_list[,'party'] <- ifelse(candidate_list[,'party'] == 'D',100,
                              ifelse(candidate_list[,'party'] == 'R',200,
                              ifelse(candidate_list[,'party'] == 'G',400,
                              ifelse(candidate_list[,'party'] == 'L',500,
                              ifelse(candidate_list[,'party'] == 'P&F',700,
                              ifelse(candidate_list[,'party'] == 'AE',800,328))))))
  party.label <- ifelse(candidate_list[,'party'] == 100,'DEM',
                 ifelse(candidate_list[,'party'] == 200,'REP',
                 ifelse(candidate_list[,'party'] == 400,'GRN',
                 ifelse(candidate_list[,'party'] == 500,'LIB',
                 ifelse(candidate_list[,'party'] == 700,'P&F',
                 ifelse(candidate_list[,'party'] == 800,'AE','IND'))))))
  # seat <- ifelse (candidate_list[,'friendly_seat'] == 'Governor', 'state:gov',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Lieutenant Governor', 'state:ltgov',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Secretary of State', 'state:sos',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Controller', 'state:ctrlr',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Treasurer', 'state:treas',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Attorney General', 'state:ag',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Insurance Commissioner', 'state:inscomm',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Superintendent of Public Instruction', 'state:super',
  #         ifelse (candidate_list[,'friendly_seat'] == 'Board of Equalization', 'state:eqbrd',
  #         ifelse (candidate_list[,'friendly_seat'] == 'US Representative', 'federal:house',
  #         ifelse (candidate_list[,'friendly_seat'] == 'State Senator', 'state:upper',
  #         ifelse (candidate_list[,'friendly_seat'] == 'State Assembly', 'state:lower',''))))))))))))
  seat <- ifelse (candidate_list[,'friendly_seat'] == 'Federal House', 'federal:lower',
          ifelse (candidate_list[,'friendly_seat'] == 'Federal Senate', 'federal:upper'))
  
  cand.list <- cbind(candidate_list[,c('name',
                                            'friendly_seat',
                                            'party')],
                          party.label = party.label,
                          seat = seat,
                          candidate_list[,c('district',
                                            'bonica_rid',
                                            'bonica_id2',
                                            'cp.score',
                                            'cf.score',
                                            'cand.url',
                                            'fb.id',
                                            'twitter.id',
                                            'wiki.url',
                                            'goog.plus',
                                            'youtube')],
                          fecyear = year,
                          election = year,
                          election_date = election_date,
                          state = state)

  # <<<<<<<<<<<<<<<<<<<<<----------------------------     !!!!!!!!!!!!!!!!!!!!
  # modify the below for when we have a bonica_rid, and a cf.score, but not cp.score

  # cand_bonica_rid <- candidate_list[candidate_list[,'bonica_rid'] != '',c('name','fecyear','election','election_date','seat','friendly_seat','party','party.label','district','state','bonica_rid','cp.score','cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')] 
  # cand_bonica_id2 <- candidate_list[candidate_list[,'bonica_id2'] != '',c('name','fecyear','election','election_date','seat','friendly_seat','party','party.label','district','state','bonica_id2','cf.score','cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')]

  indices.w.cpscore <- which(cand.list[,'cp.score'] != '' & !is.na(cand.list[,'cp.score']))
  temp <- c(1:nrow(cand.list))
  indices.wo.cpscore <- temp[which(!temp %in% indices.w.cpscore)]
  cand.assembled.list.rid <- cand.list[indices.w.cpscore,c('name','fecyear','election','election_date','seat','friendly_seat','party','party.label','district','state','bonica_rid','cp.score','cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')] 
  cand.list.wo.cpscore <- cand.list[indices.wo.cpscore,]

  indices.w.cfscore <- which(cand.list.wo.cpscore[,'cf.score'] != '' & !is.na(cand.list.wo.cpscore[,'cf.score']))
  temp <- c(1:nrow(cand.list.wo.cpscore))
  indices.wo.cfscore <- temp[which(!temp %in% indices.w.cfscore)]
  cand.assembled.list.id2 <- cbind(cand.list.wo.cpscore[indices.w.cfscore,c('name','fecyear','election','election_date','seat','friendly_seat','party','party.label','district','state','bonica_id2','cf.score','cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')])
  cand.list.wo.cfscore <- cand.list.wo.cpscore[indices.wo.cfscore,]

  indices.w.rid <- which(cand.list.wo.cfscore[,'bonica_rid'] != '' & !is.na(cand.list.wo.cfscore[,'bonica_rid']))
  temp <- c(1:nrow(cand.list.wo.cfscore))
  indices.wo.rid <- temp[which(!temp %in% indices.w.rid)]
  vec.contents.rid <- cbind(cand.list.wo.cfscore[indices.w.rid,c('name','fecyear','election','election_date','seat','friendly_seat','party','party.label','district','state','bonica_rid')],cp.score='',cand.list.wo.cfscore[indices.w.rid,c('cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')])
  cand.assembled.list.rid <- rbind(cand.assembled.list.rid,vec.contents.rid)
  cand.list.wo.rid <- cand.list.wo.cfscore[indices.wo.rid,]

  indices.w.id2 <- which(cand.list.wo.rid[,'bonica_id2'] != '' & !is.na(cand.list.wo.rid[,'bonica_id2']))
  temp <- c(1:nrow(cand.list.wo.rid))
  indices.wo.id2 <- temp[which(!temp %in% indices.w.id2)]

  if (length(indices.w.id2 != 0)) {
    indices.wo.id2 <-temp[which(!temp %in% indices.w.id2)]
    vec.contents.id2 <- cbind(cand.list.wo.rid[indices.w.id2,c('name','fecyear','election','election_date','seat','friendly_seat','party','party.label','district','state','bonica_id2')],cf.score='',cand.list.wo.rid[indices.w.id2,c('cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')])
    cand.assembled.list.id2 <- rbind(cand.assembled.list.id2,vec.contents.id2)
  }
  if (length(indices.wo.id2 != 0)) {
    cand.list.remaining <- cand.list.wo.rid[indices.wo.id2,]
  } else {
    cand.list.remaining <- rep(NA,18)
  }

  ################################
  # for those with bonica_rid
  ################################

  if (!exists('cands')) {
    print(load('~/workspace/contributions/finished_runs/cands_79_14_current.rda'))
  }

  if (!exists('cbios')) {
    print(load('~/workspace/cand_cont_db/cbios_00_14_with_degrees_matched.rda'))  #variable: candidate.political, match on bonica_rid
  }

  if (!exists('lmat')){
    print(load('~/workspace/crowdpac/data/legdb.rda'))  # column: fec1, match on bonica_rid
  }

  # have to order these because in the next step (the match), only the first match is taken
  cands[,'election'] <- substr(cands[,'election'],nchar(cands[,'election']) -3, nchar(cands[,'election']))
  cands <- cands[rev(order(cands[,'election'])),]
  
  qq1 <- generate.rescaling.params()

  m_rid <- match(cand.assembled.list.rid[,'bonica_rid'],cands[,'bonica_rid'])
  m_cbios <- match(cand.assembled.list.rid[,'bonica_rid'],cbios[,'bonica_rid'])
  m_lmat <- match(cand.assembled.list.rid[,'bonica_rid'],lmat[,'bonica_rid'])

  # convert to json for widget
  all.resumes <- cbios[m_cbios,'candidate.political']
  resume <- NULL

  for (entry in all.resumes) {
    if (!is.na(entry)) {
      # print('here')
      temp <- NULL

      for (row in strsplit(entry,'\n')) {
        temp <- rbind(temp,paste('{"office": "',row,'"}',sep=''))
      }

      new <- paste(temp,collapse=',')
      resume <- rbind(resume,paste('[',new,']',sep=''))    
    } else {
      resume <- rbind(resume,'')
    }

  }


  cand_list_rid <- cbind(id=cand.assembled.list.rid[,'bonica_rid'],
                         cand.assembled.list.rid[,c('name','state','district','seat','friendly_seat')],
                         cand.assembled.list.rid[,c('fecyear','election','election_date','party','party.label')],
                         cands[m_rid,c('Incum_Chall','candStatus')],
                         FEC.ID = lmat[m_lmat,'fec1'],
                         resume = resume, 
                         cand.assembled.list.rid[,c('cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')],
                         score = as.numeric(as.character(cand.assembled.list.rid[,'cp.score'])))

  # ERIN commented out next 11 lines since no time to check last minute score updates
  # # candidates from cands file that are in the race subset
  # from_cands_rda <- cands[m_rid,c('bonica_rid','oc1')]
  # # which of the above have scores
  # indices_scores_rid <- which(!is.na(from_cands_rda[,'oc1']))
  # # what are the bonica_rid's and oc1's for those that have scores
  # cands_scores_rid <- from_cands_rda[indices_scores_rid,c('bonica_rid','oc1')]

  # # find where in the race subset these new (non-na) scores fit 
  # m_rid2 <- match(cands_scores_rid[,'bonica_rid'],cand_list_rid[,'id'])
  # # replace the old or na scores with the scaled version of those from cands
  # cand_list_rid <- data.frame(lapply(cand_list_rid, as.character), stringsAsFactors=FALSE)
  # cand_list_rid[m_rid2,'score'] <- rescale.cf(as.numeric(cands_scores_rid[,'oc1']),qq1)


  ################################
  # for those with bonica_id2
  ################################

  cand_list_id2 <- cbind(id=cand.assembled.list.id2[,'bonica_id2'],
                         cand.assembled.list.id2[,c('name','state','district','seat','friendly_seat')],
                         cand.assembled.list.id2[,c('fecyear','election','election_date','party','party.label')],
                         Incum_Chall='',
                         candStatus='',
                         FEC.ID = '',
                         resume = '',
                         cand.assembled.list.id2[,c('cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')],
                         score=cand.assembled.list.id2[,'cf.score'])

  # ERIN commented out next 16 lines since no time to check last minute score updates
  # # candidates from newconts that are in the race subset
  # query_result_id2 <- dbGetQuery(con,paste("select 
  #                                            bonica_id2,
  #                                            contributor_cfscore
  #                                          from 
  #                                            newconts 
  #                                          where 
  #                                            bonica_id2 in (",paste(cand_list_id2[,'id'],collapse=', '),");",sep=''))
  # query_result_id2 <- unique(query_result_id2)
  # query_result_id2 <- query_result_id2[which(query_result_id2[,'contributor_cfscore']!=0),]
  # query_result_id2 <- query_result_id2[which(!is.na(query_result_id2[,'contributor_cfscore'])),]

  # # see which non-na, non-zero query results were found for the race subset
  # m_id22 <- match(query_result_id2[,'bonica_id2'],cand_list_id2[,'id'])
  # # update existing cfscores with any non-na, non-zero scores found in query
  # cand_list_id2 <- data.frame(lapply(cand_list_id2, as.character), stringsAsFactors=FALSE)
  # cand_list_id2[m_id22,'score'] <- query_result_id2[,'contributor_cfscore']

  # rescale ALL cfscores
  scaled_score_id2 <- rescale.cf(as.numeric(as.character(cand_list_id2[,'score'])),qq1)
  # replace all scores with scaled scores
  cand_list_id2[,'score'] <- scaled_score_id2  

  # ----------------------------

  if (!is.null(nrow(cand.list.remaining))) {
    no.ids <- cbind(id='',
                    cand.list.remaining[,c('name','state','district','seat','friendly_seat')],
                    cand.list.remaining[,c('fecyear','election','election_date','party','party.label')],
                    Incum_Chall='',
                    candStatus='',
                    FEC.ID='',
                    resume='',
                    cand.list.remaining[,c('cand.url','fb.id','twitter.id','wiki.url','goog.plus','youtube')],
                    score='')
    cand.master <- rbind(cand_list_rid,cand_list_id2,no.ids)
  } else {
    cand.master <- rbind(cand_list_rid,cand_list_id2)
  }

  cand.master[is.na(cand.master[,'score']),'score'] <- ''
  # empty contents of Incum_Chall, candStatus and FEC.ID if is.na()

  # incorporate fake ids (using list.csv)
  # incorporate resumes from csv


  colnames(cand.master) <- c('bonica_rid','Name','State','District','seat','friendly_seat','fecyear','election','election_date',
                             'Party','party.label','Incum_Chall','candStatus','FEC.ID','current_committee','cand.url','fb.id',
                             'twitter.id','wiki.url','goog.plus','youtube.id','cfscore')

  write.csv(cand.master,file='~/workspace/crowdpac/output/candidate_master_to_import.csv',row.names=FALSE) 
  
  return(cand.master)
}



find.battleground.districts <- function() {
  print(load('~/workspace/contributions/data/cand_all_1979_2014.rda'))
  cc <-  cands[(as.numeric(cands[,'election']) >= 2012 &
                grepl('fed',cands[,'seat'],ignore.case=T)),]
  oo <- cc[ ,c('District',"cd_pres_vs_2012")]
  oo <- as.data.frame(unique(oo[!is.na(oo[,2]),]))
  oo[,2] <- as.numeric(as.character(oo[,2]))
  battleground <- ifelse(oo[,2] > .44 & oo[,2] < .56,1,0)
  oo <- cbind(oo,battleground=battleground)

  oo[,1] <- as.character(oo[,1])
  oo[,1] <- ifelse(nchar(oo[,1]) == 5,substr(oo[,1],1,3),oo[,1])
  oo <- unique(oo)

  rownames(oo) <- NULL
  write.csv(oo, file='~/workspace/crowdpac/output/battleground_by_district.csv')
  return(oo)
}



get.issue.order <- function(cand.master) {
  # ERIN: will need to load results from collect_results or prelim_contribs
  print(load('~/workspace/crowdpac/data/lda_results.rda'))
  oo <- find.battleground.districts()
  tot <- lda.results$cand.iweights
  rval <- lda.results$rval
  cc = tot[,!grepl('latent',colnames(tot))]
  champs <- NULL
  for(x in 1:ncol(cc)){
      tt <- cc[,x]
      tt <- tt[!is.na(tt)]
      o <- rev(order(tt))
      o <- o[which(!is.na(oo))[1:20]]
      top20 <- cbind(colnames(cc)[x],rval[o,c('lname','fname','state','party','icpsr','bonica_rid')],round(tt[o],2))
      colnames(top20)[1] <- 'issue'
      colnames(top20)[8] <- 'issue.weight'
      champs <- rbind(champs,top20)
  }
  save(champs,file='~/workspace/crowdpac/output/top20_by_issue.rda')
  write.csv(champs,file='~/workspace/crowdpac/output/top20_by_issue.csv')

  cc <- lda.results$cand.iweights
  cc <- cc[,!grepl('latent',colnames(cc)) &
           (colnames(cc) %in% c('jobs_and_the_economy',
                                'immigration',
                                'abortion',
                                'guns',
                                'education',
                                'energy_and_environment',
                                'social_security_and_medicare',
                                'defense_and_national_security',
                                'budget_and_taxes',
                                'welfare_and_poverty',
                                'banking_and_finance',
                                'healthcare',
                                'labor',
                                'foreign_policy'))]
  issues <- colnames(cc)
  mode(cc) <- 'numeric'
  ordering <- t(apply(cc,1,
                    function(z){
                        o <- rev(order(as.numeric(z)))
                        return(issues[o])
                    }))
  candPriorities <- cbind(rval[,c('lname','fname','state','party','icpsr','bonica_rid')],ordering)
  save(candPriorities,file='~/workspace/crowdpac/data/candPriorities.rda')

  cand.master <- cand.master[!grepl('state',cand.master[,'seat'] ),]
  cid <- as.character(candPriorities[,'bonica_rid'])

  m <- match(as.character(cand.master[,'bonica_rid']),cid)
  cip <- candPriorities[m[!is.na(m)],]
  cip1 <- cip[!is.na(cip[,2]),]

  write.csv(cip1,file='~/workspace/crowdpac/output/candidate_issue_order_to_import.csv',row.names=FALSE)
}



get.issue.positions <- function(cand.master) {
  
all_iss_scores <- read.csv('~/workspace/contributions/analysis/nytimes/output/rankmag3.csv')
iss_cands <- as.character(all_iss_scores[,'bonica_rid'])
ssm.score.unscaled <- all_iss_scores[,'Mag.Same.sex.Marriage']
abortion.score.unscaled <- all_iss_scores[,'Mag.Abortion']
energy.score.unscaled <- all_iss_scores[,'Mag.Environment']

qq_issue <- quantile(rbind(ssm.score.unscaled,abortion.score.unscaled,energy.score.unscaled),pr=c(.05,.95),na.rm=T)

ssm.score <- rescale.cf(ssm.score.unscaled,qq_issue)
abortion.score <- rescale.cf(abortion.score.unscaled,qq_issue)
energy.score <- rescale.cf(energy.score.unscaled,qq_issue)

iss_scores <- cbind(iss_cands, ssm.score, abortion.score, energy.score)
colnames(iss_scores) <- c('bonica_rid','marriage','abortion','energy_and_environment')

write.csv(iss_scores,file='~/workspace/crowdpac/output/candidate_issue_positions_to_import.csv',row.names=FALSE)

# old way - (start) ----------------------------------------------------------------------
  # print(load('~/workspace/crowdpac/data/cp_rc_out.rda'))
  # is.ip <- senout$leginfo$idealPoints
  # na.mat <- senout$leginfo$na.mat
  # is.ip[na.mat == 0] <- NA

  # c.cands <- get.current.mcs()
  # m <- match((as.character(c.cands[,'ICPSR2'])),(rownames(is.ip)))
  # cip <- is.ip[m[!is.na(m)],]
  # rownames(cip) <- c.cands[!is.na(m),'bonica_rid']

  # cip.new <- cip
  # for(xx in 1:ncol(cip)){
  #   z <- cip[,xx]
  #   m <- match(c.cands[,'bonica_rid'],rownames(cip))
  #   z2 <- z[m[!is.na(m)]]
  #   qq1 <- quantile(z2,pr=c(.01,.99),na.rm=T)    
  #   z <- rescale.cf (z,qq1)

  #   out <- data.frame(c.cands[!is.na(m),'Name'],round(z2,3))
  #   out2 <- out[!is.na(z2),]
  #   z2 <- z2[!is.na(z2)]
  #   cip.new[,xx] <- z
  # }

  # cid <- rownames(cip.new)
  # m <- match(as.character(cand.master[,'bonica_rid']),cid)
  # cip1 <- cip.new[m[!is.na(m)],]

  # # remove.scores <- list(c('cand931','healthcare'),
  # #                             c('cand931','welfare_and_poverty'),
  # #                             c('cand1118','foreign_policy'),
  # #                             c('cand1118','education'),
  # #                             c('cand1013','labor'),
  # #                             c('cand1444','abortion'))
  # # for (i in remove.scores) {
  # #   cip1[rownames(cip1)==i[1],i[2]] <- NA
  # # }
  # write.csv(cip1,file='~/workspace/crowdpac/output/candidate_issue_positions_to_import.csv')
# old way - (finish) -------------------------------------------------------------------

}



get.issue.weights <- function(cand.master) {
  cip <- read.csv('~/workspace/crowdpac/output/candidate_issue_weights.csv')
  cand.master <- cand.master[!grepl('state',cand.master[,'seat'] ),]
  cid <- as.character(cip[,1])

  m <- match(as.character(cand.master[,'bonica_rid']),cid)
  cip1 <- cip[m[!is.na(m)],]
  cip1 <- cip1[!is.na(cip1[,2]),]

  write.csv(cip1,file='~/workspace/crowdpac/output/candidate_issue_weights_to_import.csv',row.names=FALSE)
}


get.issue.means <- function(cand.master) {
  # <<<<<<<<<<<<<--------------- need to add same calculation for general score !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # !!! only have issue scores for current candidates, so average is wrt candidates from all the state's races
  cip <- as.matrix(read.csv('~/workspace/crowdpac/output/candidate_issue_positions_to_import.csv'))
  
  cid <- cip[,1] # just the id
  cip <- cip[,-1] # everything but the id
  
  cc1 <- cand.master[cand.master[,'seat'] %in% c('federal:house','federal:senate') & cand.master[,'Incum_Chall'] == 'I',]

  m <- match(cc1[,'bonica_rid'],cid)
  cip <- cip[m,]
  class(cip) <- 'numeric'

  mean.dem.issue <- colMeans(cip[cc1[,'Party'] == 100,],na.rm=T)
  mean.rep.issue <- colMeans(cip[cc1[,'Party'] == 200,],na.rm=T)

  mean.dem.genl <- mean(cc1[cc1[,'Party'] == 100,'cfscore'],na.rm=T)
  mean.rep.genl <- mean(cc1[cc1[,'Party'] == 200,'cfscore'],na.rm=T)

  genl.means <- c(mean.dem.genl,mean.rep.genl)

  party.means <- cbind(mean.dem.issue,mean.rep.issue)
  issue.names <- rownames(party.means)
  party.means <- rbind(party.means,genl.means)
  rownames(party.means) <- c(issue.names,'general')

  colnames(party.means) <- c('Avg.Dem','Avg.Rep')
  write.csv(party.means,file='~/workspace/crowdpac/output/party_means_to_import.csv')
}




#######################################################################################################

# cand.master <- get.cands('~/workspace/crowdpac/states/2014/CA/list.csv','~/workspace/crowdpac/states/2014/CA/resumes.csv',2014,'2014-06-03')
cand.master <- get.cands('~/workspace/crowdpac/states/2014/general/list.csv',2014,'2014-11-04')

############ fix this
# get.issue.order(cand.master)

############ fix this
# get.issue.positions(cand.master)

# # get.issue.priorities(cand.master)

# get.issue.means(cand.master)
