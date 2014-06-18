##

options("width"=400)

# source_data <- paste(path_to_Dropbox,'contributions/finished_runs/cands_79_14_current.rda', sep='')


# library('RecordLinkage')
library(RMySQL)
username = 'crowdpacdata'
passwd = 'crowdpacdata2014'
dbname = 'cfdb'
port = 5841
host = 'cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com'

con <- dbConnect(MySQL(), user=username, password=passwd, dbname=dbname, port=port, host=host)

source('~/Dropbox/contributions/api/calc_src.r')
source('~/Dropbox/contributions/api/api_utils.r')
source('~/Dropbox/contributions/api/get_prob.R')

load('~/Dropbox/contributions/api/alt_name_mat.rda')  #<- 321 records
alt.names <- as.matrix(alt.names)
load('~/Dropbox/contributions/data/commonlnames.rda') #<- 50 records
load('~/Dropbox/contributions/data/commonfnames.rda') #<- 150 records

###############################################################################
##FUNCTIONS USED DURING SEARCH
###############################################################################

display.info.bid <- function(out1){
  # out1 is a dataframe of potential matches 
  # pick off entries with bonica_id2=NA
  out1 <- out1[!is.na(out1[,'bonica_id2']),]
  # overwrite 'contributor_cfscore' rounded to 2 places
  out1[,'contributor_cfscore'] <- round(as.numeric(out1[,'contributor_cfscore']),2)
  # 
  cname <- out1[,'bonica_id2']
  # seed 'count' within 'display.info.bid'
  count <- 1
  # seed 'oo2' (grouped matches) and 'dout' (bonica_id2, count)
  oo2 <- dout <- NULL

  # for each unique bonica_id2 in the set of potential matches
  for(x in unique(cname)){

    # 'oo' is 'out1' *with* cname, *without* __?__
    oo <- unique(out1[cname==x,c('bonica_id2','cycle',
                                 'contributor_name',
                                 'contributor_address',
                                 'contributor_city',
                                 'contributor_state',
                                 'contributor_zipcode',
                                 'contributor_occupation',
                                 'contributor_employer',
                                 'recipient_name',
                                 'recipient_party',
                                 'amount',
                                 'contributor_cfscore'),])
    # save off amount to republicans
    to.reps <- as.numeric(oo[oo[,'recipient_party']== 200,'amount'])
    # save off amount to democrats
    to.dems <- as.numeric(oo[oo[,'recipient_party']== 100,'amount'])

    # 'oo1' is 'oo' without 'recipient_party'
    oo1 <- oo[,c('bonica_id2','cycle',
                 'contributor_name',
                 'contributor_address',
                 'contributor_city',
                 'contributor_state',
                 'contributor_zipcode',
                 'contributor_occupation',
                 'contributor_employer',
                 'recipient_name',
                 'amount',
                 'contributor_cfscore')]

    # calculate and add to 'oo1' percentage given to republicans
    oo1 <- as.matrix(cbind(count, oo1, sum(to.reps)/(sum(to.reps) + sum(to.dems)),table(oo[,'bonica_id2'])[1]))
    
    # add results from this 'cname' (=bonica_id2) to end of oo2
    oo2 <- rbind(oo2,oo1)

    ## ADAM COMMENTED OUT NEXT 2 ROWS
    ## m <- match(oo2[,'bonica_id2'],names(nn))
    ## oo2 <- cbind(oo2,nn[m])

    # increment counter for next 'cname' (=bonica_id2)
    count <- count + 1

    # add record with 'cname' (=bonica_id2) and 'count' to last row of 'dout'
    dout <- rbind(dout,c(x,count))
  } # end of looping on unique cname's (=bonica_id2's)


  # shorten recipient name to 20 chars
  oo2[,'recipient_name'] <- ifelse(nchar(oo2[,'recipient_name']) > 20,
                                   substr(oo2[,'recipient_name'],1,20),
                                   oo2[,'recipient_name'])
  # shorten contributor employer to 20 chars
  oo2[,'contributor_employer'] <- ifelse(nchar(oo2[,'contributor_employer']) > 20,
                                   substr(oo2[,'contributor_employer'],1,20),
                                   oo2[,'contributor_employer'])
  # shorten contributor address to 15 chars
  oo2[,'contributor_address'] <- ifelse(nchar(oo2[,'contributor_address']) > 15,
                                   substr(oo2[,'contributor_address'],1,15),
                                   oo2[,'contributor_address'])

  # remove column and row names
  colnames(oo2) <- NULL
  rownames(oo2) <- NULL
  # print matches to screen
  print(as.data.frame(oo2))
  
  return(dout)
}


update.cfscores <- function(){
  load('~/Dropbox/contributions/finished_runs/scaling_for_mapping_paper/aresults_79_12_5_25.rda')
  if(use.source == 1){
      nlist.orig <- read.csv('~/workspace/agency_ideology/appointees/wh_appointees_obama.csv',as.is=T)
  }
  comid <- as.numeric(aresults$contributors[,'comid'])
  m <- match(as.numeric(nlist.orig[,'ContribID']),comid)
  cfscore <- as.numeric(aresults$contributors[m,'oc1'])

  if(use.source == 1){
      nlist.orig[,'cfscore'] <- cfscore
      write.csv(nlist.orig,file='~/workspace/agency_ideology/appointees/wh_appointees_obama.csv')
  }

  ## holdover <- ifelse(nlist.orig[,'Holdover'],1,0)
  ## confirmed <- ifelse(is.na(nlist.orig[,'Confirmed']),0,1)
  ## year <- nlist.orig[,'Formal.Nomination.Date']
  ## second.term <- ifelse(grepl('2011',year) | grepl('2012',year),1,0)
  ## agency <- gsub('\\-',' ',(nlist.orig[,'Agency']))
  ## agency <- sapply(agency,function(x) strsplit(x,' ')[[1]][1])
  ## gave <- ifelse(is.na(cfscore),1,0)

  ## summary(lm(cfscore~second.term + agency  ))
  ## summary(lm(cfscore~second.term  ))
}


update.nlist <- function(){
  if(use.source == 1){
    m <- match(nlist.orig[,1],nlist[,1],incomparables=c('NA',''))
    nlist.orig[!is.na(m),'ContribID'] <- nlist[m[!is.na(m)],'ContribID']
    write.csv(nlist.orig,file='~/workspace/agency_ideology/appointees/wh_appointees_obama.csv')
  }
  if(use.source == 2){
    m <- match(nlist.orig[,1],nlist[,1],incomparables=c('NA',''))
    nlist.orig[!is.na(m),'ContribID'] <- nlist[m[!is.na(m)],'ContribID']
    write.csv(nlist.orig,file='~/workspace/agency_ideology/coding/wh_appointees_bush.csv')
  }
  if(use.source == 3){
    m <- match(nlist.orig[,'ICPSR2'],nlist[,'ICPSR2'],incomparables=c('NA',''))
    nlist.orig[!is.na(m),'ContribID'] <- nlist[m[!is.na(m)],'ContribID']
    write.csv(nlist.orig,file='/scratch1/Dropbox/BoD_ideology/cand_and_contrib_ids_update.csv',row.names=FALSE)
  }
}


prune.conts <- function(out1){
  cat('select pruning method: \n',
      '1: Split \n',
      '2: Name Search\n',
      '3: Location Search\n',
      '4: Occupation/Employer Search\n')
  contp <- unlist(scan(what='numeric',n=1))
  if(contp == 1){
    cname <- unique(paste(tolower(out1[,'contributor_lname']),
                          tolower(out1[,'contributor_fname']),
                          tolower(out1[,'contributor_mname']),
                          tolower(out1[,'contributor_suffix']),
                          tolower(out1[,'contributor_title']),
                          tolower(out1[,'contributor_employer']),
                          tolower(out1[,'contributor_occupation'])))
    print(cbind(1:length(cname),cname))
    split.off <- (scan(what='numeric',flush=T))
    ##CHANGE IDS BY NUMBER
    if(as.numeric(unlist(split.off)[1]) > 0){
      to.change <- sapply(as.numeric(unlist(split.off)),
                          function(x){
                              grep(unique(cname)[x],cname,ignore.case=T)
                          })
      to.change <- unlist(to.change)
      to.change <- unique(as.vector(to.change))
      trans1 <- paste("'",paste(out1[to.change,'transaction_id'],collapse="', '"),"'",sep='')

      rs <- dbSendQuery(con, paste("update newconts set bonica_id2 = ",mbnum,
                                       ", bonica_id_fix = ",mbnum,
                                   " where transaction_id IN (",trans1,")",sep=''))
      mbnum <- mbnum + 1
      print(mbnum)
    }
  } # end: if(contp == 1)

  if(contp == 2){
    ##CHANGE IDS BY NAME SEARCH
    browser()
    display.info.bid(out1)
    iname <- scan(what='character',flush=T)
    if(!is.null(iname) & length(iname) >0){
      to.change <- sapply(unlist(iname),
                          function(x){
                              grep(x,out1[,'contributor_name'],ignore.case=T)
                          })
      to.change <- unlist(to.change)
      ##
      to.change2 <- sapply(unlist(iname),
                           function(x){
                               grep(x,out1[,'contributor_employer'],ignore.case=T)
                           })
      to.change2 <- unlist(to.change2)
      ##
      to.change3 <- sapply(unlist(iname),
                           function(x){
                               grep(x,out1[,'contributor_suffix'],ignore.case=T)
                           })
      to.change3 <- unlist(to.change3)
      ##
      to.change4 <- sapply(unlist(iname),
                           function(x){
                               grep(x,out1[,'contributor_occupation'],ignore.case=T)
                           })
      to.change4 <- unlist(to.change4)
      ##
      to.change <- unique(c(as.vector(to.change),
                            as.vector(to.change2),
                            as.vector(to.change3),
                            as.vector(to.change4)))

      if(length(to.change)> 1){
        trans1 <- paste("'",paste(out1[to.change,'transaction_id'],collapse="', '"),"'",sep='')
        rs <- dbSendQuery(con, paste("update newconts set bonica_id2 = ",mbnum,
                                     ",bonica_id_fix = ",mbnum,
                                     " where transaction_id IN (",trans1,")",sep=''))
        mbnum <- mbnum + 1
      }
      if(length(to.change) == 1){
        trans1 <- paste("'",paste(out1[to.change,'transaction_id'],collapse="', '"),"'",sep='')
        rs <- dbSendQuery(con, paste("update newconts set bonica_id2 = ",mbnum,
                                     ",bonica_id_fix = ",mbnum,
                                     " where transaction_id = ",trans1,sep=''))
        mbnum <- mbnum + 1
      }
    } # end: if(!is.null(iname) & length(iname) >0)
  } # end: if(contp == 2)

  ##QUERYING WHETHER TO MOVE ON
  print('Continue to next set of records? \n')
  to.next <- unlist(scan(what='numeric',n=1))
  if(to.next == 1){
      print(c(to.next,tt))
  }
}


###############################################################################
## INITIALIZE
###############################################################################

# mbnum used if PRUNING
mbnum <- dbGetQuery(con,'select max(bonica_id2) from newconts')
mbnum <- as.numeric(unlist(mbnum)) + 1000000



cat('Which CSV file to load?:\n')
cat('0: No file (get input from console);
     1: Obama Appointees; 
     2: Bush Appointees; 
     3: Candidates as Contributors\n')
use.source <- as.numeric((scan(what='character',flush=T,n = 1)))

if(use.source == 1){
  nlist <- read.csv('~/workspace/agency_ideology/coding/wh_appointees_obama.csv',as.is=T)
  nlist <- nlist[,-1]
  nlist[,2] <- gsub("'","",nlist[,2])
  nlist.orig <- nlist
  nlist <- nlist[is.na(nlist[,'ContribID']),]
  count <- 1
}

if(use.source == 2){
  nlist <- read.csv('~/workspace/agency_ideology/coding/wh_appointees_bush.csv',as.is=T)
  nlist <- nlist[,-1]
  nlist[,'first'] <- gsub("'","",nlist[,'first'])
  nlist[,'last'] <- gsub("'","",nlist[,'last'])
  nlist.orig <- nlist
  nlist <- nlist[is.na(nlist[,'ContribID']),]
  count <- 1
}

if(use.source == 3){
  load('~/Dropbox/contributions/data/cand_all_1979_2014.rda') #<- approx 362,660 records
  # existing inventory of candidate and contributor IDs
  nlist <- read.csv('~/Dropbox/cand_cont_db/cand_and_contrib_ids_update.csv',as.is=T) #<- approx 121,749 records
  rval.out <- read.csv('~/Dropbox/contributions/data/state_fed_matched.csv',as.is=T) #<- approx 3245 records

  bonica_cand_id <- nlist[,'ICPSR2']
  # match nlist.ICPSR2 with rval.out.ICPSR2
  m <- match(bonica_cand_id,rval.out[,2],incomparables=c(NA,''))
  bid2 <- bonica_cand_id
  # where bid2 is NA, use rval.out[ICPSR2.1]
  bid2[!is.na(m)] <-  rval.out[m[!is.na(m)],3]
  nlist[,'ICPSR2'] <- bid2

  ## NONE OF THESE INDICES ARE USED ANYWHERE ?!?
  # m is match nlist.ICPSR2 (existing inventory) with cands.ICPSR2
  m <- match(nlist[,'ICPSR2'],cands[,'ICPSR2'])
  # m1 is match nlist.ICPSR2 with cands.ICPSR with year(?) removed from ID
  m1 <- match(nlist[,'ICPSR2'],substr(cands[,'ICPSR'],1,nchar(cands[,'ICPSR']) - 4))
  # add cands.ICPSR match indices to m
  m[is.na(m)] <- m1[is.na(m)]
  # m2 is match nlist.ICPRSR2 (existing inventory) with cands.Cand.ID
  m2 <- match(nlist[,'ICPSR2'],cands[,'Cand.ID'])
  # add cands.Cand.ID match indices to m
  m[is.na(m)] <- m2[is.na(m)]
  # match nlist.ICPSR2nominee (existing inventory) with cands.ICPSR2
  m3 <- match(paste(nlist[,'ICPSR2'],'nominee',sep=''),cands[,'ICPSR2'])
  # add 'nominee'/cands.ICPSR2 match indices to m
  # m is match indices for ICPSR2 (CONTRIBUTOR? = candidate or organization or individual)
  m[is.na(m)] <- m3[is.na(m)]

  # match indices for bonica_rid (RECIPIENT = candidate or organization)
  m1 <- match(nlist[,'bonica_rid'],cands[,'bonica_rid'])

  # remove single quotes from names
  nlist[,'Name'] <- gsub("'","",nlist[,'Name'])
  # archive version of nlist
  nlist.orig <- nlist
  # nlist trimmed to keep only ContribIDs that are NA 
  # (only keep individuals for who we need to get a ContribID)
  nlist <- nlist[is.na(as.numeric(nlist[,'ContribID'])),]

  # get input from console for which record number to start search loop with
  count <- as.numeric((scan(what='integer',flush=T,n = 1)))
}


###############################################################################
##SEARCH LOOP
###############################################################################
to.next <- 0
while(to.next != 1 & to.next != 'y'){

  # for 1. Obama Appointees
  if(use.source %in% c(1)){ 
    print(nlist[count,])
    aname <- nlist[count,'Name']
    ln <- strsplit(aname,", ")[[1]][1]
    ffname <- strsplit(aname,", ")[[1]][2]
    fn <- strsplit(ffname," ")[[1]][1]
    mname <- strsplit(ffname," ")[[1]][2]
    mn <- substr(mname,1,1)
    emp <- NA
    st <- NA
    mst <- ''
  }

  # for 2. Bush Appointees
  if(use.source %in% c(2)){  
    print(nlist[count,])
    ln <- nlist[count,'last']
    ffname <- nlist[count,'first']
    fn <- strsplit(ffname," ")[[1]][1]
    mname <- strsplit(ffname," ")[[1]][2]
    mn <- substr(mname,1,1)
    emp <- NA
    st <- NA
    mst <- ''
  }

  # for 3. Candidates as Contributors
  if(use.source == 3){  
    # starts loop at record# 'count' (for "current individual",
    # for whom we do not have a ContribID)
    # 'count' is captured from console just before entering search loop
    # then incremented by one at the end of each loop
    mq <- match(nlist[count,'ICPSR2'],cands[,'ICPSR2'])
    
    # print known info for current individual
    print(cands[mq,c('election','Name','Party','State','seat','District')])

    aname <- nlist[count,'Name']
    # if 'jr' in individual's name, 
    # first store it in 'suffix', then remove itfrom aname
    suffix <- ifelse(grepl('jr',aname),'jr',NA)
    aname <- gsub('jr','',aname)

    # last name is before the commas
    ln <- strsplit(aname,", ")[[1]][1]
    # remaining names
    ffname <- strsplit(aname,", ")[[1]][2]
    # split remaining names
    # first name is before the space
    fn <- strsplit(ffname," ")[[1]][1]
    # middle name is after the space
    mname <- strsplit(ffname," ")[[1]][2]
    # middle initial is first letter of middle name
    mn <- substr(mname,1,1)
    
    # unknown employer
    emp <- NA
    
    # state 
    st <- nlist[count,'State']
    
    if(length(st) >0){
      if(grepl('fed',nlist[count,'seat'])){
        # 'state' part of query, add in vacinity of DC if federal seat
        mst <- paste(" and contributor_state in ('",st,"','DC','MD','VA')",sep='')
      }else{
        # 'state' part of query if not a federal individual
        mst <- paste(" and contributor_state = '",st,"'",sep='')
      }
    }
    print(c(ln,fn,mname,mn,suffix))
  } # end: if use.source==3

  # for 0. No File
  if(use.source == 0){
    ##DOWNLOADING RECORDS
    cat('***New Query***\n ')
    cat('Last Name: ')
    ln <- as.character((scan(what='character',flush=T,n = 1)))
    cat('First Name: ')
    fn <- as.character((scan(what='character',flush=T,n = 1)))
    cat('Middle Name: ')
    mn <- as.character((scan(what='character',flush=T,n = 1)))
    cat('State: ')
    st <- as.character((scan(what='character',flush=T,n = 1)))
    if(length(st) >0){
        mst <- paste(" and contributor_state = '",st,"'",sep='')
    }
    cat('Employer: ')
    emp <- as.character((scan(what='character',flush=T,n = 1)))
    if(is.null(emp) | length(emp) == 0) emp <- NA
  }

  ######################################################################
  ## FOR ALL use.source's
  ######################################################################
  
  ##GETTING ALTERNATIVE FIRST NAMES
  fn <- tolower(fn)
  fn.orig <- fn
  # if this first name has any alternatives
  if(sum(fn %in% alt.names)>0){
    # find which alternative first names match 'count's first name
    mz <- match(fn,alt.names)
    # only keep indices matches
    mz <- mz[!is.na(mz)]
    
    mparse <- function(m1){
      if(m1 <= nrow(alt.names)){
        newname <- alt.names[m1,]
      }else{
        count.m1 <- 1
        m2 <- m1
        while(m2 > nrow(alt.names)){
          m2 <- m2 - nrow(alt.names)
          count.m1 <- count.m1 + 1
        }
        newname <- alt.names[m2,]
      }
      return(newname)
    }

    # get potential new names for each alternative
    nn <- unlist(sapply(mz,mparse))
    nn <- nn[nn != '']
    # keep only distinct firstnames
    fn <- unique(c(fn,nn))
  } # end: if (sum(fn %in% alt.names)>0)


  ######################################################################
  ##CONSTRUCTING QUERY
  ######################################################################
  
  ##LAST NAME for query
  lnq <- ifelse(length(ln)>0,
                paste(" (contributor_lname like '",ln,"%'",')',
                      ##" or match(contributor_name) against ('",ln,"' in boolean mode))",
                      sep=''),'')

  ##FIRST NAME for query
  if(length(fn) > 1){
    fn1 <- paste(fn,collapse=' ')
    fnq <- paste(" and (match(contributor_fname) against ('",fn1,"' in boolean mode) or",
                 " contributor_fname like '",fn.orig,"%')" ,sep='')
  }else{
    fnq <- ifelse(length(fn)>0,
                  paste(" and (contributor_fname like '",fn.orig,"%'",')',
                        ##" or match(contributor_name) against ('",fn,"' in boolean mode))",
                        sep=''),'')
  }

  ##EMPLOYER for query
  if(!is.na(emp)){
      if(length(emp) > 0){
          emp1 <- unlist(strsplit(emp,'\\|')[[1]])
          emp1 <- paste(emp1,collapse='" "')
      }else{
          emp1 <- emp
      }
      memp <- ifelse(length(emp) > 0, paste(" and match(contributor_occupation,contributor_employer) against ('",
                                            '"',emp1,'"',"' in boolean mode)",sep=''),'')
  }else{
      memp <- ''
  }

  ######################################################################
  ## Assemble and Execute QUERY
  ######################################################################
 
  # select 
  #   * 
  # from 
  #   newconts 
  # where 
  #   (contributor_lname like [ln%]) and 
  #   (match(contributor_fname) against 
  #     ([fn] in boolean mode) or
  #     contributor_fname like [fn.orig%]) and
  #   match(contributor_occupation,contributor_employer) against 
  #     [emp] in boolean mode) and
  #   contributor_state = [mst]

  out1 <- dbGetQuery(con,
                     paste("select * from newconts where ",lnq,fnq,memp,ifelse(length(st) > 0,mst,''),sep=''))

  ######################################################################
  ## Examine query results
  ######################################################################

  # if at least one record returned
  if(length(out1) > 0){

    ##FILTERING ON MIDDLE NAMES
    # if current person has a middle name
    if(length(mn)>0){
      # indices of query records returned
      kk <- (is.na(out1[,'contributor_mname']) |                   # if contributor's middle name is NA
             grepl(mn,out1[,'contributor_mname'],ignore.case=T) |  # or if contributor's middle name matches 'count's middle name
             out1[,'contributor_mname'] == '')                     # or if contributor's middle name is empty
      # filter to only rows with *potential* matches on middle name (match, NA or empty)
      out1 <- out1[kk,]
    }

    # if there are still potential matches in 'out1' after filtering on middle name
    if(!is.null(out1) & length(out1) > 0){
      # there are still more than one potential matches
      if(nrow(out1) > 1){
        # make into data frame
        out1 <- as.data.frame(out1,stringsAsFactors=F)

        # call function from above (also in sourced calc_src.r)
        # dib = matrix with 2 cols: 'bonica_id2' and 'count'
        #################################################################
        # (function call below prints count, info, 
        # percent to republicans, etc to console)
        #################################################################
        dib <- display.info.bid(out1)

        cat('1:merge, 2:no action(?), 3:prune')
        # get input from console (1 to Merge, 2 to __?__, 3 to Prune)
        cont <- (scan(what='numeric',flush=T,n = 1))

        # if a response was given
        if(length(cont) > 0){
          
          # if 'merge'
          if(cont == 1){
            cat('Which to Merge?')
            keep <- as.numeric((scan(what='character',flush=T)))
            bids <- paste(as.numeric(dib[keep,1]),collapse="','")

            ##DETERMINING WHICH CONTRIBUTOR_ID TO ASSIGN
            bids2 <- as.numeric(out1[,'bonica_id2'])
            nx <-  table(bids2[bids2 %in% dib[keep,1]])
            newid <- names(nx)[which.max(nx)]

            ##UPDATING MYSQL
            rs <- dbSendQuery(con, paste("update newconts set bonica_id2 = ",newid,
                                         " where bonica_id2 in ('",bids,"')",sep=''))
            rs <- dbSendQuery(con, paste("update newconts set bonica_id_fix = ",newid,
                                         " where bonica_id2 in ('",bids,"')",sep=''))

            ##UPDATING NLIST
            print(newid)
            if(use.source> 0){nlist[count,'ContribID'] <- newid}
          } # end: if (cont==1)
          
          # if 'no action'(?)
          if(cont == 2){
            nlist[count,'ContribID'] <- 'NA'
          } # end: if (cont==2)
          
          # if 'prune'
          if(cont == 3){
            cat('Select Entry to Prune:')
            keep <- as.numeric((scan(what='character',flush=T)))
            bids <- paste(as.numeric(dib[keep,1]),collapse="','")
            out2 <- out1[out1[,'bonica_id2'] == bids,]
            prune.conts(out2)
          } # end: if(cont==3)

        } # end: if(length(cont) > 0)... if console response to merge/?/prune
      } # end: if(nrow(out1) > 1)...if MULTIPLE query responses after filtering
    } # end: if(!is.null(out1) & length(out1) > 0)...if ANY query responses even after filtering
  } # end: if(length(out1) > 0)...if query response

  # if 1: Obama Appointees
  if(use.source== 1){
    if(is.na(nlist[count,9])){nlist[count,'ContribID'] <- -1}
    ##ADAM commented out row below
    ##update.nlist()
    write.csv(nlist,file='~/Dropbox/tmp/nlist.csv')
    count <- count + 1
  }

  # if 2: Bush Appointees
  if(use.source== 2){
    ##ADAM commented out row below
    ##if(is.na(as.numeric(nlist[count,'ContribID']))){nlist[count,'ContribID'] <- NA}
    write.csv(nlist,file='~/workspace/tmp/nlist2.csv')
    count <- count + 1
  }

  # if 3: Candidates as Contributors
  if(use.source== 3){
    # don't increment counter, go into debug mode to play with variables
    # ?: can also set to.next='y' or to.next=1
    browser()
  }

  to.next <- 0

} # end of SEARCH LOOP: while(to.next != 1 & to.next != 'y')



## ADAM commented out the rows from here to end
## library(RCurl)
## oo <- getURL("http://apis.pipl.com//search/v2/json/?first_name=jean&last_name=millon&state=AZ&city=phoenix&person_mode=one&exact_name=0&no_sponsored=0&key=ka5rhn9n59fc5vya4b8zas33")
## oo1 <- fromJSON(oo)
## oo1$records[[1]]$addresses
## oo1$results$records[[4]]$`@match_score`








