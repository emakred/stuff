rm(list=ls(all=TRUE))
gc()
library(foreign)
library(MASS)
# ADAM commented out next line
##library(ggplot2);
library(Matrix)
library(foreach)
setwd('~/workspace/contributions')

source('src/utils.r')
reload()

library(RMySQL)

con <- dbConnect(MySQL(), user="crowdpacdata", password="crowdpacdata2014", dbname='cfdb',
                 host='cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com', port=5841)

# ERIN modified e.index, evect.all, evect to add
# 113 (2013), 113 (2014)
e.index <- c(96, 96, 97, 97, 98, 98, 99, 99,
             100,100,101,101,102,102,103,103,
             104,104,105,105,106,106,107,107,
             108,108,109,109,110,110,111,111,
             112,112,113,113)

names(e.index) <- as.character(1979:2014)
evect.all <- c(1979:2014)
evect <- evect.all[(c(1979:2014)-1978)]  # same as evect.all
evect1 <- substr(evect,3,4)

cctypes <- c(
             '24K',  ##CONTRIBUTION MADE TO NON-AFFILIATED
             '24Z',  ##IN-KIND CONTRIBUTION MADE TO REGISTERED FILER
             '24E',  ##INDEPENDENT EXPENDITURE FOR
             '24C',  ##COORDINATED EXPENDITURE
             '24F'   ##COMMUNICATION COST FOR CANDIDATE (C7)
             )

###############################################################################
## ADAM: SPECIFYING ARGUMENTS THAT WILL BE PASSED TO prep_scale()
## ERIN: prep_scale() never called, but a few of these variables used in the script
###############################################################################
##DATA ARGUMENTS
ndups <- 1
ndups.use <- 1
num.cont <-  1
individuals <- TRUE
pacs <- TRUE
include.states <- TRUE
include.527s <- FALSE
cands.only <- FALSE
direct.only <- TRUE
ncores <- 0
exclude.unitem <- FALSE
exclude.gen.elect <- FALSE
out.of.state <- FALSE
convert.to.count <- TRUE
cultural.only <- TRUE

##OTHER ARGUMENTS
ps.cont.ids <- FALSE ##period specific contributor ids
remove.candidate.pacs <- FALSE
remove.party.cmtes <- TRUE
remove.presidential.candidates <- FALSE
combine.prim.general <- TRUE
remove.corporate.pacs <- FALSE
match.leadership.pacs <- FALSE
summarize.contrib.sources <- TRUE
use.only.linked.observations <- TRUE
save.progress <- TRUE

##RECIPIENT ARGUMENTS
pacs.as.recipients <- TRUE
include.pacs.as.nrc <- FALSE
ideological.subset <- NULL ##'Abortion'
only.party.cmtes <- FALSE
constrain.cc.ips <- FALSE

##AWM CONSTRAINT
normalize <- TRUE
rank.order <- FALSE

##CHANGE THIS TO A CATCH-ALL VARIABLE OF RESTRICTIONS
subset.cands <- NULL
subset.contribs <- NULL

## OTHERS
# if(individuals | include.states){organize.cmdat <- FALSE}
# split.inc.chall <- organize.cmdat <- ((length(evect) > 2))
organize.cmdat <- FALSE

dynamic.cands <- ((length(evect) > ifelse(combine.prim.general,2,1)))

if(include.states){
  state.subset <- c("DC", "AK", "AL", "AR", "AZ", "CA", "CO",
                    "CT", "DE", "FL", "GA","HI", "IA",
                    "ID", "IL", "IN", "KS", "KY", "LA",
                    "MA", "MD", "ME", "MI", "MN", "MO",
                    "MS", "MT", "NC", "ND", "NE", "NH",
                    "NJ", "NM", "NV", "NY", "OH", "OK",
                    "OR", "PA", "RI", "SC", "SD", "TN",
                    "TX", "UT", "VA", "VT", "WA", "WI",
                    "WV", "WY")
  # split.inc.chall <- FALSE
  dynamic.cands <- TRUE
  combine.prim.general <- TRUE
  organize.cmdat <- FALSE
}else{
  state.subset=NULL
}

split.inc.chall <- FALSE

##COMBINING ARGUMENTS INTO A LIST THAT WILL BE PASSED TO prep_scaling()
awm.options <- get.awm.list()
attach(awm.options)


###############################################################################
##CALLING prep.scaling()x
###############################################################################
load('~/workspace/contributions/data/cand_all_1979_2014.rda')
load('~/workspace/contributions/data/cmdat_all_1979_2014.rda')

if(!include.states){
  cands <- cands[cands[,'election'] %in% evect,]
  print(table(cands[,'election']))
}

# ADAM commented out next 14 lines
## isector <- cands[,'Sector.Long']
## isector[is.na(isector)] <- ''
## use.sector <- (is.na(isector) | (isector %in% c('Joint Candidate Cmtes','Human Rights',
##                                                 'Ideological/Single-Issue',
##                                                 'Party Cmtes','Unknown')))
## cands <- cands[(!(cands[,'igcat'] %in% c('C','L','T','V')) |  use.sector) |
##                cands[,'cand_or_committee'] =='CAND' |
##                cands[,'seat'] %in% c('federal:house','federal:senate',
##                                      'federal:president','federal:527',
##                                      'state:governor','state:committee',
##                                      'state:judicial','state:judicial:lower',
##                                      'state:lower','state:office',
##                                      'state:office:da','state:office:sheriff',
##                                      'state:upper',''),]

# ADAM commented out next line
##cands <- cands[!grep(("emily's list",cands[,'Name'],ignore.case=T),]
cands <- cands[!grepl('actblue',cands[,'Name'],ignore.case=T),]
cands <- cands[!grepl('america coming',cands[,'Name'],ignore.case=T),]
cands <- cands[!grepl('america coming',cands[,'Name'],ignore.case=T),]
cands <- cands[!(grepl('moveon',cands[,'Name'],ignore.case=T) &
                 cands[,'seat'] == 'federal:committee'),]

if(split.inc.chall & length(evect) > 2){
  cands <- get.enter.exit(cands,e.index,evect)
  cands <- split.ic(cands,e.index,evect)
  save(cands,file='~/workspace/tmp/cands_split.rda')
  load('~/workspace/tmp/cands_split.rda')
}

if(cands.only){
  cands <- cands[cands[,'cand_or_committee'] =='CAND',]
}

setwd('~/workspace/contributions/')
source('src/utils.r')
source('analysis/polarization/polarization_utils.r')
source('~/workspace/contributions/analysis/db_scripts.R')
reload()


get.name.org <- function(orgname,rel.vars){

    rval <- NULL
    for( x in orgname){
        org <- dbGetQuery(con,
                          paste("select ",rel.vars," from newconts where",
                                ' match(organization_name,parent_organization_name) against (',
                                "+'" , '"',x,'"' ,"'", "in boolean mode)",
                                sep=''))
        org2 <- dbGetQuery(con,
                           paste("select ",rel.vars," from newconts where",
                                 ' match(contributor_occupation, contributor_employer) against (',
                                 "+'" , '"',x,'"' ,"'", "in boolean mode)",
                                 sep=''))
        # this query is suffering from fulltext search memory leak
        org3 <- dbGetQuery(con,
                           paste("select ",rel.vars," from newconts where",
                                 ' match(contributor_name) against (',
                                 "+'" , '"',x,'"' ,"'", "in boolean mode)",
                                 sep=''))
        rval <- c(rval,org,org2,org3)
        # ADAM commented out next line
        ##if(x == 'conservation'){browser()}
    }
    return(rval)
}


cult.pacs <- read.csv('~/workspace/contributions/data/issue_specific/cultural_pacs.csv')
promarriagepacs <- cult.pacs[cult.pacs[,'Issue'] == 'Same-sex marriage-Support',]
profamilypacs <- cult.pacs[cult.pacs[,'Issue'] == 'Same-sex marriage-Opposed',]




# ADAM commented out next 5 lines
## rel.vars <- paste("cycle,transaction_id,bonica_id2,contributor_name,",
##                   " contributor_type, amount, recipient_name,",
##                   " seat, contributor_employer, contributor_occupation, date,",
##                   " organization_name, parent_organization_name, recipient_type,",
##                   " recipient_ext_id, recipient_party",sep='')
rel.vars <- 'bonica_id2'
rel.vars1 <- paste('bonica_id2, ','contributor_name, ','contributor_occupation, ',
              'contributor_employer, ','organization_name, ',
              'contributor_category',sep='')

###############################################################################
##PRO-SSM
###############################################################################

plp <- unique(c(as.character(promarriagepacs[,'Name']),
                as.character(promarriagepacs[,'affiliate']),
                as.character(promarriagepacs[,'ultorg'])))
plp <- plp[nchar(plp) > 3]
plp <- plp[!grepl('none',plp,ignore.case=T)]
plp <- plp[!grepl('blank',plp,ignore.case=T)]
plp <- gsub('"','',plp)
plp <- gsub("'",'',plp)
plp <- gsub("\\.",'',plp)
plp <- gsub("\\-",' ',plp)
plp <- gsub("\\/",' ',plp)
plp <- gsub('\\(','',plp)
plp <- gsub('\\)','',plp)
opc <- c('marriage equality','glbt','lesbian',plp)


o.promarriage <- NULL
for(x in opc){
    o.promarriage <- unique(c(o.promarriage,unlist(get.name.org(x,rel.vars))))
    print(x)
    # ERIN commented out next line
    # if("4930330946" %in% o.promarriage){browser()}
    print(length(o.promarriage))
}
opc = unique(o.promarriage)
save(opc,file='~/workspace/tmp/promarriage_bids.rda')


###############################################################################
##PRO-FAMILY
###############################################################################
plc <- unique(c(as.character(profamilypacs[,'Name']),
                as.character(profamilypacs[,'affiliate']),
                as.character(profamilypacs[,'ultorg'])))
plc <- plc[nchar(plc) > 3]
plc <- plc[!grepl('none',plc,ignore.case=T)]
plc <- plc[!grepl('blank',plc,ignore.case=T)]
plc <- gsub('"','',plc)
plc <- gsub("'",'',plc)
plc <- gsub("\\.",'',plc)
plc <- gsub("\\-",' ',plc)
plc <- gsub("/",' ',plc)
plc <- gsub('\\(','',plc)
plc <- gsub('\\)','',plc)
opl <- unique(c('focus on the family',
                'family research',
                'traditional values',
                'conservative coalition',
                'liberty university','eagle forum','faith and action',plc))


o.profamily <- NULL
for(x in opl){
    o.profamily <- unique(c(o.profamily,as.character(unlist(get.name.org(x,rel.vars)))))
    print(c(x,length(o.profamily)))
}
opl = unique(o.profamily)
save(opl,file='~/workspace/tmp/profamily_bids.rda')



###############################################################################
##ALL SSM
###############################################################################



###############################################################################
##STEP TWO
###############################################################################


##get from org in db:
print(load('~/workspace/tmp/promarriage_bids.rda'))
print(load('~/workspace/tmp/profamily_bids.rda'))

cult.pacs <- read.csv('~/workspace/contributions/data/issue_specific/cultural_pacs.csv')
promarriagepacs <- cult.pacs[cult.pacs[,'Issue'] == 'Same-sex marriage-Support',]
profamilypacs <- cult.pacs[cult.pacs[,'Issue'] == 'Same-sex marriage-Opposed',]


###############################################################################
##PRO-SSM
###############################################################################
qq <- opc
for(xx in 1:nrow(promarriagepacs)){
    p <- as.character(promarriagepacs[xx,'ICPSR'])
    p1 <- as.character(promarriagepacs[xx,'ICPSR2'])
    p2 <- as.character(promarriagepacs[xx,'bonica_rid'])

    cts <- dbGetQuery(con,
                      paste("select bonica_id2",
                            " from newconts",
                            " where recipient_ext_id = '",p,"'",sep=''))
    cts2 <- dbGetQuery(con,
                       paste("select bonica_id2",
                             " from newconts",
                             " where recipient_ext_id = '",p1,"'",sep=''))
    cts3 <- dbGetQuery(con,
                       paste("select bonica_id2",
                             " from newconts",
                             " where recipient_ext_id like '",p1,"%'",
                             " and seat like 'fed%'",
                            sep=''))
    # if(!is.na(p2)){
    #     cts4 <- dbGetQuery(con,
    #                        paste("select bonica_id2",
    #                              " from newconts",
    #                              " where bonica_rid = '",p2,"'",
    #                              sep=''))

    #     qq <- unique(c(qq,unlist(cts),unlist(cts2),unlist(cts3),unlist(cts4)))
    # } else {
      qq <- unique(c(qq,unlist(cts),unlist(cts2),unlist(cts3)))
      cts4 <- NULL
    # }
    print(c(xx,p,length(unique(qq))))
}
opc2 <- qq
save(opc2,file='~/workspace/tmp/promarriage_bids2.rda')

###############################################################################
##PRO-FAMILY
###############################################################################
qq2 <- opl
for(xx in 1:nrow(profamilypacs)){
    p <- as.character(profamilypacs[xx,'ICPSR'])
    p1 <- as.character(profamilypacs[xx,'ICPSR2'])
    p2 <- as.character(profamilypacs[xx,'bonica_rid'])

    cts <- dbGetQuery(con,
                      paste("select bonica_id2",
                            " from newconts",
                            " where recipient_ext_id = '",p,"'",sep=''))
    cts2 <- dbGetQuery(con,
                       paste("select bonica_id2",
                             " from newconts",
                             " where recipient_ext_id = '",p1,"'",sep=''))

    cts3 <- dbGetQuery(con,
                      paste("select bonica_id2",
                            " from newconts",
                            " where recipient_ext_id like '",p1,"%'",
                            " and seat like 'fed%'",
                            sep=''))

    # if(!is.na(p2)){
    #     cts4 <- dbGetQuery(con,
    #                        paste("select bonica_id2",
    #                              " from newconts",
    #                              " where bonica_rid = '",p2,"'",
    #                              sep=''))

    #     qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3),unlist(cts4)))
    # } else {
        qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3)))
    #     cts4 <- NULL
    # }

    print(c(xx,p,p1,length(unique(qq2))))
}
opl2 <- qq2
save(opl2,file='~/workspace/tmp/profamily_bids2.rda')

