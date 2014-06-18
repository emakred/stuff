rm(list=ls(all=TRUE))
gc()
library(foreign)
library(MASS)
library(Matrix)
library(foreach)
setwd('~/workspace/contributions')

source('src/utils.r')
reload()

library(RMySQL)

con <- dbConnect(MySQL(), user="crowdpacdata", password="crowdpacdata2014", dbname='cfdb',
                 host='cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com', port=5841)

e.index <- c(96,96,97,97,98,98,99,99,100,100,
             101,101,102,102,103,103,104,104,
             105,105,106,106,107,107,108,108,
             109,109,110,110,111,111,112,112,
             113,113)

names(e.index) <- as.character(1979:2014)
evect.all <- c(1979:2014)
evect <- evect.all[(c(1979:2014)-1978)]
evect1 <- substr(evect,3,4)

cctypes <- c(
             '24K',  ##CONTRIBUTION MADE TO NON-AFFILIATED
             '24Z',  ##IN-KIND CONTRIBUTION MADE TO REGISTERED FILER
             '24E',  ##INDEPENDENT EXPENDITURE FOR
             '24C',  ##COORDINATED EXPENDITURE
             '24F'   ##COMMUNICATION COST FOR CANDIDATE (C7)
             )

###############################################################################
##SPECIFYING ARGUMENTS THAT WILL BE PASSED TO prep_scale()
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
source('/workspace/contributions/analysis/db_scripts.R')
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

biz <- cmdat[grepl('G1',cmdat$indcode) | grepl('F1',cmdat$indcode) | grepl('X4',cmdat$indcode) | grepl('F2',cmdat$indcode),]

biz.pacs <- biz[!grepl('LG',biz[,'indcode']),]
