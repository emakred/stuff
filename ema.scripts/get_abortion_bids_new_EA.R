rm(list=ls(all=TRUE))
gc()
library(foreign)
library(MASS)
# ADAM commented out next line
##library(ggplot2)
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
ndups <- 1                                  # not used in this script
ndups.use <- 1                              # not used in this script
num.cont <-  1                              # not used in this script
individuals <- TRUE
pacs <- TRUE                                # not used in this script
include.states <- TRUE
include.527s <- FALSE                       # not used in this script
cands.only <- FALSE
direct.only <- TRUE                         # not used in this script
ncores <- 0                                 # not used in this script
exclude.unitem <- FALSE                     # not used in this script
exclude.gen.elect <- FALSE                  # not used in this script
out.of.state <- FALSE                       # not used in this script
convert.to.count <- TRUE                    # not used in this script
cultural.only <- TRUE                       # not used in this script

##OTHER ARGUMENTS
ps.cont.ids <- FALSE ##period specific contributor ids # not used in this script
remove.candidate.pacs <- FALSE              # not used in this script
remove.party.cmtes <- TRUE                  # not used in this script
remove.presidential.candidates <- FALSE     # not used in this script
combine.prim.general <- TRUE                # conditionally overwritten below
remove.corporate.pacs <- FALSE              # not used in this script
match.leadership.pacs <- FALSE              # not used in this script
summarize.contrib.sources <- TRUE           # not used in this script
use.only.linked.observations <- TRUE        # not used in this script
save.progress <- TRUE                       # not used in this script

##RECIPIENT ARGUMENTS
pacs.as.recipients <- TRUE                  # not used in this script
include.pacs.as.nrc <- FALSE                # not used in this script
ideological.subset <- NULL ##'Abortion'     # not used in this script
only.party.cmtes <- FALSE                   # not used in this script
constrain.cc.ips <- FALSE                   # not used in this script

##AWM CONSTRAINT 
normalize <- TRUE                           # not used in this script
rank.order <- FALSE                         # not used in this script

##CHANGE THIS TO A CATCH-ALL VARIABLE OF RESTRICTIONS
subset.cands <- NULL                        # not used in this script
subset.contribs <- NULL                     # not used in this script

# OTHERS
# if(individuals | include.states){organize.cmdat <- FALSE}
# split.inc.chall <- organize.cmdat <- ((length(evect) > 2))
organize.cmdat <- FALSE

dynamic.cands <- ((length(evect) > ifelse(combine.prim.general,2,1)))

if(include.states){
  state.subset <- c("DC", "AK", "AL", "AR", "AZ", "CA", "CO",
                    "CT", "DE", "FL", "GA", "HI", "IA",
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
awm.options <- get.awm.list()  # part of src/utils.R
attach(awm.options)            # bring vars into workspace without needing 'awm.options$'


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


cult.pacs <- read.csv('~/workspace/crowdpac/data/issue_advocacy_orgs/cultural_pacs.csv')
prochoicepacs <- cult.pacs[cult.pacs[,'Issue'] %in% c('Abortion policy-Pro-Choice',
                                                      'Abortion Policy-Pro-Choice'),]

prolifepacs <- cult.pacs[cult.pacs[,'Issue'] %in% c('Abortion policy-Pro-Life',
                                                    'Abortion Policy-Pro-Life'),]

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
##PRO-CHOICE
###############################################################################
cme <- cmdat[grepl('J7150',cmdat$indcode),]
plp <- unique(c(as.character(cme[,'comname']),
                as.character(cme[,'conorgname']),
                as.character(cme[,'name_short']),
                as.character(prochoicepacs[,'Name']),
                as.character(prochoicepacs[,'affiliate']),
                as.character(prochoicepacs[,'ultorg'])))
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
opc <- unique(c('planned parenthood','naral','pro-choice','emilys list',
                "Zero Population Growth",'right to choose','pro choice',
                "Voters For Choice","reproductive rights","abortion rights",
                'american civil liberties union','aclu',
                plp))


o.prochoice <- NULL
for(x in opc){
    o.prochoice <- unique(c(o.prochoice,unlist(get.name.org(x,rel.vars))))
    print(x)
    # ERIN commented out next line
    #if(4110316013 %in% o.prochoice){browser()}
    print(length(o.prochoice))
}
opc = unique(o.prochoice)

# org3 query took about 4 hours for abortion
org3 <- dbGetQuery(con,
                   paste("select ",rel.vars1," from newconts where",
                         ' contributor_category = "J7150"',
                         ' or recipient_category = "J7150"',
                         sep=''))

opc <- unique(c(opc,org3[,'bonica_id2']))
save(opc,file='~/workspace/crowdpac/data/prochoice_bids.rda')


###############################################################################
##PROLIFE
###############################################################################
cmc <- cmdat[grep('J7120',cmdat$indcode),]
plc <- unique(c(as.character(cmc[,'comname']),
                as.character(cmc[,'conorgname']),
                as.character(cmc[,'name_short']),
                as.character(prolifepacs[,'Name']),
                as.character(prolifepacs[,'affiliate']),
                as.character(prolifepacs[,'ultorg'])))
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
opl <- unique(c('family research','right to life','pro-life','prolife',
                'traditional values','conservative coalition',
                'liberty university','eagle forum','faith and action',
                'Campaign for Working Families',"Discovery Institute",
                "Free Congress Foundation","pro life",'elect life',
                "Susan B Anthony List",
                "government is not god",'liberty counsel','american life league',
                "Republican National Coalition for Life",
                "Operation Rescue National",
                "Libertarians for Life",
                "Pro-Life Alliance of Gays & Lesbians",
                "National Coalition for Life and Peace",
                "Exodus International",plc))

o.prolife <- NULL
for(x in opl){
    o.prolife <- unique(c(o.prolife,as.character(unlist(get.name.org(x,rel.vars)))))
    print(c(x,length(o.prolife)))
}
opl = unique(o.prolife)
org4 <- dbGetQuery(con,
                   paste("select ",rel.vars1," from newconts where",
                         ' contributor_category = "J7120"',
                         ' or recipient_category = "J7120"',sep=''))
opl <- unique(c(opl,org4[,'bonica_id2']))
save(opl,file='~/workspace/crowdpac/data/prolife_bids.rda')



###############################################################################
##ALL ABORTION -- ERIN: all abortion not used in rest of model
###############################################################################
# cma <- cmdat[cmdat$indcode %in% c("J7120","J7150"),]
# cca2 <- cands[cands[,'indcode'] %in% c("J7120","J7150"),]
# opp <- unique(c(cma[,'comname'],
#                 cma[,'conorgname'],
#                 cma[,'name_short'],
#                 cca2[,'Name'],'abortion'))
# opp <- opp[nchar(opp) > 3]
# opp <- opp[!grepl('none',opp,ignore.case=T)]
# opp <- opp[!grepl('blank',opp,ignore.case=T)]
# opp <- gsub('"','',opp)
# opp <- gsub("'",'',opp)
# opp <- gsub("\\.",'',opp)
# opp <- gsub("\\-",' ',opp)
# opp <- gsub("\\/",' ',opp)
# opp <- gsub('\\(','',opp)
# opp <- gsub('\\)','',opp)

# o.other <- NULL
# for(x in opp){
#     o.other <- unique(c(o.other,as.character(unlist(get.name.org(x,rel.vars)))))
#     print(x)
#     print(length(o.other))
# }
# opo = unique(o.other)
# org5 <- dbGetQuery(con,
#                    paste("select ",rel.vars1," from newconts where",
#                          ' contributor_category = "J7120" or ',
#                          ' recipient_category = "J7150"',sep=''))
# opo <- unique(c(opo,org5[,'bonica_id2'],opc,opl))
# save(opo,file='~/workspace/crowdpac/data/other_abortion_bids.rda')




###############################################################################
##STEP TWO
###############################################################################


##get from org in db:
print(load('~/workspace/crowdpac/data/prochoice_bids.rda'))
print(load('~/workspace/crowdpac/data/prolife_bids.rda'))

cult.pacs <- read.csv('~/workspace/crowdpac/data/issue_advocacy_orgs/cultural_pacs.csv',as.is=T)
prochoicepacs <- cult.pacs[cult.pacs[,'Issue'] %in% c('Abortion policy-Pro-Choice',
                                                      'Abortion Policy-Pro-Choice'),]
# ERIN commented out, not used
# m <- match(prochoicepacs[,'ICPSR'],cands[,'ICPSR'],incomparables=c('',NA))
# prochoicepacs$bonica_rid <- cands[m,'bonica_rid']

prolifepacs <- cult.pacs[cult.pacs[,'Issue'] %in% c('Abortion policy-Pro-Life',
                                                    'Abortion Policy-Pro-Life'),]

# ERIN commented out, not used
# m <- match(prolifepacs[,'ICPSR'],cands[,'ICPSR'],incomparables=c('',NA))
# prolifepacs$bonica_rid <- cands[m,'bonica_rid']


###############################################################################
##PRO CHOICE
###############################################################################
newid <- cme[!(cme[,'comid'] %in% prochoicepacs[,'ICPSR2']),'comid']
newey <- cme[!(cme[,'comid'] %in% prochoicepacs[,'ICPSR2']),'eyear']
nm <- matrix('',length(newid),ncol(prochoicepacs))
colnames(nm) <- colnames(prochoicepacs)
nm[,'ICPSR'] <- paste(newid,newey,sep='')
nm[,'ICPSR2'] <- newid
prochoicepacs <- rbind(prochoicepacs,nm)

qq <- opc
for(xx in 1:nrow(prochoicepacs)){
    p <- as.character(prochoicepacs[xx,'ICPSR'])
    p1 <- as.character(prochoicepacs[xx,'ICPSR2'])
    p2 <- as.character(prochoicepacs[xx,'bonica_rid'])

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
    if(!is.na(p2)){
        cts4 <- dbGetQuery(con,
                           paste("select bonica_id2",
                                 " from newconts",
                                 " where bonica_rid = '",p2,"'",
                                 sep=''))

        qq <- unique(c(qq,unlist(cts),unlist(cts2),unlist(cts3),unlist(cts4)))
    }else{
        qq <- unique(c(qq,unlist(cts),unlist(cts2),unlist(cts3)))
        cts4 <- NULL
    }
    print(c(xx,p,length(unique(qq))))
}
opc2 <- qq
save(opc2,file='~/workspace/tmp/prochoice_bids2.rda')

###############################################################################
##PRO-LIFE
###############################################################################
newid <- cmc[!(cmc[,'comid'] %in% prolifepacs[,'ICPSR2']),'comid']
newey <- cmc[!(cmc[,'comid'] %in% prolifepacs[,'ICPSR2']),'eyear']
nm <- matrix('',length(newid),ncol(prolifepacs))
colnames(nm) <- colnames(prolifepacs)
nm[,'ICPSR'] <- paste(newid,newey,sep='')
nm[,'ICPSR2'] <- newid
prolifepacs <- rbind(prolifepacs,nm)
qq2 <- opl
for(xx in 1:nrow(prolifepacs)){
    p <- as.character(prolifepacs[xx,'ICPSR'])
    p1 <- as.character(prolifepacs[xx,'ICPSR2'])
    p2 <- as.character(prolifepacs[xx,'bonica_rid'])

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

    if(!is.na(p2)){
        cts4 <- dbGetQuery(con,
                           paste("select bonica_id2",
                                 " from newconts",
                                 " where bonica_rid = '",p2,"'",
                                 sep=''))

        qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3),unlist(cts4)))
    }else{
        qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3)))
        cts4 <- NULL
    }

    print(c(xx,p,p1,length(unique(qq2))))
}
opl2 <- qq2
save(opl2,file='~/workspace/tmp/prolife_bids2.rda')


###############################################################################
##ALL ABORTION -- ERIN: all abortion not used in rest of model
###############################################################################
# newid <- cma[!(cma[,'comid'] %in% cult.pacs[,'ICPSR2']),'comid']
# newey <- cma[!(cma[,'comid'] %in% cult.pacs[,'ICPSR2']),'eyear']
# nm <- matrix('',length(newid),ncol(cult.pacs))
# colnames(nm) <- colnames(cult.pacs)
# nm[,'ICPSR'] <- paste(newid,newey,sep='')
# nm[,'ICPSR2'] <- newid
# cult.pacs <- rbind(cult.pacs,nm)
# qq2 <- opo
# for(xx in 1:nrow(cult.pacs)){
#     p <- as.character(cult.pacs[xx,'ICPSR'])
#     p1 <- as.character(cult.pacs[xx,'ICPSR2'])
#     p2 <- as.character(prolifepacs[xx,'bonica_rid'])
#     cts <- dbGetQuery(con,
#                       paste("select bonica_id2",
#                             " from newconts",
#                             " where recipient_ext_id = '",p,"'",sep=''))
#     cts2 <- dbGetQuery(con,
#                        paste("select bonica_id2",
#                              " from newconts",
#                              " where recipient_ext_id = '",p1,"'",sep=''))

#     cts3 <- dbGetQuery(con,
#                       paste("select bonica_id2",
#                             " from newconts",
#                             " where recipient_ext_id like '",p1,"%'",
#                             " and seat like 'fed%'",
#                             sep=''))

#     if(!is.na(p2)){
#         cts4 <- dbGetQuery(con,
#                            paste("select bonica_id2",
#                                  " from newconts",
#                                  " where bonica_rid = '",p2,"'",
#                                  sep=''))

#         qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3),unlist(cts4)))
#     }else{
#         qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3)))
#         cts4 <- NULL
#     }


#     qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3)))
#     print(c(xx,p,p1,length(unique(qq2))))
# }
# opo2 <- unique(c(opo,opc2,opl2))
# save(opo2,file='~/workspace/contributions/data/issue_specific/abortion.rda')


