options(width=400)

# source_data <- paste(path_to_Dropbox,'contributions/finished_runs/cands_79_14_current.rda', sep='')
source_data <- paste(path_to_Dropbox,'/contributions/data/cand_all_1979_2014.rda', sep='')


if (!exists('cands')) {
	load(source_data)
}

cands[,'election'] <- substr(cands[,'election'],nchar(cands[,'election']) -3, nchar(cands[,'election']))


l <- list()


l[[length(l)+1]] <- c('gooch','lesli')
l[[length(l)+1]] <- c('laird','steven')



## add in scores below and Cand.IDs

for (i in 1:length(l)) {
	print(paste("candidate is:",l[[i]][2],l[[i]][1],sep=" "))
	temp1 <- cands[grepl(l[[i]][1],cands[,'Name'],ignore.case=TRUE),c('bonica_rid','Name','state','Party','District','seat','election','Cand.ID','oc1','FEC.ID','str','str2','city','state','zip')]
	if (length(temp1)<30) {
		# print('in first if statement')
		if (length(temp1)<14) {
			print("no matches")
		} else if (length(temp1)==14) {
			print(temp1)
		} else {
			print(temp1[order(temp1[,2]),])
		}	
	} else {
		#print('in else of first if statement')
		temp2 <- temp1[grepl(l[[i]][2],temp1[,'Name'],ignore.case=TRUE),]
		if (length(temp2)>15) {
			#print('in second if statement')
			print(temp2[order(temp2[,2]),])
		} else if (length(temp2)==15) {
			#print('in else of second if statement')
			print(temp2)
		} else {
			print("no matches using first and last name")
			print(temp1[order(temp1[,2]),])
		}
	fromdisp <- scan(what='character',flush=T)
	}
}
