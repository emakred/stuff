#!/usr/bin/env python

import json
import csv
from votesmart import votesmart

votesmart.apikey = 'e96940ccce0f498344cd0c46f66d3618'

json_data = open('/scratch1/Dropbox/crowdpac/states/2014/CA/CA_matching.json')
data = json.load(json_data)
json_data.close()




committees = []

for c in votesmart.committee.getCommitteesByTypeState(typeId='H'):
   committees.append([c.committeeId, c.name, c.parentId, c.stateId])

membership = []

for x in committees:
  committeeId = x[0].encode('ascii','ignore')
  committeeName = x[1].encode('ascii','ignore')
  try:
  	committeeParent = votesmart.committee.getCommittee(x[2].encode('ascii','ignore')).name.encode('ascii','ignore')
  	committeeFullName = committeeName + ' (' + committeeParent + ' Committee)'
  except:
  	committeeFullName = committeeName
  try:
  	members = votesmart.committee.getCommitteeMembers(committeeId)
  	for y in members:
  	  membership.append([committeeFullName,y.candidateId.encode('ascii','ignore'),y.position.encode('ascii','ignore')])
  except:
  	pass



with open('/scratch1/Dropbox/crowdpac/output/committee_members.csv','wb') as tofile:
	fileobj = csv.writer(tofile,delimiter=',')
	fileobj.writerows(membership)





# VOTING RECORD FOR MIKE HONDA
temp=votesmart.candidates.getByLastname('Honda')
id=temp[0].candidateId.encode('ascii','ignore')
votesmart.votes.getByOfficial(id)




# get election candidates
for c in votesmart.election.getElectionByYearState(2014,'TX'):
  print c, c.electionId

for c in votesmart.election.getStageCandidates(2853,'G'):
  print c.district, c.firstName, c.lastName, c.party