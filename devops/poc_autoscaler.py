# Very possibly the world's most hacked-together autoscaler, intended only to demonstrate how one might
# use the /load, /timetodrain and /dogracefulshutdown endpoints to autoscale a service based on the load.
# When downscaling, this preferentially deletes the fileserver with the lowest expected time to drain.

from subprocess import check_output
import requests

def listFileservers():
    r = check_output(["gcloud","compute","instance-groups","unmanaged", "list-instances", "mytransfer-fileservers"])
    fs = str(r,'utf-8')[:-1].split('\n')[1:] #convert to utf-8, drop final newline, split into lines and drop column names
    servers = dict([tuple(x.split()) for x in fs])
    return servers

def deleteServer():
    

print( listFileservers())

'''
while(True):
    fileservers = listFileservers() # returns a dict 
    #fileservers contains PROVISIONING, RUNNING and TERMINATED instances
    terminatedServers = [k for k, v in fileservers.items() if v == 'TERMINATED'] # only those which are TERMINATED
    for s in terminatedServers:
        gcloud compute instances delete s
    activeServers = (servers.filter(isRunning)) - drainingServers
    load = 0
    capacity = 0
    for s in activeServers:
        load += curl (s + "/load")
        capacity += Constants.ServerCapacity //assuming all instances are equal size this is a constant
    averageload = load/capacity
    if averageload > 0.8:
        //this scales up one at a time, it is also possible to scale up multiple servers at a time if load is super high
        name = generateNewInstanceName
        gcloud compute instances create name
    if averageload < 0.6: #load is low, kill an instance unless it's the last one
        candidates = []
        for s in activeServers:
            edt = curl (s + "/timetodrain") 
            candidates.append((s,edt))
'''