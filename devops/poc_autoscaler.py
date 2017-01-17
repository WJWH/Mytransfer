# Very possibly the world's most hacked-together autoscaler, intended only to demonstrate how one might
# use the /load, /timetodrain and /dogracefulshutdown endpoints to autoscale a service based on the load.
# When downscaling, this preferentially deletes the fileserver with the lowest expected time to drain.

from subprocess import check_output
import requests
import uuid

serverCapacity = 62500000 # in bytes/sec, set to actual capacity when deciding on an instance type to use
#current value is for f1-micro instances

def listFileservers():
    r = check_output(["gcloud","compute","instance-groups","unmanaged", "list-instances", "mytransfer-fileservers"])
    fs = str(r,'utf-8')[:-1].split('\n')[1:] #convert to utf-8, drop final newline, split into lines and drop column names
    servers = dict([tuple(x.split()) for x in fs])
    return servers

def deleteServer(servername):
    check_output(["gcloud", "compute", "instances", "delete", servername])
    return
    
def getLoad(servername):
    r = requests.get('http://'+servername+'/load')
    return int(r.text)

def createServer():
    name = 'mytransfer-fs-'+uuid.uuid4().hex #generate unique name
    check_output(["gcloud", "compute", "instances", "delete", name, "--image", "mytransfer-fileserver"])
    check_output(["gcloud", "compute", "instance-groups", "unmanaged", "add-instances", "mytransfer-fileservers", "--instances", name])
    return name
    
def getDraintime(servername):
    r = requests.get('http://'+servername+'/timetodrain')
    return int(r.text)

drainingServers = {}
while(True):
    fileservers = listFileservers() # returns a dict 
    #fileservers contains PROVISIONING, RUNNING and TERMINATED instances
    terminatedServers = [k for k, v in fileservers.items() if v == 'TERMINATED'] # only those which are TERMINATED
    #delete the servers which have finished draining
    for s in terminatedServers:
        deleteServer(s)
    runningServers = {k for k, v in fileservers.items() if v == 'RUNNING'} # only those which are RUNNING
    activeServers = runningServers - drainingServers
    #determine the load on the servers
    load = 0
    capacity = 0
    for s in activeServers:
        load += getLoad(s)
        capacity += serverCapacity //assuming all instances are equal size this is a constant
    averageload = load/capacity
    if averageload > 0.8: # load is high, add another server
        # this scales up one at a time, it is also possible to scale up multiple servers at a time 
        # if load is super high
        createServer() # add a new server, this call only returns after the instance has been created and added to the instance group
    if (averageload < 0.5) and (len()>1): #load is low, kill an instance unless it's the last one
        currentMinTimeToDrain = float('inf') # quite long
        for s in activeServers:
            edt = getDraintime(s)
            if edt < currentMinTimeToDrain:
                serverToDrain = s
        drainServer(serverToDrain)
        