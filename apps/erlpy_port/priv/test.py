#!/usr/bin/python 
import sys
import time
import json; 
while 1:
    line = sys.stdin.readline()
    if not line: break
    Json = json.loads(line)
    #sleep: simulate doing something in your function
    time.sleep(5)
    
    # Now send our terminating string to end the transaction.
    data = {}
    data ["id"] = "01"
    person = {}
    person ["name"] = "Some name"
    person ["lastname"] = "Some lastname"
    data ["person"] = person
    json_data = json.dumps(data)
    print (json_data)
    print "FINISHED"
 
    # And finally, lets flush stdout because we are communicating with
    # Erlang via a pipe which is normally fully buffered.
    sys.stdout.flush()
