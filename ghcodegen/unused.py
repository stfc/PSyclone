def quotes(input):
    return '"'+str(input)+'"'

def appenduse(useObj,name):
    if not(useObj.isonly==True) :
        print "Error in useAdd, expecting use statement with only clause"
        exit(1)
    if not usenameexists(useObj,name):
        useObj.items.append(name)

def usenameexists(useObj,name):
    for usename in useObj.items:
        if usename==name : return True
    return False

def removeuse(useObj,name):
    if usenameexists(useObj,name):
        useObj.items.remove(name)
    else:
        # warning
        print "WARNING, USE NAME '",name,"' not found in '",useObj,"'"
        pass
