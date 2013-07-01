#contains classes Call, Arg and Args

from f2pyutils import getuse, getType

class Call:
  def __init__(self,funcName,parseTree,psyName,args=None,uniqueName=""):
    self.use=getuse(parseTree,onlyname=funcName)
    self.name=funcName
    self.args=args
    if args is None:
      self.argsAdded=False
    else:
      self.argsAdded=True
    self.parseTree=parseTree
    self.psyName=psyName
    if uniqueName=="":
      self.uniqueName=funcName
    else:
      self.uniqueName=uniqueName
  def __str__(self):
    return "callName "+self.name
  def hasUse(self):
    if self.use is None : return False
    return True
  def getUse(self):
    return self.use
  def useName(self):
    return use.name
  def addArgs(self,args):
    self.args=args
    self.argsAdded=True
  def isMultiFunction(self):
    if not self.argsAdded:
        raise RuntimeError('Error in Class Call, method is MultiFunction: I should not be called without first calling method addArgs')
    if len(self.args.argList)==0: return False # no arguments so can not be multi-function
    if self.args.argList[0].Type!="Func": return False # first argument is not a function so assume it is a single function call
    return True # must be multi-function

  def getFunctionCalls(self):
    calls=[]
    argList=[]
    first=True
    funcArg=None
    for arg in self.args.argList:
      if arg.Type=="Func":
        if first:
          funcArg=arg
          first=False
        else:
          args=Args(argList=argList)
          calls.append(Call(funcArg.name,self.parseTree,self.psyName,uniqueName=funcArg.uniqueName,args=args))
          argList=[]
          funcArg=arg
      elif arg.Type=="Type":
        argList.append(arg)
    args=Args(argList=argList)
    calls.append(Call(funcArg.name,self.parseTree,self.psyName,uniqueName=funcArg.uniqueName,args=args))
    return calls
      
class Arg:
  def __init__(self,argName,parseTree,position):
    if getType(argName,parseTree):
      self.Type="Type"
      self.typeInfo=getType(argName,parseTree)
    elif getuse(parseTree,onlyname=argName):
      self.Type="Func"
    else :
      raise RuntimeError('Error in Class Arg, function init: unsupported argument type found. Expecting one of [Type,Func]')
    self.name=argName
    self.uniqueName=argName
    self.position=position
  def __str__(self):
    return "argName "+self.name+", type "+self.Type

class Args:
  def __init__(self,argList=None,argNames=None,parseTree=None):
    if argList:
      self.argList=argList
    else:
      assert argNames is not None and parseTree is not None, "Error"
      self.argList=[]
      for id,argName in enumerate(argNames):
        self.argList.append(Arg(argName,parseTree,id+1))
      # determine unique names
      for id,arg in enumerate(self.argList):
        if self.names().count(arg.name)>1:
          arg.uniqueName=arg.name+"_"+str(id)
        else:
          arg.uniqueName=arg.name

  def names(self):
    myNames=[]
    for arg in self.argList:
      myNames.append(arg.name)
    return myNames
  def uniqueNames(self):
    myNames=[]
    for arg in self.argList:
      myNames.append(arg.uniqueName)
    return myNames
