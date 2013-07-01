def algParse(fileName,psyName="psy"):

  import fparser
  from fparser import api
  from ghclasses import Call,Args,Arg

  calls=[]

  import logging
  # needed to stop logging output from the analyze part of fparser.api.parse
  logging.disable('CRITICAL')

  # parse the algorithm code
  tree=fparser.api.parse(fileName,ignore_comments=False)

  # run through all statements looking for procedure calls
  for stmt, depth in api.walk(tree, -1):

    if isinstance(stmt,fparser.statements.Call):
      # found a procedure call

      call=Call(stmt.designator,stmt.parent,psyName)

      # which module is procedure included from?
      if not call.hasUse():
        print "Warning: no use found for ",stmt.designator,", ignoring"
      elif call.getUse().name==psyName:
        # the procedure call is included from the psy module

        calls.append(call)

        # determine information about our arguments
        args=Args(argNames=stmt.items,parseTree=stmt.parent)
        call.addArgs(args)

  return calls

def psyGen(calls,psyName="psy",infName="inf",kernName="kern"):

  from f2pygen import createmodule,addsub,addtypedecl,addexternal,adduse,addcall

  # create an empty PSy layer module
  outtree=createmodule(psyName)

  for call in calls:

    # create the psy layer subroutine that is called by the algorithm layer
    sub=addsub(call.name,call.args.uniqueNames(),outtree)

    # declare the arguments (only types and functions supported so far)
    extNames=[]
    for arg in call.args.argList:
      if arg.Type=="Type":
        decl=addtypedecl(arg.typeInfo.name,arg.typeInfo.attrspec,[arg.uniqueName],sub)
      elif arg.Type=="Func":
        extNames.append(arg.uniqueName)
      else:
        assert False, "Unsupported argument type. Supported types are [Type,Func]. Found '"+arg.Type+"' in call '"+call.name+"'"
    ext=addexternal(extNames,sub)

    # add any required use statments for the arguments (types)
    types=[]
    for arg in call.args.argList:
      if arg.Type=="Type":
        if arg.typeInfo.name not in types:
          types.append(arg.typeInfo.name)
    if types is not None:
      use=adduse(infName,sub,only=True,funcnames=types)

    # add the required kernel call(s)
    if call.isMultiFunction():
      calls=call.getFunctionCalls()
      # add kernel calls from arguments
      for call in calls:
        result=addcall(call.uniqueName,call.args.uniqueNames(),sub)
        
    else:
      # add use declaration for the kernel call and rename to avoid a clash
      rename="_kern"
      use=adduse(kernName,sub,only=True,funcnames=[call.name+rename+"=>"+call.name])
      # add kernel call
      # TEMPORARY HACK WHILE WE SORT OUT DATA STRUCTURES
      deref=[]
      for argNames in call.args.names():
        deref.append(argNames+"%array")
      result=addcall(call.name+rename,deref,sub)

  return outtree
