def getuse(parent,usename="",onlyname=""):
  import fparser
  for stmt in parent.content:
    if isinstance(stmt,fparser.statements.Use):
      if usename=="":
        if onlyname=="" : return None
        else:
          for name in stmt.items:
            if name==onlyname: return stmt
      elif stmt.name==usename:
        if onlyname=="" : return stmt
        else:
          for name in stmt.items:
            if name==onlyname: return stmt
  if parent.parent==parent or \
     isinstance(parent.parent,fparser.block_statements.BeginSource):
    return None
  return getuse(parent.parent,usename,onlyname)

def getType(name,parent):
  import fparser
  for checkstmt in parent.content:
    if isinstance(checkstmt,fparser.typedecl_statements.Type):
      for checkname in checkstmt.entity_decls:
        if checkname.split("=")[0]==name: return checkstmt
  if parent.parent==parent or \
     isinstance(parent.parent,fparser.parsefortran.FortranParser):
    return None
  return getType(name,parent.parent)
