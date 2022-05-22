
structure StrDict = RedBlackDict(structure Key = StringOrdered)
structure ScopeDict = ScopedDict(StrDict)
