
structure Symbol = StringSymbol

structure SymbolOrdered =
  SymbolOrderedFun (structure Symbol = Symbol)

structure SymbolHashable =
   SymbolHashableFun (structure Symbol = Symbol)

structure SymbolHashTable =
   HashTable (structure Key = SymbolHashable)
