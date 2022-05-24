
datatype 'a t = Foo | Bar of 'a [.deriving show]

val x = [.show: int t] (Bar 5)
val y = [.show: string t] (Bar "hi")
