signature aux = sig

  val inList : ''a -> ''a list -> bool
  val unionList: ((''a * ''a) -> order) -> ''a list -> ''a list -> ''a list
  val listToSet: (('a * 'a) -> order) -> 'a list -> 'a Splayset.set
  val tabToSet: (('a * 'a) -> order) -> ('b, 'a) table.Tabla -> 'a Splayset.set
  val tupleCompare: ((''a * ''a) -> order) -> (''a * ''a) * (''a * ''a) -> order
  val singletonList : 'a list -> 'a list list
  val setNthList: int -> ('a list) -> 'a -> ('a list)

  (* Stack *)
  type 'a stack

  val emptyStack : 'a stack
  val pop: 'a stack ref -> 'a
  val push: 'a -> 'a stack ref -> unit
  val isEmptyStack: 'a stack ref -> bool
  val stackToSet: (('a * 'a) -> order) -> 'a stack -> 'a Splayset.set

end
