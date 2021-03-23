structure aux :> aux = struct

  open table

  fun inList elem list = List.exists (fn x => x = elem) list

  fun unionList f l1 l2 =
      let
        val c = Splayset.empty f
      in
        Splayset.listItems (Splayset.addList (Splayset.addList (c, l1), l2))
      end

  (* Pasa una lista de string a un set *)
  fun listToSet f l =
      let
        val emptySet = Splayset.empty f
      in
        Splayset.addList (emptySet, l)
      end

  fun tabToSet f t = listToSet f (map (fn (x,y) => y) (table.tabAList t))

  type 'a stack = 'a list

  val emptyStack = []

  fun pop (ref []) = raise Fail "Pop a stack vacio"
    | pop (s as ref (x :: xs)) = (s := xs; x)

  fun push x (s as ref xs) = s := (x::xs)

  fun isEmptyStack (ref []) = true
    | isEmptyStack _ = false

  fun stackToSet f l =
      let
        val emptySet = Splayset.empty f
      in
        Splayset.addList (emptySet, l)
      end


  fun tupleCompare f ((n, m), (n', m')) =
      if f (n, n') = LESS then LESS
      else if f(n, n') = GREATER then GREATER
        else
          if f(m, m') = LESS then LESS
          else if f(m, m') = GREATER then GREATER
            else EQUAL

  fun singletonList l = foldr (fn (x, xs) => [x]::xs) [] l

  fun setNthList n xs i =
      let
        fun setNthList' n j [] i = []
          | setNthList' n j (x::xs) i = if j = n then (i::xs)
            else (x::(setNthList' n (j + 1) xs i))
      in
        setNthList' n 0 xs i
      end

end
