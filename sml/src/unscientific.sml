datatype node =
  Empty
| Node of { xValue : int, yValue : int, left : node, right : node }

fun setLeft {xValue, yValue, left, right} newLeft =
  {xValue = xValue, yValue = yValue, left = newLeft, right = right}

fun setRight {xValue, yValue, left, right} newRight =
  {xValue = xValue, yValue = yValue, left = left, right = newRight}

fun isNode (Node _) = true
  | isNode Empty = false

fun newNode rand x = Node
    { xValue = x
    , yValue = Random.randInt rand
    , left = Empty
    , right = Empty
    }

fun merge Empty greater = greater
  | merge lower Empty = lower
  | merge (Node lower) (Node greater) =
    if #yValue lower < #yValue greater
    then Node (setRight lower (merge (#right lower) (Node greater)))
    else Node (setLeft greater (merge (Node lower) (#left greater)))

fun splitBinary Empty _ = (Empty, Empty)
  | splitBinary (Node origNode) value =
    if #xValue origNode < value
    then
      let
        val (l, r) = splitBinary (#right origNode) value
      in
        (Node (setRight origNode l), r)
      end
    else
      let
        val (l, r) = splitBinary (#left origNode) value
      in
        (l, Node (setLeft origNode r))
      end

fun merge3 lower equal greater = merge (merge lower equal) greater

fun split orig value =
  let
    val (lower, equalGreater) = splitBinary orig value
    val (equal, greater) = splitBinary equalGreater (value+1)
  in
    (lower, equal, greater)
  end

fun hasValue node x =
  let
    val (lower, equal, greater) = split node x
  in
    (merge3 lower equal greater, isNode equal)
  end

fun insert rand tree x =
    let
      val (lower, equal, greater) = split tree x
    in
      if isNode equal
      then
        merge3 lower equal greater
      else
        merge3 lower (newNode rand x) greater
    end

fun erase tree x =
  let
    val (lower, _, greater) = split tree x
  in
    merge lower greater
  end

fun run rand =
  let
    fun loop i tree cur res =
      if i > 1000000
      then
        res
      else
        let
          val a = i mod 3
          val newCur = (cur * 57 + 43) mod 10007
          val newI = i + 1
        in
          case a of
            0 => loop newI (insert rand tree newCur) newCur res
          | 1 => loop newI (erase tree newCur) newCur res
          | 2 =>
              let
                val (newTree, has) = hasValue tree newCur
              in
                loop newI newTree newCur (if has then res+1 else res)
              end
          | _ => loop newI tree newCur res
        end
  in
    loop 1 Empty 5 0
  end

val rand = Random.rand (42,666)
val result = run rand
val () = print (Int.toString result)
