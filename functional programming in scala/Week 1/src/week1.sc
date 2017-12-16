def balance(chars: List[Char]): Boolean = {
  def loop(chars: List[Char], counter: Int): Int =
    if (chars.isEmpty) counter
    else if (counter < 0) counter
    else if (chars.head == '(') loop(chars.tail, counter + 1)
    else if (chars.head == ')') loop(chars.tail, counter - 1)
    else loop(chars.tail, counter)
  loop(chars, 0) == 0
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance("())(".toList)