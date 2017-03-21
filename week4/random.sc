def times(chars: List[Char]) = chars.groupBy(identity).mapValues(_.size).toList

def sorte(freqs: List[(Char,Int)]) = freqs.sortBy(_._2)

val freqs = times(List('a','b','a','a','c','b'))
sorte(freqs)
