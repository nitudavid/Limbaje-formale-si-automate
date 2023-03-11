import scala.collection.mutable

class Dfa[A] (var stari : Set[A], var alfabet : Set[Char], var tranzitii : Set[(A, A, Char)], var stareInitiala: A, var stareFinala: Set[A], var sinkState : A) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = new Dfa[B](stari.map(f), alfabet, tranzitii.map(x => (f(x._1),
    f(x._2), x._3)), f(stareInitiala), stareFinala.map(f), f(sinkState)) // TODO implement map

  def next(state:A, c: Char): A = {
    val tranzitiiBune = tranzitii.filter(t => t._1 == state && t._3 == c);
    tranzitiiBune.map(tranzitie => tranzitie._2);
    if(tranzitiiBune.isEmpty){
      return sinkState
    }
    else{
      return tranzitiiBune.toList.head._2
    }
  } // TODO implement next

  def accepts(str: String): Boolean = {
    var state = stareInitiala
    for(a <- str){
      state = next(state, a)
    }
    return isFinal(state)
  } // TODO implement accepts

  def getStates : Set[A] = stari // TODO implement getStates

  def isFinal(state: A): Boolean = stareFinala.contains(state)  // TODO implement isFinal

}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    var dfaStates: Set[Set[Int]] = Set.empty
    var dfaTransitions: Set[(Set[Int], Set[Int], Char)] = Set.empty
    var queue = mutable.Queue[Set[Int]]();

    var nfa = Nfa.fromPrenex(str)
    dfaStates += nfa.epsiloane(nfa.stareInitiala)
    queue += nfa.epsiloane(nfa.stareInitiala)

    while (queue.nonEmpty) {
      val stareDfa = queue.dequeue()
      for(a <- stareDfa){
        val tranzitiiBune = nfa.tranzitii.filter(t => t._1 == a && t._3 != 'ϵ')
        for(b <- tranzitiiBune) {
          val stareNoua = nfa.epsiloane(b._2)
          if (!dfaStates.contains(stareNoua)){
            queue += stareNoua
          }
          dfaStates += stareNoua
          dfaTransitions += Tuple3(stareDfa, stareNoua, b._3)
        }
      }
    }

    var sinkState :Set[Int] = Set()
    dfaStates += sinkState

    var dfa = new Dfa(dfaStates, nfa.alfabet, dfaTransitions, nfa.epsiloane(nfa.stareInitiala), dfaStates.filter(t => t.contains(nfa.stareFinala)), sinkState)
    var list = dfaStates.toList

    return dfa.map(t => list.indexOf(t))
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa


  // You can add more methods to this object
}
