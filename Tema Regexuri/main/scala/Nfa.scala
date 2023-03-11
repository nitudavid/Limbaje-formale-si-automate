import scala.collection.mutable

class Nfa[A](var stari : Set[A], var alfabet : Set[Char], var tranzitii : Set[(A, A, Char)], var stareInitiala: A, var stareFinala: A) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = new Nfa[B](stari.map(f), alfabet, tranzitii.map(x => (f(x._1),
    f(x._2), x._3)), f(stareInitiala), f(stareFinala)) // TODO implement map

  def next(state:A, c: Char): Set[A] = {
    val tranzitiiBune = tranzitii.filter(t => t._1 == state && t._3 == c);
    tranzitiiBune.map(tranzitie => tranzitie._2);
  } // TODO implement next

  def epsiloane(state:A) : Set[A] = {
    var nextStates = next(state, 'ϵ')
    var list = Set(state)
    var t : Set[A] = Set.empty
    while(nextStates.nonEmpty) {
      for (a <- nextStates) {
        if(!list.contains(a)) {
          t = t ++ next(a, 'ϵ')
        }
      }
      list = list ++ nextStates
      nextStates = t
      t = Set.empty
    }
    return list
  }

  def accepts(str: String): Boolean = {
    var lista : List[A] = List.empty
    lista = stareInitiala :: lista
    lista ++= epsiloane(stareInitiala)
    for(a <- str) {
      lista = lista.flatMap(y => next(y, a))
      for(b <- lista) {
        lista ++= epsiloane(b)
      }
    }
    if(lista.contains(stareFinala)){
      return true
    }
    else {
      return false
    }
  } // TODO implement accepts

  def getStates : Set[A] = stari // TODO implement getStates

  def isFinal(state: A): Boolean = state == stareFinala  // TODO implement isFinal

}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def fromPrenex(str: String): Nfa[Int] = {
    var i = 1;

    def nfaOneChar(str: String): Nfa[Int] = {
      var caracter = str.charAt(0);
      var stari = Set[Int](i, i + 1);
      var alfabet = Set[Char](caracter);
      var tranzitii = Set((i, i + 1, caracter));
      var stareInitiala = i;
      var stareFinala = i + 1;
      i = i + 2;
      return new Nfa[Int](stari, alfabet, tranzitii, stareInitiala, stareFinala);
    }

    def nfaConcat(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
      var stari = nfa1.stari ++ nfa2.stari;
      var alfabet = nfa1.alfabet ++ nfa2.alfabet + 'ϵ';
      var tranzitii = nfa1.tranzitii ++ nfa2.tranzitii;
      tranzitii = tranzitii + Tuple3(nfa1.stareFinala, nfa2.stareInitiala, 'ϵ');
      var stareInitiala = nfa1.stareInitiala;
      var stareFinala = nfa2.stareFinala;
      return new Nfa[Int](stari, alfabet, tranzitii, stareInitiala, stareFinala);
    }

    def nfaUnion(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] = {
      var stari = nfa1.stari ++ nfa2.stari + (i, i + 1);
      var alfabet = nfa1.alfabet ++ nfa2.alfabet + 'ϵ';
      var tranzitii: Set[(Int, Int, Char)] = nfa1.tranzitii ++ nfa2.tranzitii;
      tranzitii = tranzitii + Tuple3(i, nfa1.stareInitiala, 'ϵ');
      tranzitii = tranzitii + Tuple3(i, nfa2.stareInitiala, 'ϵ');
      tranzitii = tranzitii + Tuple3(nfa1.stareFinala, i + 1, 'ϵ');
      tranzitii = tranzitii + Tuple3(nfa2.stareFinala, i + 1, 'ϵ');
      var stareInitiala = i;
      var stareFinala = i + 1;
      i = i + 2;
      return new Nfa[Int](stari, alfabet, tranzitii, stareInitiala, stareFinala);
    }

    def nfaStar(nfa: Nfa[Int]): Nfa[Int] = {
      var stari = nfa.stari + (i, i + 1);
      var alfabet = nfa.alfabet + 'ϵ';
      var tranzitii: Set[(Int, Int, Char)] = nfa.tranzitii + Tuple3(i, nfa.stareInitiala, 'ϵ');
      tranzitii = tranzitii + Tuple3(i, i + 1, 'ϵ');
      tranzitii = tranzitii + Tuple3(nfa.stareFinala, i + 1, 'ϵ');
      tranzitii = tranzitii + Tuple3(nfa.stareFinala, nfa.stareInitiala, 'ϵ');
      var stareInitiala = i;
      var stareFinala = i + 1;
      i = i + 2;
      return new Nfa[Int](stari, alfabet, tranzitii, stareInitiala, stareFinala);
    }

    def nfaPlus(nfa: Nfa[Int]): Nfa[Int] = {
      var nfaFinal = nfaConcat(nfa, nfaStar(nfa));
      return nfaFinal;
    }

    def nfaMaybe(nfa: Nfa[Int]): Nfa[Int] = {
      var nfaFinal = nfaUnion(nfa, nfaEpsilon());
      return nfaFinal;
    }

    def nfaEpsilon(): Nfa[Int] = {
      var caracter = 'ϵ';
      var stari = Set[Int](i);
      var alfabet = Set[Char](caracter);
      var tranzitii = Set((i, i, caracter));
      var stareInitiala = i;
      var stareFinala = i;
      i = i + 1;
      return new Nfa[Int](stari, alfabet, tranzitii, stareInitiala, stareFinala);
    }

    def nfaVoid(): Nfa[Int] = {
      var caracter = 'ϵ';
      var stari = Set[Int](i, i + 1);
      var alfabet = Set[Char](caracter);
      var tranzitii = Set.empty[(Int, Int, Char)];
      var stareInitiala = i;
      var stareFinala = i + 1;
      i = i + 2;
      return new Nfa[Int](stari, alfabet, tranzitii, stareInitiala, stareFinala);
    }

    var words: List[String] = List.empty;
    var cuvant = "";
    var j = 0
    while(j < str.length){
      var c = str(j)
      if(c == ' '){
        words = cuvant :: words
        cuvant = ""
      }
      else if(c == '\''){
        cuvant = "" + str(j + 1)
        j = j + 2
      }
      else{
        cuvant = cuvant + c
      }
    j = j + 1
    }

    words = cuvant :: words

    var stiva = mutable.Stack[Nfa[Int]]();

    while (words.nonEmpty) {
      if (words.head.equals("CONCAT")) {
        var arg1 = stiva.pop();
        var arg2 = stiva.pop();
        stiva.push(nfaConcat(arg1, arg2));
      }
      else if (words.head.equals("UNION")) {
        var arg1 = stiva.pop();
        var arg2 = stiva.pop();
        stiva.push(nfaUnion(arg1, arg2));
      }
      else if (words.head.equals("STAR")) {
        var arg = stiva.pop();
        stiva.push(nfaStar(arg));
      }
      else if (words.head.equals("PLUS")) {
        var arg = stiva.pop();
        stiva.push(nfaPlus(arg));
      }
      else if (words.head.equals("MAYBE")) {
        var arg = stiva.pop();
        stiva.push(nfaMaybe(arg));
      }
      else if (words.head.equals("void")) {
        stiva.push(nfaVoid());
      }
      else if (words.head.equals("eps")) {
        stiva.push(nfaEpsilon());
      }
      else {
        stiva.push(nfaOneChar(words.head));
      }
      words = words.tail;
    }

    return stiva.pop();
  }
}// TODO implement Prenex -> Nfa transformation.

// You can add more methods to this object