object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s: String): List[Either[Char, String]] = {
    var i = 0
    var addInThisList = List[Either[Char, String]]()
    while(i<s.length){
      if(s(i) == '\'') {
        addInThisList = addInThisList.appended(Right(s.substring(i, i+3)))
        i = i + 3
      }
      else if(s(i) == '['){
        if(s(i+1)=='a') addInThisList = addInThisList.appended(Right("UNION a UNION b UNION c UNION d UNION e UNION f UNION g UNION h UNION i UNION j UNION k UNION l UNION m UNION n UNION o UNION p UNION q UNION r UNION s UNION t UNION u UNION v UNION w UNION x UNION y z"))
        if(s(i+1)=='A') addInThisList = addInThisList.appended(Right("UNION A UNION B UNION C UNION D UNION E UNION F UNION G UNION H UNION I UNION J UNION K UNION L UNION M UNION N UNION O UNION P UNION Q UNION R UNION S UNION T UNION U UNION V UNION W UNION X UNION Y Z"))
        if(s(i+1)=='0') addInThisList = addInThisList.appended(Right("UNION 0 UNION 1 UNION 2 UNION 3 UNION 4 UNION 5 UNION 6 UNION 7 UNION 8 9"))
        i = i + 5
      }
      else {
        addInThisList = addInThisList.appended(Left(s(i)))
        i = i + 1
      }
    }

    addInThisList
  }

  def toConcatenate(str: List[Either[Char, String]]): String = {
    var i = 0
    var auxList = str
    while (i < auxList.length) {
      if (auxList(i) == Left('*')) {
        val left = auxList.slice(0, i - 1)
        val right = auxList.slice(i + 1, auxList.length)
        auxList(i - 1) match {
          case Left(char) => {
            val between = List(Right("STAR " + char))
            auxList = left ++ between ++ right
          }
          case Right(string) => {
            val between = List(Right("STAR " + string))
            auxList = left ++ between ++ right
          }
        }
        i = i - 1
      }
      else if (auxList(i) == Left('?')) {
        val left = auxList.slice(0, i - 1)
        val right = auxList.slice(i + 1, auxList.length)
        auxList(i - 1) match {
          case Left(char) => {
            val between = List(Right("UNION " + char + " eps"))
            auxList = left ++ between ++ right
          }
          case Right(string) => {
            val between = List(Right("UNION " + string + " eps"))
            auxList = left ++ between ++ right
          }
        }
        i = i - 1
      }
      else if (auxList(i) == Left('+')) {
        val left = auxList.slice(0, i - 1)
        val right = auxList.slice(i + 1, auxList.length)
        auxList(i - 1) match {
          case Left(char) => {
            val between = List(Right("CONCAT " + char + " STAR " + char))
            auxList = left ++ between ++ right
          }
          case Right(string) => {
            val between = List(Right("CONCAT " + string + " STAR " + string))
            auxList = left ++ between ++ right
          }
        }
        i = i - 1
      }
      i = i + 1

    }

    if (auxList.length == 1) {
      auxList.head match {
        case Right(string) => {
          return string
        }
        case Left(char) => {
          return "" + char
        }
      }
    }

    var concatString = ""
    i = 0

    while (i < auxList.length) {
      auxList(i) match {
        case Right(string) => {
          if (auxList.length - i > 2) {
            concatString += "CONCAT " + string + " "
          }
          else {
            auxList(i + 1) match {
              case Right(string2) => {
                concatString += "CONCAT " + string + " " + string2
              }
              case Left(char2) => {
                concatString += "CONCAT " + string + " " + char2
              }
            }
            i = i + auxList.length + 3
          }

        }
        case Left(char) => {
          if (auxList.length - i > 2) {
            concatString += "CONCAT " + char + " "
          }
          else {
            auxList(i + 1) match {
              case Right(string2) => {
                concatString += "CONCAT " + char + " " + string2
              }
              case Left(char2) => {
                concatString += "CONCAT " + char + " " + char2
              }
            }
            i = i + auxList.length + 3
          }
        }
      }
      i = i + 1
    }
    concatString
  }

  def toPrenexHelper(str: List[Either[Char, String]]): String = {
    var firstParant = 0
    var lastParant = 0
    var timesParant = 0;
    var i = 0
    var auxList = str

    while(i<auxList.length){
      if(auxList(i) == Left('(')){
        timesParant = timesParant + 1
        if(timesParant == 1) firstParant = i
      }
      if (auxList(i) == Left(')')) {
        timesParant = timesParant + -1
        if(timesParant == 0) {
          lastParant = i
          val between = List(Right(toPrenexHelper(auxList.slice(firstParant+1, lastParant))))
          val left = auxList.slice(0, firstParant)
          val right = auxList.slice(lastParant+1, auxList.length)
          auxList = left ++ between ++ right
          i = firstParant
        }

      }
      i = i + 1
    }
    auxList = auxList.appended(Left('|'))
    i = 0
    var contorAntiString = 0
    while(i < auxList.length){
      if (auxList(i) == Left('|')) {
        val left = auxList.slice(0, contorAntiString)
        val between = List(Right(toConcatenate(auxList.slice(contorAntiString, i))))
        val right = auxList.slice(i + 1, auxList.length)
        i = contorAntiString
        contorAntiString = contorAntiString + 1
        auxList = left ++ between ++ right
      }
      i = i + 1
    }
    if(auxList.length==1){
      auxList.head match{
        case Right(string) => {
          return string
        }
      }
    }

    var unionString = ""
    i = 0
    while (i < auxList.length) {
      auxList(i) match{
        case Right(string) => {
          if (auxList.length - i > 2) {
            unionString += "UNION " + string + " "
          }
          else{
            auxList(i+1) match {
              case Right(string2) => {
                unionString += "UNION " + string + " " + string2
              }
            }
            i = i + auxList.length+3
          }

        }
      }
      i = i + 1
    }

    unionString
  }

  def toPrenex(str: String): String = {
    if(str == "eps"){
      "eps"
    }
    else{
      toPrenexHelper(preprocess(str))
    }
  }
}