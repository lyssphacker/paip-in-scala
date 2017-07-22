implicit val simpleGrammar = List(
  "sentence" -> ("noun-phrase", "verb-phrase"),
  "noun-phrase" -> ("Article", "Noun"),
  "verb-phrase" -> ("Verb", "noun-phrase"),
  "Article" -> "the a",
  "Noun" -> "man ball woman table",
  "Verb" -> "hit took saw liked"
)

def ruleLhs(rule : (String, AnyRef)): String = {
  rule._1
}

def ruleRhs(rule : (String, AnyRef)): AnyRef = {
  rule._2
}

def rewrites(key : String)(implicit grammar : List[(String, java.io.Serializable)]) : AnyRef = {
  grammar.toMap.get(key).get
}

ruleRhs("Verb" -> "hit took saw liked")
ruleRhs("sentence" -> ("noun-phrase", "verb-phrase"))
rewrites("sentence")
