[Paradigms of Artificial Intelligence Programming](http://norvig.com/paip.html) in Scala

# Trying things out

## chapter 18

### Othello.scala

```scala
othello(adaptStrategy(human), adaptStrategy(human))

othello(minimaxSearcher(3, adaptEvalFn(countDifference)), adaptStrategy(maximizier(countDifference)))

othello(adaptStrategy(maximizier(weightedSquares)), adaptStrategy(maximizier(countDifference)))

othello(alphaBetaSearcher(6, adaptEvalFn(countDifference)), alphaBetaSearcher(4, adaptEvalFn(weightedSquares)))

randomOthelloSeries(
    alphaBetaSearcher(4, adaptEvalFn(weightedSquares)),
    randomStrategy,
    5)

randomOthelloSeries(
    minimaxSearcher(4, adaptEvalFn(weightedSquares)),
    randomStrategy,
    5)

randomOthelloSeries(
    adaptStrategy(maximizier(modifiedWeightedSquares)),
    randomStrategy,
    5)

roundRobin(
    List(adaptStrategy(maximizier(countDifference)), adaptStrategy(maximizier(mobility)),
    adaptStrategy(maximizier(weightedSquares)), adaptStrategy(maximizier(modifiedWeightedSquares)), randomStrategy), 5, 10,
    List("count-difference", "mobility", "weighted", "modified-weighted", "random"))

roundRobin(
    List(alphaBetaSearcher(4, adaptEvalFn(countDifference)), alphaBetaSearcher(4, adaptEvalFn(weightedSquares)),
    alphaBetaSearcher(4, adaptEvalFn(modifiedWeightedSquares)), randomStrategy), 5, 10,
    List("count-difference", "weighted", "modified-weighted", "random"))

roundRobin(
    List(adaptStrategy(maximizier(countDifference)),
    adaptStrategy(maximizier(weightedSquares)), adaptStrategy(maximizier(modifiedWeightedSquares)), randomStrategy), 5, 10,
    List("count-difference", "weighted", "modified-weighted", "random"))
```

