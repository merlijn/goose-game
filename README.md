### How to run

Assuming you have [sbt](https://www.scala-sbt.org/) installed.

``` scala
sbt run
```

This starts a game.

There is a special command `quit` or `exit` to prematurely quit the game.

### Tests

There is a `GooseSpec` covering all the cases in the Assignment.

``` scala
sbt test
```

### Implementation details

I decided to use *cats-effect* for a more functional approach dealing with `IO`.

Room for improvement:

- Seperate the user parsing from command processing

   I did not do this because it seemed premature for such a small program.

- The logic in `movePlayer(..)` does get somewhat complicated

   If more rules are added to the game you might want to think about designing a better modular rule system to tackle the complexity.