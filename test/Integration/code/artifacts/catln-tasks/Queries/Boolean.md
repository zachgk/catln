A line joining what a task must be.

Two things it must be.

```tasks
(done) AND (is recurring)
```

Either of two.

```tasks
(done) OR (is recurring)
```

One of two and not both.

```tasks
(not done) XOR (has tags)
```

Something it must not be.

```tasks
NOT (has tags)
```

One thing and not another.

```tasks
(done) AND NOT (is recurring)
```

Either of two, the second negated.

```tasks
(done) OR NOT (is recurring)
```

Written between square brackets.

```tasks
[done] AND [has tags]
```

Written between braces.

```tasks
{done} AND {has tags}
```

Written between quotes.

```tasks
"done" AND "has tags"
```

Three things, and binding tighter than or.

```tasks
(done) AND (is recurring) OR (has tags)
```

Three things joined the same way, read left to right.

```tasks
(not done) AND (has tags) AND (has due date)
```

A joining written inside a joining.

```tasks
(not done) AND ((is recurring) OR (has tags))
```

A search holding the letters of a joining word.

```tasks
description includes stand OR sit
```

A joining of something nothing reads.

```tasks
(done) AND (soonish)
```
