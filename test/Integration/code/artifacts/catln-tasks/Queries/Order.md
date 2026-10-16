The order a query gives what it finds, and the count it is cut to.

By a date.

```tasks
sort by due
```

By a date, turned around.

```tasks
sort by due reverse
```

By what a task says.

```tasks
sort by description
```

By how urgent a task is.

```tasks
sort by priority
```

By the kind of status.

```tasks
sort by status.type
```

By whether a task is done.

```tasks
sort by status
```

By whether it comes round again.

```tasks
sort by recurring
```

By where a task was written.

```tasks
sort by path
```

By a tag a task wrote.

```tasks
sort by tag
```

By one key and then another.

```tasks
sort by status
sort by description
```

The orders are kept in the order they were written.

```tasks
sort by due
sort by description reverse
```

Cut to the count asked for, after the order.

```tasks
sort by due
limit 2
```

Cut with no order asked for.

```tasks
limit 2
```
