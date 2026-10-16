A query of several lines.

Every line saying what a task must be is joined by and.

```tasks
not done
has due date
```

A count is read however it is written.

```tasks
limit 10
```

The same count, spelt out.

```tasks
limit to 10 tasks
```

A query saying nothing asks for every task.

```tasks
```

A line saying nothing, and a line of comment, are passed over.

```tasks

# what is left to do
not done

```

Every line nothing reads is answered, not the first of them.

```tasks
not done
soonish
sort by urgency
```

Each order a query does not read is answered.

```tasks
sort by random
```
