Which blocks of a note are queries over its vault's tasks, and which are code.

Before, [[Welcome]].

```tasks
not done
```

After, [[Nested]]. A fence written in tildes, in capitals, or with a space after the word is a query
too.

~~~tasks
not done
~~~

```Tasks
not done
```

```tasks 
not done
```

A fence saying it is something else, or saying the word as part of a longer one, or saying nothing,
is code.

```dataview
not done
```

```tasksmore
not done
```

```
not done
```

A query holding a fence is written between a longer one.

````tasks
not done
```
sort by due
````

An indented tasks fence is code, not a query.

  ```tasks
  not done
  ```

A tasks fence inside a longer block is a line of that block.

````
```tasks
not done
```
````

A query written under a checkbox is a query of it.

- [ ] Plan it
  ```tasks
  not done
  ```

A query saying nothing is a query.

```tasks
```

A link written inside a query is writing about a link, and travels in the query as it was typed.

```tasks
path includes [[Welcome]]
```
