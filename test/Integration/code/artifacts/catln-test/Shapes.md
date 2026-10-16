A note is broken at each of its checkboxes: what is written between two is the note, what is
written under one is a comment of its task, and what is only blank lines is written nowhere.

- [ ] Read it
- [x] Write it
      Which is written

      under the task.

The note again. A checkbox indented deeper than the one above it is a task of that task.

- [ ] Plan it
  - [ ] A step of the plan

A checkbox is a list item, and a list is written with numbers as readily as with bullets.

1. [ ] Read it numbered
2) [x] Write it numbered

A fence inside a longer fence does not close it, so the checkbox between them is writing.

````
```
- [ ] A checkbox inside a fence within a longer fence
```
````

A fence is not closed by the other character.

~~~
```
- [ ] A checkbox inside a fence closed by the other character
```
~~~

A fence line saying more does not close a block.

```python
```haskell
- [ ] A checkbox inside a fence line saying more
```

A block opened and closed under a task ends there, so the checkbox after it is a task.

- [ ] Read it with a block under it
  ```
  - [ ] A checkbox inside code under a task
  ```
- [ ] Write it after the block
