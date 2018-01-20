# Features

The completed and incompleted features. Difficulty and priority are on a scale
of 1-5 in terms of implementation. Any feature not included on the list is not
currently on the roadmap (and implicitly not implemented).

## SDD Queries

| Status             | Feature              | Priority | Difficulty | Notes |
| :-------------     | :-------------       |   :----- | :--------- | :---- |
| :white_check_mark: | Eval                 |          |          1 |       |
| :x:                | SAT Enumerate        |        3 |          1 |       |
| :x:                | Implication          |        2 |          1 |       |
| :x:                | Weighted Model Count |        4 |          3 |       |
| :white_check_mark: | Equality             |          |            |       |
| :white_check_mark: | Consistency          |          |            |       |

## SDD Representation

| Status             | Feature              | Priority | Difficulty | Notes         |
| :-------------     | :-------------       |   :----- | :--------- | :------------ |
| :white_check_mark: | Complemented edges   |          |            |               |
| :white_check_mark: | Trimming             |          |            |               |
| :white_check_mark: | Canonicity           |          |            |               |
| :x:                | Garbage collection   |        5 |          3 | May break API |
| :x:                | Dynamic minimization |        1 |          5 |               |


## SDD Operations

| Status             | Feature                  | Priority | Difficulty | Notes |
| :-------------     | :-------------           | :-----   | :--------- | :---- |
| :white_check_mark: | Conjunction              |          |            |       |
| :white_check_mark: | Disjunction              |          |            |       |
| :negation:         | Negation                 |          |            |       |
| :x:                | Exists (single variable) | 4        | 1          |       |
| :x:                | Condition                | 4        | 1          |       |

## BDD Queries

| Status             | Feature              | Priority | Difficulty | Notes |
| :-------------     | :-------------       |   :----- | :--------- | :---- |
| :white_check_mark: | Eval                 |          |          1 |       |
| :x:                | SAT Enumerate        |        3 |          1 |       |
| :x:                | Implication          |        2 |          1 |       |
| :x:                | Weighted Model Count |        4 |          3 |       |
| :white_check_mark: | Equality             |          |            |       |
| :white_check_mark: | Consistency          |          |            |       |


## BDD Representation

| Status             | Feature              | Priority | Difficulty | Notes         |
| :-------------     | :-------------       |   :----- | :--------- | :------------ |
| :white_check_mark: | Complemented edges   |          |            |               |
| :white_check_mark: | Trimming             |          |            |               |
| :white_check_mark: | Canonicity           |          |            |               |
| :x:                | Garbage collection   |        5 |          3 | May break API |
| :x:                | Dynamic minimization |        1 |          5 |               |

## BDD Operations

| Status             | Feature                  | Priority | Difficulty | Notes |
| :-------------     | :-------------           | :-----   | :--------- | :---- |
| :white_check_mark: | Conjunction              |          |            |       |
| :white_check_mark: | Disjunction              |          |            |       |
| :white_check_mark: | Negation                 |          |            |       |
| :x:                | Exists (single variable) | 4        | 1          |       |
| :x:                | Condition                | 4        | 1          |       |
