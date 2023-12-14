# string-processing-by-example

This is a toy implementation of the algorithm described in the paper
"Automating String Processing in Spreadsheets Using Input-Output Examples" by Sumit Gulwani.
[pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/popl11-synthesis.pdf)

Given a few examples the algorithm generates a program in specially designed
string-processing DSL.


Example from the paper.

| Input                                                     | Output       |
------------------------------------------------------------|--------------|
| John DOE 3 Data [TS]865-000-0000 - - 453442-00 06-23-2009 | 865-000-0000 |
| A FF MARILYN 30’S 865-000-0030 4535871-00 07-07-2009      | 865-000-0030 |
| A GEDA-MARY 100MG 865-001-0020 - - 5941-00 06-23-2009     | 865-001-0020 |


Another one:

| Input                     | Output  |
|---------------------------|---------|
| BTR KRNL WK CORN 15Z      | 15Z     |
| CAMP DRY DBL NDL 3.6 OZ   | 3.6 OZ  |
| CHORE BOY HD SC SPNG 1 PK | 1 PK    |
| FRENCH WORCESTERSHIRE 5 Z | 5 Z     |
| O F TOMATO PASTE 6 OZ     | 6 OZ    |


```
SubStr(input, Pos(ε, NumTok, 1), CPos(−1))
```


Goals of the project are:

- implement the algorithm in Haskell, trying to make the code generic but also
  clean and readable

- understand how it works
