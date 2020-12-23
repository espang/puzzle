# puzzle


Solves 7x7 grids with following rules:

* Only use numbers 1 to 7
* Use 1 once, 2 twice, ... , 7 7 times
* Each row and column should contain 4 numbers
* Each row and column sum should be 20
* All numbers should be connected. A number is connected to the cell to its left, right, top and bottom
* Every 2x2 grid in the 7x7 grid has at most 3 numbers
* Each row and column can have constraints on the first and last 
  value in the row or column

The program expects inputs in following form:
```
0,0,0,5,6,0,6,7,0
0,0,2,0,0,0,0,0,6
0,2,0,0,0,0,0,0,6
5,0,0,0,0,0,0,0,4
6,0,0,0,0,0,3,0,0
0,0,0,0,0,3,0,0,0
7,0,0,0,3,0,0,0,0
6,0,0,0,0,0,0,1,0
0,6,7,5,0,0,0,0,0
```

The inner 7x7 grid is the game board. The outer rows and columns describe the additional constratins. The `5` at the start of the 4th row means that the first non-empty value for
that row is 5. The `4` at the end of that row means that the last value of that row
is 4.


## Run

Create a file with an input in the described format, e.g. `input_1.txt`.

```
dotnet run `input_1.txt` `output_1.txt`
```
