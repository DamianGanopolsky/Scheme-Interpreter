# Scheme Interpreter 

The idea for this project is to develop a Scheme Interpreter using a recursive interpretation and a functional language (Clojure). A wide range of functions and operators that are used natively in Scheme, can be now executed using this interpreter.

#### Requirements

Last version of Leiningen installed: https://leiningen.org/

### Execution

You can run the interpreter by executing the command "lein run" in the repository root. Once the interpreter is running, you can use all the functions that are being interpreted.


The file "demo.scm" contains a large set of scheme functions, you can load the file by running "(load "demo")", once it is loaded it will provide an insight into how the functions and the environment work in Scheme.

#### Water pouring puzzle:

You can use the interpreter to run the water pouring puzzle, sample usage:  

> (load "jarras")  
> (breadth-first bc)  
Ingrese el estado inicial:  
> (0 0)  
Ingrese el estado final:  
> (4 0)  

Exito !!!  
Prof ....... 11  
Solucion ... ((0 0) (5 0) (0 5) (5 5) (2 8) (2 0) (0 2) (5 2) (0 7) (5 7) (4 8) (4 0))  
#t  

You can find more information about this puzzle in: https://en.wikipedia.org/wiki/Water_pouring_puzzle


### Tests

Several unit tests were made in order to test in detail the functionalities of the project, they are located in the test/ folder. In case you want to run those tests, you need to execute the command "lein test" in the repository root.

> lein test  
Ran 21 tests containing 177 assertions.  
0 failures, 0 errors.  


