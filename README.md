# CENG 798 First Assignment

### How To

You can use the executable on terminal by writing ```./hw1``` or install ghc haskell and write command ```ghci hw1```

You can download compiler from 

```https://www.haskell.org/downloads/ and interpret the hw1.hs file as you wish```


### Explanations:

1 - Due to floating number error, numbers could be changed after a lot of computation. You can check out by creating a ```1|0> + 0|0>``` qubit and apply hadamard transform continuosly

2 - You can create Complex Numbers directy by using ```Complex flaota floatb```

3 - You can create Qubit directly for example : ```Qubit (Complex 1.0 0.0) (Complex 0.0 0.0)```

4 - If you try to take absolute value or signum of a Qubit, it is not implemented and ends up with error.

5 - The executable is built on MacOSX, if there is any problem regarding Windows or Linux, please rebuilt with ghc --make hw1.hs
