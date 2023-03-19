# compiladores

The aim of this proyect was to create a compiler of a reduced version of C. Is called miniC and the extension is .mc.

This project has an imput.txt where the miniC code is written, then you can use 
  $make run 

or 
  $./miniC < <imput file>.txt > <output file>.s
  
and it creates an output that has mips code and to run it we can use the terminal with the comand 
  $./spim -file output.s

There are some files.txt that we use in order to debug and see if the code was working how it should.  
