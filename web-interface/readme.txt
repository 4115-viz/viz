VizOnline Web Interface

Overview

We have implemented an online text editor for our language in order to provide Viz programming language users a more interactive way of developing code. With this deployed web application, the user doesn’t have to worry about having Dune, Ocaml, and LLVM installed locally on their machine. VizOnline affords users the ability to write, compile, and run viz source code in an online text editor. Furthermore, we provide a look into what happens under the hood during compilation through 6 different run options: 

Running the code and displaying output
A look at the full build process
Displaying the parsed abstract syntax tree
Presenting the syntactically checked abstract syntax tree
Viewing the LLVM IR assembly code
Seeing how programs are scanned into tokens
8.2 How To Use VizOnline

Currently, VizOnline can be accessed by following the link provided below:

http://ec2-23-22-206-12.compute-1.amazonaws.com/


We have the frontend application located at the below repo. The code is ready to clone, and instructions are present there.

https://github.com/jakobgabrield/vizonline

