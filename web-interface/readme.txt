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

Unfortunately, due to server costs, we are unable to keep the hosted version online permanently. Therefore, we have included the source code for both the frontend and backend of the web interface in our project repository under the web-interface folder.

In order to run the project locally, it requires that the machine has both node and npm installed in addition to opam, dune, and llvm which are required to compile and run the actual code. If you do not already have node installed, you can do so by a tutorial that can be found here. After you have both node and npm installed you can navigate into the ./web-interface directory of the Viz GitHub Repository and run the below commands to start both the backend and frontend servers locally.

Local Run Commands:

# Start Backend Server
npm install
node server.js

# Start Frontend Server
cd client
npm start
