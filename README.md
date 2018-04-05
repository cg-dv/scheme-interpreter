# Write Yourself A Scheme In 48 Hours

This is an open-source Scheme interpreter from the wikibook ["Write Yourself A Scheme in 48 Hours"](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). 

This repository contains code written while working through the wikibook 
["Write Yourself A Scheme in 48 Hours"](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) and its exercises. 

Note: This interpreter is still partially broken.  The lambda expression
and function definition functionalities are broken.  I need to fix
the mistakes I made follwing the second half of the book. 

## Usage

Compiling and Running:

cd write-yourself-a-scheme/ch10

ghc -package parsec -fglasgow-exts -o scheme listing10_1.hs

./scheme

Scheme>>>  (* 3 4)

12

Type the string "quit" without quotes and without parenthesis in the
interpreter to exit the interpreter.

## License

"Write Yourself a Scheme in 48 Hours" is a text submitted to [Wikibooks](https://en.wikibooks.org/wiki/Main_Page), and is available under the terms of the [Creative Commons Attribution-ShareAlike License](https://creativecommons.org/licenses/by-sa/3.0/). 
