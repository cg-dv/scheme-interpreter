# Ch. 8 Listing Notes

 After adding the PrimitiveFunc and Func constructors for the LispVal data type,
 my data type definitions break where the deriving instance of Eq is used. 

 If the 'deriving (Eq)' clause is removed from the data constructor list,
 listing8_1.hs works, but not using a deriving instance of Eq breaks my use of 
 the 'elem' function in the definition of Scheme's 'case' syntax in the 
 definition of 'eval'.  

 Links to documentation about deriving instances and type classes:

 [Haskell Wiki](https://wiki.haskell.org/GHC/Stand-alone_deriving_declarations) info on stand-alone deriving declarations

 [Learn You A Haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses) info on types and typeclasses
 
 [Hackage](https://hackage.haskell.org/package/deriving-compat-0.3.6/docs/Data-Eq-Deriving.html) Eq info
