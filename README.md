dhall-output
==========

Dhall is a nice configuration language, especially when working with Haskell. One can specify the shape of the data in dhall and Haskell and the type checker will make sure errors happen early so there are no surprises. However, it is a pain to define out types in both languages. Using dhall-output, one can write your types in Haskell and then export them to dhall, easing the pain somewhat. This is a huge help when dealing with large enum types.

How to use
-----------

Make the data type you wish to serialize an instance of `HasDatatypeInfo` from `generics-sop` and make sure each of its fields are instances of `Interpret`. Create an `OutputType` from `makeOutputype`, and then convert it to a text value with `prettyOutputType`. You can save this to file and import it into each dhall file. There is a small example provided.
