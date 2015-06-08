# diff-as-list

Compare clojure data structures. The differences come back as a list with paths to where the elements differ.

`(diffl ...)` compares two data structures that are alike structurally.  It's origins come from the need to compare large xml files.  Clojure core's diff was not adequate because I wanted to see a list of all the differences with the path to the most primitive element difference possible.

## Usage

### Basic Example

    (diffl obj1 obj2)

An essential feature to diffl is to be able compare maps inside sequences.  To make this possible you will need to tell diffl how to determine uniqueness for the maps in the list.  In other words... how to find their identifier.  That way diffl will know how to find it's match in the other object's list.

### Nested list example

    (def obj1 {:k1 "val1"
               :a-vec [{:k2 1
                         :oops "one"}
                        {:k2 2
                         :oops "two"}]})
    (def obj2 {:k1 "val1"
               :a-vec [{:k2 1
                         :oops "one"}
                        {:k2 2
                         :oops "three"}]}) ;; <--difference will lie here

    (def list-ident-funcs
         {[:a-vec] #(:k2 %)}  ;; <-- key is the path too the list and the value is the function that 
                               ;;     describes how to find the id of objects in the list

    (diffl obj1 obj2 list-ident-funcs)


## License

Copyright © 2015 Frank Henard

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
