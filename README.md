# diff-as-list

Compare clojure maps. The differences come back as a list with paths to where the elements differ.

`(diffl ...)` compares two maps or "primitives" (int, string, keyword).  It does not work with sequences.  It's origins come from the need to compare large xml files.  Clojure core's diff was not adequate because I wanted to see a list of all the differences with the path to the most primitive element difference possible.

## Reason for no sequences - version 2.0.0 - 5/24/18

The original version worked with sequences, and the user had to provide a mapping of the path to the sequence, and a function to determine what the "key" of that sequence was.  I realized that if there's a way to determine a unique key in a sequence, then that sequence can be transformed into a map (and Clojure rocks at data transformation).  So, if your data structure has sequences, it is expected that they would be tranformed to a map before using `diffl`.  Perhaps in the future this should be adjusted to allow sequences of primitives.

## Usage

### Basic Example

    (diffl obj1 obj2)

## License

Copyright © 2018 Frank Henard

Distributed under the MIT License
