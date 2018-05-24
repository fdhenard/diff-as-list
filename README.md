# diff-as-list

Compare clojure maps. The differences come back as a list with paths to where the elements differ.

`(diffl ...)` compares two maps or "primitives" (int, string, keyword).  It does not work with sequences.  It's origins come from the need to compare large xml files.  Clojure core's diff was not adequate because I wanted to see a list of all the differences with the path to the most primitive element difference possible.

## Usage

### Basic Example

    (diffl obj1 obj2)

## License

Copyright © 2018 Frank Henard

Distributed under the MIT License
