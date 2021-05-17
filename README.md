# Tiling Windows

This repository contains code for a tiling windows manager.

The code is written for Common Lisp.

The program TilingWindows.lisp contains the functions and procedures for making and maintaining viewports and test.lisp runs tests on these functions and procedures.

### Prerequisites

* Must have a UNIX system.

* Must have a Common Lisp compiler.

### Installing

If you do not yet have a Common Lisp compiler on your system go to https://common-lisp.net/downloads and follow the instructions.

Once you have it installed you can download the files TilingWindows.lisp and test.lisp.

You can use the procedures and functions for Tiling Windows by typing the following in a Common Lisp compiler.

```
(require 'TilingWindows)
```

### Procedures and Functions

\> (make_screen real real) -> screen
Returns a special viewport object with an identifier of 1 and dimesions given to the procedure.

\> (width viewport) -> real
Returns the width of the given viewport.

\> (height viewport) -> real
Returns the height of the given viewport.

\> (id viewport) -> integer
Returns the identifier of the given viewport.

\> (active viewport) -> boolean
Returns T if the viewport is active and nil otherwise.

\> (passive viewport) -> boolean
Returns True if the viewport is passive and nil otherwise.

\> (left viewport) -> viewport
Returns the left node viewport, created from a split, of the given viewport.

\>(right viewport) -> viewport
Returns the right node viewport, created from a split, of the given viewport.

\> (neighbor viewport) -> viewport
Returns the neighbor viewport, created from a split, of the given viewport.

\> (hsplit viewport real) -> (list viewport viewport)
Returns a list of two viewports created in a horizontal split at a given coordinate.

\> (vsplit viewport real) -> (list viewport viewport)
Returns a list of two viewports created in a vertical split at a given coordinate.

\> (_delete_ screen identifier) -> null
Deletes the viewport with the given identifier. If the viewport is active it will try to delete its neighbor if applicable. If the viewport is passive it will delete all child viewports that were derived from it.

\> (search_and screen identifier) -> viewport
Returns a viewport with the identifier given that was derived from the screen given.

\> (destroy viewport) -> null
Deletes the viewport given. If the viewport is active it will try to delete its neighbor if applicable. If the viewport is passive it will delete all child viewports that were derived from it.

\> (resize screen viewport real) -> null
Resizes the viewport based on a percentage given of the parent viewport's size.

\> (select viewport (list real real)) -> viewport
Returns the smallest existing viewport that contains the coordinates of the pixel given.

\> (contains viewport real real) -> boolean
Returns T if the given viewport contains the pixel and nil otherwise.

### Running the tests

One way to run the test file is to open terminal (Mac), shell (Linux), or anything alike and navigate to the folder with the files TilingWindows.lisp and test.lisp.

Next, open Common Lisp on the terminal by typing the following.

```
clisp
```

GNU CLISP should open.

If you are using an IDE navigate to the files.

To run the test file simply type the following.

```
(load "test.lisp")
```

All the tests will run at once and be output to the screen.



### Break down into end to end tests

Each type of test is orgnaized by exercise.

It will test the basic functionality and their edge cases of a function or procedure.

### Problems to note

The program can only handle one screen at a time and it does not take into account wether it is outside of its parent viewport when resizing.

## Authors

* **Delano Yoder** 

## License

No License.

## Acknowledgments

* Practical Common Lisp http://gigamonkeys.com/book/
* Lisp Tutorial https://www.youtube.com/watch?v=ymSq4wHrqyU&t=1507s&ab_channel=DerekBanas
* Lisp, The Quantum Programmer's Choice - Computerphile https://www.youtube.com/watch?v=svmPz5oxMlI&ab_channel=Computerphile
