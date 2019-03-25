# postkrypt

A simple PostScript subset that compiles to even smaller PostScript subset. Made as a task for MIMUW Languages and Programming Paradigms course.

## Functionality:

Input is limited to following expressions:

 - numbers – that are pushed on a stack
 - `add`, `sub`, `mul`, `div` – that pop two numbers from stack, apply operation to them and push result to the stack
 - `translate` and `rotate` – that update current transformation (they pop arguments from stack as well)
 - `lineto`, `moveto`, `closepath` – drawing lines and moving coursor. `closepath` draws line into beginning of current path.
 
Output is limited only to numbers and `lineto` and `moveto` commands.
