++++++++  Set Cell #0 to 8 (loop counter)
[
  >++++   Set Cell #1 to 4 (will be set to this each loop)
  [
    >++   Add 2 to Cell #2
    >+++  Add 3 to Cell #3
    >+    Add 1 to Cell #4
    <<<-  Decrement the inner loop counter
  ]

  >+      Add 1 to Cell #2
  >+      Add 1 to Cell #3
  >>+     Add 1 to Cell #5

  [<]     Move back to Cell #1

  <-      Decrement the outer loop counter
]

After running this we have the following memory contents:
| Cell  | 0 | 1 | 2  | 3   | 4  | 5 |
| Value | 0 | 0 | 72 | 104 | 32 | 8 |

>>.      Print out 'H'
>+.      Print out 'i'
>+.      Print out '!'
>++.     Print out '\n'
