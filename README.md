Dice language interpreter

Some sample dice expressions:
- `d6` : Roll a 6 sided die
- `2 # d6` : Roll two 6 sided dice
- `2d6` : Roll and add two 6 sided dice
- `d(d 6)` : Roll a 6 sided die to get n, and then roll an n-sided die
- `d(sum(2#d6))` : Roll an n-sided die where n is the sum of two six-sided die rolls.
- `if d6 < 4 then 4 else 2#d4` : Roll a d6, and if that
  is less than 4 return 4, otherwise return a bag consisting of the results
  of rolling 2 d4's.
- `max 2 (3#d5)` : Return the largest 2 values after
  rolling three five-sided dice.
- `char = 6#sum max 3 4#d6` : Define `char` to be the result of six trials
  of the following: roll 4 six-sided dice and sum the 3 largest.
- `min 2 (3#(if d6 < d6 then 1 else 2))` : Roll
  two dice. If the first is smaller, return 1, else 2. Do this three times
  and select the lowest two values.


