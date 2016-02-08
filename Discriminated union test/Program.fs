type A =
  | AA of A
  | AB of A*B
and B =
  | BA of A*B
  | BB of B

