let rec funca x y =
  x + funcb y
and funcb y =
  funcc y - 9
and funcc y =
  y + 5


printfn "%A" (funca 5 8)