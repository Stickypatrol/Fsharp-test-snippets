let a = System.BitConverter.GetBytes(7us)
let a' = System.Collections.BitArray(a)

for x in 0..a'.Length-1 do printfn "%A" a'.[x]