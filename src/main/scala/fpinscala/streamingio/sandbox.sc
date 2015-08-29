val p = Process.liftOne[Int, Int](x => x * 2)
p.apply(Stream(1,2,3,4))
