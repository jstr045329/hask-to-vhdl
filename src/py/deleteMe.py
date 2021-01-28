



for i in range(50):
  s = ''
  s += "x%d <= " % i
  for j in range(20):
    s += " x%d" % j
    if j < 19:
      s += " + "
    else:
      s += ";"
  print(s)

