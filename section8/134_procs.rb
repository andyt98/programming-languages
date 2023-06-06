# Programming Languages, Dan Grossman
# Section 7: Procs

a = [3, 5, 7, 9]

# no need for Procs here
b = a.map { |x| x + 1 }
puts(b)

i = b.count { |x| x >= 6 }
puts(i)

# need Procs here: want an array of closures
c = a.map { |x| lambda { |y| x >= y } }
puts(c)

# elements of c are Proc objects with a call method
test = c[2].call 17
puts(test)

j = c.count { |x| x.call(5) }
puts(j)


