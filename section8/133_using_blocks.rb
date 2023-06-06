# Programming Languages, Dan Grossman
# Section 7: Using Blocks

class Foo
  def initialize(max)
    @max = max
  end

  def silly
    yield(4, 5) + yield(@max, @max)
  end

  def count(base)
    if base > @max
      raise "reached max"
    elsif yield base # if the block return true when passed base
      1
    else
      1 + (count(base + 1) { |i| yield i })
    end
  end
end

f = Foo.new(1000)

test1 = f.silly { |a, b| 2 * a - b } # (2 * 4 - 5) - (2 * 1000 - 1000)
puts(test1)

test2 = f.count(10) { |i| (i * i) == (34 * i) } # 25
puts(test2)
