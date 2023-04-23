class List<T> {
    T head;
    List<T> tail;

    List(T x, List<T> xs) {
        head = x;
        tail = xs;
    }

    // * the advantage of a static method is it allows xs to be null
    //    -- a more OO way would be a subclass for empty lists (see below)
    // * a more efficient way in Java would be a messy while loop
    //   where you keep a pointer to the previous element and mutate it
    //   -- (try it if you don't believe it's messy)
    //   -- it's more efficient because Java VM doesn't optimize tail calls
    //      (maybe some day)
    static <A, B> List<B> map(Func<B, A> f, List<A> xs) {
        if (xs == null) return null;
        return new List<B>(f.m(xs.head), map(f, xs.tail));
    }

    static <A> List<A> filter(Pred<A> f, List<A> xs) {
        if (xs == null) return null;
        if (f.m(xs.head)) return new List<A>(xs.head, filter(f, xs.tail));
        return filter(f, xs.tail);
    }

    // * again recursion would be more elegant but less efficient
    // * again an instance method would be more common, but then
    //   all clients have to special-case null
    static <A> int length(List<A> xs) {
        int ans = 0;
        while (xs != null) {
            ++ans;
            xs = xs.tail;
        }
        return ans;
    }
}