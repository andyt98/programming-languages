public class ExampleClients {
    // no environment needed
    static List<Integer> doubleAll(List<Integer> xs) {
        return List.map((new Func<Integer, Integer>() {
            public Integer m(Integer x) {
                return x * 2;
            }
        }), xs);
    }

    // the key point here is that the "final int n" is in the environment
    // without inner classes, we'd need a class definition with an explicit
    // field to hold n and pass n to the constructor as shown next
    // (In Java 8 and later, you can leave final off, but it is inferred --
    //  if countNs does mutate n, then the inner object won't be allowed to use
    // it.)
    static int countNs(List<Integer> xs, final int n) {
        return List.length(List.filter((new Pred<Integer>() {
            public boolean m(Integer x) {
                return x == n;
            }
        }), xs));
    }

    static int countNs2(List<Integer> xs, int n) {
        return List.length(List.filter(new ForCountNs2(n), xs));
    }
}