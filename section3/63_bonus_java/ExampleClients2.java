public class ExampleClients2 {
    static List2<Integer> doubleAll(List2<Integer> xs) {
        return xs.map(new Func<Integer, Integer>() {
            public Integer m(Integer x) {
                return x * 2;
            }
        });
    }

    // the key point here is that the "final int n" is in the environment
    // without inner classes, we'd need a class definition with an explicit
    // field to hold n and pass n to the constructor
    static int countNs(List2<Integer> xs, final int n) {
        return xs.filter(new Pred<Integer>() {
            public boolean m(Integer x) {
                return x == n;
            }
        }).length();
    }
}