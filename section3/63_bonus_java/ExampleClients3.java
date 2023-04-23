// now lambdas with implicit conversion to one-method interfaces
// basically syntactic sugar for clients
public class ExampleClients3 {
    static List2<Integer> doubleAll(List2<Integer> xs) {
        return xs.map((Integer x) -> x * 2);
    }

    static int countNs(List2<Integer> xs, final int n) {
        return xs.filter((Integer x) -> x == n).length();
    }
}