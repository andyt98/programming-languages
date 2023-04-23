public class ForCountNs2 implements Pred<Integer> {
    int n; // aha, explicit environment, created by constructor

    ForCountNs2(int _n) {
        n = _n;
    }

    public boolean m(Integer x) {
        return x == n;
    }
}