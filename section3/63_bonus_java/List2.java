// now here's a version that does the more OO thing.  If we use null instead
// instance methods, then clients have to check the null case, which is far
// less convenient, so we choose instead /not/ to represent empty lists with
// null.
public abstract class List2<T> {
    abstract <B> List2<B> map(Func<B, T> f);

    abstract List2<T> filter(Pred<T> f);

    abstract int length();
}