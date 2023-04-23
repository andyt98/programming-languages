public class NonEmptyList<T> extends List2<T> {
    T head;
    List2<T> tail;

    NonEmptyList(T x, List2<T> xs) {
        head = x;
        tail = xs;
    }

    // ooh, covariant subtyping on return type
    <B> NonEmptyList<B> map(Func<B, T> f) {
        return new NonEmptyList<B>(f.m(head), tail.map(f));
    }

    List2<T> filter(Pred<T> f) {
        if (f.m(head)) return new NonEmptyList<T>(head, tail.filter(f));
        return tail.filter(f);
    }

    int length() {
        return 1 + tail.length();
    }
}