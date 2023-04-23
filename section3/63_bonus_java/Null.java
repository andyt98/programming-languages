class Null<T> extends List2<T> {
    Null() {
    } // could omit this as it's implied

    // ooh, covariant subtyping on return type
    <B> Null<B> map(Func<B, T> f) {
        return new Null<B>();
        // the above is "right" for the type system but wasteful
        // this will rightfully generate a warning but would be okay:
        // return (Null<B>)this;
    }

    Null<T> filter(Pred<T> f) {
        return this;
    }

    int length() {
        return 0;
    }
}