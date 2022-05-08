public class InvokeVirtual {
    int foo;

    InvokeVirtual(int foo) {
        this.foo = foo;
    }

    int get() {
        return this.foo;
    }

    public static boolean test() {
        InvokeVirtual obj = new InvokeVirtual(42);
        return obj.get() == 42;
    }
}
