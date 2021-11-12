public class InvokeStatic extends InvokeStaticSuper {
    static int get() {
        return 42;
    }

    public static boolean test(boolean is_true, boolean is_false) {
        return get_super(get(), is_true, is_false) & is_true & !is_false;
    }
}
