public class InvokeStatic extends InvokeStaticSuper {
    static int ldc() {
        return 42;
    }

    public static boolean test() {
        return ldc() == 42 & ldc_super() == 42;
    }
}
