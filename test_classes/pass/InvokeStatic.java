public class InvokeStatic {
    static int ldc() {
        return 42;
    }

    public static int add(int a) {
        return a + ldc();
    }
}
