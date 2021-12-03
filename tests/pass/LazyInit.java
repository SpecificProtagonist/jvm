public class LazyInit {
    public static int foo;

    public static int test(boolean init_2) {
        if (init_2) {
            new LazyInitHelper2();
        } else {
            new LazyInitHelper1();
        }
        return foo;
    }
}