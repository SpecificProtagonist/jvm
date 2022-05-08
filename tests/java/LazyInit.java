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

    static class LazyInitHelper1 {
        static {
            LazyInit.foo = 1;
        }
    }

    static class LazyInitHelper2 {
        static {
            LazyInit.foo = 2;
        }
    }
}