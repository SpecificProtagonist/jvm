public class InitLock {
    static {
        Counter.count += 1;
    }

    public static boolean check() {
        return Counter.count == 1;
    }

    static class Counter {
        static int count;
    }
}
