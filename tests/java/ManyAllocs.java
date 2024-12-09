public class ManyAllocs {
    static void test() {
        for (int i = 0; i < 100000; i++) {
            new Object();
        }
    }
}
