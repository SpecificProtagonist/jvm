public class ManyAllocs {
    static void test() {
        for (int i = 0; i < 200000; i++) {
            new Object();
        }
    }
}