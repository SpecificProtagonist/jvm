public class GetStatic {
    static int value;

    public static void set(int val) {
        value = val;
    }

    public static int get() {
        return value;
    }
}