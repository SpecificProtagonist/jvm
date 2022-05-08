// Test both static and instance get/put
public class FieldAccess {
    static FieldAccess instance;
    int value;

    public static void set(int val) {
        instance = new FieldAccess();
        instance.value = val;
    }

    public static int get() {
        return instance.value;
    }
}