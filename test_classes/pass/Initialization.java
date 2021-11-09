public class Initialization {
    // <clinit>
    static boolean foo = true;
    // ConstValue attribute
    final static boolean bar = true;

    public static boolean check_init() {
        return foo & bar;
    }
}
