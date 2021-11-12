public class InvokeStaticSuper {
    static boolean get_super(long double_wide, boolean is_true, boolean is_false) {
        return double_wide == 42 & is_true & !is_false;
    }
}
