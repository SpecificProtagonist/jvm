public class ControlFlow {
    public static boolean is_even(int a) {
        double acc = 0;
        for (int i = 1; i <= 2 * a; i++) {
            acc += (double) i;
        }
        return (int) acc % 2 == 0;
    }
}
