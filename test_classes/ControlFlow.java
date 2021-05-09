public class ControlFlow {
    public static boolean test(int a) {
        double acc = 0;
        for(int i = 0; i < a; i++) {
            acc += (double)i;
        }
        return (int)acc % 2 == 0;
    }
}
