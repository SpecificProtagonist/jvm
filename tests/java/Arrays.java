public class Arrays {
    public static boolean char_array() {
        char[] array = new char[4];
        array[2] = 'A';
        return array.length == 4 & array[2] == 'A';
    }

    public static boolean double_array() {
        double[] array = new double[4];
        array[2] = 3.;
        return array.length == 4 & array[2] == 3.;
    }

    public static boolean object_array() {
        Object[] array = new Object[4];
        Object object = new Object();
        array[2] = object;
        return array.length == 4 & array[2] == object;
    }

    public static boolean multianewarray() {
        boolean[][][] foo = new boolean[1][2][3];
        boolean lengths_match = (foo.length == 1) & (foo[0].length == 2) & (foo[0][0].length == 3);
        foo[0][1][2] = true;
        return lengths_match & foo[0][1][2];
    }
}
