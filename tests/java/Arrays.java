public class Arrays {
    public static boolean test() {
        boolean[] primitive = new boolean[4];
        primitive[2] = true;
        Object[] objects = new Object[4];
        objects[2] = new Object();
        return primitive.length == 4 & primitive[2] & objects.length == 4 & objects[2] != null;
    }

    public static boolean multianewarray() {
        boolean[][][] foo = new boolean[1][2][3];
        boolean lengths_match = (foo.length == 1) & (foo[0].length == 2) & (foo[0][0].length == 3);
        foo[0][1][2] = true;
        return lengths_match & foo[0][1][2];
    }
}
