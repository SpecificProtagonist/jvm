public class Arrays {
    public static boolean test() {
        boolean[] primitive = new boolean[4];
        primitive[2] = true;
        Object[] objects = new Object[4];
        objects[2] = new Object();
        return primitive.length == 4 & primitive[2] & objects.length == 4 & objects[2] != null;
    }
}
