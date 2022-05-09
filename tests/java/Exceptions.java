class Exceptions {
    void throwing(boolean npe, Exceptions null_pointer) {
        if (npe) {
            null_pointer.throwing(false, null);
        } else {
            int[] array = {};
            int bar = array[5];
        }
    }

    static boolean test(boolean npe) {
        try {
            Exceptions foo = new Exceptions();
            foo.throwing(npe, null);
        } catch (NullPointerException e) {
            return true;
        } catch (ArrayIndexOutOfBoundsException e) {
            return false;
        }
        throw new NullPointerException();
    }
}