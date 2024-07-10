public class Native {
    long value;

    Native() {
        this.value = 1;
    }

    public native void call_instance();
    
    public static native long call_static(int a, int b);
}
