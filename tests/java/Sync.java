public class Sync {
    static int static_counter;
    int instance_counter;

    static synchronized void static_sync() {
        for(int i = 0; i<100; i++) {}
        static_counter += 1;
    }

    void instance_sync() {
        synchronized(this) {
            for(int i = 0; i<100; i++) {}
            instance_counter += 1;
        }
    }
}
