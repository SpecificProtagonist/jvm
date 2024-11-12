class Monitor {
    void stress(int iterations, int per_iteration) {
        for(int i = 0; i<iterations; i++) {
            synchronized (this) {
                for(int delay=0; delay<per_iteration; delay++) {}
            }
        }
    }
}
