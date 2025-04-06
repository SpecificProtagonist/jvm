class Interpreter {
    static void fibonacci(int n) {
        long a = 0;
        long b = 1;
        for(int i = 0; i<n; i++) {
            long next = a + b;
            a = b;
            b = next;
        }
    }
}
