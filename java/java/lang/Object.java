package java.lang;

public class Object {    
    
    public boolean equals(Object obj) {
        return (this == obj);
    }
    
    public native int hashCode();
    public final native void notify();
    public final native void notifyAll();
    public final native void wait();
}
