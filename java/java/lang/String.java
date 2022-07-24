package java.lang;

public class String extends Object {
    // The JVM populates this in the case of string literals.
    // Mutating it is forbidden
    char[] chars;
}