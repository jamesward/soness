import java.util.*;
import java.util.function.*;

/** Each kata's test: `java KatasTest.java <kata>` runs one, no argument runs all; exits 1 when any fails. */
public class KatasTest {
    private static final Map<String, Runnable> TESTS = new LinkedHashMap<>();
    private static int failures = 0;

    private static void check(String kata, Object actual, Object expected) {
        if (!Objects.equals(actual, expected)) {
            failures++;
            System.out.println("FAIL " + kata + ": expected " + expected + ", got " + actual);
        }
    }

    static {
        TESTS.put("roman", () -> {
            check("roman", Katas.roman(1), "I");
            check("roman", Katas.roman(4), "IV");
            check("roman", Katas.roman(1994), "MCMXCIV");
        });
        TESTS.put("runLength", () -> {
            check("runLength", Katas.runLength("aaabcc"), "3a1b2c");
            check("runLength", Katas.runLength(""), "");
        });
        TESTS.put("balanced", () -> {
            check("balanced", Katas.balanced("([]{})"), true);
            check("balanced", Katas.balanced("(]"), false);
        });
        TESTS.put("wordFrequencies", () -> {
            check("wordFrequencies", Katas.wordFrequencies("the cat the end"), "the=2,cat=1,end=1");
        });
        TESTS.put("luhn", () -> {
            check("luhn", Katas.luhn("4539 3195 0343 6467"), true);
            check("luhn", Katas.luhn("8273 1232 7352 0569"), false);
        });
        TESTS.put("spiral", () -> {
            check("spiral", Katas.spiral(3), "1 2 3/8 9 4/7 6 5");
            check("spiral", Katas.spiral(0), "");
        });
    }

    public static void main(String[] args) {
        var names = args.length > 0 ? List.of(args) : List.copyOf(TESTS.keySet());
        for (var name : names) {
            var test = TESTS.get(name);
            if (test == null) { System.out.println("FAIL " + name + ": no such kata"); failures++; continue; }
            try {
                int before = failures;
                test.run();
                if (failures == before) System.out.println("PASS " + name);
            } catch (Throwable e) {
                failures++;
                System.out.println("FAIL " + name + ": " + e);
            }
        }
        System.exit(failures == 0 ? 0 : 1);
    }
}
