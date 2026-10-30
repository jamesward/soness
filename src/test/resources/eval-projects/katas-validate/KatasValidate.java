import java.util.*;

/** The eval's hidden check: more cases per kata than the project's tests; prints "PASS <kata>" or "FAIL <kata>: why". */
public class KatasValidate {
    interface Check { void run() throws Exception; }

    private static String fail = null;

    private static void eq(Object actual, Object expected) {
        if (fail == null && !Objects.equals(actual, expected)) fail = "expected " + expected + ", got " + actual;
    }

    public static void main(String[] args) {
        var checks = new LinkedHashMap<String, Check>();
        checks.put("roman", () -> {
            eq(Katas.roman(3), "III"); eq(Katas.roman(9), "IX"); eq(Katas.roman(40), "XL"); eq(Katas.roman(90), "XC");
            eq(Katas.roman(400), "CD"); eq(Katas.roman(3999), "MMMCMXCIX"); eq(Katas.roman(2024), "MMXXIV");
        });
        checks.put("runLength", () -> {
            eq(Katas.runLength("a"), "1a"); eq(Katas.runLength("aaaaaaaaaaaa"), "12a"); eq(Katas.runLength("abab"), "1a1b1a1b");
            eq(Katas.runLength("  x"), "2 1x");
        });
        checks.put("balanced", () -> {
            eq(Katas.balanced(""), true); eq(Katas.balanced("a(b[c]d)e"), true); eq(Katas.balanced("(("), false);
            eq(Katas.balanced("([)]"), false); eq(Katas.balanced("}{"), false); eq(Katas.balanced("{[()()]}"), true);
        });
        checks.put("wordFrequencies", () -> {
            eq(Katas.wordFrequencies("b a b a c"), "a=2,b=2,c=1");
            eq(Katas.wordFrequencies("Don't stop, don't STOP!"), "don't=2,stop=2");
            eq(Katas.wordFrequencies(""), "");
        });
        checks.put("luhn", () -> {
            eq(Katas.luhn("0"), false); eq(Katas.luhn(" 0 0 "), true); eq(Katas.luhn("059"), true);
            eq(Katas.luhn("055-444-285"), false); eq(Katas.luhn("091"), true); eq(Katas.luhn("59a"), false);
        });
        checks.put("spiral", () -> {
            eq(Katas.spiral(1), "1"); eq(Katas.spiral(2), "1 2/4 3");
            eq(Katas.spiral(4), "1 2 3 4/12 13 14 5/11 16 15 6/10 9 8 7");
        });
        for (var e : checks.entrySet()) {
            fail = null;
            try { e.getValue().run(); } catch (Throwable t) { fail = String.valueOf(t); }
            System.out.println(fail == null ? "PASS " + e.getKey() : "FAIL " + e.getKey() + ": " + fail);
        }
    }
}
