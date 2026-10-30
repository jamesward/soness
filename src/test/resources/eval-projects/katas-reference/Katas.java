import java.util.*;
import java.util.stream.*;

public class Katas {
    public static String roman(int n) {
        int[] v = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
        String[] s = {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
        var b = new StringBuilder();
        for (int i = 0; i < v.length; i++) while (n >= v[i]) { b.append(s[i]); n -= v[i]; }
        return b.toString();
    }
    public static String runLength(String s) {
        var b = new StringBuilder();
        for (int i = 0; i < s.length();) { int j = i; while (j < s.length() && s.charAt(j) == s.charAt(i)) j++; b.append(j - i).append(s.charAt(i)); i = j; }
        return b.toString();
    }
    public static boolean balanced(String s) {
        var st = new ArrayDeque<Character>();
        for (char c : s.toCharArray()) {
            if ("([{".indexOf(c) >= 0) st.push(c);
            else if (")]}".indexOf(c) >= 0) { if (st.isEmpty() || "([{".indexOf(st.pop()) != ")]}".indexOf(c)) return false; }
        }
        return st.isEmpty();
    }
    public static String wordFrequencies(String text) {
        var m = new HashMap<String, Integer>();
        var mt = java.util.regex.Pattern.compile("[A-Za-z']+").matcher(text);
        while (mt.find()) m.merge(mt.group().toLowerCase(), 1, Integer::sum);
        return m.entrySet().stream().sorted(Map.Entry.<String, Integer>comparingByValue().reversed().thenComparing(Map.Entry.comparingByKey()))
            .map(e -> e.getKey() + "=" + e.getValue()).collect(Collectors.joining(","));
    }
    public static boolean luhn(String number) {
        var d = number.replace(" ", "");
        if (d.length() < 2 || !d.chars().allMatch(Character::isDigit)) return false;
        int sum = 0;
        for (int i = 0; i < d.length(); i++) { int x = d.charAt(d.length() - 1 - i) - '0'; if (i % 2 == 1) { x *= 2; if (x > 9) x -= 9; } sum += x; }
        return sum % 10 == 0;
    }
    public static String spiral(int n) {
        int[][] m = new int[n][n]; int t = 0, b = n - 1, l = 0, r = n - 1, k = 1;
        while (k <= n * n) {
            for (int j = l; j <= r; j++) m[t][j] = k++; t++;
            for (int i = t; i <= b; i++) m[i][r] = k++; r--;
            for (int j = r; j >= l; j--) m[b][j] = k++; b--;
            for (int i = b; i >= t; i--) m[i][l] = k++; l++;
        }
        return Arrays.stream(m).map(row -> Arrays.stream(row).mapToObj(String::valueOf).collect(Collectors.joining(" "))).collect(Collectors.joining("/"));
    }
}
