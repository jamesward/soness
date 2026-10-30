import java.net.URI;
import java.util.concurrent.*;

/**
 * The eval's hidden check of an Easy Racer client: each scenario of Main.Scenarios named in args[2] (comma-separated,
 * default all 11) against the scenario server at args[0], one at a time, each with args[1] seconds, args[3] times
 * (default 1: the server keeps state per scenario, so a second run on it isn't reliable); prints one line per
 * scenario, "N right|wrong <body>|error <message>".
 */
public class Validate {
    public static void main(String[] args) throws Exception {
        var scenarios = new Main.Scenarios(new URI(args[0]));
        var limit = Long.parseLong(args.length > 1 ? args[1] : "90");
        var runs = args.length > 3 ? Integer.parseInt(args[3]) : 1;
        var which = args.length > 2 ? args[2].split(",") : "1,2,3,4,5,6,7,8,9,10,11".split(",");
        for (var w : which) {
            final int n = Integer.parseInt(w.trim());
            java.lang.reflect.Method method;
            try {
                method = Main.Scenarios.class.getMethod("scenario" + n);
            } catch (NoSuchMethodException e) {
                System.out.println(n + " error no scenario" + n + "() method");
                continue;
            }
            // a race done wrong can still win by luck: each scenario runs `runs` times, and must be right every time
            String line = "right";
            for (int run = 1; run <= runs && line.equals("right"); run++) {
                final var m = method;
                var future = Executors.newVirtualThreadPerTaskExecutor().submit(() -> m.invoke(scenarios));
                String result;
                try {
                    var body = String.valueOf(future.get(limit, TimeUnit.SECONDS));
                    result = body.equals("right") ? "right" : "wrong " + body.replace('\n', ' ');
                } catch (TimeoutException e) {
                    future.cancel(true);
                    result = "error timed out after " + limit + "s";
                } catch (ExecutionException e) {
                    var cause = e.getCause() instanceof java.lang.reflect.InvocationTargetException ite ? ite.getCause() : e.getCause();
                    result = "error " + String.valueOf(cause).replace('\n', ' ');
                }
                line = result.equals("right") ? "right" : result + (runs > 1 ? " (run " + run + " of " + runs + ")" : "");
            }
            System.out.println(n + " " + line);
        }
        System.exit(0);
    }
}
