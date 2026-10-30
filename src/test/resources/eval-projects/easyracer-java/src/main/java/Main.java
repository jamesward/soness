import java.net.URI;
import java.util.List;

public class Main {

    /** Easy Racer scenarios 1, 2, 4, against the scenario server at {@code url}. Each returns the winner's body. */
    static class Scenarios {
        private final URI url;

        Scenarios(URI url) {
            this.url = url;
        }

        public String scenario1() throws Exception {
            throw new UnsupportedOperationException("scenario 1 is not implemented yet");
        }

        public String scenario2() throws Exception {
            throw new UnsupportedOperationException("scenario 2 is not implemented yet");
        }

        public String scenario4() throws Exception {
            throw new UnsupportedOperationException("scenario 4 is not implemented yet");
        }

        public List<String> results() throws Exception {
            return List.of(scenario1(), scenario2(), scenario4());
        }
    }

    public static void main(String[] args) throws Exception {
        var scenarios = new Scenarios(new URI(args.length > 0 ? args[0] : "http://localhost:8080"));
        scenarios.results().forEach(System.out::println);
    }
}
