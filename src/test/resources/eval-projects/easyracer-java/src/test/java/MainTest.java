import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.HttpWaitStrategy;
import org.testcontainers.utility.DockerImageName;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Each scenario against the scenario server, one test each, so one can run alone: --tests 'MainTest.scenario2'. A
 * scenario that hasn't answered in 15 seconds fails (a race that never ends is a race done wrong; the slowest right
 * scenario takes about 9); it runs in a thread of its own, so one that ignores the interrupt fails at 15 seconds too,
 * and the other tests still run.
 */
@Timeout(value = 15, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
public class MainTest {
    private static GenericContainer<?> scenarioServer;
    private static Main.Scenarios scenarios;

    @BeforeAll
    static void start() throws Exception {
        // a scenario server already running (EASYRACER_URL, say one a developer keeps up between runs) is used as is:
        // a run is then seconds, not the container's start
        var running = System.getenv("EASYRACER_URL");
        if (running != null && !running.isBlank()) {
            scenarios = new Main.Scenarios(new URI(running));
            return;
        }
        scenarioServer = new GenericContainer<>(DockerImageName.parse("ghcr.io/jamesward/easyracer"))
            .withExposedPorts(8080).waitingFor(new HttpWaitStrategy());
        scenarioServer.start();
        scenarios = new Main.Scenarios(new URI("http://" + scenarioServer.getHost() + ":" + scenarioServer.getFirstMappedPort()));
    }

    @AfterAll
    static void stop() {
        if (scenarioServer != null) scenarioServer.stop();
    }

    @Test
    void scenario1() throws Exception {
        assertEquals("right", scenarios.scenario1());
    }

    @Test
    void scenario2() throws Exception {
        assertEquals("right", scenarios.scenario2());
    }

    @Test
    void scenario4() throws Exception {
        assertEquals("right", scenarios.scenario4());
    }

}
