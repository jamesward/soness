# Design Principals

- System One is the Orchastrator
- System One is fast, and cheap. So use it abundently.
- LLMs fill in tool params and summarize.
- Context is a CQRS style event log, with views (agent driven)
- Within a session, users shouldn't need to do any manual context management (compact, etc)
- Tool Calls / Processes don't block & don't poll
- Parellize everything possible
- Don't wait around for something to finish, figure out what to do while you're waiting
- Coordinate across all concurrent surfaces (worktrees are an anti-pattern)
- Decide with tools instead of script (code mode is an anti-pattern)
- Memoize tool output (in the event log) and don't re-run things when we can instead filter / search in the results of a previous tool run
- Let the user prompt to redirect, add / change goals, etc (Esc is an anti-pattern)
- A prompt is a node in the graph. Nodes can finish. New prompts when there are existing nodes, are just new nodes; coordinated. For example, if node a is running tests, and node b made updates and wants to run tests, node a's tests should be canceled and replaced by node b's new test run.
