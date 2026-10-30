---
name: database-connection-management
description: Managing datasources/database connections for DataGrip and other JetBrains IDEs. Use when you need to create, edit, or test datasources/database connections, listing configured data sources, and verifying connectivity.
allowed-tools: execute_tool
---

# Manage Datasources/Database Connections

> Prefer `datagrip` MCP server if it is connected over the `idea` MCP server.

Use these tools to create, edit, test, and discover JetBrains IDEs data sources/database connections. Prefer them over the terminal (`psql`, `mysql`, generic shell) for setting up and verifying connections.

## Core Concepts

- **`connectionId`** — the id of an existing data source; required to edit or test a connection. Get it with `list_database_connections` — never invent or guess it.
- **DDL data sources** — collections of SQL scripts with no live connection. `test_database_connection` and `edit_database_connection` refuse to run against them. Inspect `isDDL` from `list_database_connections` before calling them.
- **Diagnostic info** — `create_database_connection` and `edit_database_connection` return the connection test result directly (`hasProblems`, `connectionInfo`, `summary`); the discovery tools return structured JSON.

## Tool Capabilities

Use the universal `execute_tool` to call any of the connection tools.

### Command syntax

The `command` string has exactly one accepted shape:

```
toolName --paramName1 value1 --paramName2 value2
```

- Every parameter is a `--`-prefixed flag, separated from its value by a **space** — never `=`.
- **Do not** use `name=value` or `--name=value`. **Do not** use positional arguments. All are rejected.
- Quote values that contain spaces, e.g. `--name 'PostgreSQL localhost:5432'`.

Good vs. bad:
- Good: `test_database_connection --id <id>`
- Bad: `test_database_connection id=<id>` (no `--`, uses `=`)
- Bad: `test_database_connection --id=<id>` (uses `=`)
- Bad: `test_database_connection <id>` (positional)

### Create a new connection

Use this when the user asks to add, create, or set up a new DB data source. With `needToCheckDs=true` the tool also runs a connection test after creation and returns the diagnostic info — surface it to the user without a separate `test_database_connection` call.

- `execute_tool(command="create_database_connection --name <name> --dbms <dbms> --url <jdbc-url> --needToCheckDs true")` — creates a new data source, runs a connection test against it (when `needToCheckDs=true`), and returns the diagnostic info (`hasProblems`: yes/no/unknown, `connectionInfo`: DBMS type/version/driver, `summary`: test result, with DBMS-provided error description on failure). Save the connection name/id for subsequent operations.

Tool parameters:
- `name` — unique, descriptive connection name.
- `dbms` — DBMS name (`PostgreSQL`, `MySQL`, `Oracle`, `MariaDB`, `Microsoft SQL Server`, etc.).
- `url` — fully-formed JDBC URL.
- `needToCheckDs` — `true` to test the connection right after creation; `false` to skip the test. **Set to `false` when creating more than one connection in a batch** — the per-connection probe is expensive. After the batch, call `test_database_connection` for the connections that actually need to be verified. When `false` the returned diagnostic carries `hasProblems=unknown` and only the new connection id.

**You assemble the JDBC URL yourself from connection components — do not ask the user for the raw `jdbc:...` string.** The user is expected to provide the underlying pieces; you combine them via the templates below.

Information to request from the user (when not already provided in the conversation):
- DBMS (PostgreSQL / MySQL / Oracle / Microsoft SQL Server / MariaDB / …).
- Host
- Port.
- Database name — or SID / service name for Oracle, if relevant for the chosen DBMS.
- Any DBMS-specific extras when the engine has multiple common URL shapes (e.g. Oracle SID vs service name, SQL Server `databaseName=` vs `;` parameters).

**Do not guess or invent missing components.** If any required piece cannot be determined from the user's message or from clear context, stop and ask the user before proceeding. Specifically, ask when:
- The DBMS is not stated and cannot be unambiguously inferred (e.g. user said just "create a connection" without naming the engine).
- The host or port is missing — do not assume `localhost` or a default port.
- The DBMS has multiple common URL shapes and the choice is unclear (see above).

When asking, list only the components that are still missing — do not re-ask for values the user already provided. Prefer collecting everything in one round.

What you **may** infer without asking:
- A connection `name`, if not given — generate a descriptive one (e.g. `PostgreSQL localhost:54339`) and mention it in the final report so the user can rename it.
- Omitting the database segment when the user clearly didn't specify a database (e.g. `jdbc:postgresql://<host>:<port>/`).

JDBC URL templates — assemble the `url` argument from these once all required components are known:
- PostgreSQL: `jdbc:postgresql://<host>:<port>/<database>`
- MySQL: `jdbc:mysql://<host>:<port>/<database>`
- Oracle: `jdbc:oracle:thin:@<host>:<port>:<sid>`
- SQL Server: `jdbc:sqlserver://<host>:<port>;databaseName=<database>`

Report the result from the returned diagnostic info:
- Name and ID of the created connection.
- DBMS version and ping on success.
- Error message on failure, with a hint to check whether the DB is running and the port is reachable.

Notes:
- Do not ask the user for credentials (username/password). The tool does not accept them as separate parameters, and the user normally enters them through the DataGrip UI when authentication is required. Exception: if the user has already provided credentials in the conversation, embed them as JDBC URL parameters when assembling `url` (e.g. append `?user=<user>&password=<password>` for PostgreSQL/MySQL, or `;user=<user>;password=<password>` for SQL Server).
- If the returned diagnostic shows a failure, apply the retry rules in "Edit an existing connection" before calling `edit_database_connection` to correct the URL.
- `test_database_connection` does not work for DDL data sources (they have no real DBMS connection).
- Always pass `projectPath` when the underlying tool exposes it — this reduces ambiguity when multiple projects are open.

### Edit an existing connection

Use this when the user asks to change an existing data source's DBMS or connection URL (e.g. move to a different host/port, switch database name, or fix a wrong driver). The tool updates `dbms` and `url` of the data source identified by `connectionId`; the connection `name` is preserved.

- `execute_tool(command="edit_database_connection --connectionId <id> --dbms <dbms> --url <jdbc-url> --needToCheckDs true")` — updates the data source, runs a connection test against it (when `needToCheckDs=true`), and returns the diagnostic info (`hasProblems`: yes/no/unknown, `connectionInfo`: DBMS type/version/driver, `summary`: test result, with DBMS-provided error description on failure). With `needToCheckDs=true`, do not call `test_database_connection` separately.

Tool parameters:
- `connectionId` — id of the existing data source (from `list_database_connections`); never invent or guess it.
- `dbms` — DBMS name; supply the current one if it does not change.
- `url` — fully-formed JDBC URL.
- `needToCheckDs` — `true` to test the connection after editing; `false` to skip the test. **Set to `false` when editing more than one connection in a batch** — same rationale as for `create_database_connection`. Call `test_database_connection` after the batch on the connections that need verification.

Same URL-assembly rules as for creation apply:
- **Do not ask the user for the raw `jdbc:...` string.** Ask only for the components that actually change (host, port, database name, DBMS-specific extras) and build the URL yourself using the per-DBMS templates listed under "Create a new connection".
- Do not guess missing components — if the user says "change the port" but the new port is not given, ask.
- It is fine to reuse the existing host/port/database from `list_database_connections` for components the user did not ask to change. Read them off the current data source rather than re-asking the user.

Report the result from the returned diagnostic info:
- The connection `id` and resulting `dbms` / `url`.
- DBMS version and ping on success.
- Error message on failure, with a hint to check whether the DB is running and the port is reachable.

Notes:
- This tool cannot rename a connection. If the user wants a new name, surface that limitation.
- Do not ask the user for credentials (username/password). Direct the user to the DataGrip UI when authentication needs to change. Exception: if the user explicitly provided new credentials in the conversation, embed them in the updated `url` as JDBC URL parameters (same syntax as in "Create a new connection").
- Cap retry attempts: do not call `edit_database_connection` more than twice in a row to fix the same failing connection without new input from the user. The counter resets only when the user provides new information (e.g. a different port, host, or database name).
- Before retrying, classify the failure from the returned diagnostic:
  - Network/host unreachable, port refused, or connect timeout → infrastructure issue, not fixable by editing; stop, report the error verbatim, and ask the user to verify that the database is running and the port is reachable. Do not retry on your own.
  - Driver / JDBC URL parse error / "no suitable driver" → at most one corrected attempt with re-checked DBMS, host, port, and database name.
  - Authentication failure → defer to the DataGrip UI (or embed user-provided credentials in `url` per the credentials rule above). Do not loop on authentication errors.
- Editing a DDL data source is not supported (no live DBMS connection).

### Discover connections

- `execute_tool(command="list_database_connections")` — lists every configured data source with `id`, `name`, `dbms`, `driverName`, `isDDL`, `readOnly`, `jdbcUrl`. Run first to resolve a `connectionId` before editing or testing.
- `execute_tool(command="test_database_connection --id <connectionId>")` — verifies the connection is configured correctly and the database is reachable (driver, credentials, network). Not valid for DDL data sources.

## Minimal Examples

```json
{"tool":"execute_tool","arguments":{"command":"list_database_connections"}}
```

```json
{"tool":"execute_tool","arguments":{"command":"create_database_connection --name 'PostgreSQL localhost:54339' --dbms PostgreSQL --url 'jdbc:postgresql://localhost:54339/' --needToCheckDs true"}}
```

```json
{"tool":"execute_tool","arguments":{"command":"edit_database_connection --connectionId 1c0a... --dbms PostgreSQL --url 'jdbc:postgresql://db.internal:5432/orders' --needToCheckDs true"}}
```

```json
{"tool":"execute_tool","arguments":{"command":"test_database_connection --id 1c0a1c0a-1234-1234-1234-1234567890ab"}}
```

## Short Rules

- Pass arguments as `--paramName value` (space-separated, `--` prefix). Never `name=value` or `--name=value`, never positional.
- Never hard-code or invent a `connectionId`; always resolve it through `list_database_connections`.
- When creating a new connection, never ask the user for the raw JDBC URL — ask for the components (DBMS, host, port, database name, DBMS-specific extras) and assemble the `url` yourself from the templates.
- The same rule applies to `edit_database_connection`: ask only for the components that change, reuse unchanged pieces from the existing data source, and assemble the new `url` yourself.
- Never invent missing `dbms`, host, or port — ask the user. A descriptive `name` may be generated if not given.
- `create_database_connection` and `edit_database_connection` return connection diagnostic info directly — surface it to the user without a separate `test_database_connection` call, unless `needToCheckDs=false` was used.
- When creating/editing more than one connection in a batch, pass `needToCheckDs=false` to skip the per-connection probe, then call `test_database_connection` only for the connections the user actually cares about.
- Cap consecutive failed `edit_database_connection` calls at 2 per user intent. Classify the failure (from the returned diagnostic) before any retry — never loop on network-unreachable or authentication errors.
- Inspect `isDDL` before edit or test operations — fail fast instead of letting the tool reject.
- After a connection exists, use the `database-tools` skill to browse schemas, introspect metadata, preview tables, and run SQL.
