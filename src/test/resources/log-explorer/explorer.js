// The soness log explorer's page for one log: its totals, its turns, and every record, filterable, each opening to
// what was sent where (requests, responses, arguments, results) with its time and tokens. Plain JS: no build step.
(() => {
  const app = document.getElementById("app");
  const file = app.dataset.file;
  const q = (params) => new URLSearchParams({ file, ...params }).toString();

  const KINDS = {
    Prompt: "bg-sky-700", Answer: "bg-emerald-700", Metrics: "bg-emerald-900",
    Jev: "bg-violet-700", Llm: "bg-fuchsia-700", Writer: "bg-pink-700",
    ToolCall: "bg-amber-700", ToolResult: "bg-amber-900", ToolFailed: "bg-red-700",
    Acp: "bg-slate-700", Warning: "bg-orange-700", Info: "bg-slate-600", Debug: "bg-slate-800",
  };
  // ACP's streamed chunks are most of a log's records: off by default
  const CHUNKS = /agent_message_chunk|agent_thought_chunk|user_message_chunk|_kiro\.dev\//;

  const el = (tag, attrs = {}, ...children) => {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs)) {
      if (k === "class") e.className = v;
      else if (k.startsWith("on")) e.addEventListener(k.slice(2), v);
      else if (v !== undefined && v !== null) e.setAttribute(k, v);
    }
    for (const c of children.flat()) if (c !== null && c !== undefined) e.append(c instanceof Node ? c : String(c));
    return e;
  };
  const ms = (n) => (n === null || n === undefined ? "" : n < 1000 ? `${n} ms` : `${(n / 1000).toFixed(1)} s`);
  const tok = (n) => (n ? (n >= 10000 ? `${(n / 1000).toFixed(1)}k` : String(n)) : "");
  const tokens = (i, o) => (i || o ? `${tok(i) || 0} / ${tok(o) || 0}` : "–");
  const clock = (t) => new Date(t).toISOString().slice(11, 23);

  const state = { data: null, kinds: new Set(), turn: "", session: "", text: "", chunks: false, open: new Set(), limit: 1500 };

  const filtered = () =>
    state.data.entries.filter((e) =>
      state.kinds.has(e.kind) &&
      (!state.turn || e.turn === state.turn) &&
      (!state.session || e.session === state.session) &&
      (state.chunks || e.kind !== "Acp" || !CHUNKS.test(e.summary)) &&
      (!state.text || `${e.title} ${e.target ?? ""} ${e.summary}`.toLowerCase().includes(state.text)));

  function totals() {
    return el("section", { class: "grid grid-cols-2 gap-2 md:grid-cols-4 xl:grid-cols-7" },
      state.data.totals.map((t) =>
        el("div", { class: "rounded border border-slate-800 bg-slate-900 p-2" },
          el("div", { class: "flex items-center gap-2" },
            el("span", { class: `rounded px-1.5 text-xs text-white ${KINDS[t.kind] ?? ""}` }, t.kind),
            el("span", { class: "font-semibold" }, t.count)),
          el("div", { class: "mt-1 text-xs text-slate-400" },
            t.ms ? `${ms(t.ms)} total` : "",
            t.inputTokens || t.outputTokens ? ` · ${tok(t.inputTokens)} in / ${tok(t.outputTokens)} out` : ""))));
  }

  function turns() {
    if (!state.data.turns.length) return el("p", { class: "text-slate-400" }, "No turns (prompts) in this log.");
    const head = ["turn", "request", "ending", "time", "tools", "Jev", "Jev tokens", "LLM", "LLM tokens", "tool time"];
    return el("section", {},
      el("h2", { class: "mb-1 font-semibold text-slate-300" }, "Turns"),
      el("table", { class: "w-full" },
        el("thead", {}, el("tr", { class: "text-left text-xs text-slate-400" }, head.map((h) => el("th", { class: "px-1" }, h)))),
        el("tbody", {}, state.data.turns.map((t) =>
          el("tr", {
            class: `cursor-pointer border-t border-slate-800 hover:bg-slate-900 ${state.turn === t.id ? "bg-slate-800" : ""}`,
            onclick: () => { state.turn = state.turn === t.id ? "" : t.id; render(); },
          },
            el("td", { class: "px-1 font-mono text-xs text-cyan-300" }, t.id),
            el("td", { class: "max-w-xl truncate px-1", title: t.request }, t.request),
            el("td", { class: `px-1 ${t.ending === "answered" ? "text-emerald-400" : "text-orange-400"}` }, t.ending),
            el("td", { class: "px-1" }, ms(t.wallMs)),
            el("td", { class: "px-1" }, t.toolCalls, t.failedCalls ? el("span", { class: "text-red-400" }, ` (${t.failedCalls} failed)`) : ""),
            el("td", { class: "px-1" }, `${t.jevCalls} · ${ms(t.jevMs)}`),
            el("td", { class: "px-1" }, tokens(t.jevInputTokens, t.jevOutputTokens)),
            el("td", { class: "px-1" }, `${t.llmCalls} · ${ms(t.llmMs)}`),
            el("td", { class: "px-1", title: "an ACP writer (Kiro) reports no tokens" }, tokens(t.llmInputTokens, t.llmOutputTokens)),
            el("td", { class: "px-1" }, ms(t.toolMs)))))));
  }

  function filters() {
    const sessions = [...new Set(state.data.entries.map((e) => e.session).filter(Boolean))];
    const select = (value, options, on) =>
      el("select", { class: "rounded bg-slate-900 px-1 py-0.5", onchange: (ev) => on(ev.target.value) },
        options.map(([v, label]) => { const o = el("option", { value: v }, label); if (v === value) o.selected = true; return o; }));
    return el("section", { class: "flex flex-wrap items-center gap-2" },
      Object.keys(KINDS).filter((k) => state.data.totals.some((t) => t.kind === k)).map((k) =>
        el("button", {
          class: `rounded px-1.5 text-xs text-white ${KINDS[k]} ${state.kinds.has(k) ? "" : "opacity-30"}`,
          onclick: () => { state.kinds.has(k) ? state.kinds.delete(k) : state.kinds.add(k); render(); },
        }, k)),
      select(state.turn, [["", "all turns"], ...state.data.turns.map((t) => [t.id, t.id])], (v) => { state.turn = v; render(); }),
      select(state.session, [["", "all sessions"], ...sessions.map((s) => [s, s.slice(0, 8)])], (v) => { state.session = v; render(); }),
      el("label", { class: "flex items-center gap-1 text-xs" },
        (() => { const c = el("input", { type: "checkbox", onchange: (ev) => { state.chunks = ev.target.checked; render(); } }); c.checked = state.chunks; return c; })(),
        "ACP chunks"),
      (() => {
        const i = el("input", { class: "w-64 rounded bg-slate-900 px-2 py-0.5", placeholder: "search", value: state.text });
        i.addEventListener("input", () => { state.text = i.value.toLowerCase(); renderEntries(); });
        return i;
      })());
  }

  // a bar per entry that took time, from its start (logged at its end) to its end, over the shown entries' span
  function bar(e, from, span) {
    const start = e.t - (e.ms ?? 0);
    const left = ((start - from) / span) * 100;
    const width = Math.max(((e.ms ?? 0) / span) * 100, 0.4);
    return el("div", { class: "relative h-2 w-40 rounded bg-slate-900" },
      el("div", { class: `absolute h-2 rounded ${KINDS[e.kind] ?? "bg-slate-500"}`, style: `left:${left}%;width:${width}%` }));
  }

  function partView(p) {
    const body = p.json !== null && p.json !== undefined ? JSON.stringify(p.json, null, 2) : p.text ?? "";
    const big = body.length > 4000;
    const pre = el("pre", { class: "max-h-[32rem] overflow-auto whitespace-pre-wrap break-words rounded bg-slate-950 p-2 font-mono text-xs" }, body);
    const copy = el("button", { class: "text-xs text-cyan-300 hover:underline", onclick: () => navigator.clipboard.writeText(body) }, "copy");
    const d = el("details", { class: "rounded border border-slate-800" },
      el("summary", { class: "cursor-pointer px-2 py-1 text-xs text-slate-300" },
        `${p.label} `, el("span", { class: "text-slate-500" }, `${body.length.toLocaleString()} chars`), " ", copy),
      pre);
    if (!big) d.open = true;
    return d;
  }

  async function detail(i, cell) {
    cell.replaceChildren(el("span", { class: "text-slate-400" }, "Loading…"));
    const r = await fetch(`/api/entry?${q({ i })}`);
    if (!r.ok) { cell.replaceChildren(el("span", { class: "text-red-400" }, await r.text())); return; }
    const d = await r.json();
    const e = d.entry;
    cell.replaceChildren(el("div", { class: "space-y-2 p-2" },
      el("div", { class: "flex flex-wrap gap-4 text-xs text-slate-400" },
        el("span", {}, `at ${new Date(e.t).toISOString()}`),
        e.ms !== undefined && e.ms !== null ? el("span", {}, `took ${ms(e.ms)}`) : null,
        e.inputTokens || e.outputTokens ? el("span", {}, `${e.inputTokens ?? 0} tokens in, ${e.outputTokens ?? 0} out`) : null,
        e.session ? el("span", {}, `session ${e.session}`) : null,
        e.loop ? el("span", {}, `loop ${e.loop}`) : null,
        e.turn ? el("span", {}, `turn ${e.turn}`) : null),
      d.parts.map(partView),
      partView({ label: "as logged", text: d.raw })));
    cell.querySelectorAll("details").forEach((x, n, all) => { if (n === all.length - 1) x.open = false; });
  }

  let entriesBox;
  function renderEntries() {
    const shown = filtered();
    const rows = shown.slice(0, state.limit);
    const from = Math.min(...rows.map((e) => e.t - (e.ms ?? 0)));
    const span = Math.max(Math.max(...rows.map((e) => e.t)) - from, 1);
    const start = rows.length ? rows[0].t : 0;
    const body = el("tbody", {});
    for (const e of rows) {
      const cell = el("td", { colspan: 8, class: "bg-slate-900" });
      const extra = el("tr", { class: state.open.has(e.i) ? "" : "hidden" }, cell);
      const row = el("tr", {
        id: `e${e.i}`,
        class: "cursor-pointer border-t border-slate-800/60 hover:bg-slate-900",
        onclick: () => {
          if (state.open.has(e.i)) { state.open.delete(e.i); extra.classList.add("hidden"); }
          else { state.open.add(e.i); extra.classList.remove("hidden"); detail(e.i, cell); history.replaceState(null, "", `#e${e.i}`); }
        },
      },
        el("td", { class: "whitespace-nowrap px-1 font-mono text-xs text-slate-400", title: clock(e.t) }, `+${((e.t - start) / 1000).toFixed(2)}s`),
        el("td", { class: "px-1" }, el("span", { class: `rounded px-1.5 text-xs text-white ${KINDS[e.kind] ?? ""}` }, e.kind)),
        el("td", { class: "whitespace-nowrap px-1" }, e.title),
        el("td", { class: "whitespace-nowrap px-1 text-right" }, ms(e.ms)),
        el("td", { class: "whitespace-nowrap px-1 text-right text-xs" }, e.inputTokens || e.outputTokens ? `${tok(e.inputTokens)} / ${tok(e.outputTokens)}` : ""),
        el("td", { class: "px-1" }, e.ms ? bar(e, from, span) : ""),
        el("td", { class: "whitespace-nowrap px-1 font-mono text-xs text-slate-500" }, e.turn ?? ""),
        el("td", { class: "max-w-0 w-full truncate px-1 text-slate-300", title: e.summary }, e.summary));
      body.append(row, extra);
      if (state.open.has(e.i)) detail(e.i, cell);
    }
    entriesBox.replaceChildren(
      el("div", { class: "mb-1 text-xs text-slate-400" }, `${shown.length.toLocaleString()} of ${state.data.entries.length.toLocaleString()} records`,
        shown.length > rows.length
          ? el("button", { class: "ml-2 text-cyan-300 hover:underline", onclick: () => { state.limit += 1500; renderEntries(); } }, `show ${Math.min(1500, shown.length - rows.length)} more`)
          : null),
      el("table", { class: "w-full table-fixed" },
        el("colgroup", {}, ["w-20", "w-24", "w-56", "w-20", "w-28", "w-44", "w-28", ""].map((w) => el("col", { class: w }))),
        el("thead", {}, el("tr", { class: "text-left text-xs text-slate-400" },
          ["at", "kind", "what", "took", "tokens in/out", "timeline", "turn", "summary"].map((h) => el("th", { class: "px-1" }, h)))),
        body));
  }

  function render() {
    entriesBox = el("section", {});
    app.replaceChildren(totals(), turns(), filters(), entriesBox);
    renderEntries();
  }

  fetch(`/api/log?${q({})}`)
    .then((r) => (r.ok ? r.json() : r.text().then((t) => Promise.reject(new Error(t)))))
    .then((data) => {
      state.data = data;
      state.kinds = new Set(data.totals.map((t) => t.kind).filter((k) => k !== "Debug"));
      // a link to a record (`#e42`) opens it, shown whatever the filters would hide
      const linked = /^#e(\d+)$/.exec(location.hash);
      const target = linked && data.entries[Number(linked[1])];
      if (target) {
        state.open.add(target.i);
        state.kinds.add(target.kind);
        if (target.kind === "Acp") state.chunks = true;
        state.limit = Math.max(state.limit, target.i + 1);
      }
      render();
      if (target) document.getElementById(`e${target.i}`)?.scrollIntoView({ block: "start" });
    })
    .catch((err) => app.replaceChildren(el("p", { class: "text-red-400" }, `Couldn't read the log: ${err.message}`)));
})();
