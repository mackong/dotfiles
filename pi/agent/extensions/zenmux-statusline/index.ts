/**
 * ZenMux Status Line Extension for pi
 *
 * Two-line custom footer, ported from the Claude Code status-line.sh:
 *   Line 1: session info  → [model] 📁 dir 🌿 git | <ctx bar> N% ctx | ↑in ↓out $cost
 *   Line 2: ZenMux account → ⚡ ZenMux <plan> 🔑 <key> | 5h <bar> N% · 7d <bar> N% 💳 Bal $X
 *
 * Line 2 requires ZENMUX_MANAGEMENT_KEY in the environment. When absent, a
 * setup hint is shown instead. ZENMUX_API_KEY, if present, is displayed masked.
 *
 * Account data is fetched from the ZenMux management API and cached in-process
 * (120s TTL) with a background refresh so rendering stays instant.
 */

import type { AssistantMessage } from "@earendil-works/pi-ai";
import type { ExtensionAPI, Theme } from "@earendil-works/pi-coding-agent";
import { truncateToWidth } from "@earendil-works/pi-tui";

// ── ZenMux account fetching ──────────────────────────────────────────

interface QuotaInfo {
	usage_percentage?: number;
	resets_at?: string | null;
}

interface ZenmuxAccount {
	plan: string;
	status: string;
	fiveHourPct: number;
	fiveHourResetsAt: string | null;
	sevenDayPct: number;
	sevenDayResetsAt: string | null;
	paygBalance: number | null;
}

const MGMT_BASE = "https://zenmux.ai/api/v1/management";
const CACHE_TTL_MS = 120_000;

let cachedAccount: ZenmuxAccount | null = null;
let cacheTimestamp = 0;
let fetchInFlight = false;

function managementKey(): string | undefined {
	return process.env.ZENMUX_MANAGEMENT_KEY?.trim() || undefined;
}

async function fetchJson(path: string, key: string, signal: AbortSignal): Promise<any | null> {
	try {
		const res = await fetch(`${MGMT_BASE}${path}`, {
			headers: { Authorization: `Bearer ${key}` },
			signal,
		});
		if (!res.ok) return null;
		return await res.json();
	} catch {
		return null;
	}
}

async function refreshAccount(): Promise<void> {
	const key = managementKey();
	if (!key || fetchInFlight) return;
	fetchInFlight = true;

	const controller = new AbortController();
	const timeout = setTimeout(() => controller.abort(), 5000);

	try {
		const [sub, payg] = await Promise.all([
			fetchJson("/subscription/detail", key, controller.signal),
			fetchJson("/payg/balance", key, controller.signal),
		]);

		if (sub?.data) {
			const d = sub.data;
			const q5: QuotaInfo = d.quota_5_hour ?? {};
			const q7: QuotaInfo = d.quota_7_day ?? {};
			const rawPlan: string = d.plan?.tier ?? "—";
			const plan = rawPlan.charAt(0).toUpperCase() + rawPlan.slice(1).toLowerCase();

			cachedAccount = {
				plan,
				status: d.account_status ?? "unknown",
				fiveHourPct: Math.round((q5.usage_percentage ?? 0) * 100),
				fiveHourResetsAt: q5.resets_at ?? null,
				sevenDayPct: Math.round((q7.usage_percentage ?? 0) * 100),
				sevenDayResetsAt: q7.resets_at ?? null,
				paygBalance:
					typeof payg?.data?.total_credits === "number" ? payg.data.total_credits : null,
			};
			cacheTimestamp = Date.now();
		}
	} finally {
		clearTimeout(timeout);
		fetchInFlight = false;
	}
}

// ── Formatting helpers ───────────────────────────────────────────────

function fmtTokens(n: number): string {
	return n < 1000 ? `${n}` : `${(n / 1000).toFixed(1)}k`;
}

// Format ISO 8601 timestamp → "Xh Ym" / "Xm" / "soon" from now
function fmtTimeUntil(iso: string | null): string | null {
	if (!iso) return null;
	const target = Date.parse(iso);
	if (Number.isNaN(target)) return null;
	const diff = target - Date.now();
	if (diff <= 0) return "soon";
	const hours = Math.floor(diff / 3_600_000);
	const mins = Math.floor((diff % 3_600_000) / 60_000);
	return hours > 0 ? `${hours}h${mins}m` : `${mins}m`;
}

// Pick a theme color name based on a 0-100 usage percentage.
function usageColor(pct: number): "success" | "warning" | "error" {
	if (pct >= 90) return "error";
	if (pct >= 70) return "warning";
	return "success";
}

// Build a themed █/░ progress bar.
function makeBar(theme: Theme, pct: number, width: number): string {
	const clamped = Math.max(0, Math.min(100, pct));
	let filled = Math.round((clamped * width) / 100);
	if (filled > width) filled = width;
	const bar = "█".repeat(filled) + "░".repeat(width - filled);
	return theme.fg(usageColor(clamped), bar);
}

// Mask a ZenMux API key: sk-ss-v1-abc...xyz
function maskKey(raw: string): { type: string; masked: string } {
	const prefixMatch = raw.match(/^sk-[a-z]+-v[0-9]+/);
	const suffix = raw.slice(-3);
	let masked: string;
	if (prefixMatch) {
		const prefix = prefixMatch[0];
		masked = `${prefix}-${raw.slice(prefix.length + 1, prefix.length + 4)}...${suffix}`;
	} else {
		masked = `${raw.slice(0, 6)}...${suffix}`;
	}
	let type = "Key";
	if (raw.startsWith("sk-ss-v1-")) type = "Sub";
	else if (raw.startsWith("sk-ai-v1-")) type = "PAYG";
	return { type, masked };
}

// ── Extension ────────────────────────────────────────────────────────

export default function (pi: ExtensionAPI) {
	// Fires the background refresh when the cache is stale. Non-blocking.
	function ensureFresh(requestRender?: () => void): void {
		if (!managementKey()) return;
		if (fetchInFlight) return;
		if (cachedAccount && Date.now() - cacheTimestamp < CACHE_TTL_MS) return;
		void refreshAccount().then(() => requestRender?.());
	}

	pi.on("session_start", (_event, ctx) => {
		if (ctx.mode !== "tui") return;

		ctx.ui.setFooter((tui, theme, footerData) => {
			const unsub = footerData.onBranchChange(() => tui.requestRender());
			// Periodic refresh of ZenMux data so reset timers/balance stay current.
			const interval = setInterval(() => {
				ensureFresh(() => tui.requestRender());
			}, 30_000);
			ensureFresh(() => tui.requestRender());

			const dim = (t: string) => theme.fg("dim", t);
			const sep = ` ${dim("|")} `;

			return {
				dispose() {
					unsub();
					clearInterval(interval);
				},
				invalidate() {},
				render(width: number): string[] {
					ensureFresh(() => tui.requestRender());

					// ── Line 1: session info ──────────────────────────
					const modelId = ctx.model?.id ?? "no-model";
					const dirName = ctx.cwd.split("/").pop() || ctx.cwd;

					let input = 0;
					let output = 0;
					let cost = 0;
					for (const e of ctx.sessionManager.getBranch()) {
						if (e.type === "message" && e.message.role === "assistant") {
							const m = e.message as AssistantMessage;
							input += m.usage.input;
							output += m.usage.output;
							cost += m.usage.cost.total;
						}
					}

					const usage = ctx.getContextUsage();
					const ctxPct = usage?.percent != null ? Math.round(usage.percent) : 0;
					const ctxBar = makeBar(theme, ctxPct, 10);

					const branch = footerData.getGitBranch();
					const gitPart = branch ? `${sep}${theme.fg("success", `🌿 ${branch}`)}` : "";

					const line1 =
						theme.fg("accent", `[${modelId}]`) +
						` 📁 ${dirName}` +
						gitPart +
						sep +
						`${ctxBar} ${ctxPct}% ctx` +
						sep +
						dim(`↑${fmtTokens(input)} ↓${fmtTokens(output)} $${cost.toFixed(3)}`);

					const lines = [truncateToWidth(line1, width)];

					// ── Line 2: ZenMux account ────────────────────────
					lines.push(truncateToWidth(renderAccountLine(theme, dim, sep), width));

					return lines;
				},
			};
		});
	});

	pi.on("session_shutdown", (_event, ctx) => {
		if (ctx.mode === "tui") ctx.ui.setFooter(undefined);
	});
}

// Build the ZenMux account line (line 2).
function renderAccountLine(theme: Theme, dim: (t: string) => string, sep: string): string {
	if (!managementKey()) {
		return (
			dim("⚙ Set ") +
			theme.fg("warning", "ZENMUX_MANAGEMENT_KEY") +
			dim(" to display account data → ") +
			theme.fg("accent", "zenmux.ai/platform/management")
		);
	}

	if (!cachedAccount) {
		return dim("⚡ ZenMux · loading account…");
	}

	const acct = cachedAccount;
	const healthy = acct.status === "healthy";
	const statusPart = healthy
		? theme.fg("accent", `⚡ ZenMux ${acct.plan}`)
		: theme.fg("error", `⚡ ZenMux ${acct.plan} ⚠`);

	// API key (masked)
	let keyPart = "";
	const apiKey = process.env.ZENMUX_API_KEY?.trim();
	if (apiKey) {
		const { type, masked } = maskKey(apiKey);
		const typeColor = type === "Sub" ? "accent" : type === "PAYG" ? "warning" : "dim";
		keyPart = `${sep}🔑 ${theme.fg(typeColor, type)} ${dim(masked)}`;
	}

	// Quota bars
	const fiveBar = makeBar(theme, acct.fiveHourPct, 5);
	const sevenBar = makeBar(theme, acct.sevenDayPct, 5);

	let fiveReset = "";
	if (acct.fiveHourPct >= 100) {
		const eta = fmtTimeUntil(acct.fiveHourResetsAt);
		if (eta) fiveReset = ` ${theme.fg("warning", `⏳ ${eta}`)}`;
	}
	let sevenReset = "";
	if (acct.sevenDayPct >= 100) {
		const eta = fmtTimeUntil(acct.sevenDayResetsAt);
		if (eta) sevenReset = ` ${theme.fg("warning", `⏳ ${eta}`)}`;
	}

	// PAYG balance
	let paygPart = "";
	if (acct.paygBalance != null) {
		paygPart = `${sep}💳 Bal ${theme.fg("success", `$${acct.paygBalance.toFixed(2)}`)}`;
	}

	return (
		statusPart +
		keyPart +
		sep +
		`5h ${fiveBar} ${acct.fiveHourPct}%${fiveReset} ${dim("·")} 7d ${sevenBar} ${acct.sevenDayPct}%${sevenReset}` +
		paygPart
	);
}
