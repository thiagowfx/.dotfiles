/**
 * Pi Notify Extension
 *
 * Alerts when Pi agent is done and waiting for input without desktop notifications.
 * Uses terminal bell on every platform plus macOS Glass chime.
 */

import { execFile } from "node:child_process";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

function playMacOSChime(): void {
	if (process.platform !== "darwin") return;
	execFile("/usr/bin/afplay", ["/System/Library/Sounds/Glass.aiff"], () => {});
}

function notify(): void {
	process.stdout.write("\x07");
	playMacOSChime();
}

export default function (pi: ExtensionAPI) {
	pi.on("agent_settled", async () => {
		notify();
	});
}
