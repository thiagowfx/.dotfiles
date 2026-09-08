import assert from "node:assert/strict";
import test from "node:test";
import { cmuxRoutesNotifications } from "../notify.ts";

test("lets cmux own completion notifications only for an active surface", () => {
	assert.equal(cmuxRoutesNotifications({ CMUX_SURFACE_ID: "surface" }), true);
	assert.equal(cmuxRoutesNotifications({ CMUX_SURFACE_ID: "surface", CMUX_PI_HOOKS_DISABLED: "1" }), false);
	assert.equal(cmuxRoutesNotifications({}), false);
});
