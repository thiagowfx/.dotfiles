# Helper functions for Alpine Linux.

apklint() {
	abuild sanitycheck
	apkbuild-lint "${1:-APKBUILD}"
}
