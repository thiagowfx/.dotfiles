# Helper functions for Alpine Linux.

# Lint the given APKBUILD.
apklint() {
	abuild sanitycheck
	apkbuild-lint "${1:-APKBUILD}"
}
