#include <HsFFI.h>

static void hasklime_enter(void) __attribute__((constructor));
static void hasklime_enter(void) {
	static char* argv[] = {"hasklime", 0};
	static char** argv_ = argv;
	static int argc = 1;

	hs_init(&argc, &argv_);
}

static void hasklime_exit(void) __attribute__((destructor));
static void hasklime_exit(void) {
	hs_exit();
}
