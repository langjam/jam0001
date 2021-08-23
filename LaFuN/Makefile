OUT ?= build

LIBSRCS := \
	src/fun/Codegen.cc \
	src/fun/IdentResolver.cc \
	src/fun/Lexer.cc \
	src/fun/parse.cc \
	src/fun/prelude.cc \
	src/fun/print.cc \
	src/lafun/parse.cc \
	src/lafun/prelude.cc \
	src/lafun/print.cc \
	src/lafun/codegen.cc \
	src/lafun/resolve.cc \
	src/Reader.cc \
#

MAINSRCS := \
	src/lafun.cc \
#

ALLSRCS := ${MAINSRCS} ${LIBSRCS}

CXX ?= g++
CXXFLAGS ?= -Isrc -std=c++17 -Wall -Wextra -g
LDFLAGS ?=
LDLIBS ?=

ifeq ($(SANITIZE),1)
	CXXFLAGS += -fsanitize=address,undefined
	LDFLAGS += -fsanitize=address,undefined
endif

all: $(OUT)/lafun

$(OUT)/lafun: $(OUT)/src/lafun.cc.o $(patsubst %,$(OUT)/%.o,$(LIBSRCS))
	@mkdir -p $(@D)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(OUT)/%.cc.o: %.cc
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -o $@ -c $<

$(OUT)/%.cc.d: %.cc
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -MM -MT "$(patsubst %,$(OUT)/%.o,$<) $(patsubst %,$(OUT)/%.d,$<)" -o $@ $<

include $(patsubst %,$(OUT)/%.d,$(ALLSRCS))

.PHONY: clean
clean:
	rm -rf $(OUT)
