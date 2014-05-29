#################### 
GROUP_NUMBER := 17
####################

NODE_NAME := datarace
PROP_ERL := erl -env ERL_LIBS ../../
ERLC := erlc
ERLC_FLAGS := -W -I include

TEST_FILES := $(wildcard tests/*.erl)
ERL_FILES := $(wildcard src/*.erl)
BEAM_FILES :=$(patsubst  src/%.erl, ebin/%.beam, $(ERL_FILES))
TEST_BEAM_FILES := $(patsubst tests/%.erl, ebin/%.beam, $(TEST_FILES))



comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(ERL_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))]

REQUIRED_DIR_NAME := OSM_2014_project_group_$(GROUP_NUMBER)

PROJECT_DIR := $(notdir $(shell pwd))

USER=$(shell whoami)
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H:%M:%S")__.tar.gz
ARCHIVE_DIR := ..

all: source test_source

source: $(BEAM_FILES)

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

test_source: $(TEST_BEAM_FILES)

ebin/%.beam: tests/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<


start: all
	(cd ebin && $(PROP_ERL) -eval 'server:start(true)')

test: all
	(cd ebin && $(PROP_ERL) -noinput -eval 'server:start(false), timer:sleep(500), eunit:test({dir, "."}, [verbose]), server:stop(), init:stop()')

doc: $(BEAM_FILES)
	erl -noshell -eval "edoc:application($(notdir $(shell pwd)) , '.', [])" -s init stop

clean:
	rm -fr .#* *.dump
	rm -fr ebin/*.beam
	rm -fr *.erl~
	(cd doc/html && find . -name "*" -a ! -name overview.edoc -exec rm -rf {} \;)

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" src/*.erl include/*.hrl doc/* doc/html/*

archive: clean
ifeq ($(REQUIRED_DIR_NAME), $(PROJECT_DIR))
	(cd $(ARCHIVE_DIR) && tar cvfz $(ARCHIVE_NAME) $(PROJECT_DIR) )
	@echo 
	@echo NOTE: Archive created in $(ARCHIVE_DIR)/$(ARCHIVE_NAME)
	@echo 
else
	@echo Error: Wrong directory name >$(PROJECT_DIR)<, change to >$(REQUIRED_DIR_NAME)<
endif

