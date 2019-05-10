.PHONY: clean all

SRC_DIR     := "src"
ELM_SOURCES := $(shell find $(SRC_DIR)/elm -name '*.elm')
HS_SOURCES  := $(shell find $(SRC_DIR) -name '*.hs')
HS_VERSION  := $(shell ghc --version|sed 's/.*version /ghc-/')
APP_VERSION := $(shell grep '^version:' *.cabal|sed 's/.* //')
APP_NAME    := $(shell grep '^executable .' *.cabal|sed 's/.* //'|head -1)
APP_ARCH    := $(shell uname -m)
APP_OS      := $(shell uname -s|tr 'A-Z' 'a-z')

TARGET      := dist-newstyle/build/$(APP_ARCH)-$(APP_OS)/$(HS_VERSION)/$(APP_NAME)-$(APP_VERSION)/x/$(APP_NAME)/build/$(APP_NAME)/$(APP_NAME)

.PHONY: clean all run
.DEFAULT_GOAL := all

all: $(TARGET)

static/main.js: $(ELM_SOURCES) elm.json
	elm make src/elm/Main.elm --output=static/main.js

$(TARGET): $(ELM_SOURCES) static/main.js $(HS_SOURCES)
	cabal v2-build

clean:
	rm -rf dist* static/main.js elm-stuff

run: all
	$(TARGET)
