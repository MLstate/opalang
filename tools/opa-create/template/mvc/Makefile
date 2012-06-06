########################################
# USER VARIABLES

EXE = application_name.exe
PACKNAME =
SRC =
PCKDIR = ./plugins/
PCK =
PLUGIN =
PLUGINDIR =
OTHER_DEPENDS = resources/*
CONF_FILE = configuration

#Compiler variables
OPACOMPILER ?= opa
FLAG = --opx-dir _build --import-package stdlib.database.mongo
PORT = 8080

RUN_OPT = --db-local db/

default: exe

run: exe
	./$(EXE) $(RUN_OPT) || true

include Makefile.common
