PROJECT = cluecon_drives_car
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = gun jsx cowboy 
BUILD_DEPS = serial
dep_cowboy_commit = 2.8.0
#dep_erlport = git https://github.com/heyoka/erlport master
dep_serial = git https://github.com/tonyg/erlang-serial

SHELL_NAME ?= cluecon_drives_car@127.0.0.1
SHELL_COOKIE ?= cluecon_drives_car
SHELL_OPTS = -eval 'application:ensure_all_started(cluecon_drives_car)' -name ${SHELL_NAME} -setcookie ${SHELL_COOKIE}

include erlang.mk
