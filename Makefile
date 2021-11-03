.PHONY: compile rel cover test typecheck doc

REBAR=./rebar3
SHORTSHA=`git rev-parse --short HEAD`
PKG_NAME_VER=${SHORTSHA}

OS_NAME=$(shell uname -s)

grpc_services_directory=src/grpc/autogen

ifeq (${OS_NAME},FreeBSD)
make="gmake"
else
MAKE="make"
endif

compile:
	$(REBAR) compile

shell:
	$(REBAR) shell

clean:
	$(REBAR) clean

cover:
	$(REBAR) cover

test:
	$(REBAR) as test do eunit,ct

typecheck:
	$(REBAR) dialyzer

release:
	$(REBAR) as prod release -n gateway_config

devrelease:
	$(REBAR) as dev release

doc:
	$(REBAR) edoc

grpc: | $(grpc_services_directory)
	@echo "generating grpc client services"
	REBAR_CONFIG="config/grpc_client_gen.config" $(REBAR) grpc gen

clean_grpc:
	@echo "cleaning grpc services"
	rm -rf $(grpc_services_directory)

$(grpc_services_directory):
	@echo "grpc service directory $(directory) does not exist"
	$(REBAR) get-deps
