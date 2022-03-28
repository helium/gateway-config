.PHONY: compile rel cover test typecheck doc

REBAR=./rebar3
SHORTSHA=`git rev-parse --short HEAD`
PKG_NAME_VER=${SHORTSHA}

OS_NAME=$(shell uname -s)

GRPC_SERVICES_DIR=src/grpc/autogen

ifeq (${OS_NAME},FreeBSD)
make="gmake"
else
MAKE="make"
endif

compile:
	$(MAKE) grpc
	$(REBAR) compile

shell:
	$(REBAR) shell

clean:
	$(MAKE) clean_grpc
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

grpc:
	@echo "generating gateway_config grpc services"
	REBAR_CONFIG="config/grpc_client_gen_local.config" $(REBAR) grpc gen

$(GRPC_SERVICE_DIR):
	@echo "gateway_config service directory $(directory) does not exist"
	$(REBAR) get-deps
	$(MAKE) grpc

clean_grpc:
	@echo "cleaning gateway_config grpc services"
	rm -rf $(GRPC_SERVICE_DIR)

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
