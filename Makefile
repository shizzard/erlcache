all: 
	rebar compile

build:
	rebar generate

configure:
	./configure.sh ${ENV}

clean:
	rebar clean

depends:
	rm -rf deps/*
	rebar get-deps

console: all build
	rel/erlcache/bin/erlcache console

cconsole: all build
	rel/erlcache/bin/erlcache console_clean

cluster: node1 node2 node3
	for n in node/node*; do $$n/bin/erlcache start; done;
	sleep 5;
	for n in node/node2 node/node3; do $$n/bin/erlcache-admin join erlcache1; done;

cluster_stop:
	for n in node/node*; do $$n/bin/erlcache stop; done

cluster_check:
	for n in node/node*; do $$n/bin/erlcache-admin ringready; done

node1 node2 node3:
	mkdir -p node
	(cd rel && rebar generate target_dir=../node/$@ overlay_vars=vars/$@.config)

ping:
	for n in node/node*; do $$n/bin/erlcache ping; done
