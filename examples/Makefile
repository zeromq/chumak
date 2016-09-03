.PHONY: *.erl

RUN_ARGS = erl -pa ../_build/default/lib/chumak/ebin -eval

*.erl:
	erlc $@

req_client: req_client.erl
	${RUN_ARGS} "req_client:main()"

req_server: req_server.erl
	${RUN_ARGS} "req_server:main()"

rep_server: rep_server.erl
	${RUN_ARGS} "rep_server:main()"

router_with_dealer: router_with_dealer.erl
	${RUN_ARGS} "router_with_dealer:main()"

router_with_req: router_with_req.erl
	${RUN_ARGS} "router_with_req:main()"

publisher: publisher.erl
	${RUN_ARGS} "publisher:main()"

subscriber_a: subscriber.erl
	${RUN_ARGS} 'subscriber:main(<<"A">>)'

subscriber_b: subscriber.erl
	${RUN_ARGS} 'subscriber:main(<<"B">>)'

push: push.erl
	${RUN_ARGS} "push:main()"

pull: pull.erl
	${RUN_ARGS} 'pull:main()'

pair_server: pair_server.erl
	${RUN_ARGS} "pair_server:main()"

pair_client: pair_client.erl
	${RUN_ARGS} 'pair_client:main()'

resource_server: resource_server.erl
	${RUN_ARGS} 'resource_server:main()'

resource_client: resource_client.erl
	${RUN_ARGS} 'resource_client:main()'
