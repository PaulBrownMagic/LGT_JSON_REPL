:- initialization((
	logtalk_load([
		types(loader),
		term_io(loader),
		json(loader),
		dictionaries(loader),
		nested_dictionaries(loader)
	]),
	logtalk_load([
		repl_hijack
	],
    [
		optimize(on)
	])
)).
