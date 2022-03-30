:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load([
		types(loader),
		term_io(loader),
		json(loader),
		dictionaries(loader),
		nested_dictionaries(loader)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		repl_hijack
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
