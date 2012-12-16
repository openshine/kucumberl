-record(action, {step, desc, line, text="", table=[], tabletxt=""}).
-record(scenario, {type, desc, actions = [], examples = [], tags = [], enabled = true}).
-record(feature, {id, path, desc, background = [], scenarios = [], fcode = [], tags = []}).

-record(module, {path, mod = [], warnings = [], errors = []}).
-record(feature_code, {
	  modules = [],
	  steps = [],
	  setup_mod = [],
	  teardown_mod = [],
	  status = ok
	 }).
