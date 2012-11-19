-record(action, {step, desc, line, text="", table=[]}).
-record(scenario, {type, desc, actions = [], examples = []}).
-record(feature, {id, path, desc, background = [], scenarios = [], fcode = []}).

-record(module, {path, mod = [], warnings = [], errors = []}).
-record(feature_code, {
	  modules = [],
	  steps = [],
	  setup_mod = [],
	  teardown_mod = [],
	  status = ok
	 }).
