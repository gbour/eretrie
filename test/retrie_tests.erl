
%
-module(retrie_tests).
-include_lib("eunit/include/eunit.hrl").
-include("retrie.hrl").

encode_test() ->
	?assertEqual([#node{s=$a,min=1,max=1}]       , retrie:encode("a")),
	?assertEqual([#node{s=$a,min=0,max=1}]       , retrie:encode("a?")),
	?assertEqual([#node{s=$a,min=0,max=infinity}], retrie:encode("a*")),
	?assertEqual([#node{s=$a,min=1,max=infinity}], retrie:encode("a+")),

	?assertEqual([#node{s=$a,min=1,max=1}, #node{s=$a,min=1,max=1}], retrie:encode("aa")),
	?assertEqual([#node{s=$a,min=1,max=1}, #node{s=$b,min=1,max=1}], retrie:encode("ab")),
	ok.

reduce_test() ->
	% reference: cannot be reduced
	% ab -> ab
	?assertEqual([#node{s=$a,min=1,max=1},#node{s=$b,min=1,max=1}], retrie:reduce([
		#node{s=$a,min=1,max=1},
		#node{s=$b,min=1,max=1}
	])),

	% aa -> a{2,2}
	?assertEqual([#node{s=$a,min=2,max=2}], retrie:reduce([
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=1,max=1}
	])),

	% aa? -> a{1,2}
	?assertEqual([#node{s=$a,min=1,max=2}], retrie:reduce([
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=0,max=1}
	])),

	% a?a -> a{1,2}
	?assertEqual([#node{s=$a,min=1,max=2}], retrie:reduce([
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=1,max=1}
	])),

	% aa* -> a+
	?assertEqual([#node{s=$a,min=1,max=infinity}], retrie:reduce([
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=0,max=infinity}
	])),

	% a*a -> a+
	?assertEqual([#node{s=$a,min=1,max=infinity}], retrie:reduce([
		#node{s=$a,min=0,max=infinity},
		#node{s=$a,min=1,max=1}
	])),

	% aa+ -> a{2,infinity}
	?assertEqual([#node{s=$a,min=2,max=infinity}], retrie:reduce([
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=1,max=infinity}
	])),

	% a+a -> a{2,infinity}
	?assertEqual([#node{s=$a,min=2,max=infinity}], retrie:reduce([
		#node{s=$a,min=1,max=infinity},
		#node{s=$a,min=1,max=1}
	])),

	% a?a? -> a{0,2}
	?assertEqual([#node{s=$a,min=0,max=2}], retrie:reduce([
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=0,max=1}
	])),

	% a?a+ -> a+
	?assertEqual([#node{s=$a,min=1,max=infinity}], retrie:reduce([
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=1,max=infinity}
	])),

	% a?a* -> a*
	?assertEqual([#node{s=$a,min=0,max=infinity}], retrie:reduce([
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=0,max=infinity}
	])),

	% a*a+ -> a+
	?assertEqual([#node{s=$a,min=1,max=infinity}], retrie:reduce([
		#node{s=$a,min=0,max=infinity},
		#node{s=$a,min=1,max=infinity}
	])),

	ok.

merge_test_() ->
	{setup, local,
		fun() -> pass end,
		fun(_) -> [
		% a U a
		{"a U a", 
			?_assertEqual([#node{s=$a,min=1,max=1,do=[alpha,beta]}], 
				retrie:merge(
					[#node{s=$a,min=1,max=1,do=[alpha]}], 
					[#node{s=$a,min=1,max=1,do=[beta]}]
			))
		},

		{"a U b", ?_assertEqual([#branch{nodes=[
				[#node{s=$a,min=1,max=1,do=[alpha]}],
				[#node{s=$b,min=1,max=1,do=[beta]}]
			]}], 
			retrie:merge(
				[#node{s=$a,min=1,max=1,do=[alpha]}], 
				[#node{s=$b,min=1,max=1,do=[beta]}]
			))
		},

		{"ab U ac", ?_assertEqual([
			#node{s=$a},
			#branch{nodes=[
				[#node{s=$b,do=[alpha]}],
				[#node{s=$c,do=[beta]}]
			]}], 
			retrie:merge(
				[#node{s=$a}, #node{s=$b,do=[alpha]}], 
				[#node{s=$a}, #node{s=$c,do=[beta]}]
			))
		},

		% same symbol, same min value
	
		{"inclusive, min1==min2. a{5,10} U a{5,15}", ?_assertEqual([
				#node{s=$a,min=5,max=10,do=[alpha,beta]},
				#node{s=$a,min=1,max=5,do=[beta]}
			],
			retrie:merge(
				[#node{s=$a,min=5,max=10,do=[alpha]}],
				[#node{s=$a,min=5,max=15,do=[beta]}]
			))
		},
	
		{"a{5,15} U a{5,10}", ?_assertEqual([
				#node{s=$a,min=5,max=10,do=[beta,alpha]},
				#node{s=$a,min=1,max=5,do=[alpha]}
			],
			retrie:merge(
				[#node{s=$a,min=5,max=15,do=[alpha]}],
				[#node{s=$a,min=5,max=10,do=[beta]}]
			))
		},
	
		{"a{5,10}b U a{5,15}c", ?_assertEqual([
				#node{s=$a,min=5,max=10},
				#branch{nodes=[
					[#node{s=$b,do=[alpha]}],
					[#node{s=$c,do=[beta]}],
					[#node{s=$a,min=1,max=5},#node{s=$c,do=[beta]}]
			]}],
			retrie:merge(
				[#node{s=$a,min=5,max=10},#node{s=$b,do=[alpha]}],
				[#node{s=$a,min=5,max=15},#node{s=$c,do=[beta]}]
			))
		},

		{"a{5,15}b U a{5,10}c", ?_assertEqual([
				#node{s=$a,min=5,max=10},
				#branch{nodes=[
					[#node{s=$c,do=[beta]}],
					[#node{s=$b,do=[alpha]}],
					[#node{s=$a,min=1,max=5},#node{s=$b,do=[alpha]}]
			]}],
			retrie:merge(
				[#node{s=$a,min=5,max=15},#node{s=$b,do=[alpha]}],
				[#node{s=$a,min=5,max=10},#node{s=$c,do=[beta]}]
			))
		},

		% inclusive
		% same symbol, but min is different and intersection is not empty

		{"inclusive, different min and intersection not empty. a{5,15} U a{10,20}", ?_assertEqual([
				#node{s=$a,min=5,max=9,do=[alpha]},
				#node{s=$a,min=1,max=6,do=[alpha,beta]},
				#node{s=$a,min=1,max=5,do=[beta]}
			],
			retrie:merge(
				[#node{s=$a,min=5,max=15,do=[alpha]}],
				[#node{s=$a,min=10,max=20,do=[beta]}]
			))
		},

		{"a{10,20} U a{5,15}", ?_assertEqual([
				#node{s=$a,min=5,max=9,do=[alpha]},
				#node{s=$a,min=1,max=6,do=[alpha,beta]},
				#node{s=$a,min=1,max=5,do=[beta]}
			],
			retrie:merge(
				[#node{s=$a,min=10,max=20,do=[beta]}],
				[#node{s=$a,min=5,max=15,do=[alpha]}]
			))
		},

		{"inclusive, unlimited boundaries. a{5,15} U a{10,infinity}", ?_assertEqual([
				#node{s=$a,min=5,max=9,do=[alpha]},
				#node{s=$a,min=1,max=6,do=[alpha,beta]},
				#node{s=$a,min=1,max=infinity,do=[beta]}
			],
			retrie:merge(
				[#node{s=$a,min=5,max=15,do=[alpha]}],
				[#node{s=$a,min=10,max=infinity,do=[beta]}]
			))
		},

		{"a{5,15}b U a{10,20}c", ?_assertEqual([
				#node{s=$a,min=5,max=9},
				#branch{nodes=[
					[#node{s=$b,do=[alpha]}],
					[
						#node{s=$a,min=1,max=6},
						#branch{nodes=[
							[#node{s=$b,do=[alpha]}],
							[#node{s=$c,do=[beta]}],
							[#node{s=$a,min=1,max=5},#node{s=$c,do=[beta]}]
						]}
					]
				]}],
				retrie:merge(
					[#node{s=$a,min=5,max=15},#node{s=$b,do=[alpha]}],
					[#node{s=$a,min=10,max=20},#node{s=$c,do=[beta]}]
			))
		},

		{"a{10,20}c U a{5,15}b", ?_assertEqual([
				#node{s=$a,min=5,max=9},
				#branch{nodes=[
					[#node{s=$b,do=[alpha]}],
					[
						#node{s=$a,min=1,max=6},
						#branch{nodes=[
							[#node{s=$b,do=[alpha]}],
							[#node{s=$c,do=[beta]}],
							[#node{s=$a,min=1,max=5},#node{s=$c,do=[beta]}]
						]}
					]
				]}],
				retrie:merge(
				[#node{s=$a,min=10,max=20},#node{s=$c,do=[beta]}],
				[#node{s=$a,min=5,max=15},#node{s=$b,do=[alpha]}]
			))
		},

		%% EXCLUSIVE
	
		{"a{5,10} U a{15,20}", ?_assertEqual([
				#node{s=$a,min=5,max=10,do=[alpha]},
				#node{s=$a,min=5,max=10,do=[beta]}
			],
			retrie:merge(
				[#node{s=$a,min=5,max=10,do=[alpha]}],
				[#node{s=$a,min=15,max=20,do=[beta]}]
			))
		},
	
		{"a{15,20} U a{5,10}", ?_assertEqual([
				#node{s=$a,min=5,max=10,do=[alpha]},
				#node{s=$a,min=5,max=10,do=[beta]}
			],
			retrie:merge(
				[#node{s=$a,min=15,max=20,do=[beta]}],
				[#node{s=$a,min=5,max=10,do=[alpha]}]
			))
		},

		{"a{5,10}b U a{15,20}c", ?_assertEqual([
				#node{s=$a,min=5,max=10},
				#branch{nodes=[
					[#node{s=$b,do=[alpha]}],
					[#node{s=$a,min=5,max=10}, #node{s=$c,do=[beta]}]
			]}],
			retrie:merge(
				[#node{s=$a,min=5,max=10}, #node{s=$b,do=[alpha]}],
				[#node{s=$a,min=15,max=20}, #node{s=$c,do=[beta]}]
			))
		}

	] end}.

