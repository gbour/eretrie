
%
-module(retrie_tests).
-include_lib("eunit/include/eunit.hrl").
-include("retrie.hrl").

encode_test() ->
	?assertEqual({retrie, [#node{s=$a,min=1,max=1,do=[true]}]}       , retrie:encode("a")),
	?assertEqual({retrie, [#node{s=$a,min=0,max=1,do=[true]}]}       , retrie:encode("a?")),
	?assertEqual({retrie, [#node{s=$a,min=0,max=infinity,do=[true]}]}, retrie:encode("a*")),
	?assertEqual({retrie, [#node{s=$a,min=1,max=infinity,do=[true]}]}, retrie:encode("a+")),

	?assertEqual({retrie, [#node{s=$a,min=1,max=1}, #node{s=$a,min=1,max=1,do=[true]}]}, retrie:encode("aa")),
	?assertEqual({retrie, [#node{s=$a,min=1,max=1}, #node{s=$b,min=1,max=1,do=[true]}]}, retrie:encode("ab")),
	ok.

reduce_test() ->
	% reference: cannot be reduced
	% ab -> ab
	?assertEqual({retrie, [#node{s=$a,min=1,max=1},#node{s=$b,min=1,max=1}]}, retrie:reduce({retrie, [
		#node{s=$a,min=1,max=1},
		#node{s=$b,min=1,max=1}
	]})),

	% aa -> a{2,2}
	?assertEqual({retrie, [#node{s=$a,min=2,max=2}]}, retrie:reduce({retrie, [
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=1,max=1}
	]})),

	% aa? -> a{1,2}
	?assertEqual({retrie, [#node{s=$a,min=1,max=2}]}, retrie:reduce({retrie, [
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=0,max=1}
	]})),

	% a?a -> a{1,2}
	?assertEqual({retrie, [#node{s=$a,min=1,max=2}]}, retrie:reduce({retrie, [
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=1,max=1}
	]})),

	% aa* -> a+
	?assertEqual({retrie, [#node{s=$a,min=1,max=infinity}]}, retrie:reduce({retrie, [
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=0,max=infinity}
	]})),

	% a*a -> a+
	?assertEqual({retrie, [#node{s=$a,min=1,max=infinity}]}, retrie:reduce({retrie, [
		#node{s=$a,min=0,max=infinity},
		#node{s=$a,min=1,max=1}
	]})),

	% aa+ -> a{2,infinity}
	?assertEqual({retrie, [#node{s=$a,min=2,max=infinity}]}, retrie:reduce({retrie, [
		#node{s=$a,min=1,max=1},
		#node{s=$a,min=1,max=infinity}
	]})),

	% a+a -> a{2,infinity}
	?assertEqual({retrie, [#node{s=$a,min=2,max=infinity}]}, retrie:reduce({retrie, [
		#node{s=$a,min=1,max=infinity},
		#node{s=$a,min=1,max=1}
	]})),

	% a?a? -> a{0,2}
	?assertEqual({retrie, [#node{s=$a,min=0,max=2}]}, retrie:reduce({retrie, [
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=0,max=1}
	]})),

	% a?a+ -> a+
	?assertEqual({retrie, [#node{s=$a,min=1,max=infinity}]}, retrie:reduce({retrie, [
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=1,max=infinity}
	]})),

	% a?a* -> a*
	?assertEqual({retrie, [#node{s=$a,min=0,max=infinity}]}, retrie:reduce({retrie, [
		#node{s=$a,min=0,max=1},
		#node{s=$a,min=0,max=infinity}
	]})),

	% a*a+ -> a+
	?assertEqual({retrie, [#node{s=$a,min=1,max=infinity}]}, retrie:reduce({retrie, [
		#node{s=$a,min=0,max=infinity},
		#node{s=$a,min=1,max=infinity}
	]})),

	ok.

merge_test_() ->
	{setup, local,
		fun() -> pass end,
		fun(_) -> [
		% a U a
		{"a U a", 
			?_assertEqual({retrie, [#node{s=$a,min=1,max=1,do=[alpha,beta]}]}, 
				retrie:merge(
					{retrie, [#node{s=$a,min=1,max=1,do=[alpha]}]}, 
					{retrie, [#node{s=$a,min=1,max=1,do=[beta]}]}
			))
		},

		{"a U b", ?_assertEqual({retrie, [#branch{nodes=[
				[#node{s=$a,min=1,max=1,do=[alpha]}],
				[#node{s=$b,min=1,max=1,do=[beta]}]
			]}]}, 
			retrie:merge(
				{retrie, [#node{s=$a,min=1,max=1,do=[alpha]}]}, 
				{retrie, [#node{s=$b,min=1,max=1,do=[beta]}]}
			))
		},

		{"ab U ac", ?_assertEqual({retrie, [
			#node{s=$a},
			#branch{nodes=[
				[#node{s=$b,do=[alpha]}],
				[#node{s=$c,do=[beta]}]
			]}]}, 
			retrie:merge(
				{retrie, [#node{s=$a}, #node{s=$b,do=[alpha]}]}, 
				{retrie, [#node{s=$a}, #node{s=$c,do=[beta]}]}
			))
		},

		% same symbol, same min value
	
		{"inclusive, min1==min2. a{5,10} U a{5,15}", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=10,do=[alpha,beta]},
				#node{s=$a,min=1,max=5,do=[beta]}
			]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=10,do=[alpha]}]},
				{retrie, [#node{s=$a,min=5,max=15,do=[beta]}]}
			))
		},
	
		{"a{5,15} U a{5,10}", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=10,do=[beta,alpha]},
				#node{s=$a,min=1,max=5,do=[alpha]}
			]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=15,do=[alpha]}]},
				{retrie, [#node{s=$a,min=5,max=10,do=[beta]}]}
			))
		},
	
		{"a{5,10}b U a{5,15}c", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=10},
				#branch{nodes=[
					[#node{s=$b,do=[alpha]}],
					[#node{s=$c,do=[beta]}],
					[#node{s=$a,min=1,max=5},#node{s=$c,do=[beta]}]
			]}]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=10},#node{s=$b,do=[alpha]}]},
				{retrie, [#node{s=$a,min=5,max=15},#node{s=$c,do=[beta]}]}
			))
		},

		{"a{5,15}b U a{5,10}c", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=10},
				#branch{nodes=[
					[#node{s=$c,do=[beta]}],
					[#node{s=$b,do=[alpha]}],
					[#node{s=$a,min=1,max=5},#node{s=$b,do=[alpha]}]
			]}]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=15},#node{s=$b,do=[alpha]}]},
				{retrie, [#node{s=$a,min=5,max=10},#node{s=$c,do=[beta]}]}
			))
		},

		% inclusive
		% same symbol, but min is different and intersection is not empty

		{"inclusive, different min and intersection not empty. a{5,15} U a{10,20}", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=9,do=[alpha]},
				#node{s=$a,min=1,max=6,do=[alpha,beta]},
				#node{s=$a,min=1,max=5,do=[beta]}
			]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=15,do=[alpha]}]},
				{retrie, [#node{s=$a,min=10,max=20,do=[beta]}]}
			))
		},

		{"a{10,20} U a{5,15}", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=9,do=[alpha]},
				#node{s=$a,min=1,max=6,do=[alpha,beta]},
				#node{s=$a,min=1,max=5,do=[beta]}
			]},
			retrie:merge(
				{retrie, [#node{s=$a,min=10,max=20,do=[beta]}]},
				{retrie, [#node{s=$a,min=5,max=15,do=[alpha]}]}
			))
		},

		{"inclusive, unlimited boundaries. a{5,15} U a{10,infinity}", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=9,do=[alpha]},
				#node{s=$a,min=1,max=6,do=[alpha,beta]},
				#node{s=$a,min=1,max=infinity,do=[beta]}
			]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=15,do=[alpha]}]},
				{retrie, [#node{s=$a,min=10,max=infinity,do=[beta]}]}
			))
		},

		{"a{5,15}b U a{10,20}c", ?_assertEqual({retrie, [
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
				]}]},
			retrie:merge(
					{retrie, [#node{s=$a,min=5,max=15},#node{s=$b,do=[alpha]}]},
					{retrie, [#node{s=$a,min=10,max=20},#node{s=$c,do=[beta]}]}
			))
		},

		{"a{10,20}c U a{5,15}b", ?_assertEqual({retrie, [
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
			]}]},
			retrie:merge(
				{retrie, [#node{s=$a,min=10,max=20},#node{s=$c,do=[beta]}]},
				{retrie, [#node{s=$a,min=5,max=15},#node{s=$b,do=[alpha]}]}
			))
		},

		%% EXCLUSIVE
	
		{"a{5,10} U a{15,20}", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=10,do=[alpha]},
				#node{s=$a,min=5,max=10,do=[beta]}
			]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=10,do=[alpha]}]},
				{retrie, [#node{s=$a,min=15,max=20,do=[beta]}]}
			))
		},
	
		{"a{15,20} U a{5,10}", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=10,do=[alpha]},
				#node{s=$a,min=5,max=10,do=[beta]}
			]},
			retrie:merge(
				{retrie, [#node{s=$a,min=15,max=20,do=[beta]}]},
				{retrie, [#node{s=$a,min=5,max=10,do=[alpha]}]}
			))
		},

		{"a{5,10}b U a{15,20}c", ?_assertEqual({retrie, [
				#node{s=$a,min=5,max=10},
				#branch{nodes=[
					[#node{s=$b,do=[alpha]}],
					[#node{s=$a,min=5,max=10}, #node{s=$c,do=[beta]}]
			]}]},
			retrie:merge(
				{retrie, [#node{s=$a,min=5,max=10}, #node{s=$b,do=[alpha]}]},
				{retrie, [#node{s=$a,min=15,max=20}, #node{s=$c,do=[beta]}]}
			))
		}

	] end}.

match_test_() ->
	{"left is regex, right is matched string", [

		{"a vs a", ?_assertEqual(true, retrie:match("a","a"))},
		{"a vs b", ?_assertEqual(false, retrie:match("a","b"))},
		{"a vs aa", ?_assertEqual(false, retrie:match("a","aa"))},

		{"aa vs a",    ?_assertEqual(false, retrie:match("aa","a"))},
		{"aa vs b",    ?_assertEqual(false, retrie:match("aa", "b"))},
		{"aa vs aa",   ?_assertEqual(true, retrie:match("aa","aa"))},

		{"a? vs empty"    , ?_assertEqual(true , retrie:match("a?", ""))},
		{"a? vs a"        , ?_assertEqual(true , retrie:match("a?", "a"))},
		{"a? vs aa"       , ?_assertEqual(false, retrie:match("a?", "aa"))},
		{"a? vs b"        , ?_assertEqual(false, retrie:match("a?", "b"))},

		{"a* vs empty"    , ?_assertEqual(true , retrie:match("a*", ""))},
		{"a* vs a"        , ?_assertEqual(true , retrie:match("a*", "a"))},
		{"a* vs aa"       , ?_assertEqual(true , retrie:match("a*", "aa"))},
		{"a* vs aaa"      , ?_assertEqual(true , retrie:match("a*", "aaa"))},
		{"a* vs b"        , ?_assertEqual(false, retrie:match("a*", "b"))},
		{"a* vs ab"       , ?_assertEqual(false, retrie:match("a*", "ab"))},

		{"a+ vs empty"    , ?_assertEqual(false, retrie:match("a+", ""))},
		{"a+ vs a"        , ?_assertEqual(true , retrie:match("a+", "a"))},
		{"a+ vs aa"       , ?_assertEqual(true , retrie:match("a+", "aa"))},
		{"a+ vs aaa"      , ?_assertEqual(true , retrie:match("a+", "aaa"))},
		{"a+ vs b"        , ?_assertEqual(false, retrie:match("a+", "b"))},
		{"a+ vs ab"       , ?_assertEqual(false, retrie:match("a+", "ab"))},

		{"composition", [
			{"ab vs ab"   , ?_assertEqual(true , retrie:match("ab", "ab"))},
			{"ab vs a"    , ?_assertEqual(false, retrie:match("ab", "a"))},
			{"ab vs b"    , ?_assertEqual(false, retrie:match("ab", "b"))},
			{"ab vs aa"   , ?_assertEqual(false, retrie:match("ab", "aa"))},
			{"ab vs bb"   , ?_assertEqual(false, retrie:match("ab", "bb"))},
			{"ab vs aab"  , ?_assertEqual(false, retrie:match("ab", "aab"))},
			{"ab vs abc"  , ?_assertEqual(false, retrie:match("ab", "abc"))},

			{"a?b vs ab"   , ?_assertEqual(true , retrie:match("a?b", "ab"))},
			{"a?b vs b"    , ?_assertEqual(true , retrie:match("a?b", "b"))},
			{"a?b vs a"    , ?_assertEqual(false, retrie:match("a?b", "a"))},
			{"a?b vs aa"   , ?_assertEqual(false, retrie:match("a?b", "aa"))},
			{"a?b vs bb"   , ?_assertEqual(false, retrie:match("a?b", "bb"))},
			{"a?b vs aab"  , ?_assertEqual(false, retrie:match("a?b", "aab"))},
			{"a?b vs abc"  , ?_assertEqual(false, retrie:match("a?b", "abc"))},

			{"a*b vs ab"   , ?_assertEqual(true , retrie:match("a*b", "ab"))},
			{"a*b vs b"    , ?_assertEqual(true , retrie:match("a*b", "b"))},
			{"a*b vs a"    , ?_assertEqual(false, retrie:match("a*b", "a"))},
			{"a*b vs aa"   , ?_assertEqual(false, retrie:match("a*b", "aa"))},
			{"a*b vs bb"   , ?_assertEqual(false, retrie:match("a*b", "bb"))},
			{"a*b vs aab"  , ?_assertEqual(true , retrie:match("a*b", "aab"))},
			{"a*b vs abc"  , ?_assertEqual(false, retrie:match("a*b", "abc"))},

			{"a+b vs ab"   , ?_assertEqual(true , retrie:match("a+b", "ab"))},
			{"a+b vs b"    , ?_assertEqual(false, retrie:match("a+b", "b"))},
			{"a+b vs a"    , ?_assertEqual(false, retrie:match("a+b", "a"))},
			{"a+b vs aa"   , ?_assertEqual(false, retrie:match("a+b", "aa"))},
			{"a+b vs bb"   , ?_assertEqual(false, retrie:match("a+b", "bb"))},
			{"a+b vs aab"  , ?_assertEqual(true , retrie:match("a+b", "aab"))},
			{"a+b vs abc"  , ?_assertEqual(false, retrie:match("a+b", "abc"))}
		]},

		% not working: returns [true] at now
		{"branching", [
			{"a U b"       , ?_assertEqual(true , retrie:match(retrie:merge("a","b"), "a"))}
		]},

		% does not work at now
		{"backtracking", [
			{"a*b?a vs a"  , ?_assertEqual(true , retrie:match("a*b?a", "a"))}
		]}
	]}.
