
%%	retrie, regex trie
%%	Copyright (C) 2012, Guillaume Bour <guillaume@bour.cc>
%%
%%	This program is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, version 3.
%%
%%	This program is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
%%	along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%

-module(retrie).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([encode/1, reduce/1, merge/2, match/2]).
-ifdef(TEST).
	-export([encode/2]).
-endif.

-include("retrie.hrl").

%% internal utils funs
%%
%% infinity-compatible addition/substraction
%%
add(infinity, _) -> infinity;
add(_, infinity) ->	infinity;
add(A, B)        -> A+B.

sub(infinity, _) -> infinity;
sub(_, infinity) -> error;
sub(A,B)         -> A-B.

merge_do(true, _)      -> true;
merge_do(_, true)      -> true;
merge_do(A, undefined) -> A;
merge_do(undefined, B) -> B;
merge_do(A, B)         -> A++B.

%% @doc Encode regex
%%
encode(Re) ->
	encode(Re, true).
encode(Re, Do) ->
	[Last|Trie] = encode_(Re, []),
	{retrie, lists:reverse([Last#node{do=Do}|Trie])}.

encode_([], Re) ->
	Re;
encode_([$?|T], [H|Re]) ->
	encode_(T, [H#node{min=0,max=1} |Re]);
encode_([$*|T], [H|Re]) ->
	encode_(T, [H#node{max=infinity,min=0} |Re]);
encode_([$+|T], [H|Re]) ->
	encode_(T, [H#node{min=1,max=infinity} | Re]);
encode_([H|T], Re) ->
	encode_(T, [#node{s=H} |Re]).

%% @doc Reduce regex trie
%%
-spec reduce([#node{}]) -> [#node{}].
reduce({retrie, Re}) ->
	{retrie, lists:reverse(reduce(Re, #node{}, []))}.

reduce([], N, Re) ->
	lists:sublist([N|Re], length(Re));
reduce([N=#node{s=S,min=Min1,max=Max1} |T], #node{s=S,min=Min2,max=Max2}, Re) ->
	reduce(T, N#node{min=add(Min1, Min2), max=add(Max1, Max2)}, Re);
reduce([N|T], Prev, Re) ->
	reduce(T, N, [Prev|Re]).

%% @doc merge 2 retrie
%%
merge({retrie, Re1}, {retrie, Re2}) ->
	{retrie, lists:reverse(merge(Re1, Re2, []))};
merge({retrie, Re1}, Re2) ->
	merge({retrie, Re1}, reduce(encode(Re1)));
merge(Re1, {retrie, Re2}) ->
	merge({retrie, Re2}, Re1);
merge(Re1, Re2) ->
	merge(reduce(encode(Re1)), reduce(encode(Re2))).

merge([], [], Acc)     ->
	Acc;
merge([N|Re], [], Acc) ->
	merge(Re, [], [N|Acc]);
merge([], [N|Re], Acc) ->
	merge([], Re, [N|Acc]);

% EQUALITY
% both top-node's have same symbol and same min/max occurrences
%
% 0 u 0 -> 0
merge([N1=#node{s=S,min=Min,max=Max,do=Do1}|Re1], [#node{s=S,min=Min,max=Max,do=Do2}|Re2], Acc) ->
	merge(Re1, Re2, [N1#node{do=merge_do(Do1,Do2)}|Acc]);

% EXCLUSIVE
merge([N1=#node{s=S,min=Min1,max=Max1}|Re1], [N2=#node{s=S,max=Max2}|Re2], Acc) when Min1 > Max2 ->
	merge([N1#node{min=sub(Min1,Max2),max=sub(Max1,Max2)}|Re1], Re2, [N2|Acc]);
merge([N1=#node{s=S,max=Max1}|Re1], [N2=#node{s=S,min=Min2,max=Max2}|Re2], Acc) when Min2 > Max1 ->
	merge(Re1, [N2#node{min=sub(Min2,Max1),max=sub(Max2,Max1)}|Re2], [N1|Acc]);

% INCLUSIVE with same start
%
% 0 u 0+ -> O(empty u 0+)
merge([N1=#node{s=S,min=Min1,max=Max1,do=Do1}|Re1],[N2=#node{s=S,min=Min1,max=Max2,do=Do2}|Re2], Acc) when Max1 < Max2 ->
	%merge(Re1, [N2#node{min=1,max=Max2-Max1}|Re2], [N1#node{do=Do1++Do2}|Acc]);
	Acc2   = [N1#node{do=merge_do(Do1,Do2)}|Acc],
	RightP = N2#node{min=1,max=sub(Max2,Max1)},

	case {length(Re1), length(Re2)} of
		{0,0} ->
			merge([], [RightP], Acc2);
		{L1,L2} ->

			Nodes1 = [lists:reverse(merge([], [RightP | Re2], []))],
			Nodes2 = if L2 > 0 -> [Re2]++Nodes1 end,
			Nodes3 = if L1 > 0 -> [Re1]++Nodes2 end,

			[#branch{nodes=Nodes3} | Acc2]
	end;
merge([N1=#node{s=S,min=Min1}|Re1],[N2=#node{s=S,min=Min1}|Re2], Acc) ->
	% change order
	merge([N2|Re2],[N1|Re1],Acc);

% INCLUSIVE with differente start
merge([N1=#node{s=S,min=Min1,max=Max1}|Re1],[N2=#node{s=S,min=Min2,max=Max2}|Re2], Acc) when Min1 < Min2 ->
	%merge([N1#node{min=1,max=Max1-Min2+1}|Re1], [N2#node{min=1,max=Max2-Min2+1}|Re2], [N1#node{max=Min2-1}|Acc]);
%	[
%		#branch{nodes=[
%			Re1,
%			merge([N1#node{min=1,max=Max1-Min2+1}|Re1], [N2#node{min=1,max=Max2-Min2+1}|Re2])
%		]}
%		% minimal left-match of N1/N2 nodes
%		|[N1#node{max=Min2-1}
%		|Acc
%	]];
	Acc2   = [N1#node{max=sub(Min2,1)}|Acc],
	LeftP  = [N1#node{min=1,max=add(sub(Max1,Min2),1)}|Re1], 
	RightP = [N2#node{min=1,max=add(sub(Max2,Min2),1)}|Re2],

	case length(Re1) of
		0 -> merge(LeftP, RightP, Acc2);
		_ -> [#branch{nodes=[Re1, lists:reverse(merge(LeftP, RightP, []))]} | Acc2]
	end;
merge([N1=#node{s=S}|Re1],[N2=#node{s=S}|Re2], Acc) ->
	% swich Re's
	merge([N2|Re2], [N1|Re1], Acc);

% DISJONTION
% Re1 and Re2 top-node's symbol are different
%
% 0 u 1 -> branch
merge(Re1, Re2, Acc) ->
	[#branch{nodes=[Re1, Re2]}|Acc].

%% @doc match string with retrie
%%
match({retrie, Re}, String) ->
	match(Re, String, false);
match(Re, String) ->
	match(reduce(encode(Re)), String).

% terminal steps
match([], [S|Rest], _) ->
	false;
match([#node{min=0, do=Do}|Re]  , [], State) ->
	% if 'do' is not undefined, node is a terminal one,
	% so we are allowed to exit with Do value
	case Do of
		undefined -> match(Re, [], Do);
		_         -> Do
	end;
match([#node{min=Min}|Re], [], _)     ->
	false;
match([],[], State) ->
	State;

match([#node{s=S,max=1,do=Do}|Re], [S|Rest], State) ->
	match(Re, Rest, Do);
match([N=#node{s=S,min=0,max=0}|Re], [S|Rest], State) ->
	match(Re, [S|Rest], State);
match([N=#node{s=S,min=0,max=Max}|Re], [S|Rest], State) ->
	match([N#node{max=sub(Max,1)}|Re], Rest, State);
match([N=#node{s=S,min=Min,max=Max}|Re], [S|Rest], State) ->
	match([N#node{min=sub(Min,1),max=sub(Max,1)}|Re], Rest, State);

match([#node{min=0}|Re], [S|Rest], State) ->
	match(Re, [S|Rest], State);

match([#branch{nodes=[]}], _, _) ->
	false;
match([#branch{nodes=[N|Tail]}], String, State) ->
	case match(N, String, State) of
		% try next leaf
		false  -> match([#branch{nodes=Tail}], String, State);
		BState -> BState
	end;

% re top-node does not match string left-most character
match(_,_,_) ->
	false.


