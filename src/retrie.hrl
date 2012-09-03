
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

-record(node, {
	% symbol
	s      = $\0         :: char(),
	min    = 1           :: infinity|integer(),  
	max    = 1           :: infinity|integer(),  
	% either boolean (true/false) or list of ordered callbacks:
	% [{10, M, F, A}, {15, M, F, A}, ...]
	do     = undefined   :: undefined|binary()|list()
}).

-record(branch, {
	nodes  = []          :: list()
}).

