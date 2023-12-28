% 단위는 km
road(구로, 신도림, 1.1).
road(신도림, 신길, 2.5).
road(신길, 대방, 0.8).
road(대방, 노량진, 1.5).
road(노량진, 동작, 3.6).
road(신도림, 영등포구청, 2.1).
road(영등포구청, 신길, 2.0).
road(신길, 여의도, 1.0).
road(대방, 샛강, 0.6).
road(샛강, 노량진, 1.2).
road(노량진, 용산, 2.6).
road(동작, 이촌, 2.7).
road(영등포구청, 당산, 1.1).
road(당산, 합정, 2.0).
road(당산, 여의도, 2.4).
road(여의도, 샛강, 0.8).
road(여의도, 공덕, 3.6).
road(합정, 공덕, 3.6).
road(공덕, 효창공원앞, 0.9).
road(효창공원앞, 용산, 1.5).
road(용산, 이촌, 1.9).
road(효창공원앞, 삼각지, 1.2).
road(삼각지, 이촌, 2.0).
road(합정, 홍대입구, 1.1).
road(홍대입구, 공덕, 2.8).
road(공덕, 충정로, 2.0).
road(공덕, 서울역, 3.3).
road(서울역, 용산, 3.2).
road(서울역, 삼각지, 2.2).
road(서울역, 충무로, 2.3).
road(서울역, 시청, 1.1).
road(충무로, 을지로3가, 0.7).
road(충정로, 시청, 1.1).
road(시청, 을지로3가, 1.5).
road(을지로3가, 을지로4가, 0.6).
road(신촌, 홍대입구, 1.3).
road(신촌, 충정로, 2.5).


r(X, Y, Z):-road(X, Y, Z).
r(X, Y, Z):-road(Y, X, Z).



subway(구로, 신도림, '1호선').
subway(신도림, 신길, '1호선').
subway(신길, 대방, '1호선').
subway(대방, 노량진, '1호선').
subway(노량진, 동작, '9호선').
subway(신도림, 영등포구청, '2호선').
subway(영등포구청, 신길, '5호선').
subway(신길, 여의도, '5호선').
subway(대방, 샛강, '신림선').
subway(샛강, 노량진, '9호선').
subway(노량진, 용산,  '1호선').
subway(동작, 이촌, '4호선').
subway(영등포구청, 당산, '2호선').
subway(당산, 합정, '2호선').
subway(당산, 여의도, '9호선').
subway(여의도, 샛강, '9호선').
subway(여의도, 공덕, '5호선').
subway(합정, 공덕, '6호선').
subway(공덕, 효창공원앞, '6호선').
subway(효창공원앞, 용산, '경의중앙선').
subway(용산, 이촌, '경의중앙선').
subway(효창공원앞, 삼각지, '6호선').
subway(삼각지, 이촌, '4호선').
subway(합정, 홍대입구, '2호선').
subway(홍대입구, 공덕, '경의중앙선/공항철도').
subway(공덕, 충정로, '5호선').
subway(공덕, 서울역,  '공항철도').
subway(서울역, 용산, '1호선').
subway(서울역, 삼각지, '4호선').
subway(서울역, 충무로, '4호선').
subway(서울역, 시청, '1호선').
subway(충무로, 을지로3가, '3호선').
subway(홍대입구, 충정로, '2호선').
subway(충정로, 시청, '2호선').
subway(시청, 을지로3가, '2호선').
subway(을지로3가, 을지로4가, '2호선').
subway(신촌, 홍대입구, '2호선').
subway(신촌, 충정로, '2호선').



coordinates(37.503039, 126.881966, 구로).
coordinates(37.508725, 126.891295, 신도림).
coordinates(37.52497, 126.895951, 영등포구청).
coordinates(37.517122, 126.917169, 신길).
coordinates(37.53438, 126.902281, 당산).
coordinates(37.521624, 126.924191, 여의도).
coordinates(37.517274, 126.928422, 샛강).
coordinates(37.513342, 126.926382, 대방).
coordinates(37.514219, 126.942454, 노량진).
coordinates(37.529849, 126.964561, 용산).
coordinates(37.522272, 126.974345, 이촌).
coordinates(37.502971, 126.979306, 동작).
coordinates(37.549463, 126.913739, 합정).
coordinates(37.544018, 126.951592, 공덕).
coordinates(37.539261, 126.961351, 효창공원앞).
coordinates(37.534777, 126.97311, 삼각지).
coordinates(37.557192, 126.925381, 홍대입구).
coordinates(37.554648, 126.972559, 서울역).
coordinates(37.561243, 126.99428, 충무로).
coordinates(37.559973, 126.963672, 충정로).
coordinates(37.564718, 126.977108, 시청).
coordinates(37.566295, 126.99191, 을지로3가).
coordinates(37.566941, 126.998079, 을지로4가).
coordinates(37.555134, 126.936893, 신촌).


sb1(X, Y, Z):-subway(X, Y, Z).
sb1(X, Y, Z):-subway(Y, X, Z).

shortest_path(Point, Destination):-
	astar([[0,Point]],Destination,ReversePath),
	reverse(ReversePath, Path),
	write('최단경로: '), print_path(Path,Subways),
	write('경로의 역간 이용 지하철 호선: '),print_subways(Subways).



astar(Paths, Destination, [C,Destination|Path]):-
	member([C,Destination|Path],Paths),
	choosebest(Paths, Destination, [C1|_]),
	C1 == C.
astar(Paths, Destination, BestPath):-
	choosebest(Paths, Destination, Best),
	delete(Paths, Best, PrevPaths),
	expand(Best, NewPaths),
	append(PrevPaths, NewPaths, L),
	astar(L, Destination, BestPath).

choosebest([X],_,X):-!.
choosebest([[C1,Station1|Y],[C2,Station2|_]|Z], Destination,Best):-
	hvalue(Station1, Destination,H1),
	hvalue(Station2, Destination,H2),
	H1 + C1 =< H2 + C2,
	choosebest([[C1,Station1|Y]|Z], Destination,Best).
choosebest([[C1,Station1|_],[C2,Station2|Y]|Z], Destination,Best):-
	hvalue(Station1, Destination, H1),
	hvalue(Station2,Destination, H2),
	H1 + C1 > H2 + C2,
	choosebest([[C2,Station2|Y]|Z], Destination,Best).

hvalue(Station, Destination, Distance) :-
	coordinates(X1, Y1, Station),
	coordinates(X2, Y2, Destination),
	Dlat is (X2 - X1) * pi / 180,
	Dlon is (Y2 - Y1) * pi / 180,
	A is sin(Dlat / 2) ** 2 + cos(X1 * pi / 180) * cos(X2 * pi / 180) * sin(Dlon / 2) ** 2,
	C is 2 * atan2(sqrt(A), sqrt(1 - A)),
	Distance is C * 6371.0.

expand([Cost,Station|Path],Paths):-
	findall([Cost,NewStation,Station|Path],
	(r(Station, NewStation,_),
	not(member(NewStation,Path))),L),
	update_costs(L, Paths).

update_costs([],[]):-!.
update_costs([[Total_Cost,Station1,Station2|Path]|Y],[[NewCost_Total,Station1,Station2|Path]|Z]):-
	r(Station2, Station1, Distance),
	NewCost_Total is Total_Cost + Distance,
	update_costs(Y,Z).


print_path([Station_Destination,Cost],[]):- write(Station_Destination), write('.'), write(' '), nl, write('경로의 총 거리: '), write(Cost), write(' km'),nl.
print_path([Station1,Station2|Y],Subways):-
	sb1(Station1,Station2,Subway),
	append([Subway],R,Subways),
	write(Station1),write(', '),
	print_path([Station2|Y],R).

print_subways([X]):- write(X), nl, nl.
print_subways([X|Y]):-
	write(X),write(' - '),
	print_subways(Y),!.