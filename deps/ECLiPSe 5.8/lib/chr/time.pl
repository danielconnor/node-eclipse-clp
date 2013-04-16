
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- ['time-pc.pl'].
ground(_497) :- not nonground(_497).
inf(3.40282e+38).
minf(-3.40282e+38).
sup(1e-45).
msup(-1e-45).
delay empty(_566, _563, _573) if var(_566), var(_563).
empty(0, [], _585).
delay universal(_598, _606, _605) if var(_598).
delay universal(_627, _619, _626) if var(_619).
delay universal(_648, _647, _640) if var(_640).
universal(_671, _685, _672) :- (is_quantl(_685) -> _685 = [_708 - _704], _708 =< minf, inf =< _704 ; _672 = p - p -> sort(_685, [eq, ge, le]) ; size(_672, _671)), !.
size(i - i, 13).
size(p - p, 3).
size(p - i, 5).
size(i - p, 5).
size(s - s, 5).
delay equality(_806, _800) if var(_800).
delay equality(_819, _825) if var(_819).
equality(_842, i - i) :- !, member(equals, _842).
equality(_865, s - s) :- !, member(eq, _865).
equality(_917, p - p) :- (is_quall(_917) -> member(_936, _917), (_936 = eq ; number(_936), _936 =:= 0) ; member(_904 - _900, _917), (_904 = 0, _900 = 0 ; _904 =< 0, 0 =< _900)), !.
delay unique(_965) if nonground(_965).
unique([_987 - _986]) :- !, _987 =:= _986.
unique([_1006]).
bind_value(_1027, _1028, [_1035]) :- (_1035 = _1026 - _1041 ; _1035 = _1026) -> _1028 =:= _1027 + _1026.
shift_interval(_1059, [], []).
shift_interval(_1085, [_1100 - _1092|_1084], [_1101 - _1093|_1083]) :- !, _1101 is _1100 - _1085, _1093 is _1092 - _1085, shift_interval(_1085, _1084, _1083).
shift_interval(_1135, [_1142|_1134], [_1143|_1133]) :- _1143 is _1142 - _1135, shift_interval(_1135, _1134, _1133).
delay intersection(_1164, _1174, _1173, _1172) if var(_1164).
delay intersection(_1197, _1187, _1196, _1195) if var(_1187).
intersection(_1244, _1238, _1230, _1272) :- qtype(_1244, _1243), qtype(_1238, _1237), (_1243 == quall, _1237 == quall -> intersection(_1244, _1238, _1230) ; qualquant(_1244, _1243, _1232), qualquant(_1238, _1237, _1231), interint(_1232, _1231, _1230)), !.
interint([], _1284, []).
interint(_1300, [], []) :- _1300 = [_1298|_1299].
interint([_1351|_1339], [_1338|_1337], _1346) :- isless(_1351, _1338) -> interint(_1339, [_1338|_1337], _1346) ; isless(_1338, _1351) -> interint([_1351|_1339], _1337, _1346) ; overlaps1(_1351, _1338, _1345) -> _1346 = [_1345|_1334], interint([_1351|_1339], _1337, _1334) ; overlaps2(_1351, _1338, _1345) -> _1346 = [_1345|_1334], interint(_1339, [_1338|_1337], _1334).
isless(_1430 - _1419, _1418 - _1426) :- _1419 < _1418.
overlaps1(_1466 - _1459, _1465 - _1458, _1464 - _1457) :- _1459 >= _1458, _1465 =< _1459, _1466 =< _1458, my_max(_1466, _1465, _1464), my_min(_1459, _1458, _1457).
overlaps2(_1524 - _1517, _1523 - _1516, _1522 - _1515) :- _1516 >= _1517, _1523 =< _1517, _1524 =< _1516, my_max(_1524, _1523, _1522), my_min(_1517, _1516, _1515).
my_max(_1567, _1571, _1566) :- _1567 >= _1571, !, _1567 = _1566.
my_max(_1588, _1587, _1587).
my_min(_1609, _1613, _1608) :- _1609 =< _1613, !, _1609 = _1608.
my_min(_1630, _1629, _1629).
delay transl(_1649, _1646, _1658, _1657) if var(_1649), var(_1646).
delay transl(_1677, _1686, _1674, _1685) if var(_1677), var(_1674).
delay transl(_1714, _1705, _1702, _1713) if var(_1705), var(_1702).
transl(_1776, _1770, _1764, _1755) :- qtype(_1776, _1775), qtype(_1770, _1769), qtype(_1764, _1763), (_1755 = p - p - p, (_1775 == quantl ; _1769 == quantl ; _1763 == quantl) -> qualquant(_1776, _1775, _1758), qualquant(_1770, _1769, _1757), qualquant(_1764, _1763, _1756), transl(_1758, _1757, _1756, _1755, quantl) ; quantqual(_1776, _1775, _1758), quantqual(_1770, _1769, _1757), quantqual(_1764, _1763, _1756), transl(_1758, _1757, _1756, _1755, quall)), !.
transl(_1908, _1904, _1874, _1873, _1872) :- var(_1874), !, setof(_1898, _1900 ^ _1899 ^ (member(_1900, _1908), member(_1899, _1904), trans(_1900, _1899, _1898, _1873, _1872)), _1875), mergerel(_1875, _1874, _1873, _1872).
transl(_1977, _1943, _1973, _1942, _1941) :- var(_1943), !, setof(_1968, _1969 ^ _1967 ^ (member(_1969, _1977), member(_1967, _1973), trans(_1969, _1968, _1967, _1942, _1941)), _1944), mergerel(_1944, _1943, _1942, _1941).
transl(_2012, _2046, _2042, _2011, _2010) :- var(_2012), !, setof(_2038, _2037 ^ _2036 ^ (member(_2037, _2046), member(_2036, _2042), trans(_2038, _2037, _2036, _2011, _2010)), _2013), mergerel(_2013, _2012, _2011, _2010).
mergerel(_2075, _2074, _2091, _2085) :- (_2085 == quantl -> mergerel(_2075, _2074) ; _2075 = _2074), !.
mergerel([], []).
mergerel([_2147 - _2141, _2146 - _2140|_2130], _2127) :- _2141 + sup >= _2146, !, my_min(_2147, _2146, _2135), my_max(_2141, _2140, _2134), mergerel([_2135 - _2134|_2130], _2127).
mergerel([_2187|_2181], [_2187|_2180]) :- mergerel(_2181, _2180).
trans(_2209, _2208, _2207, s - s - s, quall) :- !, strans(_2209, _2208, _2207).
trans(_2241, _2240, _2239, p - p - p, quall) :- !, prans(_2241, _2240, _2239).
trans(_2273, _2272, _2271, p - p - p, quantl) :- !, qtrans(_2273, _2272, _2271).
trans(_2306, _2305, _2304, _2315 - _2314 - _2310, quall) :- !, itrans(_2315 - _2314 - _2310, _2306, _2305, _2304).
delay qtype(_2340, _2346) if nonground(_2340).
qtype(_2359, quantl) :- is_quantl(_2359).
qtype(_2375, quall) :- is_quall(_2375).
is_quantl([_2391|_2395]) :- is_quant(_2391).
is_quall([_2408|_2412]) :- is_qual(_2408).
delay is_quant(_2425) if var(_2425).
is_quant(_2443 - _2442).
delay is_qual(_2456) if var(_2456).
is_qual(_2473) :- atomic(_2473).
delay qualquant(_2494, _2501, _2491) if var(_2494), var(_2491).
qualquant(_2522, _2526, _2521) :- _2526 == quall -> qualquant(_2522, _2536), mergerel(_2536, _2521) ; _2526 = quantl -> _2522 = _2521.
delay quantqual(_2565, _2572, _2562) if var(_2565), var(_2562).
quantqual(_2593, _2597, _2592) :- _2597 == quantl -> quantqual(_2593, _2592) ; _2597 = quall -> _2593 = _2592.
qualquant([], []).
qualquant([_2643|_2638], [_2642|_2637]) :- qualquant1(_2643, _2642), qualquant(_2638, _2637).
qualquant1(le, _2674 - _2670) :- !, _2674 is sup, _2670 is inf.
qualquant1(eq, 0 - 0) :- !.
qualquant1(ge, _2720 - _2716) :- !, _2720 is minf, _2716 is msup.
qualquant1(_2740, _2741 - _2741) :- _2741 is _2740.
quantqual(_2767, _2762) :- findall(_2766, quantqual1(_2767, _2766), _2762).
quantqual1(_2800, eq) :- once (member(_2796 - _2792, _2800), _2796 =< 0, 0 =< _2792).
quantqual1(_2829, le) :- once (member(_2833 - _2825, _2829), 0 < _2825).
quantqual1(_2859, ge) :- once (member(_2855 - _2863, _2859), _2855 < 0).
quantqual1(_2890, _2886) :- once (member(_2886 - _2885, _2890), _2886 =:= _2885).
:- ['allentable.pl'].
check_ii(_2924, _2922, _2923) :- interval_point(_2924, _2923, _2922).
interval_point([_2957, _2943], before, [_2942, _2952]) :- _2943 < _2942.
interval_point([_2971, _2986], after, [_2981, _2972]) :- _2972 < _2971.
interval_point([_3015, _3001], meets, [_3000, _3010]) :- _3001 =:= _3000.
interval_point([_3029, _3044], met_by, [_3039, _3030]) :- _3030 =:= _3029.
interval_point([_3067, _3062], starts, [_3066, _3061]) :- _3067 =:= _3066, _3062 < _3061.
interval_point([_3102, _3096], started_by, [_3101, _3097]) :- _3102 =:= _3101, _3097 < _3096.
interval_point([_3131, _3137], finishes, [_3132, _3136]) :- _3137 =:= _3136, _3132 < _3131.
interval_point([_3167, _3171], finished_by, [_3180, _3166]) :- _3171 =:= _3166, _3167 < _3166.
interval_point([_3206, _3202], during, [_3207, _3201]) :- _3207 < _3206, _3202 < _3201.
interval_point([_3242, _3236], contains, [_3241, _3237]) :- _3242 < _3241, _3237 < _3236.
interval_point([_3283, _3275], overlaps, [_3279, _3274]) :- _3283 < _3279, _3279 < _3275, _3275 < _3274.
interval_point([_3320, _3315], overlapped_by, [_3324, _3316]) :- _3324 < _3320, _3320 < _3316, _3316 < _3315.
interval_point([_3359, _3354], equals, [_3358, _3353]) :- _3359 =:= _3358, _3354 =:= _3353.
itrans(_3407 - _3416 - _3406, _3421, _3412, _3402) :- encode(_3407 - _3416, _3421, _3397), encode(_3416 - _3406, _3412, _3396), encode(_3407 - _3406, _3402, _3395), cons_tri(_3397, _3396, _3395).
delay encode(_3465 - _3464, _3454, _3451) if var(_3454), var(_3451).
encode(i - i, _3483, _3482) :- !, encode(_3483, _3482).
encode(p - i, _3515, _3510) :- !, pi_ii(_3515, _3511), encode(_3511, _3510).
encode(i - p, _3547, _3542) :- !, ip_ii(_3547, _3543), encode(_3543, _3542).
encode(p - p, _3586, _3577) :- !, pp_pi(_3586, _3582), pi_ii(_3582, _3578), encode(_3578, _3577).
delay encode(_3612, _3609) if var(_3612), var(_3609).
encode(before, 1).
encode(after, 2).
encode(during, 3).
encode(contains, 4).
encode(overlaps, 5).
encode(overlapped_by, 6).
encode(meets, 7).
encode(met_by, 8).
encode(starts, 9).
encode(started_by, 10).
encode(finishes, 11).
encode(finished_by, 12).
encode(equals, 13).
check_pp(_3771, _3772, _3779 - _3770) :- !, _3771 + _3779 < _3772, _3772 < _3771 + _3770.
check_pp(_3811, _3806, _3810) :- number(_3810), !, _3811 + _3810 =:= _3806.
check_pp(_3854, _3838, _3837) :- \+ member(_3837, [le, eq, ge]), !, _3838 = _3837.
check_pp(_3874, _3873, _3872) :- number(_3874), number(_3873) -> check_ppn(_3874, _3873, _3872) ; check_ppt(_3874, _3873, _3872).
check_ppn(_3907, _3906, le) :- _3907 < _3906.
check_ppn(_3926, _3925, eq) :- _3926 =:= _3925.
check_ppn(_3945, _3944, ge) :- _3945 > _3944.
check_ppt(_3964, _3963, le) :- _3964 @< _3963.
check_ppt(_3983, _3982, eq) :- _3983 = _3982.
check_ppt(_4002, _4001, ge) :- _4002 @> _4001.
prans(_4038, _4034, _4030) :- (number(_4038) ; number(_4034) ; number(_4030)), !, qtrans(_4038 - _4038, _4034 - _4034, _4030 - _4030).
prans(le, le, le).
prans(le, eq, le).
prans(le, ge, le).
prans(le, ge, eq).
prans(le, ge, ge).
prans(eq, le, le).
prans(eq, eq, eq).
prans(eq, ge, ge).
prans(ge, le, le).
prans(ge, le, eq).
prans(ge, le, ge).
prans(ge, eq, ge).
prans(ge, ge, ge).
qtrans(_4236 - _4227, _4235 - _4226, _4237 - _4228) :- var(_4236), var(_4227) -> safe_is(_4236, _4237 - _4226), safe_is(_4227, _4228 - _4235) ; var(_4235), var(_4226) -> safe_is(_4235, _4237 - _4227), safe_is(_4226, _4228 - _4236) ; var(_4237), var(_4228) -> safe_is(_4237, _4236 + _4235), safe_is(_4228, _4227 + _4226).
safe_is(_4338, _4337 - _4336) :- _4337 =:= minf, _4336 =:= inf -> _4338 is minf ; _4337 =:= inf, _4336 =:= minf -> _4338 is inf ; _4337 =:= msup, _4336 =:= sup -> _4338 is msup ; _4337 =:= sup, _4336 =:= msup -> _4338 is sup ; _4338 is _4337 - _4336.
safe_is(_4435, _4434 + _4433) :- _4434 =:= inf, _4433 =:= inf -> _4435 is inf ; _4434 =:= minf, _4433 =:= minf -> _4435 is minf ; _4434 =:= sup, _4433 =:= sup -> _4435 is sup ; _4434 =:= msup, _4433 =:= msup -> _4435 is msup ; _4435 is _4434 + _4433.
check_pi(_4516, [_4515, _4525], before) :- _4516 < _4515.
check_pi(_4540, [_4539, _4549], starts) :- _4540 =:= _4539.
check_pi(_4567, [_4571, _4566], during) :- _4571 < _4567, _4567 < _4566.
check_pi(_4594, [_4603, _4593], finishes) :- _4594 =:= _4593.
check_pi(_4617, [_4627, _4618], after) :- _4618 < _4617.
check_pi([_4641, _4651], _4642, after) :- _4642 < _4641.
check_pi([_4665, _4675], _4666, started_by) :- _4666 =:= _4665.
check_pi([_4697, _4692], _4693, contains) :- _4697 < _4693, _4693 < _4692.
check_pi([_4729, _4719], _4720, finished_by) :- _4720 =:= _4719.
check_pi([_4753, _4744], _4743, before) :- _4744 < _4743.
delay pi_ii(_4772, _4769) if var(_4772), var(_4769).
pi_ii(before, before).
pi_ii(before, meets).
pi_ii(before, finished_by).
pi_ii(before, contains).
pi_ii(before, overlaps).
pi_ii(starts, starts).
pi_ii(starts, equals).
pi_ii(starts, started_by).
pi_ii(during, during).
pi_ii(during, finishes).
pi_ii(during, overlaped_by).
pi_ii(finishes, met_by).
pi_ii(after, after).
delay ip_ii(_4926, _4923) if var(_4926), var(_4923).
ip_ii(before, before).
ip_ii(finished_by, meets).
ip_ii(contains, contains).
ip_ii(contains, overlaps).
ip_ii(contains, finished_by).
ip_ii(started_by, starts).
ip_ii(started_by, equals).
ip_ii(started_by, started_by).
ip_ii(after, during).
ip_ii(after, finishes).
ip_ii(after, overlaped_by).
ip_ii(after, met_by).
ip_ii(after, after).
delay pp_pi(_5080, _5077) if var(_5080), var(_5077).
pp_pi(le, before).
pp_pi(eq, starts).
pp_pi(ge, during).
pp_pi(ge, finishes).
pp_pi(ge, after).
delay pp_ii(_5154, _5151) if var(_5154), var(_5151).
pp_ii(_5181, _5176) :- pp_pi(_5181, _5177), pi_ii(_5177, _5176).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
