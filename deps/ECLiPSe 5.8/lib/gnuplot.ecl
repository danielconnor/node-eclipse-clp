% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and Parc Technologies Ltd 2002
% Version:	$Id: gnuplot.ecl,v 1.7 2004/11/25 13:00:41 ajs2 Exp $
% ----------------------------------------------------------------------

:-module(gnuplot).

:- comment(summary, "Interface to the function and data plotting program - gnuplot").
:- comment(author, "Andrew J Sadler, IC-Parc").
:- comment(copyright, "Imperial College London and Parc Technologies").
:- comment(date, "$Id: gnuplot.ecl,v 1.7 2004/11/25 13:00:41 ajs2 Exp $").

:- comment(desc, html(
"<P> This library provides an interface to the function and data
 plotting program - gnuplot.

<P> The gnuplot program is available for most platforms and can be
 downloaded from <a href=\"http://www.gnuplot.info/\">http://www.gnuplot.info/</a>.
 This version of lib(gnuplot) officially supports gnuplot version 3.7
 and higher though it may work with earlier versions as well.

<P> The executable <code>gnuplot</code> on Unices, and
 <code>pgnuplot.exe</code> on Windows must be installed on the default
 path, or be present in the current working directory. 
 NOTE: On Windows it is NOT sufficient to simply have the
 <code>gnuplot.exe</code> or <code>wgnuplot.exe</code>, you must have the
 <code>pgnuplot.exe</code> as well.

<P> See the various <code>plot</code> predicates for further example usage.

<P> For a complete description of the avilable options we refer the
 user to the excellent documentation which accompanies gnuplot.  
 Most features have an obvious analogue in this library.")).

:- comment(eg,"
:-lib(gnuplot).

sample:-
        % x-y pairs with 'points'
        A=[1-3,5-2,9-2,8-2,5-7], plot(A, [with:points]),

        % y values with large 'smooth' lines and points
        B=[1,2,3,4,8,9,4,2,4,6], plot(B, [smooth: csplines,
                                          with:linespoints,
                                          pointsize:3]),

        % multiple y values in nested lists with lines, boxes and titles
        C=[[1,2,4,9,18,27,3],[1,4,16,36,25,16,9]],
        plot(C, [with:[lines, boxes], title:['data 1', 'data 2']]),

        % multiple y values in an array, in a certain range, with impulses of
        % different widths
        D=[]([](1,2,4,6,7,8,9),[](1,4,16,36,49,64,81)),
           plot(D, [ranges:(3:6), with:impulses, linewidth:[8,2]]),

        % multiple t-r pairs, in polar coordinates with a grid and lines
        E=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]],
           plot(E,[set:[polar, grid=polar], with:lines]),

        
        % compute sin and cos and tan points
        (for(I, 0, 314),
         foreach(R-X,SinPoints),
         foreach(R-Y,CosPoints),
         foreach(R-Z,TanPoints)
        do
            R is I / 100,
            X is sin(R),
            Y is cos(R),
            Z is tan(R)
        ),
        % plot sin and cos on a polar plot
        plot([SinPoints, CosPoints],
             [set:[polar, grid=polar], with:linespoints,
              title:[\"sin\", \"cos\"]]),

        % plot sin, cos and tan on a logarithmic polar plot
        plot([SinPoints, CosPoints, TanPoints],
             [set:[polar, grid=polar,logscale=xy], with:linespoints,
              title:[\"sin\", \"cos\", \"tan\"]]),
        
        % a(x,y,error) = x-y data with error values for the y values
        F=[a(1,1,0.1),a(2,2,0.1),a(5,3,0.5),a(6,2,0.5), a(7,3,0.5)],
        plot(F, [ranges:[ 0:8, 0:6], with:boxes]),
        plot(F, [ranges:[ 0:8, 0:6], with:errorbars]),
        plot(F, [ranges:[ 0:8, 0:6], with:boxerrorbars]),
        
        % a(x,y,error) = x-y data with error values for the y values
        % and a gap between x=2 and x=5
        G=[a(1,1,0.1),a(2,2,0.1),-,a(5,3,0.5),a(6,2,0.5), a(7,3,0.5)],
        plot(G, [ranges:[ 0:8, 0:6], with:boxes]),
        plot(G, [ranges:[ 0:8, 0:6], with:errorbars]),
        plot(G, [ranges:[ 0:8, 0:6], with:boxerrorbars]).
        
").

:-lib(module_options).
:-lib(matrix_util).

:- local struct(plot_options(set,
                             ranges,
                             datafile,
                             index,
                             every,
                             thru,
                             using,
                             smooth,
                             axes,
                             title,
                             with,
                             linestyle,
                             linetype,
                             linewidth,
                             pointtype,
                             pointsize)).

valid_option_field(set, set of plot_options).
valid_option_field(ranges, ranges of plot_options).
valid_option_field(datafile, datafile of plot_options).
valid_option_field(axes, axes of plot_options).
valid_option_field(title, title of plot_options).


valid_option_field(index, index of plot_options).
valid_option_field(every, every of plot_options).
valid_option_field(thru, thru of plot_options).
valid_option_field(using, using of plot_options).
valid_option_field(smooth, smooth of plot_options).

valid_option_field(with, with of plot_options).
valid_option_field(linestyle, linestyle of plot_options).
valid_option_field(linetype, linetype of plot_options).
valid_option_field(linewidth, linewidth of plot_options).
valid_option_field(pointtype, pointtype of plot_options).
valid_option_field(pointsize, pointsize of plot_options).


valid_option_value(_Field, Value):-
        var(Value),!.
valid_option_value(Field, ValueList):-
        ValueList=[_|_],!,
        (foreach(Value,ValueList), param(Field) do
            valid_option_value(Field, Value)
        ).


valid_option_value(set, KeyValue) :-
        functor(KeyValue, _, Arity),
        Arity =< 2,
        % should check for known key=value pairs?
        true.

valid_option_value(ranges, (_Min:_Max)).

valid_option_value(datafile, Value):-
        exists(Value).

valid_option_value(axes, x1y1).
valid_option_value(axes, x2y2).
valid_option_value(axes, x1y2).
valid_option_value(axes, x2y1).

valid_option_value(title, _Value).


valid_option_value(with, lines).
valid_option_value(with, points).
valid_option_value(with, linespoints).
valid_option_value(with, impulses).
valid_option_value(with, dots).
valid_option_value(with, steps).
valid_option_value(with, fsteps).
valid_option_value(with, histeps).
valid_option_value(with, errorbars).
valid_option_value(with, xerrorbars).
valid_option_value(with, yerrorbars).
valid_option_value(with, xyerrorbars).
valid_option_value(with, boxes).
valid_option_value(with, boxerrorbars).
valid_option_value(with, boxxyerrorbars).
valid_option_value(with, financebars).
valid_option_value(with, candlesticks).
valid_option_value(with, vector).
valid_option_value(with, Other):-
        !,
        printf(error,
               "Unknown plot style: %w, continuing anyway.%n",
               [Other]).

valid_option_value(index,Value) :-
        valid_range(Value, integer, Depth),
        Depth =< 3.

valid_option_value(every, Value) :-
        valid_range(Value, integer, Depth),
        Depth =< 6.

valid_option_value(thru, Value) :-
        compound(Value).

valid_option_value(using,Records=Format) :-
        valid_range(Records, true, _Depth),
        string(Format).

valid_option_value(smooth,unique).
valid_option_value(smooth,csplines).
valid_option_value(smooth,acsplines).
valid_option_value(smooth,bezier).
valid_option_value(smooth,sbezier).



valid_option_value(linestyle, Value) :-
        var(Value),!.
valid_option_value(linestyle, Value) :-
        integer(Value).
valid_option_value(linetype, Value) :-
        var(Value),!.
valid_option_value(linetype, Value) :-
        integer(Value).
valid_option_value(linewidth, Value) :-
        var(Value),!.
valid_option_value(linewidth, Value) :-
        number(Value).
valid_option_value(pointtype, Value) :-
        var(Value),!.
valid_option_value(pointtype, Value) :-
        integer(Value).
valid_option_value(pointsize, Value) :-
        var(Value),!.
valid_option_value(pointsize, Value) :-
        number(Value).




valid_range(Range, Pred, Depth):-
        valid_range_aux(Range, Pred, 1, Depth).

valid_range_aux(Min:Max, Pred, DepthIn, DepthOut):-
        !,
        valid_range(Min, Pred, 1),
        NewDepthIn is DepthIn+1,
        valid_range_aux(Max, Pred, NewDepthIn, DepthOut).
valid_range_aux(_Range, true, DepthIn, DepthIn):-
        !.
valid_range_aux(Range, Pred, DepthIn, DepthIn):-
        Pred =.. [Functor|Args],
        append(Args, [Range], NewArgs),
        Goal =.. [Functor|NewArgs],
        call(Goal).





default_options(plot_options with [datafile:"-",
                                   title:"",
                                   with:lines
                                  ]
               ).


validate_options(Options, OptionStruct):-
        ( get_options(Options, OptionStruct) ->
              true
        ;
              printf(error, "Invalid option list: %w%n", [Options]),
              print_default_options(error),
              abort
        ).




:-export plot/1.
:- comment(plot/1, [
        summary:"Plot the given data using default options",
        amode:plot(++),
        args:[
                 "Data": "The data to be plotted, array or list."
             ],
        desc: html( 
"<P> Produces an on screen plot of the data given using the default
 settings.

<P> Can either be a list or a nested list of a 1D or 2D array of y
 values or x-y points."),
        fail_if: "Data is not in a valid format",
        resat: false,
        eg: "
% x-y pairs
?- A=[1-3,5-2,9-2,8-2,5-7], plot(A).

% y values
?- A=[1,2,3,4,8,9,4,2,4,6], plot(A).

% multiple y values in nested lists
?- A=[[1,2,4,6,7,8,9],[1,4,16,36,49,64,81]], plot(A).            

% multiple y values in an array
?- A=[]([](1,2,4,6,7,8,9),[](1,4,16,36,49,64,81)), plot(A).

% multiple x-y pairs
?- A=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]], plot(A).
",
        see_also: [plot/2, plot/3, plot/4]]).

plot(Data):-
        plot(Data, []).


:- export plot/2.
:- comment(plot/2, [
        summary:"Plots the given data using the given options",
        amode:plot(++,+),
        args:[
                 "Data": "The data to be plotted, array or list.",
                 "Options": "Options list"
             ],
        desc: html("
<P> Plots the given data to screen using the supplied options.

<P> Can either be a list or a nested list of a 1D or 2D array of y
 values or x-y points.

<P> The options correspond directly to the options of the gnuplot
 'plot' command.
"),
        fail_if: "Options are malformed, or data is not in a valid format.",
        resat: false,
        eg: "
% x-y pairs with 'points'
?- A=[1-3,5-2,9-2,8-2,5-7], plot(A, [with:points]).

% y values with large 'smooth' lines and points
?- A=[1,2,3,4,8,9,4,2,4,6], plot(A, [smooth: csplines,
                                     with:linespoints,
                                     pointsize:3]).

% multiple y values in nested lists with lines, boxes and titles
?- A=[[1,2,4,9,18,27,3],[1,4,16,36,25,16,9]],
   plot(A, [with:[lines, boxes], title:['data 1', 'data 2']]).

% multiple y values in an array, in a certain range, with impulses of
% different widths
?- A=[]([](1,2,4,6,7,8,9),[](1,4,16,36,49,64,81)),
   plot(A, [ranges:(3:6), with:impulses, linewidth:[8,2]]).

% multiple t-r pairs, in polar coordinates with a grid and lines
?- A=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]],
   plot(A,[set:[polar, grid=polar], with:lines]).
            ",
        see_also: [plot/1, plot/3, plot/4]]).

plot(Data,Options):-
        validate_options(Options, PlotOptions),
        process_data(Data, ProcessedData),
	get_flag(hostarch, ARCH),
        ( ARCH=="i386_nt" ->
            exec([pgnuplot,'-'],[S,_,_],_Pid)
        ;
            exec([gnuplot,'-persist','-'],[S,_,_],_Pid)
            %S=output
        ),
        output_preamble(ProcessedData, PlotOptions, S),
        output_plot_command(ProcessedData, PlotOptions, S),
        flush(S),
        close(S).




:-export plot/3.
:- comment(plot/3, [
        summary:"Plots the given data to a file.",
        amode:plot(++,+,+),
        args:[
                 "Data": "The data to be plotted, array or list.",
                 "Options": "Options list",
                 "OutputFile": "Name of file to store plot in"
             ],
        desc: html("
<P> Plots the data to a file as Postscript (by default).

<P> Can either be a list or a nested list of a 1D or 2D array of y
 values or x-y points.

<P> The output format can be changed either by explicitly setting the
 'terminal' variable using the 'set' option, or by calling plot/4.
"),
        fail_if: "Options are malformed, or data is not in a valid format.",
        resat:false,
        eg:"
% multiple t-r pairs, in polar coordinates with a grid and lines, into
% a postscript file
?- A=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]],
   plot(A,[set:[polar, grid=polar], with:lines], \"polar.ps\").
",
        see_also: [plot/1, plot/2, plot/4]]).
plot(Data, Options, OutputFile) :-
        plot(Data, Options, "postscript", OutputFile).


:-export plot/4.
:- comment(plot/4, [
        summary:"Plots the given data to a file in the given format.",
        amode:plot(++,+,+,+),
        args:[
                 "Data": "The data to be plotted, array or list.",
                 "Options": "Options list",
                 "Format": "Output file format",
                 "OutputFile": "Name fo file to store plot in"
             ],
        desc: html("
<P> Plots the data to a file in the specified format.

<P> The Data can either be a list or a nested list of a 1D or 2D array
 of y values or x-y points.

<P> The output format is defined as being the 'terminal' setting for
 gnuplot.
"),
        fail_if: "Options are malformed, or data is not in a valid format.",
        resat:false,
        eg:"
% multiple t-r pairs, in polar coordinates with a grid and lines, into
% a postscript file
?- A=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]],
   plot(A,[set:[polar, grid=polar], with:lines], postscript, \"polar.ps\").

% multiple t-r pairs, in polar coordinates with a grid and lines, into
% a postscript file with color
?- A=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]],
   plot(A,[set:[polar, grid=polar], with:lines], \"postscript color\", \"polar.ps\").

% multiple t-r pairs, in polar coordinates with a grid and lines, into
% a PNG file
?- A=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]],
   plot(A,[set:[polar, grid=polar], with:lines], png, \"polar.png\").

% multiple t-r pairs, in polar coordinates with a grid and lines, into
% a XFIG file
?- A=[[1-3,5-2,9-2,8-2,5-7], [1-2,5-4,8-6,9-1,12-4]],
   plot(A,[set:[polar, grid=polar], with:lines], fig, \"polar.fig\").
",
        see_also: [plot/1, plot/2, plot/3]]).
plot(Data, Options, Format, OutputFile) :-
        TerminalTerm=(terminal=Format),
        concat_string(["\"", OutputFile, "\""], OutputStr),
        OutputTerm=(output=OutputStr),
        (lists:delete(set:Value, Options, Options1) ->
            (Value=[_|_] ->
                NewValue = [TerminalTerm, OutputTerm| Value]
            ;
                NewValue = [TerminalTerm, OutputTerm, Value]
            )
        ;
            NewValue = [TerminalTerm, OutputTerm],
            Options1 = Options
        ),
        plot(Data, [set:NewValue | Options1]).




process_data(Datas,NewDatas):-
        Datas=[SubList|_],
        SubList=[_|_],
        !,
        % build multi-array from lists
        length(Datas,NumRow),
        dim(NewDatas,[NumRow]),
        (foreach(Data,Datas), foreacharg(NewData,NewDatas) do
            NewData =.. [[]|Data]
        ).
process_data(Datas, NewDatas):-
        Datas=[_|_],
        !,
        NewRow =.. [[]|Datas],
        NewDatas = [](NewRow).
process_data(Datas, Datas).

output_preamble(_Datas, plot_options with [set:KeyValueList], S):-
        (foreach(KeyValue, KeyValueList), param(S) do
            functor(KeyValue, _, Arity),
            (Arity=2 ->
                arg(1, KeyValue, Key),
                arg(2, KeyValue, Value)
            ;
                Key=" ",
                Value=KeyValue
            ),
            write(S, "set "),
            write(S, Key), write(S, " "), write(S, Value),
            write(S,"; ")
        ).

output_plot_command(Datas, PlotOptions, S):-
        (dim(Datas,[Rows,Columns]) ->
            write(S,"plot "),
            output_plot_option(ranges, PlotOptions, 1, S),
            output_plot_options(Datas, PlotOptions, 1, S),
            (for(Row,2,Rows), param(S,PlotOptions,Datas) do
                write(S,", "),
                output_plot_options(Datas, PlotOptions, Row, S)
            ),
            nl(S),
            (for(Row,1,Rows), param(S,Datas,Columns,Rows) do
                (for(Col,1,Columns), param(S,Row,Datas) do
                    E is Datas[Row,Col],
                    write_data_point(S,Col,E)
                ),
                (Row<Rows -> writeln(S,"e") ; true)
            )
        ;
            write(S,"plot "),
            output_plot_option(ranges, PlotOptions, 1, S),
            output_plot_options(Datas, PlotOptions, 1, S),
            (foreach(Data,Datas),count(I,1,_), param(S) do
                write_data_point(S,I,Data)
            )
        ).        


output_plot_options(_Datas, PlotOptions, RowNumber, S):-
        (foreach(Field, [datafile, index, every, thru, using, smooth,
                         axes, title,
                         with, linestyle, linetype, linewidth,
                         pointtype, pointsize]),
         param(PlotOptions, RowNumber, S) do
            output_plot_option(Field, PlotOptions, RowNumber, S)
        ).

output_plot_option(Field, PlotOptions, _RowNumber, _S):-
        valid_option_field(Field, FieldArg),
        arg(FieldArg, PlotOptions, Value),
        var(Value), !.
output_plot_option(Field, PlotOptions, RowNumber, S):-
        output_plot_option_nonvar(Field, PlotOptions, RowNumber, S).

output_plot_option_nonvar(ranges, PlotOptions, _RowNumber, S):-
        !,
        valid_option_field(ranges, FieldArg),
        arg(FieldArg, PlotOptions, ValueList),
        (ValueList=[_|_] ->
            RangeList=ValueList
        ;
            RangeList=[ValueList]
        ),
        (foreach(Range, RangeList), param(S) do
            output_field_prefix(ranges, S),
            output_field_value(ranges, Range, S),
            output_field_postfix(ranges, S)
        ).
output_plot_option_nonvar(Field, PlotOptions, RowNumber, S):-
        valid_option_field(Field, FieldArg),
        arg(FieldArg, PlotOptions, ValueList),
        (ValueList=[_|_] ->
            nth(ValueList,RowNumber,Value)
        ;
            Value=ValueList
        ),
        output_field_prefix(Field, S),
        output_field_value(Field, Value, S),
        output_field_postfix(Field, S).


output_field_prefix(Field, _S):-
        memberchk(Field,[datafile]),!.
output_field_prefix(axes, S):- !,
        write(S, " axes [").
output_field_prefix(ranges, S):- !,
        write(S, " [").
output_field_prefix(with, S):- !,
        write(S, " with ").
output_field_prefix(Field, S):-
        write(S, " "), write(S, Field), write(S, " ").


output_field_value(Field,Value, S):-
        memberchk(Field, [datafile, title]),!,
        (atom(Value)->
            atom_string(Value,Str)
        ;
            Str=Value
        ),
        writeq(S, Str).
output_field_value(_Field,Value, S):-
        write(S, Value).

output_field_postfix(axes, S):- !,
        write(S, "] ").
output_field_postfix(ranges, S):- !,
        write(S, "] ").
output_field_postfix(_Field, _S).


write_data_point(S,I,Data):-
        (number(Data) ->
            write(S,I),write(S,' '),writeln(S,Data)
        ; Data = [_|_] ->
            (foreach(Arg,Data), param(S) do
                write(S,Arg),write(S,' ')
            )
        ; Data =.. [_|Args] ->
            (foreach(Arg,Args), param(S) do
                write(S,Arg),write(S,' ')
            ),
            nl(S)
        ;
            true
        ).

nth([X|_],1,X):-!.
nth([_H|T],N,X):-
        N>1,
        N1 is N-1,
        nth(T,N1,X).

