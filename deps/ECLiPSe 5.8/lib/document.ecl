% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright (C) Imperial College London and Parc Technologies 2000
% Version:	$Id: document.ecl,v 1.54 2004/11/19 14:54:48 js10 Exp $
% ----------------------------------------------------------------------

:- module(document).

:- comment(summary, "Tools for generating documentation from ECLiPSe sources").
:- comment(date, "$Date: 2004/11/19 14:54:48 $").
:- comment(copyright, "Imperial College London and Parc Technologies").
:- comment(author, "Kish Shen and Joachim Schimpf, IC-Parc").
:- comment(status, stable).

:- export  icompile/1, icompile/2.

:- tool(icompile/1, icompile_body/2).

:- tool(icompile/2, icompile_body/3).

:- import get_bip_error/1,
	  set_bip_error/1
   from sepia_kernel.

:- comment(desc, html("
    This library contains the tools necessary to generate documentation
    from ECLiPSe sources. It complements the file-to-file compiler
    library(fcompile) as follows:
    <PRE>
    xyz.ecl  ---fcompile--&gt;  xyz.eco
    xyz.ecl  ---icompile--&gt;  xyz.eci  ---eci_to_html--&gt;  xyz/...html
    </PRE>
    ")).

:- comment(icompile/1, [
    summary: "Generates an information file from the ECLiPSe source File.",
    args: ["File":"Name of source file (Atom or string)"], 
    amode: icompile(+),
    desc:html("
    Given an ECLiPSe source file (usually a file with a .ecl suffix), this
    tool generates an ECLiPSe interface information file (with .eci suffix).
    <P>
    The given source file must contain a module definition. icompile then
    extracts all information about this module's interface (in particular
    all export directives), as well as the corresponding documentation
    (comment/2) directives, and writes those into an interface information
    file with .eci suffix. The latter can then be used to generate HTML
    documentation about the module.
    "),
    see_also:[icompile/2,comment/2,eci_to_html/3,ecis_to_htmls/4]
    ]).
:- comment(icompile/2, [
    summary: "Generates an information file from the ECLiPSe source File in directory Destination.",
    args: ["File":"Name of source file (Atom or string)",
       "Destination":"Destination directory (Atom or string)"], 
    amode: icompile(+,+),
    see_also:[icompile/1]
    ]).


icompile_body(File, M) :-
	get_flag(cwd, CWD),
	icompile_body(File, CWD, M).

icompile_body(File, OutDir, M) :-
	atom_or_string(File),
	atom_or_string(OutDir),
	% compile must be done before streams are set
	get_inout_names(File, OutDir, InDir, InFile, InterFile),
	ensure_compiled(InFile, M),
	check_open(read, InFile, In), 
	( check_open(write, InterFile, Out) -> true ; close(In), fail),
	getcwd(Cwd), cd(InDir),
	block( (icompile1(In, Out, InterFile, M) ->
	            cd(Cwd), close(Out), close(In)
	       ;
	            cd(Cwd), close(Out), close(In), fail
	       ), Tag,
	       (cd(Cwd), close(Out), close(In), 
	        printf(error,"icompile(%w, %w) aborted%n", [File,OutDir]),
	        exit_block(Tag)
               )
	), !.
icompile_body(File, OutDir, _M) :-
	get_bip_error(E),
	error(E, icompile(File, OutDir)).

icompile1(In, Out, InterFile, Module0) :-
	Start is cputime,
	pathname(InterFile, _, BaseName, _),
	gather_comments(In, BaseName, Comments0, no, FoundM, Module0, _),
	(FoundM \== no ->
	    merge_comments(Comments0, Comments),
	    arg(1, FoundM, MainModule),
	    gen_interface_file(Out, Comments, MainModule), !,
	    Elasped is cputime - Start,
	    printf(log_output, "%w generated in %.2f seconds.%n", [InterFile, Elasped])
	;   writeln(error, "Main module not found, no comments file generated.")
	).

% merge seperate comment directives for the same predicate into one comment
merge_comments(Comments0, Comments) :-
        get_predicate_comments(Comments0, PredComs0, OtherComs),
	sort(1, =<, PredComs0, PredComs1),
	merge_predcomments(PredComs1, PredComs),
	append(OtherComs, PredComs, Comments).

get_predicate_comments([], P, O) ?- !, P = [], O = [].
get_predicate_comments([comment(T,C)|Cs], P, O) ?-
        (T = _F/_A ->
	   P = [comment(T,C)|P0], O = O0
	;  O = [comment(T,C)|O0], P = P0
	),
	get_predicate_comments(Cs, P0, O0).

merge_predcomments([], MCs) :- !, MCs = [].
merge_predcomments([comment(F/A,I0),comment(F/A,I1)|Cs], MC) ?- !,
        append(I0, I1, I),
	merge_predcomments([comment(F/A,I)|Cs], MC).
merge_predcomments([C|Cs], MC) :-
	MC = [C|MC0],
	merge_predcomments(Cs, MC0).


gen_interface_file(Out, Comments, Module) :-
	(nonvar(Module) ->
	    get_module_info(Module, raw_interface, Decls),
	    printf(Out, ":- module(%Qw).%n", Module),
	    (foreach(Dec, Decls), param(Out) do
	         printf(Out, ":- %QDVw.%n", Dec)
	    ),
	    (foreach(Com, Comments), param(Out) do
                 printf(Out, ":- %QDVw.%n", Com)
	    ),
	    % now create 'hidden' comments for all exported tool bodies
	    findall(ToolBody, (
		    current_module_predicate(exported, Pred)@Module,
		    get_flag(Pred, tool, on)@Module,
		    tool_body(Pred, ToolBody, _)@Module
		),
		ToolBodies),
	    (foreach(Dec, Decls), param(Out,ToolBodies) do
	    	( Dec = (export N/A), memberchk(N/A, ToolBodies) ->
		    printf(Out, ":- comment(%Qw, hidden).%n", N/A)
		;
		    true
		)
	    )
	;   true
        ).

gather_comments(In, BaseName, Comments0, FoundM0, FoundM, Module0, Module) :-
	read(In, Item)@Module0,
	process_item(Item, In, BaseName, Comments0, Comments1, FoundM0, FoundM1,
	   Module0, Module1),
	((FoundM1 == after ; Item == end_of_file) ->
	    FoundM1 = FoundM,
	    Module1 = Module
	;    
	    gather_comments(In, BaseName, Comments1, FoundM1, FoundM,
	        Module1, Module)
        ).


process_item(end_of_file, _In, _, Comments0, Comments, FoundM0, FoundM, M0, M) ?- !,
        Comments0 = [],
	Comments = Comments0,
	FoundM0 = FoundM,
	M0 = M.
process_item(:-module(M), _In, BaseName, Comments0, Comments, FoundM0, FoundM, _Module0,  Module) ?- !,
        Comments0 = Comments,
	set_module(M, BaseName, Module, FoundM0, FoundM).
process_item(:-module(M,_), _In, BaseName, Comments0, Comments, FoundM0, FoundM, _Module0, Module) ?- !,
        Comments0 = Comments,
	set_module(M, BaseName, Module, FoundM0, FoundM).
process_item(:-module(M,_,_), _In, BaseName, Comments0, Comments, FoundM0,
    FoundM, _Module0, Module) ?- !,
	Comments0 = Comments,
	set_module(M, BaseName, Module, FoundM0, FoundM).
process_item(:-module_interface(M), _In, BaseName, Comments0, Comments,
    FoundM0, FoundM, _Module0, Module) ?- !, 
        Comments0 = Comments,
	set_module(M, BaseName, Module, FoundM0, FoundM).
process_item(:-begin_module(M), _In, _BaseName, Comments0, Comments, FoundM0,
    FoundM, Module0, Module) ?- !,
        Comments0 = Comments,
	set_beginmodule(M, Module0, Module, FoundM0, FoundM).
process_item(:-Directive, In, BaseName, Comments0, Comments, FoundM0,
    FoundM, Module0, Module) ?- !,
	process_directives(Directive, In, BaseName, Comments0, Comments,
            FoundM0, FoundM, Module0, Module).
process_item(_Item, _In, _BaseName, Comments, Comments, FoundM, FoundM,
    Module, Module).


process_directives((D1,D2), In, BaseName, Comments0, Comments, FoundM0,
    FoundM, Module0, Module) ?- !,
	process_directives(D1, In, BaseName, Comments0, Comments1, FoundM0,
             FoundM1, Module0, Module1),
	process_directives(D2, In, BaseName, Comments1, Comments, FoundM1,
             FoundM, Module1, Module).
process_directives(comment(include,Files0), _In, BaseName, Comments0,
    Comments, FoundM0, FoundM, Module0, Module) ?- !,
	(Files0 = [_|_] -> Files = Files0 ; Files = [Files0]),
	(foreach(F, Files), fromto(Comments0, C1,C2, Comments), 
         fromto(Module0, M1,M2, Module), fromto(FoundM0, F1,F2, FoundM),
         param([BaseName]) do
	    (get_inout_names(F, ".", InDir, InName, _) ->
		process_include(InName, InDir, BaseName, C1, C2, F1, F2, M1, M2)
	    ;   get_bip_error(Er),
	        error(Er, comment(include,F))
	    )
	).
process_directives(comment(T,C), _In, _BaseName, Comments0, Comments,
    FoundM0, FoundM, Module0, Module) ?- !,
        FoundM0 = FoundM,
	Module0 = Module,
	(FoundM0 = yes(_) ->
	    Comments0 = [comment(T,C)|Comments]
	;   % not the main module yet
	    writeln(warning_output, "Comments not in main module ignored."),
	    Comments0 = Comments
	).
process_directives(include(Files0), _In, BaseName, Comments0, Comments,
    FoundM0, FoundM, Module0, Module) ?- !,
	(Files0 = [_|_] -> Files = Files0 ; Files = [Files0]),
	(foreach(F, Files), fromto(Comments0, C1,C2, Comments), 
         fromto(Module0, M1,M2, Module), fromto(FoundM0, F1,F2, FoundM),
         param([BaseName]) do
	    (get_inout_names(F, ".", InDir, InName, _) ->
		process_include(InName, InDir, BaseName, C1, C2, F1, F2, M1, M2)
	    ;   get_bip_error(E),
	        error(E, include(F))
	    )
	).
process_directives(_Directive, _, _BaseName, Comments, Comments, Found,
    Found, Module, Module).


% set the current module if it is not already set
set_module(Module, BaseName, NewCurrent, FoundM0, FoundM) :-
	(nonvar(Module) -> 
	% this check that Module and BaseName are the same name, regardless
        % of their types
	  (FoundM0 == no ->
	      ((concat_string([Module], StringName),
	        concat_string([BaseName], StringName)) ->
		      Module = NewCurrent,
		      FoundM = yes(Module)
	       ;      printf(warning_output, "Encountering a module %w different from filename, ignoring...%n", [Module]),
	              Module = NewCurrent,
		      FoundM = no
	      )
          ;    printf(warning_output, "Encountering a module %w after main module, ignoring...%n", [Module]),
	       arg(1, FoundM0, Main),
	       FoundM = after(Main),
	       Module = NewCurrent
	  )
        ; % var(Module)
	  writeln(warning_output, "Unable to determine new module name..."),
	  set_bip_error(6)
	).

set_beginmodule(Module, OldCurrent, NewCurrent, FoundM0, FoundM) :-
	(nonvar(Module) ->
	    (FoundM0 == no ->
	        NewCurrent = Module,
		FoundM = yes(Module)
	    ;   
	        (OldCurrent == Module ->
		    NewCurrent = OldCurrent,
		    FoundM = yes(OldCurrent)
		;
                    printf(warning_output, "Encountering a module %w after main module, ignoring...%n", [Module]),
		    arg(1, FoundM0, Main),
	            FoundM = after(Main),
	            Module = NewCurrent
		)
	  )
        ; % var(Module)
	  writeln(warning_output, "Unable to determine new module name..."),
	  set_bip_error(6)
	).


% get the comments from an included file
process_include(InFile, InDir, BaseName, Comments0, Comments, FoundM0,
   FoundM, Module0, Module) :-
	printf(output, "including %w%n", [InFile]),
	check_open(read, InFile, In),
	getcwd(CWD), cd(InDir),
	(process_include1(In, BaseName, Comments0, Comments, FoundM0, FoundM, Module0, Module) ->
	    close(In), cd(CWD) ; close(In), cd(CWD), fail
	).

process_include1(In, BaseName, Comments0, Comments, FoundM0, FoundM, Module0, Module) :-
	gather_comments(In, BaseName, IncComments, FoundM0, FoundM,
	   Module0, Module),
	append(IncComments, Comments, Comments0).




% only compile file if needed. File is a string
ensure_compiled(File, _M) :-
	get_file_info(File, mtime, MTime),
	(atom(File) -> AFile = File ; atom_string(AFile, File)), 
        % current_compiled_file expects atoms
	current_compiled_file(AFile, CTime, _),
	MTime =< CTime, !.  % no need to compile it
ensure_compiled(File, M) :-
	compile(File)@M.


atom_or_string(Term) :- atom(Term), !.
atom_or_string(Term) :- string(Term).



check_open(read, FileName, Stream) ?-
	((get_file_info(FileName, readable, on),
	 get_file_info(FileName, mode, Mode),
	 8'040000 =\= Mode /\ 8'170000 /* not directory*/) ->
             open(FileName, read, Stream)
	 ;
	     set_bip_error(170)
	).
check_open(write, FileName, Stream) ?-
	(exists(FileName) ->
	    % file exists, check can overwrite it
	    ((get_file_info(FileName, writable, on), 
	     get_file_info(FileName, mode, Mode),
	     8'040000 =\= Mode /\ 8'170000 /*not directory*/) ->
	         open(FileName, write, Stream)
	     ;
	         set_bip_error(170)
	    )
        ;
            % file does not exist, check can write in directory
	    canonical_path_name(FileName, FullName),
	    pathname(FullName, ParentDir, _, _),
	    (get_file_info(ParentDir, writable, on) ->
		open(FileName, write, Stream)
	    ;  set_bip_error(170)
	    )
	 ).



try_extension(Base, FullName) :-
	get_flag(prolog_suffix, PExts),
	existing_file(Base, PExts, [readable], FullName), !.


get_inout_names(File0, OutDir, InDir, IFile, IntFile) :-
        (File0 = library(FName) -> true ; FName = File0),
	atom_or_string(FName),
	pathname(FName, _, Base, Suffix),
	(Suffix == "" -> 
	    % need to append extension
	    try_extension(File0, IFile0)
	;   existing_file(File0, [""], [readable], IFile0), !
	), 
	canonical_path_name(IFile0, IFile), % full path for input file
	pathname(IFile, InDir, _, _),  
	get_flag(eclipse_info_suffix, INT),
	canonical_path_name(OutDir, AbsOutDir),
	get_file_info(AbsOutDir, mode, DirMode),
	8'040000 =:= DirMode /\ 8'170000, !, % is a directory...
	concat_string([AbsOutDir, Base, INT], IntFile).
get_inout_names(_, _, _, _, _) :-
	set_bip_error(171).



%----------------------------------------------------------------------
% Generate html documentation tree from .eci files
%----------------------------------------------------------------------

:- export
	ecis_to_htmls/0,
	ecis_to_htmls/3,
	ecis_to_htmls/4.

% The structure in which we collect the information from a
% predicate or structure comment.
%
% 'text' means one of:
%	string, html(string), ascii(string), ascii_fmt(string)
%

:- local struct(pdoc(
	index,		% term
	template,	% string
	modes,		% [term]
	summary,	% text
	args,		% [stratom:text]
	fields,		% [stratom:text]
	desc,		% text
	chp,		% text
	fail,		% text
	exc,		% [int:text]
	eg,		% string
	see		% term
    )).


% Dummy definition to avoid warnings (will be replaced via compile_term)
bip(_Name, _Arity, _Group, _Library, _File).

:- comment(ecis_to_htmls/0, [
    summary:"Generate the HTML documentation tree for all ECLiPSe libraries",
    desc:html("Generate the HTML documentation tree for all ECLiPSe libraries.
    It takes into account all .eci files in the library path and generates
    an HTML documentation tree in <ECLiPSe installation directory>/doc/bips.
    "),
    see_also:[ecis_to_htmls/4]
    ]).

ecis_to_htmls :-
	get_flag(library_path, LibDirs),
	get_flag(installation_directory, EclipseDir),
	concat_string([EclipseDir,/,doc,/,bips], HtmlTopDir),
	concat_string([EclipseDir,/,doc,/,bips,/,kernel], KernelDocs),
	AllDirs = [KernelDocs|LibDirs],
	ecis_to_htmls(AllDirs, HtmlTopDir,
	    "<A HREF=\"../index.html\">All ECLiPSe Documentation</A>",
	    "ECLiPSe").


% ecis_to_htmls(+LibDir, +HtmlDir, +Header, +SystemName)
% generate documentation for the libraries in LibDir in HtmlDir

:- comment(ecis_to_htmls/3, [
    summary:"Generate an HTML documentation tree for the given library directories",
    args:["LibDirs":"A list of directory path names",
    	"HtmlTopDir":"A directory path name",
    	"LinkBack":"HTML string (may be empty)"],
    amode:ecis_to_htmls(++,++,++),
    desc:html("See ecis_to_htmls/4 for details"),
    see_also:[ecis_to_htmls/4,ecis_to_htmls/0]
    ]).

ecis_to_htmls(LibDirs, HtmlTopDir, LinkBack) :-
	ecis_to_htmls(LibDirs, HtmlTopDir, LinkBack, "All Modules").

:- comment(ecis_to_htmls/4, [
    summary:"Generate an HTML documentation tree for the given library directories",
    args:["LibDirs":"A directory path name or a list of them",
    	"HtmlTopDir":"A directory path name",
    	"LinkBack":"HTML string (may be empty)",
    	"SystemName":"A string (may be empty)"],
    amode:ecis_to_htmls(++,++,++,++),
    see_also:[ecis_to_htmls/0,eci_to_html/3],
    desc:html("This predicate finds all ECLiPSe interface information (.eci)
	files in a list of directories and generates .html files in the
	directory HtmlTopDir (which is created if it does not exist).
	The generated files are:
	<UL>
	<LI>a toplevel index of all the processed directories
		(only if LibDirs has more than one element)
	<LI>for each directory an index of all the processed .eci files
	<LI>the documentation generated from each .eci file
	</UL>
	The LinkBack argument should contain a hyperlink in HTML format
	which will appear at the top of the toplevel index.  The
	SystemName argument will appear in several places and should
	be the name of the documented software system.")
    ]).

ecis_to_htmls(LibDirs0, HtmlTopDir, LinkBack, SystemName) :-
	( LibDirs0 = [_|_] -> LibDirs = LibDirs0 ; LibDirs = [LibDirs0] ),
	makedir(HtmlTopDir),
	writeln(log_output, "collecting indexing information..."),
	(
	    foreach(LibDir,LibDirs),			% Pass 1
	    fromto(Groups,Groups1,Groups0,[]),
	    fromto(GroupedFiles,GroupedFiles1,GroupedFiles0,[]),
	    fromto(IndexList,IndexList3,IndexList0,[])	% and index info
	do
	    % group name is the library directory name (may have trailing / !)
	    pathname(LibDir, _, Group0, _),
	    split_string(Group0, "", "/", [Group]),
	    find_tree(LibDir, "*.eci", EciFiles),
	    ( EciFiles = [] ->
		IndexList3 = IndexList0,
		Groups1 = Groups0,
		GroupedFiles1 = GroupedFiles0
	    ;
		(
		    foreach(EciFile,EciFiles),
		    fromto(IndexList3,IndexList2,IndexList1,IndexList0),
		    param(Group)
		do
		    eci_to_index(EciFile, Group, IndexList2, IndexList1)
		),
		Groups1 = [Group|Groups0],
		GroupedFiles1 = [EciFiles|GroupedFiles0]
	    )
	),
	% now compile bip/5 (needed to generate cross-references)
	sort(2, =<, IndexList, SortedIndexList0),
	alphasort(1, =<, SortedIndexList0, SortedIndexList1),
	remove_subsumed(SortedIndexList1, SortedIndexList2),
	( Groups = [_] ->
	    remove_group_name(SortedIndexList2, SortedIndexList)
	;
	    SortedIndexList = SortedIndexList2
	),
	fillin_empty_files(SortedIndexList),
	gen_prolog_index(HtmlTopDir, SortedIndexList),
	compile_term(SortedIndexList),
	( Groups = [_] ->
	    GroupSummary = SystemName,
	    GroupedFiles = [EciFiles],
	    concat_string(["<A HREF=\"../index.html\">",GroupSummary,
		"</A> | <A HREF=\"../fullindex.html\">Alphabetic Index</A>"
		], Header),
	    GroupDir = HtmlTopDir,
	    (
		foreach(EciFile,EciFiles),			% Pass 2
		foreach(LibDesc,Libs),
		param(GroupDir,Header)
	    do
		writeln(log_output, processing:EciFile),
		eci_to_html(EciFile, GroupDir, Header, LibDesc)
	    ),
	    concat_string(["[ ",LinkBack," | <A HREF=\"fullindex.html\">Alphabetic Index</A> ]"], SubHeader),
	    gen_library_index(GroupDir, GroupSummary, Libs, SubHeader),
	    gen_full_index(HtmlTopDir, LinkBack, SystemName, nogroups)
	;
	    (
		foreach(Group,Groups),			% Pass 2
		foreach(EciFiles,GroupedFiles),
		param(HtmlTopDir,SystemName)
	    do
		group_summary(SystemName, Group,GroupSummary),
		concat_string(["<A HREF=\"../index.html\">",GroupSummary,
		    "</A> | <A HREF=\"../../index.html\">Reference Manual</A> | <A HREF=\"../../fullindex.html\">Alphabetic Index</A>"
		    ], Header),
		concat_string([HtmlTopDir,/,Group], GroupDir),
		makedir(GroupDir),
		(
		    foreach(EciFile,EciFiles),			% Pass 2
		    foreach(LibDesc,Libs),
		    param(GroupDir,Header)
		do
		    writeln(log_output, processing:EciFile),
		    eci_to_html(EciFile, GroupDir, Header, LibDesc)
		),
		SubHeader = "[ <A HREF=\"../index.html\">Reference Manual</A> | <A HREF=\"../fullindex.html\">Alphabetic Index</A> ]",
		gen_library_index(GroupDir, GroupSummary, Libs, SubHeader)
	    ),
	    gen_group_index(HtmlTopDir, Groups, LinkBack, SystemName),
	    gen_full_index(HtmlTopDir, LinkBack, SystemName, groups)
	).

    remove_subsumed([], []).
    remove_subsumed([bip(N,A,G,L,F),bip(N,A,G,L,F)|Bips], UBips) :- !,
	remove_subsumed([bip(N,A,G,L,F)|Bips], UBips).
    remove_subsumed([Bip|Bips], [Bip|UBips]) :-
	remove_subsumed(Bips, UBips).

    fillin_empty_files([]).
    fillin_empty_files([Bip|Bips]) :-
    	( Bip = bip(_,_,_,_,'') -> true ; true ),
	fillin_empty_files(Bips).

    % replace group name with '.' to indicate that there is no
    % group-level in the generated directory structure
    remove_group_name([], []).
    remove_group_name([bip(N,A,_G,L,F)|Bips], [bip(N,A,'.',L,F)|RBips]) :-
	remove_group_name(Bips, RBips).


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Pass 1 - collect predicate index information
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

eci_to_index(EciFile, Group, IndexList, IndexList0) :-
	pathname(EciFile, _, LibNameString, _),
	atom_string(LibName, LibNameString),
	open(EciFile, read, EciStream),
	( eci_stream_to_index(EciStream, Group, LibName, IndexList, IndexList0) ->
	    true
	;
	    IndexList=IndexList0,
	    printf(error, "Error while processing %w for index%n", [EciFile])
	),
	close(EciStream).

eci_stream_to_index(EciStream, Group, LibName, IndexList, IndexList0) :-
	atom_string(GroupA, Group),
	% make an index entry for the library as a whole
	add_index_entries([LibName],GroupA,LibName,_NoFile,IndexList,IndexList4),
	read(EciStream, FirstDirective),
	(
	    fromto(FirstDirective,Directive,NextDirective,end_of_file),
	    fromto(IndexList4,IndexList2,IndexList1,IndexList5),
	    fromto(Exports,Exports1,Exports0,[]),
	    fromto(Hidden,Hidden1,Hidden0,[]),
	    param(EciStream,GroupA,LibName)
	do
	    ( Directive = (:- export N/A) ->
		IndexList2 = IndexList1,
		Hidden1 = Hidden0,
		Exports1 = [N/A|Exports0]
	    ; Directive = (:- export struct(Struct)) ->
		Hidden1 = Hidden0,
		Exports1 = Exports0,
		functor(Struct, N, _),
		IndexList2 = [bip(N,struct,GroupA,LibName,_NoFile)|IndexList1]
	    ; Directive = (:- global N/A) ->
		Hidden1 = Hidden0,
		Exports1 = Exports0,
		IndexList2 = [bip(N,A,GroupA,LibName,_NoFile)|IndexList1]
	    ;  Directive = (:- comment(N/A, Comment)) ->
		Exports1 = Exports0,
		( Comment == hidden ->
		    Hidden1 = [N/A|Hidden0],
		    IndexList2 = IndexList1
		;
		    Hidden1 = Hidden0,
		    object_spec_to_filename(N/A, PredFileName),
		    atom_string(PredFileNameA, PredFileName),
		    IndexList2 = [bip(N,A,GroupA,LibName,PredFileNameA)|IndexList3],
		    ( memberchk(index:ExtraIndex,Comment) ->
			add_index_entries(ExtraIndex,GroupA,LibName,PredFileNameA,IndexList3,IndexList1)
		    ;
			IndexList3=IndexList1
		    )
		)
	    ;  Directive = (:- comment(struct(N), _)) ->
		Hidden1 = Hidden0,
		Exports1 = Exports0,
		object_spec_to_filename(struct(N), FileName),
		IndexList2 = [bip(N,struct,GroupA,LibName,FileName)|IndexList1]
	    ;  Directive = (:- comment(index, ExtraIndex)) ->
		Hidden1 = Hidden0,
		Exports1 = Exports0,
		add_index_entries(ExtraIndex,GroupA,LibName,_NoFile,IndexList2,IndexList1)
	    ;
		Hidden1 = Hidden0,
		Exports1 = Exports0,
	    	IndexList2 = IndexList1
	    ),
	    read(EciStream, NextDirective)
	),
	% add index entries for the exports, unless they are hidden
	(
	    foreach(N/A,Exports),
	    fromto(IndexList5,IndexList6,IndexList7,IndexList0),
	    param(Hidden,GroupA,LibName)
	do
	    ( memberchk(N/A, Hidden) ->
		IndexList6 = IndexList7
	    ;
		IndexList6 = [bip(N,A,GroupA,LibName,_NoFile)|IndexList7]
	    )
	).

    add_index_entries([],_Group,_LibName,_PredFileName,IL,IL).
    add_index_entries([Word|Words],G,L,P, IL, IL0) :-
	( atomic(Word) ->
	    concat_atom([Word],WordA),
	    IL = [bip(WordA,index,G,L,P)|IL1]
	; Word = N/A ->
	    IL = [bip(N,A,G,L,P)|IL1]
	; Word = M:N/A ->
	    IL = [bip(N,A,G,M,P)|IL1]
	;
	    printf(error, "Invalid index entry: %w%n", [Word]),
	    IL = IL1
	),
	add_index_entries(Words,G,L,P,IL1,IL0).

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Pass 2 - generate the html files
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

% eci_to_html(+EciFile, +HtmlTopDir, +LinkBack, -LibDesc)
% create documentation for one library from one .eci file

:- export eci_to_html/3.
:- comment(eci_to_html/3, [
    summary:"Create HTML documentation from one .eci file",
    args:["EciFile":"String","HtmlTopDir":"String","LinkBack":"HTML string (may be empty)"],
    amode:eci_to_html(++,++,++),
    see_also:[ecis_to_htmls/3,ecis_to_htmls/0,comment/2],
    desc:html("
        This predicate processes one ECLiPSe interface information file
	EciFile (usually with .eci Extension) and generates .html
	files. The .html files are placed in a subdirectory in HtmlTopDir
	with the same name as EciFile but without the extension. This
	directory is created if it does not exist. The generated files are
	index.html, containing a summary description of the library, plus
	one file for each predicate that was commented using a comment/2
	directive in the source. The LinkBack argument should contain a
	hyperlink in HTML format. This will appear at the top of the
	generated pages and should usually point back to a parent page.")
    ]).

eci_to_html(FullEciFile, HtmlTopDir, Header) :-
	eci_to_html(FullEciFile, HtmlTopDir, Header, _).

eci_to_html(EciFile0, HtmlTopDir, Header, lib(LibName,LibSumm,LibTitle)) :-
	existing_file(EciFile0, [".eci",""], [readable], FullEciFile),
	!,
	pathname(FullEciFile, _, LibNameString, EciSuffix),
	concat_strings(LibNameString, EciSuffix, EciFile),
	atom_string(LibName, LibNameString),
	concat_string([HtmlTopDir,/,LibName],HtmlDir),
	makedir(HtmlDir),
	open(FullEciFile, read, EciStream),
	( eci_stream_to_html(EciStream, HtmlDir, LibName, LibTitle, LibPreds, LibStructs, OtherComments, OtherExports, ReExports, Header) ->
	    close(EciStream),
	    ( gen_library_page_html(HtmlDir, LibName, LibTitle, LibPreds, LibStructs, OtherComments, OtherExports, ReExports, Header, EciFile) -> true
	    ; printf(error, "Error in generating toplevel html page for %w%n", [LibTitle]) ),
	    ( memberchk(comment(summary,LibSumm), OtherComments) -> true
	    ; LibSumm="No description available" )
	;
	    close(EciStream),
	    printf(error, "Error while processing %w%n", [FullEciFile])
	).
eci_to_html(EciFile0, _, _, _) :-
	printf(error, "File not found: %w%n", [EciFile0]).

gen_library_page_html(HtmlDir, LibName, LibTitle, LibPreds, LibStructs, OtherComments, OtherExports, ReExports, Header, EciFile) :-
	concat_string([HtmlDir,/,"index.html"], IndexFile),
	open(IndexFile, write, Stream),
	chmod644(IndexFile),
%	writeln(log_output, making:IndexFile),
	printf(Stream, "<HTML><HEAD><TITLE>%w</TITLE></HEAD><BODY>%n", [LibTitle]),
	printf(Stream, "[ %w ]", [Header]),
	printf(Stream, "<H1>%w</H1>%n", [LibTitle]),
	( memberchk(comment(summary,LibSumm), OtherComments) ->
	    writeln(Stream, LibSumm),
            gen_library_ascii_file(summary, HtmlDir, LibName, LibSumm)
	;
	    true
	),

	html_write_section(Stream, "Predicates", LibPreds),
	html_write_section(Stream, "Structures", LibStructs),

	( ReExports = [] -> true ;
	    writeln(Stream, "<H2>Reexports</H2>"),
	    writeln(Stream, "<BLOCKQUOTE><DL>"),
	    ( foreach(ReExport,ReExports), param(Stream,LibName) do
		html_write_reexport(Stream, ReExport, LibName)
	    ),
	    writeln(Stream, "</DL></BLOCKQUOTE>")
	),
	( OtherExports = [] -> true ;
	    writeln(Stream, "<H2>Other Exports</H2>"),
	    writeln(Stream, "<BLOCKQUOTE><DL>"),
	    ( foreach(ExportItem,OtherExports), param(Stream) do
		printf(Stream, "<DT><STRONG>%w</STRONG></DT><DD></DD>%n", [ExportItem])
	    ),
	    writeln(Stream, "</DL></BLOCKQUOTE>")
	),
	( memberchk(comment(desc,Desc), OtherComments) ->
	    writeln(Stream, "<H2>Description</H2>"),
	    html_write_par(Stream, Desc),
            gen_library_ascii_file(desc, HtmlDir, LibName, Desc)

	; true ),
	( memberchk(comment(eg,Examples), OtherComments) ->
	    writeln(Stream, "<H2>Examples</H2>"),
	    ( string(Examples) -> html_write_par(Stream, ascii(Examples))
	    ; html_write_par(Stream, Examples) )
	; true ),
	( memberchk(comment(status,Status), OtherComments) ->
	    writeln_lazily(Stream, "<H2>About</H2><UL COMPACT>", Printed),
	    printf(Stream, "<LI><STRONG>Status: </STRONG>", []),
	    html_write_par(Stream, Status)
	; true ),
	( memberchk(comment(author,Auth), OtherComments) ->
	    writeln_lazily(Stream, "<H2>About</H2><UL COMPACT>", Printed),
	    printf(Stream, "<LI><STRONG>Author: </STRONG>", []),
	    html_write_par(Stream, Auth)
	; true ),
	( memberchk(comment(copyright,Copy), OtherComments) ->
	    writeln_lazily(Stream, "<H2>About</H2><UL COMPACT>", Printed),
	    printf(Stream, "<LI><STRONG>Copyright &copy; </STRONG>", []),
	    html_write_par(Stream, Copy)
	; true ),
	( memberchk(comment(date,Date), OtherComments) ->
	    writeln_lazily(Stream, "<H2>About</H2><UL COMPACT>", Printed),
	    printf(Stream, "<LI><STRONG>Date: </STRONG>", []),
	    html_write_par(Stream, Date)
	; true ),
	writeln_if(Stream, "</UL>", Printed),
	( memberchk(comment(see_also,SeeAlso), OtherComments) ->
	    printf(Stream, "<H2>See Also</H2>%n", []),
	    write_references(Stream, LibName, SeeAlso)
	; true),
	date(GenDate),
	printf(Stream, "<HR>Generated from %w on %w%n", [EciFile,GenDate]),
	writeln(Stream, "</BODY></HTML>"),
	close(Stream).

    html_write_section(_Stream, _SectionName, []) :- !.
    html_write_section(Stream, SectionName, LibPreds) :-
	printf(Stream, "<H2>%w</H2>%n", [SectionName]),
	writeln(Stream, "<BLOCKQUOTE>"),
	writeln(Stream, "<DL>"),
	sort(1, =<, LibPreds, SortedLibPreds),
	(
	    foreach(pred(_NA,Tmpl0,Summ,File),SortedLibPreds),
	    param(Stream)
	do
	    ( Tmpl0 = [_|_] -> join_string(Tmpl0, ", ", Tmpl) ; Tmpl = Tmpl0 ),
	    ( var(File) ->
		printf(Stream, "<DT><STRONG>%w</STRONG></DT>%n<DD>%w</DD>%n",
		    [Tmpl,Summ])
	    ;
		printf(Stream,
		    "<DT><A HREF=\"%w\"><STRONG>%w</STRONG></A></DT>%n<DD>%w</DD>%n",
		    [File,Tmpl,Summ])
	    )
	    
	),
	writeln(Stream, "</DL>"),
	writeln(Stream, "</BLOCKQUOTE>").

    gen_library_ascii_file(Type, Dir, LibName, Info) :-
        concat_string([Dir,/,LibName,"_", Type, ".txt"], AsciiFile),
        open(AsciiFile, write, S),
        chmod644(AsciiFile),
        concat_string(["lib(", LibName, ")"], LibString),
        ascii_write_par(S, LibString, 0),
        nl(S),
        ascii_write_par(S, Info, 3),
        nl(S),
        close(S).


    html_write_reexport(Stream, (reexport P1,P2 from Module), Lib) ?- !,
	html_write_reexport(Stream, (reexport P1 from Module), Lib),
	html_write_reexport(Stream, (reexport P2 from Module), Lib).
    html_write_reexport(Stream, (reexport N/A from Module), Lib) ?- !,
	( bip(N, A, _, Lib, _) ->
	    % suppress if already listed in parent module
	    true
	;
	    write(Stream, "<DT><STRONG>reexport "),
	    ( bip(N, A, System, Module, File), File \= '' ->
		write(Stream, "<A HREF=\""),
		html_write_module_prefix(Stream, System, Module),
		printf(Stream, "%w.html\">%w</A>", [File, N/A])
	    ;
		write(Stream, N/A)
	    ),
	    write(Stream, " from "),
	    html_write_module(Stream, Module),
	    writeln(Stream, "</STRONG></DT><DD></DD>")
	).
    html_write_reexport(Stream, (reexport Other from Module), _Lib) ?- !,
        printf(Stream, "<DT><STRONG>reexport %_w from ", [Other]),
	html_write_module(Stream, Module),
	writeln(Stream, "</STRONG></DT><DD></DD>").
    html_write_reexport(Stream, (reexport Module except Stuff), _Lib) ?- !,
	write(Stream, "<DT><STRONG>reexport "),
	html_write_module(Stream, Module),
	printf(Stream, "</STRONG></DT><DD>except %99_w</DD>%n", [Stuff]).
    html_write_reexport(Stream, (reexport Module), _Lib) ?-
	write(Stream, "<DT><STRONG>reexport "),
	html_write_module(Stream, Module),
	writeln(Stream, "</STRONG></DT><DD></DD>").

    html_write_module_prefix(Stream, '.', Module) :- !,
	printf(Stream, "../%w/", [Module]).
    html_write_module_prefix(Stream, System, Module) :-
	printf(Stream, "../../%w/%w/", [System,Module]).

    html_write_module(Stream, Module) :-
	( bip(_, _, System, Module, _) ->
	    write(Stream, "<A HREF=\""),
	    html_write_module_prefix(Stream, System, Module),
	    printf(Stream, "index.html\">%w</A>", [Module])
	;
	    write(Stream, Module)
	).


% Reads the .eci file which is open on EciStream
% Generates html predicate description pages in HtmlDir and returns:
% LibName - the library name
% LibPreds - a list of pred(N/A,Template,Summary,File) for exported/documented preds
% LibStructs - a list of pred(struct2(Name,Def),Template,Summary,File) for exported/documented structs
% OtherComments - a list of comments other than pred/struct-related
% OtherExports - a list of exports other than pred/struct-related
% ReExports - a list of reexports
% LibTitle - either library(Libname) or the value of a comment(alias, ...)
% fails on error

eci_stream_to_html(EciStream, HtmlDir, LibName, LibTitle, LibPreds, LibStructs, OtherComments, OtherExports, ReExports, Header) :-
	read(EciStream, FirstDirective),
	(
	    fromto(FirstDirective,Directive,NextDirective,end_of_file),
	    fromto(Exports,Exports1,Exports0,[]),
	    fromto(ReExports,ReExports1,ReExports0,[]),
	    fromto(LibPreds2,LibPreds1,LibPreds0,[]),
	    fromto(LibStructs2,LibStructs1,LibStructs0,[]),
	    fromto(Hidden,Hidden1,Hidden0,[]),
	    fromto(OtherComments,Other1,Other0,[]),
	    fromto(library(LibName),LibTitle1,LibTitle2,LibTitle),
	    param(EciStream,HtmlDir,LibName,Header)
	do
	    (  Directive = (:- comment(N/A, Comment)) ->
		Exports1 = Exports0,
		ReExports1 = ReExports0,
		Other1 = Other0,
		LibStructs1 = LibStructs0,
		LibTitle2 = LibTitle1,
		( Comment == hidden ->
		    LibPreds1 = LibPreds0,
		    Hidden1 = [N/A|Hidden0]
		;
		    Hidden1 = Hidden0,
		    ( comment_to_page(N/A, Comment, Page) ->
			LibPreds1 = [pred(N/A,Tmpl,PredSumm,HtmlFile)|LibPreds0],
			Page = pdoc with [template:Tmpl,summary:PredSumm],
			gen_html_file(Header, HtmlDir, LibTitle2, LibName, Page, HtmlFile),
			gen_ascii_file(HtmlDir, LibTitle2, LibName, Page)
		    ;
			LibPreds1 = LibPreds0,
			printf(error, "Error in analysing predicate comment for %w%n", [N/A])
		    )
		)

	    ;  Directive = (:- comment(struct(Name),Comment)) ->
		Exports1 = Exports0,
		ReExports1 = ReExports0,
		LibPreds1 = LibPreds0,
		LibTitle2 = LibTitle1,
		Hidden1 = Hidden0,
		Other1 = Other0,
		( comment_to_page(struct2(Name,Def), Comment, Page) ->
		    LibStructs1 = [pred(struct2(Name,Def),Tmpl,Summary,HtmlFile)|LibStructs0],
		    Page = pdoc with [template:Tmpl,summary:Summary],
		    gen_html_file(Header, HtmlDir, LibTitle2, LibName, Page, HtmlFile),
		    gen_ascii_file(HtmlDir, LibTitle2, LibName, Page)
		;
		    LibStructs1 = LibStructs0,
		    printf(error, "Error in analysing structure comment %w%n", [struct(Name)])
		)

	    ;  Directive = (:- comment(What,Comm)) ->
		Exports1 = Exports0,
		ReExports1 = ReExports0,
		LibPreds1 = LibPreds0,
		LibStructs1 = LibStructs0,
		Hidden1 = Hidden0,
		( What == alias -> LibTitle2 = Comm ; LibTitle2 = LibTitle1 ),
		Other1 = [comment(What,Comm)|Other0]

	    ;  Directive = (:- export Something) ->
		Exports1 = [Something|Exports0],
		ReExports1 = ReExports0,
		LibPreds1 = LibPreds0,
		LibStructs1 = LibStructs0,
		LibTitle2 = LibTitle1,
		Hidden1 = Hidden0,
		Other1 = Other0

	    ;  Directive = (:- global(Something)) ->	% obsolete
		Exports1 = [Something|Exports0],
		ReExports1 = ReExports0,
		LibPreds1 = LibPreds0,
		LibStructs1 = LibStructs0,
		LibTitle2 = LibTitle1,
		Hidden1 = Hidden0,
		Other1 = Other0

	    ;  Directive = (:- reexport(Something)) ->
		Exports1 = Exports0,
		ReExports1 = [reexport(Something)|ReExports0],
		LibPreds1 = LibPreds0,
		LibStructs1 = LibStructs0,
		LibTitle2 = LibTitle1,
		Hidden1 = Hidden0,
		Other1 = Other0
	    ;
		Exports1 = Exports0,
		ReExports1 = ReExports0,
		LibPreds1 = LibPreds0,
		LibStructs1 = LibStructs0,
		LibTitle2 = LibTitle1,
		Hidden1 = Hidden0,
		Other1 = Other0
	    ),
	    read(EciStream, NextDirective)
	),

	% add pseudo entries for the uncommented exported predicates/structures
	% (if not hidden) and return the remaining exports as OtherExports
	(
	    foreach(Export,Exports),
	    fromto(LibPreds,LibPreds4,LibPreds3,LibPreds2),
	    fromto(LibStructs,LibStructs4,LibStructs3,LibStructs2),
	    fromto(OtherExports,OtherExports1,OtherExports0,[]),
	    param(LibPreds2,LibStructs2,Hidden)
	do
	    ( Export = _/_ ->
		( memberchk(pred(Export,_Tmpl,_Summ,_File), LibPreds2) ->
		    LibPreds4=LibPreds3
		; memberchk(Export, Hidden) ->
		    LibPreds4=LibPreds3
		;
		    default_template(Export, Tmpl),
		    LibPreds4=[pred(Export,Tmpl,"No description available",_)|LibPreds3]
		),
		LibStructs4 = LibStructs3,
	    	OtherExports1 = OtherExports0
	    ; Export = struct(Struct) ->
		functor(Struct, Name, _),
		( memberchk(pred(struct2(Name,CommentStruct),_Tmpl,_Summ,_File), LibStructs2) ->
		    % check whether declaration and comment match
		    (
			(
			    foreacharg(CommentFieldName,CommentStruct), foreacharg(FieldName,Struct)
			do
			    ( FieldName = CommentFieldName -> true
			    ; FieldName = CommentFieldName:_
			    )
			)
		    ->
			true
		    ;
			printf(warning_output, "WARNING: comment does not match declaration: %w%n", [struct(Name)])
		    ),
		    LibStructs4 = LibStructs3
		;
		    term_string(Struct, StructString),
		    concat_strings("struct ", StructString, ExportTmpl),
		    LibStructs4 = [pred(struct2(Name,Struct),ExportTmpl,"No description available",_)|LibStructs3]
		),
		LibPreds4 = LibPreds3,
		OtherExports1 = OtherExports0
	    ;
	    	OtherExports1 = [(export Export)|OtherExports0],
		LibPreds4 = LibPreds3,
		LibStructs4 = LibStructs3
	    )
	).


%
% Make an embedded html page
%
:- export comment_to_html/2.
comment_to_html(Stream, comment(What,Comment)) :-
	comment_to_html(Stream, What, Comment).

comment_to_html(Stream, desc, Desc) ?- !,
	writeln(Stream, "<H1>Description</H1>"),
	html_write_par(Stream, Desc).
comment_to_html(Stream, eg, Examples) ?- !,
	writeln(Stream, "<H1>Examples</H1>"),
	( string(Examples) -> html_write_par(Stream, ascii(Examples))
	; html_write_par(Stream, Examples) ).
comment_to_html(Stream, Other, Desc) ?-
	atom(Other), is_simple_description(Desc),
	!,
	atom_string(Other, String),
	substring(String, 1, 1, First),
	RestLen is string_length(String)-1,
	substring(String, 2, RestLen, Rest),
	printf(Stream, "<H1>%A%s</H1>", [First,Rest]),
	html_write_par(Stream, Desc).
comment_to_html(Stream, N/A, Comment) ?-
	comment_to_page(N/A, Comment, PageDesc),
	gen_html_body(Stream, '', PageDesc).
comment_to_html(Stream, struct(N), Comment) ?-
	comment_to_page(struct2(N,_Def), Comment, PageDesc),
	gen_html_body(Stream, '', PageDesc).


    is_simple_description(html(_)) ?- true.
    is_simple_description(ascii(_)) ?- true.
    is_simple_description(ascii_fmt(_)) ?- true.
    is_simple_description(Text) :- atom(Text).
    is_simple_description(Text) :- string(Text).


%
% Analyse the comment list and fill the information into the
% corresponging fields in the Page-structure. Do some checks.
%
comment_to_page(CommentedThing, Comment, Page) :-
	Page = pdoc with [index:Index,modes:Modes,exc:Exceptions,
			    fields:Fields,template:Tmpl,summary:Summary],
	% fill the regular fields into the Page structure
	(
	    foreach(Item,Comment),
	    param(Page,CommentedThing)
	do
	    ( nonvar(Item), Item = Key:Value ->
		( page_field(CommentedThing, Key, Value, Page) ->
		    true
		;
		    printf(warning_output, "WARNING: Unrecognised or duplicate keyword in comment for %w: %w%n",
			    [CommentedThing,Key])
		)
	    ;
		printf(warning_output, "WARNING: Unrecognised item in comment for %w:%n%w%n",
			[CommentedThing,Item])
	    )
	),
	% treat some irregular fields
	( CommentedThing = N/A ->
	    Index = CommentedThing,
	    % make template if none given
	    ( var(Tmpl) ->
		construct_pred_template(N, A, Comment, Tmpl)
	    ;
		true
	    ),
	    % treat (multiple) amode fields specially
	    (
		foreach(Item,Comment),
		fromto(Modes,Modes2, Modes1,[]),
		param(N,A)
	    do
	        ( Item = amode:Mode ->
		    ( functor(Mode, N, A) ->
		    	Modes2=[Mode|Modes1]
		    ;
			printf(warning_output, "WARNING: Mismatched amode field in comment for %w%n", [N/A]),
		    	Modes2=Modes1
		    )
		;
		    Modes2=Modes1
		)
	    )
	; CommentedThing = struct2(Name,Struct) ->
	    Index = struct(Name),
	    (
		nonvar(Fields),
		( var(Tmpl) ->
		    ( foreach(FieldName:_,Fields), foreach(FieldNameAtom,FieldNames) do
			concat_atom([FieldName], FieldNameAtom)
		    ),
		    % we store Struct only to check it later
		    % against the structure declaration
		    Struct =.. [Name|FieldNames],
		    term_string(Struct, StructString),
		    concat_strings("struct ", StructString, Tmpl)
		;
		    true
		)
	    ->
	    	true
	    ;
		printf(warning_output, "WARNING: Improper field list in comment for %w%n",
			[CommentedThing])
	    )
	;
	    Index = CommentedThing,
	    ( var(Tmpl) ->
	    	term_string(CommentedThing,Tmpl)
	    ;
		true
	    )
	),
	% check the exception numbers
	( var(Exceptions) ->
	    true
	;
	    ( foreach(ExcId:_,Exceptions), param(CommentedThing) do
		( integer(ExcId), error_id(ExcId,_) -> true
		; atom(ExcId) -> true
		; string(ExcId) -> true
		;
		    printf(warning_output, "WARNING: Unrecognised error number %w in %w%n", [ExcId,CommentedThing])
		)
	    )
	),
	% always require a summary
	( nonvar(Summary) -> true ;
	    printf(error, "No summary in comment for %w%n", [CommentedThing]),
	    fail
	).

page_field(_,		Key,		_Val, _Page) :- var(Key), !, fail.
page_field(_,		summary,	Val, pdoc with summary:Val) :- !.
page_field(_/_,		args,		Val, pdoc with args:Val) :- !.
page_field(_,		desc,		Val, pdoc with desc:Val) :- !.
page_field(_,		eg,		Val, pdoc with eg:Val) :- !.
page_field(_,		see_also,	Val, pdoc with see:Val) :- !.
page_field(struct2(_,_), fields,	Val, pdoc with fields:Val) :- !.
page_field(struct2(_,_), args,		Val, pdoc with fields:Val) :- !.
page_field(_/_,		resat,		Val, pdoc with chp:Val) :- !.
page_field(_/_,		fail_if,	Val, pdoc with fail:Val) :- !.
page_field(_/_,		exceptions,	Val, pdoc with exc:Val) :- !.
page_field(_/_,		template,	Val, pdoc with template:Val) :- !.
page_field(_/_,		amode,		_Val, _Page) :- !. % handled separately
page_field(_,		index,		_Val, _Page) :- !. % handled separately


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Pass 3 - generate the indices
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

% Libs is a list of lib(Libname, LibSummary, LibTitle)
% LibPreds is a list of pred(N/A, Template, Summary, RelHtmlFileRoot)

gen_library_index(HtmlTopDir, GroupSummary, Libs, Header) :-
	concat_string([HtmlTopDir,/,"index.html"], IndexFile),
	open(IndexFile, write, Stream),
	chmod644(IndexFile),
%	writeln(log_output, making:IndexFile),
	printf(Stream, "<HTML><HEAD><TITLE>%w</TITLE></HEAD><BODY>", [GroupSummary]),
	writeln(Stream, Header),
	printf(Stream, "<H1>%w</H1>", [GroupSummary]),
	writeln(Stream, "<DL>"),
	sort(1, =<, Libs, SortedLibs),
	(
	    foreach(lib(LibName, LibSummary, LibTitle),SortedLibs),
	    param(Stream)
	do
	    ( LibTitle = library(LibTitle1) -> true ; LibTitle1=LibTitle ),
	    printf(Stream,
		"<DT><A HREF=\"%w/index.html\"><STRONG>%w</STRONG></A></DT>%n<DD>%w</DD>%n",
		[LibName,LibTitle1,LibSummary])
	),
	writeln(Stream, "</DL>"),
	date(Date),
	printf(Stream, "<HR>Generated %w%n", [Date]),
	writeln(Stream, "</BODY></HTML>"),
	close(Stream).

gen_group_index(HtmlTopDir, Groups, LinkBack, SystemName) :-
	concat_string([HtmlTopDir,/,"index.html"], IndexFile),
	open(IndexFile, write, Stream),
	chmod644(IndexFile),
%	writeln(log_output, making:IndexFile),
	printf(Stream, "<HTML><HEAD><TITLE>%w Reference Manual</TITLE></HEAD><BODY>", [SystemName]),
	printf(Stream, "[ %w | <A HREF=\"fullindex.html\">Alphabetic Index</A> ]%n", [LinkBack]),
	( SystemName == "ECLiPSe" ->
	    get_flag(version, Version),
	    printf(Stream, "<H1>%w %w Reference Manual</H1>", [SystemName,Version])
	;
	    printf(Stream, "<H1>%w Reference Manual</H1>", [SystemName])
	),
	writeln(Stream, "<DL>"),
	msort(Groups, SortedGroups),
	writeln(Stream, "<OL>"),
	(
	    foreach(Group,SortedGroups),
	    param(Stream,SystemName)
	do
	    group_summary(SystemName, Group, GroupSummary),
	    printf(Stream,
		"<LI><A HREF=\"%w/index.html\"><STRONG>%w</STRONG></A>%n",
		[Group,GroupSummary])
	),
	writeln(Stream, "</OL>"),
	date(Date),
	printf(Stream, "<HR>Generated %w%n", [Date]),
	writeln(Stream, "</BODY></HTML>"),
	close(Stream).

group_summary("ECLiPSe", "kernel", "The ECLiPSe Built-In Predicates") :- !.
group_summary("ECLiPSe", "lib", "The ECLiPSe Libraries") :- !.
group_summary("ECLiPSe", "lib_public", "Third Party Libraries") :- !.
group_summary(_, Other, Other).


gen_full_index(HtmlTopDir, LinkBack, SystemName, GroupFlag) :-
	concat_string([HtmlTopDir,/,"fullindex.html"], IndexFile),
	open(IndexFile, write, Stream),
	chmod644(IndexFile),
%	writeln(log_output, making:IndexFile),
	printf(Stream, "<HTML><HEAD><TITLE>%w Alphabetic Predicate Index</TITLE></HEAD><BODY>", [SystemName]),
	printf(Stream, "[ %w | <A HREF=\"index.html\">Reference Manual</A> ]%n", [LinkBack]),
	printf(Stream, "<H1>%w Alphabetic Predicate Index</H1>", [SystemName]),
	% make A-Z jump table
	( for(Letter, 0'A, 0'Z), param(Stream) do
	    printf(Stream, "  <A HREF=\"#%c\">%c</A>", [Letter,Letter])
	),
	nl(Stream),
	writeln(Stream, "<OL>"),
	(
	    setval(initial, 0'A),

	    bip(Name, Arity, Group0, SubGroup, File),	% nondet

	    % insert the targets for the A-Z jump table
	    atom_string(Name, NameString),
	    string_list(NameString, [ThisInitial|_]),
	    getval(initial, NextInitial),
	    char_key(NextInitial, NextKey),
	    char_key(ThisInitial, ThisKey),
	    ( ThisKey >= NextKey ->
		printf(Stream, "<A name=\"%c\"></A>%n", [NextInitial]),
		incval(initial)
	    ;
		true
	    ),

	    ( number(Arity) -> IndexTerm = Name/Arity
	    ; Arity = (struct) -> IndexTerm = Name/(struct)
	    ; IndexTerm = Name ),
	    ( GroupFlag = nogroups -> Group = '.' ; Group = Group0 ),
	    ( File == '' ->
		printf(Stream,
		    "<LI><STRONG> %w</STRONG>..........<A HREF=\"%w/%w/index.html\">%w/%w</A>%n",
		    [IndexTerm,Group,SubGroup,Group,SubGroup])
	    ;
		printf(Stream,
		    "<LI><STRONG><A HREF=\"%w/%w/%w.html\"> %w</A></STRONG>..........<A HREF=\"%w/%w/index.html\">%w/%w</A>%n",
		    [Group,SubGroup,File,IndexTerm,Group,SubGroup,Group,SubGroup])
	    ),
	    fail
	;
	    true
	),
	writeln(Stream, "</OL>"),
	date(Date),
	printf(Stream, "<HR>Generated %w%n", [Date]),
	writeln(Stream, "</BODY></HTML>"),
	close(Stream).

gen_prolog_index(HtmlTopDir, SortedIndexList) :-
	concat_string([HtmlTopDir,"/index.pl"], IndexFile),
%	writeln(log_output, making:IndexFile),
	open(IndexFile, write, S),
	chmod644(IndexFile),
	printf(S, "%%%n%% This file is generated - do not edit%n", []),
	printf(S, "%% bip(Name, Arity, Group, Library, Filename)%n%%%n%n", []),
	( foreach(Index,SortedIndexList), param(S) do
	    writeq(S, Index), writeln(S, .)
	),
	close(S).


%----------------------------------------------------------------------
% Auxiliaries
%----------------------------------------------------------------------

% Recursively find all files matching Pattern below the directory
% or list of directories Roots.

find_tree(Root, Pattern, AllFiles) :- (atom(Root);string(Root)), !,
	find_dir(Root, "", "", Pattern, AllFiles, []).
find_tree(Roots, Pattern, AllFiles) :-
	find_tree(Roots, Pattern, AllFiles, []).

find_tree([], _Pattern, AllFiles, AllFiles).
find_tree([Root|Roots], Pattern, AllFiles, AllFiles0) :-
	find_dir(Root, "", "", Pattern, AllFiles, AllFiles1),
	find_tree(Roots, Pattern, AllFiles1, AllFiles0).

find_dir(_, _, [], _, AllFiles, AllFiles) :- !.
find_dir(Root, RelPath, [Dir|Dirs], Pattern, AllFiles, AllFiles0) :- !,
	find_dir(Root, RelPath, Dir, Pattern, AllFiles, AllFiles1),
	find_dir(Root, RelPath, Dirs, Pattern, AllFiles1, AllFiles0).
find_dir(Root, RelPath, Dir, Pattern, AllFiles, AllFiles0) :-
	( RelPath=="" -> RelDir=Dir
	; concat_string([RelPath,/,Dir], RelDir) ),
	( RelDir=="" -> AbsDir=Root
	; concat_string([Root,/,RelDir], AbsDir) ),
%	printf("Directory %w%n%b", [AbsDir]),
	read_directory(AbsDir, Pattern, SubDirs, Files),
	find_files(Root, RelDir, Files, AllFiles, AllFiles1),
	find_dir(Root, RelDir, SubDirs, Pattern, AllFiles1, AllFiles0).

find_files(_, _, [], AbsFiles, AbsFiles).
find_files(Root, RelDir, [File|Files], [AbsFile|AbsFiles], AbsFiles0) :-
	( RelDir=="" -> concat_string([Root,/,File], AbsFile)
	; concat_string([Root,/,RelDir,/,File], AbsFile) ),
%	writeln(AbsFile),
	find_files(Root, RelDir, Files, AbsFiles, AbsFiles0).


makedir(Dir) :-
	( exists(Dir) ->
	    true
	;
	    mkdir(Dir),
	    ( get_flag(hostarch, "i386_nt") -> true
	    ; exec([chmod,755,Dir], []))
	).

chmod644(File) :-
	( get_flag(hostarch, "i386_nt") -> true
	; exec([chmod,644,File], [])).


writeln_lazily(_Stream, _String, Flag) :- nonvar(Flag).
writeln_lazily(Stream, String, Flag) :- var(Flag),
    	writeln(Stream, String), Flag = 1.

writeln_if(_Stream, _String, Flag) :- var(Flag).
writeln_if(Stream, String, Flag) :- nonvar(Flag),
    	writeln(Stream, String).


% Generate a filename from a predicate name/arity
% which contains no funny characters

:- export object_spec_to_filename/2.
object_spec_to_filename(N/A, FileName) :- !,
    	atom_string(N,S),
	string_list(S,L),
	( foreach(C,L), foreach(TC,TL) do
	    get_chtab(C, CC),
	    filename_char(CC, C, TC)
	),
	string_list(TS,TL),
	concat_string([TS,-,A], FileName).
object_spec_to_filename(struct(N), FileName) :-
	object_spec_to_filename(N/s, FileName).

    filename_char(upper_case, C, C) :- !.
    filename_char(underline, C, C) :- !.
    filename_char(lower_case, C, C) :- !.
    filename_char(digit, C, C) :- !.
    filename_char(_, 0'@, 0'A) :- !.
    filename_char(_, 0'`, 0'B) :- !.
    filename_char(_, 0',, 0'C) :- !.
    filename_char(_, 0'., 0'D) :- !.
    filename_char(_, 0'=, 0'E) :- !.
    filename_char(_, 0'/, 0'F) :- !.
    filename_char(_, 0'>, 0'G) :- !.
    filename_char(_, 0'#, 0'H) :- !.
    filename_char(_, 0'!, 0'I) :- !.
    filename_char(_, 0'', 0'K) :- !.
    filename_char(_, 0'<, 0'L) :- !.
    filename_char(_, 0'", 0'M) :- !.
    filename_char(_, 0':, 0'N) :- !.
    filename_char(_, 0';, 0'O) :- !.
    filename_char(_, 0'+, 0'P) :- !.
    filename_char(_, 0'?, 0'Q) :- !.
    filename_char(_, 0'\, 0'R) :- !.
    filename_char(_, 0'$, 0'S) :- !.
    filename_char(_, 0'~, 0'T) :- !.
    filename_char(_, 0'^, 0'U) :- !.
    filename_char(_, 0'*, 0'X) :- !.
    filename_char(_, 0'&, 0'Y) :- !.
    filename_char(_, 0'%, 0'Z) :- !.
    filename_char(_, _C, 0'-).

:- mode stringify(+,-).
stringify(S, S) :- string(S).
stringify(A, S) :- atom(A), atom_string(A,S).


alphasort(Arg, Rel, List, Sorted) :-
	( foreach(X,List), foreach(Key-X,KeyList), param(Arg) do
	    arg(Arg, X, Val),
	    string_to_sorting_key(Val, Key)
	),
	sort(1, Rel, KeyList, SortedKeyList),
	( foreach(_Key-X,SortedKeyList), foreach(X,Sorted) do true ).


% produce a key that puts symbols first, then numbers,
% then letters ignoring case
string_to_sorting_key(A, K) :- atom(A),
	atom_string(A,S),
	string_to_sorting_key(S, K).
string_to_sorting_key(S, K) :- string(S),
	string_list(S, Chars),
	( foreach(Char,Chars), foreach(Key,K) do
	    char_key(Char, Key)
	).

    char_key(Char, Key) :-
	    get_chtab(Char, Class),
	    char_key(Char, Class, Key).

    :- mode char_key(+,+,-).
    char_key(C, blank_space, C) :- !.
    char_key(C, upper_case, Key) :- !, Key is 768 + C - 0'A.
    char_key(C, lower_case, Key) :- !, Key is 768 + C - 0'a.
    char_key(C, digit, Key) :- !, Key is 512 + C - 0'0.
    char_key(C, _symbol, Key) :- !, Key is 256 + C.


%----------------------------------------------------------------------
% generate html page for one commented item (predicate,structure,...)
%----------------------------------------------------------------------

% Standalone page
gen_html_file(Header, HtmlDir, LibTitle, LibName, Page, HtmlFile) :-
	Page = pdoc with [template:Tmpl, index:Spec],
	object_spec_to_filename(Spec, FileName),
	concat_string([FileName,.,html], HtmlFile),
	concat_string([HtmlDir,/,HtmlFile], FullHtmlFile),
	open(FullHtmlFile, write, S),
	chmod644(FullHtmlFile),
%	writeln(log_output, making:FullHtmlFile),
	printf(S, "<HTML><HEAD>", []),
	printf(S, "<TITLE>%w</TITLE>%n", [Tmpl]),
	printf(S, "</HEAD>", []),
	printf(S, "<BODY>", []),
	printf(S, "[ <A HREF=\"index.html\">%w</A> | %w ]%n", [LibTitle,Header]),
	( gen_html_body(S, LibName, Page) ->
	    true
	;
	    printf(error, "Error in generating html page for %w%n", [LibName:Spec])
	),
	printf(S, "</BODY></HTML>%n", []),
	close(S).


% Embedded page
% when Libname is '', the see_also references are suppressed
gen_html_body(S, LibName, pdoc with [
	    template:Temp,summary:Onel,args:Args,modes:Modes,fields:Fields,
	    desc:Desc,chp:Chp,fail:Fail,exc:Exc,eg:Exmp,see:See]) :-
	( Temp = [_|_] ->
	    ( foreach(T,Temp), param(S) do
		printf(S, "<H1>%w</H1>%n", [T])
	    )
	;
	    printf(S, "<H1>%w</H1>%n", [Temp])
	),

	html_write_par(S, Onel),

	( nonvar(Args) ->
	    printf(S, "<DL>%n", []),
	    write_args(S, Args),
	    printf(S, "</DL>%n", [])
	; nonvar(Fields) ->
	    printf(S, "<H2>Fields</H2><DL>%n", []),
	    write_args(S, Fields),
	    printf(S, "</DL>%n", [])
	;
	    true
	),

	( nonvar(Modes), Modes = [_,_|_] ->	% print modes only if more than one
	    printf(S, "<H3>Calling Modes</H3><UL>%n", []),
	    ( foreach(Mode,Modes),param(S) do printf(S, "<LI>%w%n", [Mode]) ),
	    writeln(S, "</UL>")
	; true
	),

	( LibName \== obsolete -> true ;
	    printf(S, "<H3>This built-in predicate is obsolete!</H3>%n", [])
	),

	html_write_optional_section(S, "<H2>Description</H2>", Desc),
	html_write_optional_section(S, "<H3>Fail Conditions</H3>", Fail),
	html_write_optional_section(S, "<H3>Resatisfiable</H3>", Chp),

	( var(Exc) -> true ;
	    printf(S, "<H3>Exceptions</H3>%n", []),
	    printf(S, "<DL>%n", []),
	    write_exc(S, Exc),
	    printf(S, "</DL>%n", [])
	),

	( var(Exmp) -> true ;
	    printf(S, "<H2>Examples</H2>%n", []),
	    ( string(Exmp) -> html_write_par(S, ascii(Exmp))
	    ; html_write_par(S, Exmp) )
	),

	( var(See) -> true ;
	  LibName = '' -> true ;
	    printf(S, "<H2>See Also</H2>%n", []),
	    write_references(S, LibName, See)
	),

	nl(S).


html_write_optional_section(_S, _Header, Text) :- var(Text).
html_write_optional_section(S, Header, Text) :- nonvar(Text),
	writeln(S, Header),
	html_write_par(S, Text).

ascii_write_optional_section(_S, _Header, Text, _Indent) :- var(Text).
ascii_write_optional_section(S, Header, Text, Indent) :- nonvar(Text),
	writeln(S, Header),
	ascii_write_par(S, Text, Indent),
	nl(S).


write_args(_, []).
write_args(S, [N:T:D|As]) :- !,
	printf(S, "<DT><EM>%w</EM></DT>%n<DD>", [N:T]),
	html_write_par(S, D),
	writeln(S, "</DD>"),
	write_args(S, As).
write_args(S, [N:D|As]) :-
	printf(S, "<DT><EM>%w</EM></DT>%n<DD>", [N]),
	html_write_par(S, D),
	writeln(S, "</DD>"),
	write_args(S, As).

ascii_write_args(_, []).
ascii_write_args(S, [N:T:D|As]) :- !,
	concat_string([N,:,T],NT),
	printf(S, "   %-20s", [NT]),
	ascii_write_par(S, D, 0),
	ascii_write_args(S, As).
ascii_write_args(S, [N:D|As]) :-
	printf(S, "   %-20s", [N]),
	ascii_write_par(S, D, 0),
	ascii_write_args(S, As).


html_write_par(S, html(HtmlString)) :-
	writeln(S, HtmlString).
html_write_par(S, ascii(AsciiString)) :-
	htmlify_string(AsciiString, HtmlString),
	printf(S, "<PRE>%w</PRE>%n", [HtmlString]).
html_write_par(S, ascii_fmt(AsciiString)) :-
	html_write_par(S, AsciiString).
html_write_par(S, AsciiAtom) :- atom(AsciiAtom),
	atom_string(AsciiAtom, AsciiString),
	html_write_par(S, AsciiString).
html_write_par(S, AsciiString) :- string(AsciiString),
	htmlify_string(AsciiString, HtmlString),
	writeln(S, HtmlString).


ascii_write_par(S, html(HtmlString), _Indent) :-
        string(HtmlString), !,
	html_to_ascii(HtmlString, AsciiString),
	writeln(S, AsciiString).
ascii_write_par(S, ascii(AsciiString), Indent) :-
        string(AsciiString), !,
	ascii_write_par(S, AsciiString, Indent).
ascii_write_par(S, html(HtmlAtom), Indent) :-
        % unfortunately some comment directives are atoms...
        atom(HtmlAtom), !,
        atom_string(HtmlAtom, HtmlString),
        ascii_write_par(S, html(HtmlString), Indent).
ascii_write_par(S, ascii(AsciiAtom), Indent) :-
        % unfortunately some comment directives are atoms...
        atom(AsciiAtom), !,
        atom_string(AsciiAtom, AsciiString),
        ascii_write_par(S, AsciiString, Indent).
ascii_write_par(S, AsciiAtom, Indent) :- atom(AsciiAtom),
	atom_string(AsciiAtom, AsciiString),
	ascii_write_par(S, AsciiString, Indent).
ascii_write_par(S, AsciiString, Indent) :- string(AsciiString),
	printf(S, "%*c%w%n", [Indent, 0' , AsciiString]).


write_exc(_S, []).
write_exc(S, [N:D|Ls]) :-
	exc(N,Name),
	printf(S, "<DT><EM>(%w) %w</EM>%n<DD>", [N,Name]),
	html_write_par(S, D),
	write_exc(S, Ls).

ascii_write_exc(_S, []).
ascii_write_exc(S, [N:D|Ls]) :-
	( integer(N) ->
	    printf(S, "   %3d --- %w%n", [N,D])
	;
	    printf(S, "   %w --- %w%n", [N,D])
	),
	ascii_write_exc(S, Ls).

exc(N, Error) :-
	integer(N), 
	error_id(N, ErrorMessage), !,
	concat_strings(ErrorMessage, " ", Error).
exc(_, "").


% Generate the "see also" references. 
% S is the output stream
% List is the see_also list
% LibName is the library where this lists occurs

write_references(S, LibName, List) :-
	findall(Ref, (member(X,List),find_ref(X,LibName,Ref)), Refs),
	( Refs = [R1|Refs1] ->
	    print_ref(S, LibName, R1),
	    ( foreach(R,Refs1),param(S,LibName) do
	    	write(S, ", "), print_ref(S,LibName,R)
	    )
	;
	    true
	).

:- mode find_ref(?,+,-). % is nondet
find_ref(Group:N/A, Lib, bip(N, A, System, Group, File)) ?-
	( var(Group) ->
	    not not (bip(N, A, _, Group, _), Group \== Lib), !,
	    bip(N, A, System, Group, File), Group \== Lib
	;
	    bip(N, A, System, Group, File), !	% exact match - take it
	).
find_ref(N/A, Group, bip(N, A, System, Group, File)) ?-
	bip(N, A, System, Group, File), !.		% match in the group - take it
find_ref(N/A, _Lib, bip(N, A, kernel, Group, File)) ?-
	bip(N, A, kernel, Group, File), !.	% kernel match - take it
find_ref(N/A, Lib, bip(N, A, System, Group, File)) ?-
	not not (bip(N, A, _, Group, _), Group \== Lib), !,
	bip(N, A, System, Group, File), Group \== Lib.	% take all others
find_ref(struct(N), Lib, Match) ?- !,
	find_ref(N/(struct), Lib, Match).
find_ref(library(Group), _Lib, Match) ?-
	bip(_N, _A, System, Group, _File), !,
	Match = library(System, Group).
find_ref(N, Lib, index(N, System, Group, File)) :- atom(N), !,
	find_ref(N/index, Lib, bip(N, index, System, Group, File)).
find_ref(S, Lib, Match) :- string(S), !,
	atom_string(N, S),
	find_ref(N, Lib, Match).
find_ref(N, _Lib, noref(N)) :-
	printf(warning_output, "%nWARNING: Ghost reference to %w%n%b", [N]).

print_ref(S, _Lib, noref(N)) ?-
	printf(S, "%w", [N]).
print_ref(S, _Lib, index(N, System, Group, File)) ?-
	( System = '.' ->
	    printf(S, "<A HREF=\"../%w/%w.html\">%w</A>", [Group, File, N])
	;
	    printf(S, "<A HREF=\"../../%w/%w/%w.html\">%w</A>",
		    [System, Group, File, N])
	).
print_ref(S, Lib, library(System,Group)) ?-
	( System = '.' ->
	    printf(S, "<A HREF=\"../%w/index.html\">", [Group])
	;
	    printf(S, "<A HREF=\"../../%w/%w/index.html\">", [System, Group])
	),
	ascii_print_ref(S, Lib, library(System,Group)),
	write(S, "</A>").

print_ref(S, Lib, bip(N, A, System, Group, File)) ?-
	( System = '.' ->
	    printf(S, "<A HREF=\"../%w/", [Group])
	;
	    printf(S, "<A HREF=\"../../%w/%w/", [System, Group])
	),
	( File = '' ->
	    % predicate has no page, refer to library page instead
	    printf(S, "index.html\">", [])
	;
	    printf(S, "%w.html\">", [File])
	),
	( Group == obsolete, Lib \== obsolete ->
	    printf(warning_output,
		"%nWARNING: reference to obsolete predicate %w in %w%n",
	    	[Group:N/A, Lib])
	;
	    true
	),
	ascii_print_ref(S, Lib, bip(N, A, System, Group, File)),
	write(S, "</A>").


ascii_write_references(S, LibName, List) :-
	findall(Ref, (member(X,List),find_ref(X,LibName,Ref)), Refs),
	( Refs = [R1|Refs1] ->
	    ascii_print_ref(S, LibName, R1),
	    ( foreach(R,Refs1),param(S,LibName) do
	    	write(S, ", "), ascii_print_ref(S,LibName,R)
	    )
	;
	    true
	).

ascii_print_ref(S, _Lib, noref(N)) ?-
	printf(S, "%w", [N]).
ascii_print_ref(S, _Lib, index(N, _System, _Group, _File)) ?-
	printf(S, "%w", [N]).
ascii_print_ref(S, _Lib, library(_System,Group)) ?-
	write(S, library(Group)).
ascii_print_ref(S, Lib, bip(N, A, System, Group, _File)) ?-
	( atom(A) -> Item =.. [A,N] ; Item = N/A ),
	( System == kernel ->
	    write(S, Item)
	; Lib == Group ->
	    write(S, Item)
	;
	    write(S, Group:Item)
	).


:- export htmlify_string/2.
htmlify_string(In, Out) :-
	string_list(In, InList),
	htmlify_list(InList, OutList),
	string_list(Out, OutList).

htmlify_list([], []).
htmlify_list([C|Cs], HtmlCs) :-
	htmlify(C, HtmlCs, HtmlCs0),
	htmlify_list(Cs, HtmlCs0).

htmlify(0'>, [0'&,0'g,0't,0';|Cs], Cs) :- !.
htmlify(0'<, [0'&,0'l,0't,0';|Cs], Cs) :- !.
htmlify(0'&, [0'&,0'a,0'm,0'p,0';|Cs], Cs) :- !.
htmlify(C, [C|Cs], Cs).


%---------------------------------------------------------------------- 
% generate ASCII
%---------------------------------------------------------------------- 

gen_ascii_file(HtmlDir, LibTitle, LibName, Page) :-
	Page = pdoc with [index:Spec],
	object_spec_to_filename(Spec, FileName),
	concat_string([FileName,.,txt], TxtFile),
	concat_string([HtmlDir,/,TxtFile], FullTxtFile),
	open(FullTxtFile, write, TxtStream),
	chmod644(FullTxtFile),
%	writeln(log_output, making:FullTxtFile),
	( gen_ascii(TxtStream, LibTitle, LibName, Page) ->
	    true
	;
	    printf(error, "Error in generating txt page for %w%n", [Spec])
	),
	close(TxtStream).


gen_ascii(S, LibTitle, LibName, pdoc with [template:Temp,summary:Onel,args:Args,
	    fields:Fields,desc:Desc,chp:Chp,fail:Fail,exc:Exc,eg:Exmp,see:See]) :-
	nl(S),
	( Temp = [_|_] ->
	    ( foreach(T,Temp), param(S) do
		writeln(S, T)
	    )
	;
	    writeln(S, Temp)
	),
	nl(S),

	ascii_write_par(S, Onel, 3),
	nl(S),

	( nonvar(Args) ->
	    writeln(S, "Arguments"),
	    ascii_write_args(S, Args), nl(S)
	; nonvar(Fields) ->
	    writeln(S, "Fields"),
	    ascii_write_args(S, Fields), nl(S)
	;
	    true
	),

	writeln(S, "Type"), write(S, "   "),
	writeln(S, LibTitle), nl(S),

	ascii_write_optional_section(S, "Description", Desc, 0),
	ascii_write_optional_section(S, "Resatisfiable", Chp, 3),
	ascii_write_optional_section(S, "Fail Conditions", Fail, 3),

	( var(Exc) -> true ;
	    writeln(S, "Exceptions"),
	    ascii_write_exc(S, Exc), nl(S)
	),

	ascii_write_optional_section(S, "Examples", Exmp, 3),

	( var(See) -> true ;
	    writeln(S, "See Also"),
	    write(S, "   "),
	    ascii_write_references(S, LibName, See)
	),

	nl(S).


html_to_ascii(Html, Ascii) :-
	string_list(Html, HtmlList),
	html_string(AsciiList, [], HtmlList, []),
	string_list(Ascii, AsciiList).

html_string(Cs,Cs0) --> [0'<], tag_end, !, html_string(Cs,Cs0).
html_string(Cs,Cs0) --> [0'<], tag_end, !, html_string(Cs,Cs0).
html_string([0'<|Cs1],Cs0) --> [0'&,0'l,0't,0';], !, html_string(Cs1,Cs0).
html_string([0'>|Cs1],Cs0) --> [0'&,0'g,0't,0';], !, html_string(Cs1,Cs0).
html_string([0'&|Cs1],Cs0) --> [0'&,0'a,0'm,0'p,0';], !, html_string(Cs1,Cs0).
html_string([0'"|Cs1],Cs0) --> [0'&,0'q,0'u,0'o,0't,0';], !, html_string(Cs1,Cs0).
html_string([169|Cs1],Cs0) --> [0'&,0'c,0'o,0'p,0'y,0';], !, html_string(Cs1,Cs0).
html_string([160|Cs1],Cs0) --> [0'&,0'n,0'b,0's,0'p,0';], !, html_string(Cs1,Cs0).
html_string([C|Cs1],Cs0) --> [0'&,0'#], charRef(C), [0';], !, html_string(Cs1,Cs0).
html_string([C|Cs1],Cs0) --> [C], !, html_string(Cs1,Cs0).
html_string(Cs,Cs) --> [].

charRef(N) --> [C], {dec(C,N0)}, decNum(N0,N), {N =< 255}.

decNum(N0,N) --> [C], { dec(C,N1), N2 is 10*N0+N1 }, !, decNum(N2,N).
decNum(N,N) --> [].

    :- mode dec(+,-).
    dec(0'0, 0). dec(0'1, 1). dec(0'2, 2). dec(0'3, 3). dec(0'4, 4).
    dec(0'5, 5). dec(0'6, 6). dec(0'7, 7). dec(0'8, 8). dec(0'9, 9).

tag_end --> [0'>], !.
tag_end --> [_], tag_end.
	

%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Construct template string from the predicate's name/arity and
% the template or amode+args field of the predicate comment.
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

construct_pred_template(Name, Arity, PComs, Template) :-
	(memberchk(template:Template,PComs) ->
	    true
	;
	    return_pred_modes(Name, Arity, PComs, Modes),
	    generalise_modes(Modes, Mode),
	    construct_onepred_template(Name, Arity, PComs, Mode, Template)
	).

construct_onepred_template(Name, Arity, PComs, Mode, DText) :-
	( memberchk(args:ArgDs, PComs) ->
	    ( length(ArgDs, Arity) ->
		(foreach(ArgDesc, ArgDs), foreach(Name, ANames) do
		    ((ArgDesc = Name0:_, string(Name0)) -> 
			Name = Name0 ; Name = ""
		    )
		)
	    ;
		(count(_,1,Arity), foreach("", ANames) do true),
		printf(warning_output, "WARNING: Argument list mismatch in comment for %w%n",
		    [Name/Arity])
	    )
	;   
	    (count(_,1,Arity), foreach("", ANames) do true)
        ),
	(foreach(AName, ANames), foreacharg(AMode, Mode), foreach(Arg, ArgsString) do
            concat_string([AMode,AName], Arg)
	),
        construct_pred_template_with_args(Name, Arity, ArgsString, DText).


construct_pred_template_with_args(Name, _, Args, DText) :-
	(Args == [] ->
            concat_string([Name], DText)
	;
	    DTextList = [Name, "("|ArgList],
	    construct_arglist(Args, ArgList),
	    concat_string(DTextList, DText)
	).

construct_arglist([Last], Out) ?- !,
	Out = [Last, ")"].
construct_arglist([Arg|Args], Out) ?-
	Out = [Arg, ", "|Out0],
	construct_arglist(Args, Out0).

return_pred_modes(Name, Arity, PComs, Modes) :-
	findall(Mode, (member(amode:Mode, PComs), functor(Mode, Name, Arity)), 
           Modes0),
	(Modes0 == [] ->
	    % no modes found, generate an all '?' mode.
	    default_template(Name/Arity, GenMode),
	    Modes = [GenMode]
	;   Modes0 = Modes
        ).

generalise_modes([Mode|Modes], GenM) :-
	generalise_modes(Modes, Mode, GenM).

generalise_modes([], Gen, Gen).
generalise_modes([Mode1|Modes], Mode2, Gen) :-
	functor(Mode1, Name, Arity),
	functor(GenMode1, Name, Arity),
	(foreacharg(M1, Mode1), foreacharg(M2, Mode2), foreacharg(G, GenMode1) do
            lub(M1, M2, G)
	),
	generalise_modes(Modes, GenMode1, Gen).


default_template(N/A, Template) :-
	functor(Template, N, A),
	( foreacharg(X,Template) do X = ? ).


% lub(PX, PY, PLub) - least upper bound of 2 modes

lub(?, _, ?) :- !.
lub(_, ?, ?) :- !.
lub(-, -, -) :- !.
lub(-, +, ?) :- !.
lub(-, ++, ?) :- !.
lub(+, -, ?) :- !.
lub(+, +, +) :- !.
lub(+, ++, +) :- !.
lub(++, -, ?) :- !.
lub(++, +, +) :- !.
lub(++, ++, ++) :- !.

