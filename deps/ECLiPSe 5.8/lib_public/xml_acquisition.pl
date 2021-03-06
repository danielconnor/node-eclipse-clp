/* xml_acquisition.pl : XML -> Document translation.
 *
 * Copyright (C) 2001-2003 Binding Time Limited
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * $Revision: 1.2 $
 *
 */

:- ensure_loaded( xml_utilities ).

/* xml_to_document( +Controls, +XML, ?Document ) translates the list of
 * character codes XML into the Prolog term Document. Controls is a list
 * of terms controlling the treatment of layout characters and character
 * entities.
 */
xml_to_document( Controls, XML, Document ) :-
	initial_context( Controls, Context ),
	( xml_declaration( Attributes0, XML, XML1 ) ->
		Attributes = Attributes0
	; otherwise ->
		XML1 = XML,
		Attributes = []
	),
	xml_to_document( XML1, Context, Terms, [], WellFormed ),
	xml_to_document1( WellFormed, Attributes, Terms, Document ).

xml_to_document1( true,  Attributes, Terms, xml(Attributes, Terms) ).
xml_to_document1( false, Attributes, Terms, malformed(Attributes, Terms) ).

% unparsed( +Unparsed, +Context, ?Terms, ?Residue, ?WellFormed )
unparsed( Unparsed, _Context, [unparsed(Unparsed)], [], false ).

xml_declaration( Attributes ) -->
	spaces,
	"<?",
	nmtoken( xml ),
	xml_declaration_attributes( Attributes ),
	spaces,
	"?>".

xml_to_document( [], Context, Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
xml_to_document( [Char|Chars], Context, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		entity_reference( Chars, Context, Terms, Residue, WF )
	; Char =< " ",
	  \+ space_preserve( Context ) ->		
		layouts( Chars, Context, [Char|T], T, Terms, Residue, WF )
	; void_context( Context ) ->
		unparsed( [Char|Chars], Context, Terms, Residue, WF )
	; otherwise ->
		Terms = [pcdata([Char|Chars1])|Terms1],
		acquire_pcdata( Chars, Context, Chars1, Terms1, Residue, WF )
	).

layouts( [], Context, _Plus, _Minus, Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
layouts( [Char|Chars], Context, Plus, Minus, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		Chars1 = [],
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		entity_reference( Chars, Context, Terms, Residue, WF )
	; Char =< " " ->
		Minus = [Char|Minus1],
		layouts( Chars, Context, Plus, Minus1, Terms, Residue, WF )
	; void_context( Context ) ->
		unparsed( [Char|Chars], Context, Terms, Residue, WF )
	; otherwise ->
		Terms = [pcdata(Plus)|Terms1],
		Minus = [Char|Chars1],
		context_update( space_preserve, Context, true, Context1 ),
		acquire_pcdata( Chars, Context1, Chars1, Terms1, Residue, WF )
	).

acquire_pcdata( [], Context, [], Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
acquire_pcdata( [Char|Chars], Context, Chars1, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		Chars1 = [],
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		reference_in_pcdata( Chars, Context, Chars1, Terms, Residue, WF )
	; otherwise ->
		Chars1 = [Char|Chars2],
		acquire_pcdata( Chars, Context, Chars2, Terms, Residue, WF )
	).

xml_markup_structure( [], Context, Terms, Residue, WF ) :-
	unparsed( "<", Context, Terms, Residue, WF ).
xml_markup_structure( Chars, Context, Terms, Residue, WF ) :-
	Chars = [Char|Chars1],
	( Char =:= "/" ->
		closing_tag( Context, Chars1, Terms, Residue, WF )
	; Char =:= "?" ->
		pi_acquisition( Chars1, Context, Terms, Residue, WF )
	; Char =:= "!" ->
		declaration_acquisition( Chars1, Context, Terms, Residue, WF )
	; open_tag(Tag,Context,Attributes,Type, Chars, Chars2 ) ->
		push_tag( Tag, Chars2, Context, Attributes, Type, Terms, Residue, WF )
	; otherwise ->
		unparsed( [0'<|Chars], Context, Terms, Residue, WF )
	).

push_tag( Tag, Chars, Context, Attributes, Type, Terms, Residue, WF ) :-
	new_element(Tag, Chars, Context, Attributes, Type, Term, Rest, WF0),
	push_tag1( WF0, Context, Term, Rest, Terms, Residue, WF ).

push_tag1( true, Context, Term, Chars, [Term|Terms], Residue, WF ) :-
	xml_to_document( Chars, Context, Terms, Residue, WF ).
push_tag1( false, _Context, Term, Chars, [Term], Chars, false ).

new_element( TagChars, Chars, Context, Attributes0, Type, Term, Residue, WF ) :-
	input_attributes( Attributes0, Context, Context1, Attributes ),
	current_namespace( Context, CurrentNamespace ),
	( append( NSChars, [0':|TagChars1], TagChars ),
	  specific_namespace( NSChars, Context1, SpecificNamespace ) ->
		Namespace0 = SpecificNamespace
	; otherwise ->
		NSChars = "",
		TagChars1 = TagChars,
		default_namespace( Context1, Namespace0 )
	),
	( Namespace0 == CurrentNamespace ->
		Term = element(Tag, Attributes, Contents),
		Context2 = Context1
	; otherwise ->
		Term = namespace( Namespace0, NSChars,
					element(Tag, Attributes, Contents)
					),
		context_update( current_namespace, Context1, Namespace0, Context2 )
	),
	atom_codes( Tag, TagChars1 ),
	close_tag( Type, Chars, Context2, Contents, Residue, WF ).

close_tag( empty, Residue, _Context, [], Residue, true ).
close_tag( push(Tag), Chars, Context0, Contents, Residue, WF ) :-
	context_update( element, Context0, Tag, Context1 ),
	xml_to_document( Chars, Context1, Contents, Residue, WF ).

pi_acquisition( Chars, Context, Terms, Residue, WellFormed ) :-
	( inline_instruction(Target, Processing, Chars, Rest ),
	  Target \== xml ->
		Terms = [instructions(Target, Processing)|Terms1],
		xml_to_document( Rest, Context, Terms1, Residue, WellFormed )
	; otherwise ->
		unparsed( [0'<,0'?|Chars], Context, Terms, Residue, WellFormed )
	).

declaration_acquisition( Chars, Context, Terms, Residue, WF ) :-
	( declaration_type( Chars, Type, Chars1 ),
	  declaration_parse( Type, Context, Term, Context1, Chars1, Rest ) ->
		Terms = [Term|Terms1],
		xml_to_document( Rest, Context1, Terms1, Residue, WF )
	; otherwise ->
		unparsed( [0'<,0'!|Chars], Context, Terms, Residue, WF )
	).

open_tag( Tag, Namespaces, Attributes, Termination ) -->
	nmtoken_chars( Tag ),
	attributes( Attributes, [], Namespaces ),
	spaces,
	open_tag_terminator( Tag, Termination ).

open_tag_terminator( Tag, push(Tag) ) -->
	">".
open_tag_terminator( _Tag, empty ) -->
	"/>".

declaration_parse( comment, Namespaces, comment(Comment), Namespaces ) -->
	comment(Comment).
declaration_parse( cdata, Namespaces, cdata(CData), Namespaces ) -->
	cdata( CData ).
declaration_parse( doctype, Namespaces0, doctype(Name, Names), Namespaces ) -->
	doctype( Name, Names, Namespaces0, Namespaces ),
	spaces,
	">".

inline_instruction( Target, Processing, Plus, Minus  ) :-
	nmtoken(Target, Plus, Mid0 ),
	spaces( Mid0, Mid1 ),
	append( Processing, [0'?,0'>|Minus], Mid1 ),
	!.

entity_reference_name( Reference ) -->
	nmtoken_chars( Reference ),
	";".

declaration_type( [Char1,Char2|Chars1], Class, Rest ) :-
	Chars = [Char1,Char2|Chars1],
	( declaration_type1( Char1, Char2, Chars1, Class0, Residue ) ->
		Class = Class0,
		Rest = Residue
	; otherwise ->
		Class = generic,
		Rest = Chars
	).

declaration_type1( 0'-, 0'-, Chars, comment, Chars ).
declaration_type1( 0'[, 0'C, Chars, cdata, Residue ) :-
	append( "DATA[", Residue, Chars ).
declaration_type1( 0'D, 0'O, Chars, doctype, Residue ) :-
	append( "CTYPE", Residue, Chars ).

closing_tag( Context, Chars, Terms, Residue, WellFormed ) :-
	( closing_tag_name( Tag, Chars, Rest ),
	  current_tag( Context, Tag ) ->
		Terms = [],
		Residue = Rest,
		WellFormed = true
	; otherwise ->
		unparsed( [0'<,0'/|Chars], Context, Terms, Residue, WellFormed )
	).

closing_tag_name( Tag ) -->
	nmtoken_chars( Tag ),
	spaces,
	">".

entity_reference( Chars, Context, Terms, Residue, WF ) :-
	( standard_character_entity( Char, Chars, Rest ) ->
		Terms = [pcdata([Char|Chars1])|Terms1],
		acquire_pcdata( Rest, Context, Chars1, Terms1, Residue, WF )
	; entity_reference_name( Reference, Chars, Rest ),
	  defined_entity( Reference, Context, String ) ->
		append( String, Rest, Full ),
		xml_to_document( Full, Context, Terms, Residue, WF )
	; otherwise ->
		unparsed( [0'&|Chars], Context, Terms, Residue, WF )
	).

reference_in_pcdata( Chars0, Context, Chars1, Terms, Residue, WF ) :-
	( standard_character_entity(Char, Chars0, Rest ) ->
		Chars1 = [Char|Chars2],
		acquire_pcdata( Rest, Context, Chars2, Terms, Residue, WF )
	; entity_reference_name(Reference, Chars0, Rest ),
	  defined_entity( Reference, Context, String ) ->
		append( String, Rest, Full ),
		acquire_pcdata( Full, Context, Chars1, Terms, Residue, WF )
	; otherwise ->
		Chars1 = [],
		unparsed( [0'&|Chars0], Context, Terms, Residue, WF )
	).

input_attributes( [], Context, Context, [] ).
input_attributes( [Attr|Attributes0], Context0, Context, Attributes ) :-
	Attr = (NameChars=Value),
	( NameChars == "xmlns" ->
		Attributes = Attributes1,
		atom_codes( URI, Value ),
		context_update( default_namespace, Context0, URI, Context1 )
	; Attr == ("xml:space"="preserve") ->
		Attributes = ['xml:space'="preserve"|Attributes1],
		context_update( space_preserve, Context0, true, Context1 )
	; append( "xmlns:", Unqualified, NameChars ) ->
		Attributes = [Name=Value|Attributes1],
		atom_codes( Name, NameChars ),
		atom_codes( URI, Value ),
		context_update( ns_prefix(Unqualified), Context0, URI, Context1 )
	; remove_attribute_prefixes( Context0 ),
	  append( NSChars, [0':|NameChars1], NameChars ), %'
	  specific_namespace( NSChars, Context0, Namespace ),
	  current_namespace( Context0, Namespace ) ->
		Attributes = [Name=Value|Attributes1],
		atom_codes( Name, NameChars1 ),
		Context1 = Context0
	; otherwise ->
		Attributes = [Name=Value|Attributes1],
		atom_codes( Name, NameChars ),
		Context1 = Context0
	),
	input_attributes( Attributes0, Context1, Context, Attributes1 ).

attributes( [Name=Value|Attributes], Seen, Namespaces ) -->
	spaces,
	nmtoken_chars( Name ),
	{\+ member(Name, Seen)},
	spaces,
	"=",
	spaces,
	attribute_value( Value, Namespaces ),
	attributes( Attributes, [Name|Seen], Namespaces ).
attributes( [], _Seen, _Namespaces ) --> "".

xml_declaration_attributes( [] ) --> "".
xml_declaration_attributes( [Name=Value|Attributes] ) -->
	spaces,
	nmtoken( Name ),
	spaces,
	"=",
	spaces,
	xml_string( Value ),
	{xml_declaration_attribute_valid(Name, Value)},
	xml_declaration_attributes( Attributes ),
	spaces.

doctype( Name, External, Namespaces0, Namespaces1 ) -->
	spaces,
	nmtoken( Name ),
	spaces,
	doctype_id( External ),
	spaces,
	doctype1( Namespaces0, Namespaces1 ).

doctype1( Namespaces0, Namespaces1 ) -->
	"[",
	!,
	dtd( Namespaces0, Namespaces1 ),
	"]".
doctype1( Namespaces, Namespaces ) --> "".

doctype_id( system(URL) ) -->
	"SYSTEM",
	spaces,
	uri( URL ).
doctype_id( public(URN,URL) ) -->
	"PUBLIC",
	spaces,
	uri( URN ),
	spaces,
	uri( URL ).
doctype_id( local ) --> "".

dtd( Namespaces0, Namespaces1 ) -->
	spaces,
	"<!ENTITY",
	spaces,
	nmtoken_chars( Name ),
	spaces,
	quote( Quote ),
	entity_value( Quote, Namespaces0, String ),
	spaces,
	">",
	{\+ character_entity( Name, _StandardChar ), 
	 % Don't allow &lt; &quote; etc. to be updated
	 context_update( entity(Name), Namespaces0, String, Namespaces2 )
	 },
	dtd( Namespaces2, Namespaces1 ).
dtd( Namespaces0, Namespaces1 ) -->
	spaces,
	"<!--", comment(_Comment),
	dtd( Namespaces0, Namespaces1 ).
dtd( Namespaces, Namespaces ) --> spaces.

nmtokens( [Name|Names] ) -->
	spaces,
	nmtoken( Name ),
	nmtokens( Names ).
nmtokens( [] ) --> [].

entity_value( Quote, Namespaces, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_entity( Namespaces, Quote, String, Plus, Minus )
	; otherwise ->
		String = [Char|String1],
		entity_value( Quote, Namespaces, String1, Plus, Minus )
	).

attribute_value( String, Namespaces ) -->
	quote( Quote ),
	attribute_leading_layouts( Quote, Namespaces, String ).

attribute_leading_layouts( _Quote, _Namespace, [], [], [] ).
attribute_leading_layouts( Quote, Namespaces, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_layout( Namespaces, Quote, String, Plus, Minus )
	; Char > 32, Char \== 160 ->
		String = [Char|String1],
		attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
	; otherwise ->
		attribute_leading_layouts( Quote, Namespaces, String, Plus, Minus )
	).

attribute_layouts( _Quote, _Namespaces, _Layout, [], [], [] ).
attribute_layouts( Quote, Namespaces, Layout, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus )
	; Char > 32, Char \== 160 ->
		( Layout == true ->
			String = [0' ,Char|String1]
		; otherwise ->
			String = [Char|String1]
		),
		attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
	; otherwise ->
		attribute_layouts( Quote, Namespaces, true, String, Plus, Minus )
	).

reference_in_layout( NS, Quote, String, Plus, Minus ) :-
	( standard_character_entity( Char, Plus, Mid ) ->
		String = [Char|String1],
		attribute_layouts( Quote, NS, false,  String1, Mid, Minus )
	; entity_reference_name( Name, Plus, Suffix ),
	  defined_entity( Name, NS, Text ) ->
		append( Text, Suffix, Mid ),
		attribute_leading_layouts( Quote, NS, String, Mid, Minus )
	; otherwise -> % Just & is okay in a value
		String = [0'&|String1],
		attribute_layouts( Quote, NS, false, String1, Plus, Minus )
	).

reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus ) :-
	( standard_character_entity( Char, Plus, Mid ) ->
		( Layout == true ->
			String = [0' ,Char|String1]
		; otherwise ->
			String = [Char|String1]
		),
		Layout1 = false
	; entity_reference_name( Name, Plus, Suffix ),
	  defined_entity( Name, Namespaces, Text ) ->
		String = String1,
		append( Text, Suffix, Mid ),
		Layout1 = Layout
	; otherwise -> % Just & is okay in a value
		Mid = Plus,
		String = [0'&|String1],
		Layout1 = false
	),
	attribute_layouts( Quote, Namespaces, Layout1, String1, Mid, Minus ).

/* References are resolved backwards in Entity defintions so that
 * circularity is avoided.
 */
reference_in_entity( Namespaces, Quote, String, Plus, Minus ) :-
	( standard_character_entity( _SomeChar, Plus, _Rest ) ->
		String = [0'&|String1], % Character entities are unparsed
		Mid = Plus
	; entity_reference_name( Name, Plus, Suffix ), 
	  defined_entity( Name, Namespaces, Text ) -> 
		String = String1,
		append( Text, Suffix, Mid )
	),
	entity_value( Quote, Namespaces, String1, Mid, Minus ).

standard_character_entity( Char ) -->
	"#x", hex_character_reference( Char ), ";".
standard_character_entity( Char ) -->
	"#", digit( Digit ), digits( Digits ), ";",
	{number_chars( Char, [Digit|Digits])}.
standard_character_entity( C ) -->
	chars( String ),
	";",
	!,
	{character_entity(String, C)}.

uri( URI ) -->
	quote( Quote ),
	uri1( Quote, URI ).

uri1( Quote, [] ) -->
	quote( Quote ),
	!.
uri1( Quote, [Char|Chars] ) -->
	[Char],
	uri1( Quote, Chars ).

comment( Chars, Plus, Minus ) :-
	append( Chars, [0'-,0'-,0'>|Minus], Plus ),
	!.

cdata( Chars, Plus, Minus ) :-
	append( Chars, [0'],0'],0'>|Minus], Plus ),
	!.
% Syntax Components

hex_character_reference( Code ) -->
	hex_character_reference1( 0, Code ).

hex_character_reference1( Current, Code ) -->
	hex_digit_char( Value ),
	!,
	{New is (Current << 4) + Value},
	hex_character_reference1( New, Code ).
hex_character_reference1( Code, Code ) --> "".

hex_digit_char( 0 ) --> "0".
hex_digit_char( 1 ) --> "1".
hex_digit_char( 2 ) --> "2".
hex_digit_char( 3 ) --> "3".
hex_digit_char( 4 ) --> "4".
hex_digit_char( 5 ) --> "5".
hex_digit_char( 6 ) --> "6".
hex_digit_char( 7 ) --> "7".
hex_digit_char( 8 ) --> "8".
hex_digit_char( 9 ) --> "9".
hex_digit_char( 10 ) --> "A".
hex_digit_char( 11 ) --> "B".
hex_digit_char( 12 ) --> "C".
hex_digit_char( 13 ) --> "D".
hex_digit_char( 14 ) --> "E".
hex_digit_char( 15 ) --> "F".
hex_digit_char( 10 ) --> "a".
hex_digit_char( 11 ) --> "b".
hex_digit_char( 12 ) --> "c".
hex_digit_char( 13 ) --> "d".
hex_digit_char( 14 ) --> "e".
hex_digit_char( 15 ) --> "f".

quote( 0'" ) --> %"
	"""".
quote( 0'' ) -->
	"'".

spaces( [], [] ).
spaces( [Char|Chars0], Chars1 ) :-
	( Char =< 32 ->
		spaces( Chars0, Chars1 )
	; otherwise ->
		Chars1 = [Char|Chars0]
	).

nmtoken( Name ) -->
	nmtoken_chars( Chars ),
	{atom_codes(Name, Chars)}.

nmtoken_chars( [Char|Chars] ) -->
	[Char],
	{alphabet( Char )},
	nmtoken_chars_tail( Chars ).

nmtoken_chars_tail( [Char|Chars] ) -->
	[Char],
	{nmtoken_char(Char)},
	!,
	nmtoken_chars_tail( Chars ).
nmtoken_chars_tail([]) --> "".

nmtoken_char( 0'a ).
nmtoken_char( 0'b ).
nmtoken_char( 0'c ).
nmtoken_char( 0'd ).
nmtoken_char( 0'e ).
nmtoken_char( 0'f ).
nmtoken_char( 0'g ).
nmtoken_char( 0'h ).
nmtoken_char( 0'i ).
nmtoken_char( 0'j ).
nmtoken_char( 0'k ).
nmtoken_char( 0'l ).
nmtoken_char( 0'm ).
nmtoken_char( 0'n ).
nmtoken_char( 0'o ).
nmtoken_char( 0'p ).
nmtoken_char( 0'q ).
nmtoken_char( 0'r ).
nmtoken_char( 0's ).
nmtoken_char( 0't ).
nmtoken_char( 0'u ).
nmtoken_char( 0'v ).
nmtoken_char( 0'w ).
nmtoken_char( 0'x ).
nmtoken_char( 0'y ).
nmtoken_char( 0'z ).
nmtoken_char( 0'A ).
nmtoken_char( 0'B ).
nmtoken_char( 0'C ).
nmtoken_char( 0'D ).
nmtoken_char( 0'E ).
nmtoken_char( 0'F ).
nmtoken_char( 0'G ).
nmtoken_char( 0'H ).
nmtoken_char( 0'I ).
nmtoken_char( 0'J ).
nmtoken_char( 0'K ).
nmtoken_char( 0'L ).
nmtoken_char( 0'M ).
nmtoken_char( 0'N ).
nmtoken_char( 0'O ).
nmtoken_char( 0'P ).
nmtoken_char( 0'Q ).
nmtoken_char( 0'R ).
nmtoken_char( 0'S ).
nmtoken_char( 0'T ).
nmtoken_char( 0'U ).
nmtoken_char( 0'V ).
nmtoken_char( 0'W ).
nmtoken_char( 0'X ).
nmtoken_char( 0'Y ).
nmtoken_char( 0'Z ).
nmtoken_char( 0'0 ).
nmtoken_char( 0'1 ).
nmtoken_char( 0'2 ).
nmtoken_char( 0'3 ).
nmtoken_char( 0'4 ).
nmtoken_char( 0'5 ).
nmtoken_char( 0'6 ).
nmtoken_char( 0'7 ).
nmtoken_char( 0'8 ).
nmtoken_char( 0'9 ).
nmtoken_char( 0'. ).
nmtoken_char( 0'- ).
nmtoken_char( 0'_ ).
nmtoken_char( 0': ).

xml_string( String ) -->
	quote( Quote ),
	xml_string1( Quote, String ).

xml_string1( Quote, [] ) -->
	quote( Quote ),
	!.
xml_string1( Quote, [Char|Chars] ) -->
	[Char],
	xml_string1( Quote, Chars ).

alphabet( 0'a ).
alphabet( 0'b ).
alphabet( 0'c ).
alphabet( 0'd ).
alphabet( 0'e ).
alphabet( 0'f ).
alphabet( 0'g ).
alphabet( 0'h ).
alphabet( 0'i ).
alphabet( 0'j ).
alphabet( 0'k ).
alphabet( 0'l ).
alphabet( 0'm ).
alphabet( 0'n ).
alphabet( 0'o ).
alphabet( 0'p ).
alphabet( 0'q ).
alphabet( 0'r ).
alphabet( 0's ).
alphabet( 0't ).
alphabet( 0'u ).
alphabet( 0'v ).
alphabet( 0'w ).
alphabet( 0'x ).
alphabet( 0'y ).
alphabet( 0'z ).
alphabet( 0'A ).
alphabet( 0'B ).
alphabet( 0'C ).
alphabet( 0'D ).
alphabet( 0'E ).
alphabet( 0'F ).
alphabet( 0'G ).
alphabet( 0'H ).
alphabet( 0'I ).
alphabet( 0'J ).
alphabet( 0'K ).
alphabet( 0'L ).
alphabet( 0'M ).
alphabet( 0'N ).
alphabet( 0'O ).
alphabet( 0'P ).
alphabet( 0'Q ).
alphabet( 0'R ).
alphabet( 0'S ).
alphabet( 0'T ).
alphabet( 0'U ).
alphabet( 0'V ).
alphabet( 0'W ).
alphabet( 0'X ).
alphabet( 0'Y ).
alphabet( 0'Z ).

digit( C ) --> [C], {digit_table( C )}.

digit_table( 0'0 ).
digit_table( 0'1 ).
digit_table( 0'2 ).
digit_table( 0'3 ).
digit_table( 0'4 ).
digit_table( 0'5 ).
digit_table( 0'6 ).
digit_table( 0'7 ).
digit_table( 0'8 ).
digit_table( 0'9 ).

digits( [Digit|Digits] ) -->
	digit( Digit ),
	digits( Digits ).
digits( [] ) --> [].

character_entity( "quot", 0'" ). %'
character_entity( "amp", 0'&  ). %'
character_entity( "lt", 0'< ). %'
character_entity( "gt", 0'> ). %'
character_entity( "apos", 0'' ).
