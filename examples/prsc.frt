#!lina -s
\ Output a block file with name in ARG1 in a nice PostScript format.

REQUIRE ARGV
REQUIRE ^
REQUIRE if

\ ################# CONVENTIONS ###################################
\ Any LINE is a sc.
\ Any POSITION is in PostScript coordinates.
\ Any SCREEN is the number of a screen.

\ ################# TYPES #########################################

\ A coordinate pair.
: COORDINATE CREATE , , ;

\ Print a coordinate pair
: .COORDINATE  SPACE SWAP . . ;

\ ################# PostScript MANIFEST CONSTANTS #################

\ Number of screens to put in a column
5 CONSTANT #Screens

\ Number of lines to put in a column (assembler file.)
117 CONSTANT #Lines

\ upper left coordinates of top screen of left column
2500 76961 COORDINATE LeftColumn

\ upper left coordinates of top screen of right column
31550 76961 COORDINATE RightColumn

\ Stride between screens, vertical
15229 CONSTANT NextScreen

\ Stride between lines, vertical
VARIABLE LineStride
\ Leave relative vector to next line position
: NextLine LineStride @ ;
: BlockLineStride  -902  LineStride ! ;
: FileLineStride   -625 LineStride ! ;

\ Relative position of screen's lower right corner
28000 11640 COORDINATE ScreenSize

\ ################# PostScript PLUG INS ###########################
\ Warning: contains multi-line strings, perfectly allowed in scripts.

\ Output some PostScript code to fixup the use of dictionary by ``DVIps''
\ By this proper usage ``gv'' can now view each page independantly,
\ from the ``DVIps'' output and from ``prsc'' output.
: FixupDVIps
"TeXDict begin
%% /eop { eop end } def   %% This doesn't work.
{ TeXDict begin bop }
end
/bop exch def

" TYPE ;

\ Output the Prolog : PostScript comment, in fact pragma's.
: Header
"%!PS-Adobe-1.0
%%Title: Forth appendix
%%Creator:  $Id:
%%For:
%%CreationDate:
%%DocumentFonts: Courier Courier-Bold
%%Pages: 66
%%PageOrder: ascend
%%EndComments
" TYPE ;

\ Output the Prolog : PostScript code with the definitions to print our pages.
: PreLude
"/ForthDict 30 dict def
ForthDict begin
/newcodes       % foreign character encodings
[
160/space 161/exclamdown 162/cent 163/sterling 164/currency
165/yen 166/brokenbar 167/section  168/dieresis 169/copyright
170/ordfeminine 171/guillemotleft 172/logicalnot 173/hyphen 174/registered
175/macron 176/degree 177/plusminus 178/twosuperior 179/threesuperior
180/acute 181/mu 182/paragraph  183/periodcentered 184/cedilla
185/onesuperior 186/ordmasculine 187/guillemotright 188/onequarter
189/onehalf 190/threequarters 191/questiondown 192/Agrave 193/Aacute
194/Acircumflex 195/Atilde 196/Adieresis 197/Aring 198/AE 199/Ccedilla
200/Egrave 201/Eacute 202/Ecircumflex 203/Edieresis 204/Igrave  205/Iacute
206/Icircumflex 207/Idieresis 208/Eth 209/Ntilde 210/Ograve 211/Oacute
212/Ocircumflex 213/Otilde  214/Odieresis 215/multiply 216/Oslash
217/Ugrave 218/Uacute 219/Ucircumflex 220/Udieresis 221/Yacute 222/Thorn
223/germandbls 224/agrave 225/aacute 226/acircumflex 227/atilde
228/adieresis 229/aring 230/ae 231/ccedilla  232/egrave 233/eacute
234/ecircumflex 235/edieresis 236/igrave 237/iacute 238/icircumflex
239/idieresis 240/eth 241/ntilde 242/ograve 243/oacute 244/ocircumflex
245/otilde 246/odieresis 247/divide 248/oslash 249/ugrave  250/uacute
251/ucircumflex 252/udieresis 253/yacute 254/thorn 255/ydieresis
] def

/reencdict 12 dict def


    % change fonts using ISO Latin1 characters
/ChgFnt         % size psname natname  =>  font
{
    dup FontDirectory exch known        % is re-encoded name known?
    { exch pop }                        % yes, get rid of long name
    { dup 3 1 roll ReEncode } ifelse    % no, re-encode it
    findfont exch scalefont setfont
} def

/ReEncode       %
{
    reencdict begin
        /newname exch def
        /basename exch def
        /basedict basename findfont def
        /newfont basedict maxlength dict def
        basedict
        { exch dup /FID ne
            { dup /Encoding eq
                { exch dup length array copy newfont 3 1 roll put }
                { exch newfont 3 1 roll put } ifelse
            }
            { pop pop } ifelse
        } forall
        newfont /FontName newname put
        newcodes aload pop newcodes length 2 idiv
        { newfont /Encoding get 3 1 roll put } repeat
        newname newfont definefont pop
    end
} def

/#copies 1 def
/BodyFl { 679 /Courier /Courier-Latin1 ChgFnt } def
/BodyFs { 550 /Courier /Courier-Latin1 ChgFnt } def
/CW BodyFl ( ) stringwidth pop def
/TitleF {  1000 /Courier-Bold /Courier-Bold-Latin1 ChgFnt } def
/K         { -2 CW mul add exch moveto (+) show } def
/L         { CW mul add exch moveto show } def
/T         { moveto show } def
/MM         { CW mul 0 rmoveto } def
/Centre    { dup stringwidth pop 2 div neg 0 rmoveto } def
/StartPage { /SavedPage save def
  1 100 div dup scale
  0 setgray TitleF 2500 80700 moveto show 32 MM show
  BodyFs 0 setgray CW setlinewidth 1 setlinejoin } def
/EndPage   {showpage SavedPage restore end } def
{ ForthDict begin StartPage }  % Only one to be exported
end  % ForthDict
/StartPage exch def
%%EndProlog
" TYPE ;


\ Output the postlude : PostScript code.
: PostLude
"%%Trailer
%%EOF
" TYPE ;


\ ################# TOOLS #########################################
\ The kind of things that culd be useful for other programs.

\ For argument NUMBER (counting from one) : return as a string.
: ARG$  2 + CELLS   ARGV +   @ Z$@ ;

: PrintWithEscapes  OVER + SWAP ?DO
    I C@
    DUP &\ = IF &\ EMIT THEN
    DUP &( = IF &\ EMIT THEN
    DUP &) = IF &\ EMIT THEN
    EMIT
LOOP ;

\ ################# DATA ##########################################

\ Appendix letter, A..Z.
VARIABLE CurrentAppendix    &@ CurrentAppendix !  \ Increment before use
: .Appendix    CurrentAppendix @ EMIT ;

\ Page number, through going, may be starting at the end of the file
\ to which we are going to append.
VARIABLE CurrentPage

\ Print the PAGE number in behalf of viewers as PostScript comment.
: PSPagePragma
    1 CurrentPage +!
    ." %%Page: "   .Appendix &- EMIT .   CurrentPage  @ . CR
;

\ The NAME of the current file.
CREATE FileName 2 CELLS ALLOT

\ The remaining CONTENT of the current file as a string.
CREATE FileContent 2 CELLS ALLOT

\ Get a file with NAME.
: GetNextFile
    2DUP FileName 2!   GET-FILE FileContent 2!   1 CurrentAppendix +! ;

\ Get the next LINE .
: Line   FileContent 2@  OVER IF ^J $S   2SWAP FileContent 2! THEN ;

\ Consume a line and print it at current position. Move to next line.
: PrintLine
    ." ("   Line PrintWithEscapes   ." )"
    0 NextLine .COORDINATE ." rmoveto gsave show grestore" CR ;

\ Output at POSITION a NUMBER of lines.
: MultLine >R
      2DUP .COORDINATE ." moveto 1 MM " CR
      R> 0 DO PrintLine LOOP
;

\ Print the header of page NUMBER .
: PageHeader
    DUP PSPagePragma
    ." (" 4 .R ." ) (Appendix " .Appendix &. EMIT SPACE
    FileName 2@ TYPE ." ) exch StartPage " CR
;

\ Print the SCREEN number of the screen at POSITION . Leave incremented SCREEN.
\ As a side effect the proper font for printing screens is set.
: ScreenNumber   0 NextLine 2 /  NEGATE D+ .COORDINATE
    ." moveto " C/L 3 / . ." MM (SCR #"
    DUP . ." ) TitleF show BodyFl" CR #Screens + ;

\ Draw a border around the screen at POSITION .
: DrawBorder ." .5 setgray" CR
    .COORDINATE ."  moveto "
    C/L 1 +          . ." CW mul 0 rlineto "
    NextLine         . ." 16.5 mul 0 exch rlineto "
    C/L 1 + NEGATE   . ." CW mul 0 rlineto " CR
                       ." closepath CW setlinewidth .5 setgray stroke 0 setgray" CR
;

\ Output a screen at POSITION .
: OneScreen    16 MultLine DrawBorder ;

\ Output a SCREEN and following at PAGE in two columns of screens.
\ Leave incremented SCREEN and PAGE.
: NextPage
    DUP PageHeader 1+ >R
    LeftColumn  2@ ScreenNumber
    LeftColumn  2@ #Screens 0 DO 2DUP OneScreen NextScreen - LOOP 2DROP
    RightColumn 2@ ScreenNumber
    RightColumn 2@ #Screens 0 DO 2DUP OneScreen NextScreen - LOOP 2DROP
    ." EndPage " CR
    R>
;

\ Output file given by NAME , in the next appendix, printed as screens.
: OutputScreens
    BlockLineStride
    GetNextFile
    1  BEGIN NextPage FileContent @ 0= UNTIL
    DROP
;

\ Output a column at POSITION unless empty.
: OneColumn   #Lines MultLine ;

: DrawBar RightColumn 2@
    NextLine 3 * - .COORDINATE ." CW sub moveto " CR
    0 NextLine #Lines 6 + * .COORDINATE
    ." CW sub rlineto CW 10 div setlinewidth .5 setgray stroke 0 setgray" CR
;

\ Output a PAGE in two columns. Leave incremented PAGE.
: NextPageSrc
    DUP PageHeader 1+ >R
    LeftColumn 2@ OneColumn
    DrawBar
    RightColumn 2@ OneColumn
    ." EndPage " CR R>
;

\ Output the file given by NAME in the next appendix in two columns.
: OutputFile
    FileLineStride
    GetNextFile
    1 BEGIN NextPageSrc   FileContent @ 0= UNTIL
    DROP
;

    154 CurrentPage !
\     Header              \ Select this if it must be independantly printed.
    FixupDVIps          \ Select this if it is an appendix to a DVIps output
    PreLude
    1 ARG$ OutputScreens
    ARGC 2 - 2 do I ARG$ OutputFile loop
    PostLude
