#!lina -s
\ Output a block file with name in ARG1 in a nice PostScript format.

REQUIRE ARGV
REQUIRE ^

\ ################# CONVENTIONS ###################################
\ Any LINE is a sc.
\ Any POSITION is in PostScript coordinates.
\ Any SCREEN is the number of a screen.

\ ################# DATA ##########################################

\ A coordinate pair.
: COORDINATE CREATE , , ;

\ Print a coordinate pair
: .COORDINATE  SPACE SWAP . . ;

\ Number of screens to put in a column
5 CONSTANT #Screens

\ Number of lines to put in a column (assembler file.)
117 CONSTANT #Lines

\ Page number, through goiing
VARIABLE CurrentPage

\ upper left coordinates of top screen of left column
2500 76961 COORDINATE LeftColumn

\ upper left coordinates of top screen of right column
31550 76961 COORDINATE RightColumn

\ Stride between screens, vertical
15229 CONSTANT NextScreen

\ Position of screen number w.r.t. screen
\ Position of first line w.r.t. screen
\ Stride between lines, vertical
CREATE LineStride -902 ,
\ Leave relative vector to next line position
: NextLine LineStride @ ;

\ Relative position of screen's lower right corner
28000 11640 COORDINATE ScreenSize

\ Line width
\ Font size of screens content characters
\ Font size of screen number and header

\ File name for the screens file.
: ScreensName   ARGV 3 CELLS + @ Z$@ ;
\ File name of the assembler source.
: AssemblerName   ARGV 4 CELLS + @ Z$@ ;

: PrintWithEscapes  OVER + SWAP ?DO
    I C@
    DUP &\ = IF &\ EMIT THEN
    DUP &( = IF &\ EMIT THEN
    DUP &) = IF &\ EMIT THEN
    EMIT
LOOP ;

\ Get the next LINE from the screens .
CREATE FileContent 2 CELLS ALLOT
: Line   FileContent 2@  OVER IF ^J $S   2SWAP FileContent 2! THEN ;

\ Output the next line from the screens to position, first place to next line.
: PrintLine
    ." ("   Line PrintWithEscapes   ." )"
    0 NextLine .COORDINATE ." rmoveto gsave show grestore" CR ;

\ Add the next line to the output at POSITION .

\ Add a screens content of the output at POSITION .
\ Return the screen WAS empty.

\ Draw a border around the screen at POSITION .
: DrawBorder ." .5 setgray" CR
    .COORDINATE ."  moveto "
    C/L 1 +          . ." CW mul 0 rlineto "
    NextLine         . ." 16.5 mul 0 exch rlineto "
    C/L 1 + NEGATE   . ." CW mul 0 rlineto " CR
                       ." closepath CW setlinewidth .5 setgray stroke 0 setgray" CR
;
\ Print the SCREEN number of the screen at POSITION . Leave incremented SCREEN.
\ As a side effect set the proper font for printing screens.
: ScreenNumber   0 NextLine 2 /  NEGATE D+ .COORDINATE
    ." moveto " C/L 3 / . ." M (SCR #"
    DUP . ." ) TitleF show BodyFl" CR #Screens + ;

\ Output at POSITION a NUMBER lines. Return it was EMPTY.
: MultLine >R
      2DUP .COORDINATE ." moveto 1 M " CR
      R> 0 DO PrintLine LOOP
;

\ Output a screen at POSITION unless empty. Return it was EMPTY.
: OneScreen    16 MultLine DrawBorder ;

\ Print the PAGE number in behalf of viewers as PostScript comment.
: PrintPageNumber
    1 CurrentPage +!
    ." %%Page: " TYPE . CurrentPage  @ . CR
;

\ Output a SCREEN and following at PAGE in two columns of screens.
\ Leave incremented SCREEN and PAGE and indication we MUST stop.
: NextPage
      DUP "A-" PrintPageNumber
      DUP ." (" 4 .R ." ) (Appendix A. forth.lab) exch StartPage " CR 1+ >R
      LeftColumn 2@ ScreenNumber
      LeftColumn 2@ #Screens 0 DO 2DUP OneScreen NextScreen - LOOP 2DROP
      RightColumn 2@ ScreenNumber
      RightColumn 2@ #Screens 0 DO 2DUP OneScreen NextScreen - LOOP 2DROP
      ." EndPage " CR
      R>
      FileContent @ 0=
;

\ Output some PostScript code to fixup the use of dictionary by ``DVIps''
\ By this proper usage ``gv'' can now view each page independantly,
\ from the ``DVIps'' output and from ``prsc'' output.
: FixupDVIps
"TeXDict begin
    { TeXDict begin bop }
    { eop end }
end

/eop exch def
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

\ Output the Prolog : PostScript code.
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
/M         { CW mul 0 rmoveto } def
/Centre    { dup stringwidth pop 2 div neg 0 rmoveto } def
/StartPage { /SavedPage save def
  1 100 div dup scale
  0 setgray TitleF 2500 80700 moveto show 32 M show
  BodyFs 0 setgray CW setlinewidth 1 setlinejoin } def
/EndPage   {showpage SavedPage restore } def
{ ForthDict begin StartPage } {EndPage end } % Only ones to be exported
end  % ForthDict
/EndPage exch def   /StartPage exch def
%%EndProlog
" TYPE ;


\ Output the postlude : PostScript code.
: PostLude
"%%Trailer
%%EOF
" TYPE ;


\ Output the screens.
: OutputScreens
    ScreensName GET-FILE FileContent 2!
    0 1  BEGIN NextPage UNTIL
    1- PostLude DROP
;

: Interlude
." %%Appendix B
%%Endinterlude
"
-625 LineStride ! ;



\ Output a column at POSITION unless empty.
: OneColumn   #Lines MultLine ;

: DrawBar RightColumn 2@
        NextLine 3 * - .COORDINATE ." CW sub moveto " CR
        0 NextLine #Lines 6 + * .COORDINATE
        ." CW sub rlineto CW 10 div setlinewidth .5 setgray stroke 0 setgray" CR
;

\ Output a PAGE in two columns.
\ Leave incremented PAGE and indication we MUST stop.
: NextPageSrc
      DUP "B-" PrintPageNumber
      DUP ." (" 4 .R ." ) (Appendix B. assembler) exch StartPage " CR 1+ >R
      LeftColumn 2@ OneColumn
      DrawBar
      RightColumn 2@ OneColumn
      ." EndPage " CR
      R>
      FileContent @ 0=
;

\ Output the file.
: OutputFile
    AssemblerName GET-FILE FileContent 2!
    Interlude
    1 BEGIN NextPageSrc UNTIL
    1- PostLude DROP
;

    0 CurrentPage !
\     Header              \ Select this if it must be independantly pri
    FixupDVIps          \ Select this if it is an appendix to a DVIps
    PreLude
    OutputScreens
    OutputFile
