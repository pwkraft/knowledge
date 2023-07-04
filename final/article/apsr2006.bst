% BibTeX bibliography style `apsr2006'
% Version 1.10
% 2009 May 19
%
% ACKNOWLEDGEMENT
%   This document is a modified version of John G. Bullock's
%   apsr2001.bst, which was based on Peter Williams' apsr.bst,
%   which was in turn based on alpha.bst.
%
% MODIFIED BY
%   John G. Bullock, john.bullock@aya.yale.edu
%
% INSTALLATION INSTRUCTIONS
%   These instructions presume that you are already up and running
%   with your previous BibTeX style file of choice, e.g., apsr.bst.
%
%   Find your local copy of apsr.bst.  (On my hard drive, it's in
%   c:/localtexmf/bibtex/bst/harvard/.)  Put this file in the same
%   directory.  Update your file name database.  (If you use MiKTeX,
%   this just entails opening "MiKTeX Options" and clicking "Refresh
%   Now".)
%        In the preambles to your LaTeX files, specify
%
%        \bibliographystyle{apsr2006}
%
%   instead of
%
%        \bibliographystyle{apsr}
%
%   and recompile your LaTeX (.tex) files as you would whenever you
%   add a new citation to them.  In most cases, that's all that you
%   need to do.
%
%   If the format of your bibliography is unchanged, try deleting
%   all .bbl files from your working directory.  Then recompile your
%   .bib and .tex files as you normally would.
%
% REASON FOR EXISTING
%   Current APSR style rules are set out in the 2006 edition
%   of the APSA's Style Manual for Political Science.  The
%   original apsr.bst goes a long way toward meeting those
%   rules.  But there are at least 18 ways in which it falls
%   short and produces rather ugly bibliographies.  This file
%   fixes most of the problems.  It also permits the creation of
%   syllabi with the bibentry package for LaTeX; that package is
%   not compatible with apsr.bst.
%
% CHANGELOG FOR apsr2006.bst
%
%   Version 1.10, 2009 May 19
%   --Added the "monthday" field for unpublished articles.  The idea
%     is that a working paper may be updated several times over the
%     course of a year.  In the text, only the year should be cited
%     (e.g., "Imai 2009").  But in the list of references, it is
%     helpful to cite the date as well ("2009 April 27").  In the past,
%     it was not easy to cite just the year in the text but the year
%     and the month in the references.  Now it is: just add an entry
%     in the "monthday" field for the relevant unpublished-paper entry
%     in your .bib file.  For example,
%
%           @UNPUBLISHED{Imai_Identification_2009,
%              AUTHOR       = {Kosuke Imai and Luke Keele and Teppei Yamamoto},
%              TITLE        = {Identification, Inference, and Sensitivity Analysis for Causal Mediation Effects},
%              YEAR         = {2009},
%              MONTHDAY     = {April 27},
%              NOTE         = {Princeton University.  Manuscript.},
%           }
%
%   Version 1.09, 2009 April 26
%   --Revised format.translator so that it appropriately formats
%     entries for which there are multiple translators.  To do this,
%     I copied code from format.authors into format.translators.
%
%   Version 1.08, 2009 January 17
%   --Added format.names.roman numerals so that names other than
%     the first that include a roman numeral (e.g., "Robert Anderson IV")
%     will not contain a comma just before the roman numeral.
%
%   Version 1.07, 2008 December 26
%   --Updated format.final_apostrophe so that it handles titles
%     in which the last three characters are ".'" as well as
%     "?".
%
%   Version 1.06, 2008 September 27
%   --Modified the {write.url} and {article} functions to make
%     URL printing in bibliographies a little more passable.  It
%     is still badly handled -- this is a LaTeX problem, not
%     something that I can fix -- but URLs are now a little more
%     likely to be correctly formatted, and substantially less
%     likely to wreck your bibliography.  [Note: URL printing is
%     only activated in apsr2006_syllabi.bst.]
%
%   Version 1.05, 2008 June 7
%   --Added the "congressional" entry type for U.S. Congress
%     documents.
%
%   Version 1.04, 2008 May 28
%   --Added format.final_apostrophe function so that add.period$
%     won't screw up the formatting of titles that end in single
%     right quotation marks, e.g., ``Foreign Affairs and Issue
%     Voting: Do Presidential Candidates `Waltz Before a Blind
%     Audience?'""
%
%   Version 1.03, 2008 March 29
%   --Added a "translator" field for articles, just like the one
%     for books.
%
%   Version 1.02, 2008 January 17
%   --Added an "ICPSR" entry type that produces ICPSR citations and
%     references according to the Style Manual guidelines.  It takes
%     the form
%   @ICPSR{*,
%     author             = {*},
%     year               = {*},
%     computerfile       = {*},
%     computerfileyear   = {*},
%     number             = {*},
%     producer           = {*},
%     produceraddress    = {*},
%     produceryear       = {*},
%     distributor        = {*},
%     distributoraddress = {*},
%     distributoryear    = {*},
%   }
% It tries to degrade gracefully: only "author", "year", "computerfile",
% and "producer" are required; if the other fields are missing, the entry
% should still format correctly.  If "distributor" is missing, ICPSR and
% distributor address="Ann Arbor, MI" are assumed.  If "distributoryear"
% is not supplied, it assumes the value of "year".
%
%   Version 1.01, 2008 January 09
%   --Improved handling of the "originalyear" field, which is mainly
%     used for reprints of articles and books.  It can now be used
%     in article entries as well as book entries.  The originalyear
%     now automatically appears in brackets for citations as well as
%     references.
%
%   Version 1.0,  2007 December 31
%   --Fixed the way that "n.d." and "N.d." are used for forthcoming
%     work.  The 2006 APSA Style Manual (pages 17, 27) calls for
%     in-text citations to have "n.d." and references to have the
%     "N.d."  But the in-text citations produced by apsr2001.bst had
%     "N.d."  I altered output.bibitem to take care of this.  Now, if
%     a chapter in a collection is forthcoming but the publication
%     year is uncertain, just leave the year field blank in the
%     BibTeX entry.  This style file will handle the rest.
%
% CHANGELOG FOR apsr2001.bst
%   Version 1.141, 2007 December 15
%   --fixed a bug introducted in v1.13: et al. was never used for
%     in-text citations.
%
%   Version 1.14, 2007 November 25:
%   --modified output.nonnull to eliminate the second space between
%     year and article/book title in bibliographies.  It didn't look
%     good and isn't found in journal bibliographies.
%
%   Version 1.13, 2007 November 21:
%   --modified output.bibitem to allow use of this .bst file with
%     the bibentry package for LaTeX.  The bibentry package is very
%     useful for writing syllabi.

%   Version 1.12, 2007 August 1:
%   --Changed part of format.vol.num.pages from
%
%       { duplicate$ empty$
%        { pop$ " " * format.pages }      % { pop$ format.pages }
%        { ": " * pages * }
%       if$
%       }
%
%     to
%
%       { duplicate$ empty$
%        %{ pop$ " " * format.pages }      % { pop$ format.pages }
%        { ": " * pages * }
%        { ": " * pages * }
%       if$
%       }
%
%   This way, if an article has neither volume nor number information,
%   the journal title is immediately followed by a colon, a space, and
%   the page range.  (Before, it was a space and "pp. xx-yy.")
%
%   Version 1.11, 2007 May 2:
%   --Fixed a small problem with the installation instructions.
%     Thanks to Will Bullock for reporting the problem.
%
%   Version 1.10, 2007 April 9:
%   --Added new entry "COURT_CASE" type for U.S. court decisions.
%     Required fields: title, year, volume, source, firstpage.
%     Optional field: district.
%     Example:
%        @COURT_CASE{Lessard_Schmidt_1972,
%        TITLE        = {Lessard {\textnormal{v.}} Schmidt},
%        YEAR         = {1972},
%        volume       = {349},
%        source       = {F. Supp.},
%        firstpage    = {1078},
%        district     = {E.D. Wisc.}
%        }
%
%   Version 1.03, 2007 March 26:
%   --Fixed a small bug in format.pages that caused entries of
%     the "article"  type to be incorrectly formatted if they
%     did not have a volume field.
%
%   Version 1.02, 2007 February 20:
%   --Added "originalyear" field for older books.  When included
%     in a .bib file, it will cause the original year of a book
%     to be printed before the "year" field, as with
%
%        Leibniz, Gottfried.  [1765] 1996.  New Essays on Human
%        Understanding.  Trans. Peter Remnant and Jonathan
%        Bennett.  Cambridge: Cambridge University Press.
%
%     The original year will not be automatically included in the
%     citation that appears in the body of the paper.  To fix that,
%     you can insert it manually, e.g.,
%     "(\citeauthor{Leibniz_Essays} [1765] \citeyear{Leibniz_Essays})."
%     (See APSA Style Manual 2001, 32.)
%
%   Version 1.01, 2006 September 19:
%   --Restored en dashes between page numbers of cited articles.
%
%   Version 1.0,  2006 May 5:
%   --Added 'translator' field for books.  I don't know how robust
%     it is, i.e., how it will work with complicated bibliography
%     entries of the "book" type.  For multiple translators, I may
%     want to borrow the code in format.authors and format.rem.names.
%
%   --When an article cites two articles written by the same
%     author in the same year, a letter is typically appended
%     to distinguish them, e.g., Converse 1964a vs. Converse 1964b.
%     apsr.bst italicized these appended letters.  apsr2001.bst
%     does not: I edited {reverse.pass} to prevent the needless
%     italicization.
%
%   --Changed FUNCTION {article} and spacing in format.vol.num.pages
%     so wire reports from news agencies (e.g., the Associated Press)
%     will format OK.
%
%   --Often, we want to cite chapters that appear in edited volumes.
%     Typically, the chapters have the INCOLLECTION or INPROCEEDINGS
%     entry type.  If more than one chapter is to be cited from the
%     same volume, the most efficient thing to do is to cross-reference
%     (with the CROSSREF directive) a PROCEEDINGS entry that contains
%     all of the information common to all chapters: the book title,
%     editor(s) of the collection, publication year, and so forth.  But
%     cross-referencing wasn't working.  Now it is.
%
%   --Changed the proceedings function, which serves for edited volumes.
%     It now takes "booktitle" as well as "title."  If either is missing,
%     a BibTeX warning will be generated; like many BibTeX warnings, it
%     can be ignored.  This was done to make cross-referencing work; see
%     above.
%
%   --Changed the conferences function, which is also the inproceedings
%     function, to make it more workable for real conference papers.
%     (See APSA Style Manual 2001, 32.)
%
%   --Per the Style Manual, stopped n.dashifying the dashes between
%     page numbers.
%
%   --Changed format.vol.num.pages to degrade gracefully, as required by
%     APSA Style Manual 2001, 31.  Uses journal month/season if available,
%     followed by journal number.  Also added a space between the
%     volume number and the opening parenthesis of the month/season/issue
%     number; this, too, is in keeping with the Manual.
%
%   --If a journal number is printed in the bibliography, the space
%     between the volume number and the journal number will be
%     non-breaking, so that no line in the bibliography will begin
%     with a journal number.  E.g., an entry will never look like
%
%          Smith, John.  "The Article."  American Political Science Review 10
%               (2): 147-57.
%
%   --Added format.rem.names.editors, which is based on format.rem.names.
%     Also changed format.editors and format.editors.reverse to refer
%     to the new function rather than to format.rem.names.
%         This was done so that, in bibliographies, incollection
%     entries with two editors would appear with no comma (e.g.,
%     "ed. John Smith and Jane Doe") while entries with three or more
%     editors would have the appropriate final comma (e.g.,
%     "ed. John Smith, Jane Doe, and Paul Sniderman").
%
%   --Changed format.vol.num.pages to allow for a space between the
%     colon and the page numbers.
%
%   --Changed FUNCTION {incollection} so that chapter pages aren't
%     entered into bibliography.  (APSA style guide 2001, 29-30:
%         pages are not required.)
%
%   --Changed FUNCTION {phdthesis} to accord with APSA style guide:
%     Removed location printing and changed "PhD thesis" to "Ph.D.
%     diss."
%
%   --In format.lab.names.full, changed
%               { " \harvardand\ " * t * }
%            to
%                 { numnames #2 >
%                   { ", \harvardand\ " * t * }
%                 { " \harvardand\ " * t * }
%              if$
%            }
%
%   --Added ' ' after 'In' at format.in.ed.booktitle.
%     In format.rem.names, added ", " to the "harvard and" line.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ENTRY
  { address
    author
    booktitle
    branch             % for Congressional reports
    chapter
    committee          % for Congressional reports
    computerfile       % for ICPSR
    computerfileyear   % for ICPSR
    congress           % for Congressional reports
    distributor        % for ICPSR
    distributoraddress % for ICPSR
    distributoryear    % for ICPSR
    district
    edition
    editor
    firstpage
    howpublished
    institution
    journal
    key
    month
    monthday
    note
    number
    organization
    originalyear
    pages
    producer        % for ICPSR
    produceraddress % for ICPSR
    produceryear    % for ICPSR
    publisher
    school
    series
    session         % for Congressional reports
    source
    title
    titletemp
    translator
    type
    URL
    URLdate
    volume
    year
  }
  { field.used etal.allowed etal.required} %%%XXX change
  { extra.label sort.label list.year }

INTEGERS { output.state before.all mid.sentence after.sentence after.block int1 int2}

FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
}

STRINGS { s t f originalyearstring}

FUNCTION {output.nonnull}
{ 's :=
  output.state mid.sentence =
    { " " * write$ }
    { output.state after.block =
    { add.period$ write$
      newline$
      % "\newblock " write$   % Removed 2007 11 25: it adds a second space between year and book/article title in bibliography, which isn't good
    }
    { output.state before.all =
        'write$
        { add.period$ " " * write$ }
      if$
    }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {output}
{ duplicate$ empty$
    'pop$
    'output.nonnull
  if$
}

FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

FUNCTION {item.check}
{ 't :=
  empty$
    { "empty " t * " in " * cite$ * warning$ }
    { skip$ }
  if$
}

FUNCTION {fin.entry}
{ add.period$
  write$
  newline$
}

FUNCTION {new.block}
{ output.state before.all =
    'skip$
    { after.block 'output.state := }
  if$
}

FUNCTION {not}
{   { #0 }
    { #1 }
  if$
}

FUNCTION {and}
{   'skip$
    { pop$ #0 }
  if$
}

FUNCTION {or}
{   { pop$ #1 }
    'skip$
  if$
}

FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}

FUNCTION {emphasize}
{ duplicate$ empty$
    { pop$ "" }
    { "{\em " swap$ * "}" * }
  if$
}

FUNCTION {embolden}
{ duplicate$ empty$
    { pop$ "" }
    { "{\bf " swap$ * "}" * }
  if$
}

% Material for Tristan Miller's find.replace BibTeX function:
% see http://www.nothingisreal.com/biblet/.
INTEGERS{ l }
FUNCTION{ string.length }
{
  #1 'l :=
  {duplicate$ duplicate$ #1 l substring$ = not}
    {l #1 + 'l :=}
  while$
  pop$ l
}
STRINGS{replace find text}
INTEGERS {find_length}
FUNCTION {find.replace}
{ 'replace :=
  'find :=
  'text :=
  find string.length 'find_length :=
  ""
    { text empty$ not }
    { text #1 find_length substring$ find =
        { replace *
      text #1 find_length + global.max$ substring$ 'text :=
        }
    { text #1 #1 substring$ *
      text #2 global.max$ substring$ 'text :=
    }
      if$
    }
  while$
}
% End of material for Tristan Miller's find.replace BibTeX function:

FUNCTION {quote}
{ duplicate$ empty$
    { pop$ "" }
    { "``" swap$ * "''" * }
  if$
}

FUNCTION {write.url}
{ URL empty$
    { skip$ }
    { URL add.period$ write$ newline$ }
      %{ "\harvardurl{" URL * "}" * add.period$ write$ newline$}
      %{ "\newline\harvardurl{" URL * "}" * write$ newline$ }
  if$
}
  % { URL empty$
  %     { skip$ }
  %     { "\harvardurl{" URL * "}" * add.period$ write$ newline$}    %{ "\newline\harvardurl{" URL * "}" * write$ newline$ }
  %   if$
  % }

INTEGERS { nameptr namesleft numnames }

FUNCTION {format.rem.names.editors} {
  's :=
  'f :=
  #2 'nameptr :=
  s num.names$ 'numnames :=
  numnames #1 - 'namesleft :=
  { namesleft #0 > }
    { s nameptr f format.name$ 't :=
      nameptr #1 >
        { namesleft #1 >
            { ", " * t * }
            { t "others" =
                { " et~al." * }
                { % begin if loop for editor commas in citations
                  numnames #2 >
                    { ", \harvardand\ " * t * }
                    { " \harvardand\ " * t * }
                  if$
                } % end if loop for editor commas in citations
            if$
            }
        if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {format.rem.names} {
   's :=
   'f :=
   #2 'nameptr :=
   s num.names$ 'numnames :=
   numnames #1 - 'namesleft :=
   { namesleft #0 > }
     { s nameptr f format.name$ 't :=
       nameptr #1 >
         { namesleft #1 >
             { ", " * t * }
             { t "others" =
                 { " et~al." * }
                 { ", \harvardand\ " * t * }
             if$
             }
         if$
         }
         't
       if$
       nameptr #1 + 'nameptr :=
       namesleft #1 - 'namesleft :=
    }
   while$
 }

STRINGS  { nametocheck }
INTEGERS { namestartlength nameendlength }
FUNCTION {format.names.roman.numerals}
{ 'nametocheck :=                                                % nametocheck     = text passed to this function
  nametocheck string.length 'nameendlength :=                    % nameendlength   = total length of passed text
  nametocheck string.length #4 - 'namestartlength :=             % namestartlength = position three characters before string end
  nametocheck namestartlength nameendlength substring$ ", III" = % if the last characters are ", III"
    { nametocheck ", III"  "~III"  find.replace}                      % do this
    { nametocheck namestartlength nameendlength substring$ ", IV" =   % else if the last three characters are ", IV"
      { nametocheck ", IV"  "~IV"  find.replace}                          % do this
      { nametocheck ", II"  "~II"  find.replace}                          % else do this
      if$
    }
  if$
}

FUNCTION {format.authors}
{ author empty$
    { "" }
    { author #1 "{vv~}{ll}{, jj}{, ff}" format.name$
      "{ff }{vv~}{ll}{, jj}" author format.rem.names format.names.roman.numerals
    }
  if$
}




FUNCTION {format.editors}
{ editor empty$
    { "" }
    { editor #1 "{vv~}{ll}{, jj}{, ff}" format.name$
      "{ff }{vv~}{ll}{, jj}" editor format.rem.names.editors
      editor num.names$ #1 >
    { ", eds" * }
    { ", ed." * }
      if$
    }
  if$
}

FUNCTION {format.editors.reverse}
{ editor empty$
    { "" }
    { ", ed. "
      editor #1 "{ff }{vv~}{ll}{, jj}" format.name$ *
      "{ff }{vv~}{ll}{, jj}" editor format.rem.names.editors
    }
  if$
}

FUNCTION {format.originalyear}
{ originalyear empty$
    { "" }                              % if originalyear field is empty
    { "[" originalyear * "] " * } % { "\textrm{[}" originalyear * "\textrm{]} " * }       % if it isn't empty
    if$
}

STRINGS  { texttocheck }
INTEGERS { titlestartlength titleendlength }
FUNCTION {format.final_apostrophe}
{ 'texttocheck :=                                                  % text passed to this function
  texttocheck string.length 'titleendlength :=                     % total length of passed text
  texttocheck string.length #2 - 'titlestartlength :=              % three characters before
  texttocheck titlestartlength titleendlength substring$ ".'." =   % if the last three characters are ".'."
    { texttocheck ".'."  ".'"  find.replace}                             % do this
    { texttocheck "?'."  "?'"  find.replace}                             % else do this
  if$
}


%FUNCTION {format.final_apostrophe}
%{ "?'."  "?'"  find.replace
%}

FUNCTION {format.title}
{ title empty$
    { "" }
    { title add.period$ format.final_apostrophe }
  if$
}

FUNCTION {format.translator}
{ translator empty$
    { "" }                              % if translator field is empty
    %{ "Trans. " translator * ". " * }  % if it isn't empty
    { "Trans. " translator #1 "{ff }{vv~}{ll}{, jj}" format.name$ *
      "{ff }{vv~}{ll}{, jj}" translator format.rem.names.editors format.names.roman.numerals add.period$
    }
    if$
}

FUNCTION {n.dashify}
{ 't :=
  ""
    { t empty$ not }
    { t #1 #1 substring$ "-" =
    { t #1 #2 substring$ "--" = not
        { "--" *
          t #2 global.max$ substring$ 't :=
        }
        {   { t #1 #1 substring$ "-" = }
        { "-" *
          t #2 global.max$ substring$ 't :=
        }
          while$
        }
      if$
    }
    { t #1 #1 substring$ *
      t #2 global.max$ substring$ 't :=
    }
      if$
    }
  while$
}

FUNCTION {format.btitle}
{ title emphasize
}

FUNCTION {format.booktitle}
{ booktitle emphasize
}

FUNCTION {format.court_case.title}
{ " v. "  "\textnormal{ v. }"  find.replace
}

FUNCTION {tie.or.space.connect}
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$ * *
}

FUNCTION {either.or.check}
{ empty$
    'pop$
    { "can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}

FUNCTION {format.bvolume}
{ volume empty$
    { "" }
    { "Vol." volume tie.or.space.connect
      series empty$
    'skip$
    { " of " * series emphasize * }
      if$
      "volume and number" number either.or.check
    }
  if$
}

FUNCTION {format.number.series}
{ volume empty$
    { number empty$
    { series field.or.null }
    { output.state mid.sentence =
        { "number" }
        { "Number" }
      if$
      number tie.or.space.connect
      series empty$
        { "there's a number but no series in " cite$ * warning$ }
        { " {\em in} " * series quote * }
      if$
    }
      if$
    }
    { "" }
  if$
}

FUNCTION {format.edition}
{ edition empty$
    { "" }
    { output.state mid.sentence =
    { edition "l" change.case$ " ed." * }
    { edition "t" change.case$ " ed." * }
      if$
    }
  if$
}

INTEGERS { multiresult }

FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
    { #1 'multiresult := }
    { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}

FUNCTION {format.pages}
{ pages empty$
    { "" }     % if "pages" does not hold a value, do nothing
    { pages multi.page.check
    { "pp.~" pages n.dashify * }
    %{ "pp.~" pages * }
    { "p.~" pages * }
      if$
    }
  if$
}

FUNCTION {format.address}
{ address empty$
    { "" }
    { address ": " *
    }
  if$
}

FUNCTION {format.publisher}
{ publisher empty$
    { "" }
    { publisher }
  if$
}

FUNCTION {format.vol.num.pages}
%{ volume field.or.null
{ volume empty$
    {""}
    { " " * volume field.or.null }
  if$
  month empty$
    { number empty$
        'skip$ % do nothing if "number" field is empty
        { "~(" number * ")" * *
          volume empty$
            { "there's a number but no volume in " cite$ * warning$ }
            'skip$
          if$
        }
      if$
    }
    { " (" month * ")" * * }
  if$
  pages empty$
    'skip$
    { duplicate$ empty$
     %{ pop$ " " * format.pages }      % { pop$ format.pages }
     { ": " * pages * }
     { ": " * pages * }
    if$
    }
  if$
}



FUNCTION {format.chapter.pages}
{ chapter empty$
    'format.pages
    { type empty$
    { "chapter" }
    { type "l" change.case$ }
      if$
      chapter tie.or.space.connect
      pages empty$
    'skip$
    { ", " * format.pages * }
      if$
    }
  if$
}

FUNCTION {format.in.ed.booktitle}
{ booktitle empty$
    { "" }
    { editor empty$
    { " In " booktitle emphasize * }
    { " In " booktitle emphasize * format.editors.reverse * }
      if$
    }
  if$
}

FUNCTION {empty.misc.check}
{ author empty$ title empty$ howpublished empty$
  month empty$ year empty$ note empty$
  and and and and and
  key empty$ not and
    { "all relevant fields are empty in " cite$ * warning$ }
    'skip$
  if$
}

FUNCTION {format.thesis.type}
{ type empty$
    'skip$
    { pop$
      type "t" change.case$
    }
  if$
}

FUNCTION {format.tr.number}
{ type empty$
    { "Technical Report" }
    'type
  if$
  number empty$
    { "t" change.case$ }
    { number tie.or.space.connect }
  if$
}

FUNCTION {format.article.crossref}
{ key empty$
    { journal empty$
    { "need key or journal for " cite$ * " to crossref " * crossref *
      warning$
      ""
    }
    { "in {\em " journal * "\/} \cite{" * crossref * "}" *}
      if$
    }
    { "{\em in} \citeasnoun{" crossref * "}" * }
  if$

}

FUNCTION {format.book.crossref}
{ volume empty$
    { "empty volume in " cite$ * "'s crossref of " * crossref * warning$
      "in "
    }
    { "Vol." volume tie.or.space.connect
      " of " *
    }
  if$
  editor empty$
  editor field.or.null author field.or.null =
  or
    { key empty$
    { series empty$
        { "need editor, key, or series for " cite$ * " to crossref " *
          crossref * warning$
          "" *
        }
        { "{\em " * series * "\/} \cite{" * crossref * "}" *}
      if$
    }
    { " \citeasnoun{" * crossref * "}" * }
      if$
    }
    { " \citeasnoun{" * crossref * "}" * }
  if$
}

FUNCTION {format.incoll.inproc.crossref}
{ editor empty$
  editor field.or.null author field.or.null =
  or
    { key empty$
    { booktitle empty$
        { "need editor, key, or booktitle for " cite$ * " to crossref " *
          crossref * warning$
          ""
        }
        { "in {\em " booktitle * "\/}" * " \cite{" * crossref * "}" *}
      if$
    }
    { "{\em in} \citeasnoun{" crossref * "}" * }
      if$
    }
    %{ "In {\em " booktitle * ",\/} ed. " format.editors * " \citeasnoun{" crossref * "}" * }
    { " In " * format.booktitle *
     format.editors.reverse * ". " *
     format.address *
     format.publisher
      % publisher "publisher" output.check
    }
  if$

}

INTEGERS { len }

FUNCTION {chop.word}
{ 's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    's
  if$
}

INTEGERS { ind tsslen }

STRINGS { tss ret rss istr }

FUNCTION {replace.substring}{
  'rss :=
  'tss :=
  'istr :=
  "" 'ret :=
  tss text.length$ 'tsslen :=
  #1 'ind :=
    { istr ind tsslen substring$ "" = not }
    { istr ind tsslen substring$ tss =
        { ret rss * 'ret :=
          ind tsslen + 'ind :=
        }
        { ret istr ind #1 substring$ * 'ret :=
          ind #1 + 'ind :=
        }
      if$
    }
  while$
  ret
}

FUNCTION {format.lab.names.full}
{
 's :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr "{vv~}{ll}" format.name$ 't :=
      nameptr #1 >
    { namesleft #1 >
        { ", " * t * }
        { t "others" =
        { " et~al." * }
       { numnames #2 >
            { ", \harvardand\ " * t * }
            { " \harvardand\ " * t * }
        if$
       }
          if$
        }
      if$
    }
    't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}




FUNCTION {format.lab.names.abbr}
{ 's :=
  s num.names$ 'numnames :=
  numnames #1 >
    { numnames #3 >
    { s #1 "{vv~}{ll}" format.name$ " et~al." * }
    { s format.lab.names.full }
      if$
    }
    { s #1 "{vv~}{ll}" format.name$ }
  if$
}

INTEGERS { author.field editor.field organization.field title.field key.field booktitle.field }

FUNCTION {init.field.constants}
{ #0 'author.field :=
  #1 'editor.field :=
  #2 'organization.field :=
  #3 'title.field :=
  #4 'key.field :=
  #5 'booktitle.field :=
}

FUNCTION {make.list.label}
{ author.field field.used =
    { format.authors }
    { editor.field field.used =
        { format.editors }
        { organization.field field.used =
            { "The " #4 organization chop.word #3 text.prefix$ }
            { title.field field.used =
                { format.btitle }
                { key.field field.used =
                    { key #3 text.prefix$ }
                    { "Internal error :001 on " cite$ * " label" * warning$ }
                  if$
                }
              if$
            }
          if$
        }
      if$
    }
  if$
}

FUNCTION {make.full.label}
{ author.field field.used =
    { author format.lab.names.full }
    { editor.field field.used =
        { editor format.lab.names.full }
        { organization.field field.used =
            { "The " #4 organization chop.word #3 text.prefix$ }
            { title.field field.used =
                { format.btitle }
                { key.field field.used =
                    { key #3 text.prefix$ }
                    { "Internal error :001 on " cite$ * " label" * warning$ }
                  if$
                }
              if$
            }
          if$
        }
      if$
    }
  if$
}

FUNCTION {make.abbr.label} %%%XXX change
{
  etal.allowed
    { author.field field.used =
        { author format.lab.names.abbr }
        { editor.field field.used =
            { editor format.lab.names.abbr }
            { organization.field field.used =
                { "The " #4 organization chop.word #3 text.prefix$ }
                { title.field field.used =
                    { format.btitle }
                    { key.field field.used =
                        { key #3 text.prefix$ }
                        {"Internal error :001 on " cite$ * " label" * warning$ }
                      if$
                    }
                  if$
                }
              if$
            }
          if$
        }
      if$
    }
    { make.full.label }
  if$
}

FUNCTION {output.bibitem}
{ newline$
  "\bibitem[" write$
  make.abbr.label write$
  %make.full.label write$
  "(" write$
  %format.originalyear write$
  % relevant document seems to be bu1.bbl
  originalyear missing$
    {}
    {
       "{[}" originalyear * write$
       "{]} " write$
    }
  if$
  list.year "N.d." =
    { list.year "l" change.case$ write$ } % if year label would be written as "N.d.", write "n.d." instead
    { list.year write$ }
  if$
  ")]{" write$
  cite$ write$
  "}" write$
  newline$
  ""
  before.all 'output.state :=
}


FUNCTION {output.bibitem.congressional}
{ newline$
  "\bibitem[" write$
  "U.S. " write$
  branch write$
  "(" write$
  originalyear missing$
    {}
    {
       "{[}" originalyear * write$
       "{]} " write$
    }
  if$
  list.year "N.d." =
    { list.year "l" change.case$ write$ } % if year label would be written as "N.d.", write "n.d." instead
    { list.year write$ }
  if$
  ")]{" write$
  cite$ write$
  "}" write$
  newline$
  ""
  before.all 'output.state :=
}


FUNCTION {list.label.output}
{ make.list.label add.period$ " " * write$
}

FUNCTION {article}
{ output.bibitem
  list.label.output
  format.originalyear output
  list.year output.nonnull
  new.block
  author "author" item.check
  title.field field.used =
    { skip$ }
    { format.title quote "title" output.check }
  if$
  format.translator output
  crossref missing$
    { journal emphasize "journal" duplicate$ item.check
      year empty$
        { ". Forthcoming" * output new.block }
        { "" * format.vol.num.pages * output }  % { " " * format.vol.num.pages * output }
      if$
    }
    { format.article.crossref output.nonnull
      format.pages output
    }
  if$
  new.block
  note output
  fin.entry
  %new.block
  %write.url
  %new.block
}


FUNCTION {book}
{ output.bibitem
  list.label.output
  format.originalyear output
  list.year output.nonnull
  new.block
  author empty$
    { editor "author and editor" item.check }
    { crossref missing$
    { "author and editor" editor either.or.check }
    'skip$
      if$
    }
  if$
  title.field field.used =
    { skip$ }
    { format.btitle "title" output.check }
  if$
  new.block
  crossref missing$
    { format.bvolume output
      format.translator output
      format.number.series output
      format.edition output
      format.address output
      publisher "publisher" output.check
    }
    { format.book.crossref output.nonnull
      format.edition output
    }
  if$
  new.block
  note output
  fin.entry
  new.block
  %write.url
  %format.translator write$
  %newline$
}

FUNCTION {booklet}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  title.field field.used =
    { skip$ }
    { format.title quote "title" output.check }
  if$
  format.address output
  howpublished output
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {court_case}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  volume output
  source output
  firstpage output
  new.block
  district missing$
    { skip$ }
    { "(" district * ")" * output }
  if$
  fin.entry
  write.url
  empty.misc.check
}


FUNCTION {ICPSR}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  computerfileyear empty$
    { computerfile add.period$ output }
    { computerfile ", " * output
      computerfileyear output }
  if$
  number empty$
    { "[computer file]" add.period$ output }
    { "[computer file] (Study " number * ")." * output }
  if$
  "ICPSR version. " output
  produceraddress empty$
    { producer output }
    { produceraddress ": " * producer * output }
  if$
  produceryear empty$
    { "[producer]." output }
    { "[producer], " produceryear * add.period$ output }
  if$
  distributor empty$
    { "Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], " output }
    { distributoraddress empty$
        { distributor " [distributor], " * output }
        { distributoraddress ": " * distributor * output }
      if$ }
  if$
  distributoryear empty$
    { year output.nonnull }
    { distributoryear add.period$ output }
  if$
  %title.field field.used =
  %  { skip$ }
  %  { format.title quote "title" output.check }
  %if$
  new.block
  note output
  fin.entry
  write.url
}


FUNCTION {congressional}
{ %"U.S. Congress" author :=
  output.bibitem.congressional
  "U.S. Congress. " write$
  branch add.period$ " " * write$
  committee empty$
    {}
    { committee add.period$ " " * output }
  if$
  list.year output.nonnull
  new.block
  congress empty$
    {}
    { session empty$
      { congress "~Cong. " * output }
      { congress "~Cong.," * output
        session "~sess." * output }
      if$
    }
  if$
  note output
  fin.entry
  write.url
}




FUNCTION {inbook}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  author empty$
    { editor "author and editor" item.check }
    { crossref missing$
    { "author and editor" editor either.or.check }
    'skip$
      if$
    }
  if$
  title.field field.used =
    { skip$ }
    { format.btitle "title" output.check }
  if$
  new.block
  crossref missing$
    { format.bvolume output
      format.number.series output
      format.edition output
      format.address output
      publisher "publisher" output.check
    }
    { format.book.crossref output.nonnull
      format.edition output
    }
  if$
  format.chapter.pages "chapter and pages" output.check
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {incollection}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  title.field field.used =
    { skip$ }
    { format.title quote "title" output.check }
  if$
  author "author" item.check
  crossref missing$
    { format.in.ed.booktitle "booktitle" output.check
      new.block
      format.edition output
      format.bvolume add.period$ output
      format.number.series output
      format.address output
      format.publisher add.period$ output   % archived 2007 12 31: publisher "publisher" output.check
      year empty$
        { "Forthcoming." output }
        { }
      if$
    }
    { format.incoll.inproc.crossref output.nonnull
    }
  if$
  %format.chapter.pages output
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {inproceedings}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  title.field field.used =
    { skip$ }
    { format.title quote "title" output.check }
  if$
  author "author" item.check
  crossref missing$
    { booktitle empty$
        { skip$ }
       {
          format.in.ed.booktitle "booktitle" output.check
          new.block
        }
      if$
      format.bvolume output
      format.number.series output
      address empty$
        { organization output
          publisher output
        }
        { organization output
              %format.address output.nonnull
                    ", " * address output.nonnull
          %publisher output
        }
      if$
    }
    { format.incoll.inproc.crossref output.nonnull
    }
  if$
  format.pages output
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {conference} { inproceedings }

FUNCTION {manual}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  title.field field.used =
    { skip$ }
    { format.btitle "title" output.check }
  if$
  new.block
  format.edition output
  new.block
  author empty$
    { organization empty$
    { format.address output }
    'skip$
      if$
    }
    { format.address output
      organization output
    }
  if$
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {mastersthesis}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  author "author" item.check
  title.field field.used =
    { skip$ }
    { format.title "title" output.check }
  if$
  "Master's thesis" format.thesis.type output.nonnull
  school "school" output.check
  format.address output
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {misc}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  title.field field.used =
    { skip$ }
    { format.title quote output }
  if$
  howpublished add.period$ output
  new.block
  %note add.period$ output
  note missing$
    { }
    { note add.period$ output }
  if$
  %fin.entry
  write$
  newline$
  %write.url
  %URLdate empty$
  %  {  }
  %  { " (" URLdate * add.period$ ")" * write$ }
  %if$
  empty.misc.check
}


FUNCTION {phdthesis}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  author "author" item.check
  title.field field.used =
    { skip$ }
    { format.title quote "title" output.check }
  if$
  "Ph.D. diss." format.thesis.type output.nonnull
  school "school" output.check
  %format.address output
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {proceedings}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  %title.field field.used =
  %  { skip$ }
  %  { format.btitle "title" output.check }
  %if$
  booktitle.field field.used =
    { skip$ }
    { format.booktitle "booktitle" output.check }
  if$
  new.block
  format.bvolume output
  format.number.series output
  address empty$
    { editor empty$
    { skip$ }
    { organization output
    }
      if$
      publisher output
    }
    { editor empty$
    'skip$
    { organization output }
      if$
      format.address output.nonnull
      publisher output
    }
  if$
  new.block
  note output
  fin.entry
  write.url
}


FUNCTION {techreport}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  author "author" item.check
  title.field field.used =
    { skip$ }
    { format.title "title" output.check }
  if$
  format.tr.number output.nonnull
  institution "institution" output.check
  format.address output
  new.block
  note output
  fin.entry
  write.url
}

FUNCTION {unpublished}
{ output.bibitem
  list.label.output
  monthday empty$
    { list.year output.nonnull }
    { list.year " " * monthday * output.nonnull }
  if$
  new.block
  author "author" item.check
  title.field field.used =
    { skip$ }
    { format.title quote "title" output.check }
  if$
  note "note" output.check
  fin.entry
  new.block
  write.url
}

FUNCTION {unpublished_noquote}
{ output.bibitem
  list.label.output
  list.year output.nonnull
  new.block
  author "author" item.check
  title.field field.used =
    { skip$ }
    { format.title "title" output.check }
  if$
  note "note" output.check
  fin.entry
  %write.url
}

FUNCTION {default.type} { misc }

MACRO {jan} {"January"}
MACRO {feb} {"February"}
MACRO {mar} {"March"}
MACRO {apr} {"April"}
MACRO {may} {"May"}
MACRO {jun} {"June"}
MACRO {jul} {"July"}
MACRO {aug} {"August"}
MACRO {sep} {"September"}
MACRO {oct} {"October"}
MACRO {nov} {"November"}
MACRO {dec} {"December"}
MACRO {tcs} {"Theoretical Computer Science"}

MACRO {APSR} {"American Political Science Review"}

READ

EXECUTE {init.field.constants}

FUNCTION {sortify}
{ purify$
  "l" change.case$
}

FUNCTION {sortify.names}
{ " \harvardand\ " " " replace.substring
  " et~al." " zzz" replace.substring
  sortify
}

FUNCTION {author.key.label}
{ author empty$
    { key empty$
    { title.field 'field.used := }
    { key.field 'field.used := }
      if$
    }
    { author.field 'field.used := }
  if$
}

FUNCTION {author.editor.key.label}
{ author empty$
    { editor empty$
    { key empty$
        { title.field 'field.used := }
        { key.field 'field.used := }
      if$
    }
    { editor.field 'field.used := }
      if$
    }
    { author.field 'field.used := }
  if$
}

FUNCTION {author.key.organization.label}
{ author empty$
    { key empty$
    { organization empty$
        { title.field 'field.used := }
        { organization.field 'field.used := }
      if$
    }
    { key.field 'field.used := }
      if$
    }
    { author.field 'field.used := }
  if$
}

FUNCTION {editor.key.organization.label}
{ editor empty$
    { key empty$
    { organization empty$
        { title.field 'field.used := }
        { organization.field 'field.used := }
      if$
    }
    { key.field 'field.used := }
      if$
    }
    { editor.field 'field.used := }
  if$
}

FUNCTION {sort.format.title}
{ 't :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  sortify
  #1 global.max$ substring$
}

% in btxbst.doc, determines what sort of in-text citation label
% is made based on the entry type (e.g., book, article)
FUNCTION {calc.label} %%%XXX change
{
  make.abbr.label
  title.field field.used =
    { sort.format.title }
    { sortify.names }
  if$
  year field.or.null purify$ #-1 #4 substring$ sortify
  *
  'sort.label :=
}

FUNCTION {preliminaries} %%%XXX change
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.key.label
    { type$ "proceedings" =
    'editor.key.organization.label
    { type$ "manual" =
        'author.key.organization.label
        'author.key.label
      if$
    }
      if$
    }
  if$
  author.field field.used = %%%XXX change
    {
      author num.names$ #2 >
        { #1 }
        { #0 }
      if$
      'etal.required :=
    }
    {
      editor.field field.used =
        {
          editor num.names$ #2 >
            { #1 }
            { #0 }
          if$
        }
        { #0 }
      if$
      'etal.required :=
    }
  if$
  #1 'etal.allowed :=
}

FUNCTION {first.presort}
{ calc.label
  sort.label
  title.field field.used =
    { skip$ }
    { "    "
      *
      make.list.label sortify.names
      *
      "    "
      *
      title field.or.null
      sort.format.title
      *
    }
  if$
  #1 entry.max$ substring$
  'sort.key$ :=
}

ITERATE {preliminaries}

ITERATE {first.presort}

SORT

STRINGS { last.sort.label next.extra last.full.label}

INTEGERS { last.extra.num last.etal.allowed}

FUNCTION {initialize.confusion}
{ #0 int.to.chr$ 'last.sort.label :=
  #0 int.to.chr$ 'last.full.label :=
  #1 'last.etal.allowed :=
}

FUNCTION {confusion.pass}
{ last.sort.label sort.label =
    { last.etal.allowed
        { last.full.label make.full.label sortify.names =
            { skip$ }
            { #0 'etal.allowed :=
              #0 'last.etal.allowed :=
            }
          if$
        }
        { #0 'etal.allowed := }
      if$
    }
    { sort.label 'last.sort.label :=
      make.full.label sortify.names 'last.full.label :=
      #1 'last.etal.allowed :=
    }
  if$
}

EXECUTE {initialize.confusion}

ITERATE {confusion.pass}

EXECUTE {initialize.confusion}

REVERSE {confusion.pass}

FUNCTION {initialize.last.extra.num}
{ #0 int.to.chr$ 'last.sort.label :=
  "" 'next.extra :=
  #0 'last.extra.num :=
}

FUNCTION {forward.pass}
{ last.sort.label sort.label =
    { last.extra.num #1 + 'last.extra.num :=
      last.extra.num int.to.chr$ 'extra.label :=
    }
    { "a" chr.to.int$ 'last.extra.num :=
      "" 'extra.label :=
      sort.label 'last.sort.label :=
    }
  if$
}

FUNCTION {reverse.pass}
{ next.extra "b" =
    { "a" 'extra.label := }
    'skip$
  if$
  year empty$
    { "N.d." extra.label emphasize * 'list.year := }
    { year extra.label * 'list.year := }    % { year extra.label emphasize * 'list.year := }
  if$
  extra.label 'next.extra :=
}

ITERATE {first.presort}

SORT

EXECUTE {initialize.last.extra.num}

ITERATE {forward.pass}

REVERSE {reverse.pass}

FUNCTION {second.presort}
{ make.list.label
  title.field field.used =
    { sort.format.title }
    { sortify.names }
  if$
  "    "
  *
  list.year field.or.null sortify
  *
  "    "
  *
  title.field field.used =
    { skip$ }
    { title field.or.null
      sort.format.title
      *
    }
  if$
  #1 entry.max$ substring$
  'sort.key$ :=
}

ITERATE {second.presort}

SORT

FUNCTION {begin.bib}
{ preamble$ empty$
    'skip$
    { "\harvardpreambledefs{%" write$ newline$
      preamble$ write$ "}" write$ newline$
      "\harvardpreambletext{%" write$ newline$
      preamble$ write$ "}" write$ newline$ }
  if$
  "\begin{thebibliography}{xx}" write$ newline$
}

EXECUTE {begin.bib}

EXECUTE {init.state.consts}

ITERATE {call.type$}

FUNCTION {end.bib}
{ newline$
  "\end{thebibliography}" write$ newline$
}

EXECUTE {end.bib}
