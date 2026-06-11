     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/Title 'Convert Payroll Class Code & Extension to title case'
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
??   H* BNDDIR('SBNDDIR')
     H/define CONVERT
     H*****************************************************************
    H*                                                               *
    H*    PROGRAM DESCRIPTION:                                       *
    H*      This program converts the Payroll Class Code descriptions*
    H*      and Extension description to Titlecase.                  *
    H*      Files updated: WMPRP, WMPXP, WMPRPE & WMPXPE             *
    H*                                                               *
    H*    CALLED FROM:                                               *
    H*      Can be called when needed.                               *
    H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* CREATED: 04/01/14  C:242 Support: 85231                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************

     FWMPRP     uf   e             disk    rename(ppr01:ppr0p)
     FWMPXP     uf   e             disk    rename(ppx01:ppx0p)
     FWMPRPE    uf   e             disk    rename(ppr01e:ppr0pe)
     FWMPXPE    uf   e             disk    rename(ppx01e:ppx0pe)

     D syRunSQL        PR                  ExtPgm('SYRUNSQL')
     D  stmt                        550    const

     D TitleCase       PR           300    opdesc
     D  inText                      300    options(*varsize) const

     D*entry
     D CVTWMPRP2       PR                  ExtPgm('CVTWMPRP2')

     D CVTWMPRP2       PI
      /Free
       Dou %eof(WMPRP);
          Read WMPRP;
          If not %eof(WMPRP);
             PRDESC = TitleCase(PRDESC);
             Update ppr0p;
          Endif;
       Enddo;

       Dou %eof(WMPXP);
          Read WMPXP;
          If not %eof(WMPXP);
             PXDESC = TitleCase(PXDESC);
             Update ppx0p;
          Endif;
       Enddo;

       Dou %eof(WMPRPe);
          Read WMPRPe;
          If not %eof(WMPRPe);
             PREXDS = TitleCase(PREXDS);
             Update ppr0pe;
          Endif;
       Enddo;

       Dou %eof(WMPXPe);
          Read WMPXPe;
          If not %eof(WMPXPe);
             PXEXDS = TitleCase(PXEXDS);
             Update ppx0pe;
          Endif;
       Enddo;

       syRunSQL('select PRDESC from WMPRP');
       syRunSQL('select PXDESC from WMPXP');

       *inlr = *on;
       Return;
      /End-free

     P*====================================================================
     P* TitleCase - translates a character field to upper/lower case
     P*  Parameter 1 - character field (any length)
     P*  Returns - character field (length of parameter 1)
     P*====================================================================
     PTitleCase        B                   export
     DTitleCase        PI           300    opdesc
     D  inText                      300    options(*varsize) const

     D i               S             10I 0
     D j               S             10I 0
     D char            S              1A
     D prevChar        S              1A
     D size            s             10I 0
     D text            s                   like(inText)

     D lower           C                   Const('abcdefghijklmnopqrstuvwxyz')
     D UPPER           C                   Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

      /Free
       text = inText;
       size = %size(text);

       For i = 1 to size;
          prevChar = Char;
          char = %SubSt(text : i : 1);
          If prevChar >= 'A' and PrevChar <= 'Z'
           and char >= 'A' and char <= 'Z';
              %SubSt(text : i : 1) = %XLate(UPPER : lower : char);
          EndIf;
       EndFor;

       Return text;

      /End-Free

     PTitleCase        E
