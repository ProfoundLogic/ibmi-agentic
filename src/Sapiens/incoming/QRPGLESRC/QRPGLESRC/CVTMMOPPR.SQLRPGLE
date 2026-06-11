     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/Title 'Convert description (OPMNTT) to title case'
     H DFTACTGRP(*NO) ACTGRP(*NEW)

     FMMOPP     uf   e             disk    rename(pop01:pop0p)

     D syRunSQL        PR                  ExtPgm('SYRUNSQL')
     D  stmt                        550    const

     D GetLastChar     Pr            10i 0
     D  inText                             like(OPMNTT) const
     D  inChar                        1    const
     D  inFirstChar                  10i 0 const
     D  inLastChar                   10i 0 const
     D KeepCaps        Pr             1n
     D  inText                             like(OPMNTT) const
     D  firstChar                    10i 0 const
     D  lastChar                     10i 0
     D TitleCase       PR                  like(OPMNTT)
     D  inText                             like(OPMNTT) const

     D/copy sPrString

     D*entry
     D CVTMMOPPR       PR                  ExtPgm('CVTMMOPPR')

     D CVTMMOPPR       PI

      /Free
       Dou %eof(MMOPP);
          Read MMOPP;
          If not %eof(MMOPP);
             OPMNTT = TitleCase(OPMNTT);
             Update pop0p;
          Endif;
       Enddo;

       syRunSQL('select OPMNTT from MMOPP');

       *inlr = *on;
       Return;
      /End-free


     P GetLastChar     b
     D GetLastChar     PI            10i 0
     D  inText                             like(OPMNTT) const
     D  inChar                        1    const
     D  inFirstChar                  10i 0 const
     D  inLastChar                   10i 0 const

     d position        s             10i 0
      /Free
       position = %scan(inChar:inText:inFirstChar);
       If position < inLastChar
        and position <> 0;
          Return position;
       Endif;
       Return inLastChar;
      /End-free
     P GetLastChar     e


     P KeepCaps        b
     D KeepCaps        PI             1n
     D  inText                             like(OPMNTT) const
     D  firstChar                    10i 0 const
     D  lastChar                     10i 0

     D size            s             10I 0
     D length          s             10I 0
     D text            s                   like(OPMNTT)
     D blank40         s             40    inz

      /Free
       text = Upper(inText);
       size = %size(text);
       //If %scan(' ':text:firstChar) <> 0;
       //   %subst(text:%scan(' ':text:firstChar)) = blank40;
       //Endif;
       lastChar = size;
       lastChar = GetLastChar(text:'(':firstChar:lastChar);
       lastChar = GetLastChar(text:'-':firstChar:lastChar);
       lastChar = GetLastChar(text:'/':firstChar:lastChar);
       lastChar = GetLastChar(text:' ':firstChar:lastChar);
       If lastChar < size;
          %subst(text:lastChar) = blank40;
       Endif;

       Select;
       When firstChar + 3 <= size
        and %scan('EDI':%subst(text:firstChar:3)) <> 0
        and %scan('EDIT':%subst(text:%scan('EDI':text:firstChar):4)) = 0
        and %scan('EDIC':%subst(text:%scan('EDI':text:firstChar):4)) = 0;
          lastChar = firstChar + 3;
          Return *on;
       When firstChar + 4 <= size
        and %scan('WCMS':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When firstChar + 3 <= size
        and %scan('CMS':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When firstChar + 4 <= size
        and %scan('WCMC':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When firstChar + 3 <= size
        and %scan('EOB':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When firstChar + 4 <= size
        and %scan('EOMB':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When firstChar + 5 <= size
        and %scan('DARTS':text:firstChar) <> 0;
          lastChar = firstChar + 5;
          Return *on;
       When %scan('VDARTS':text:firstChar) <> 0;
          lastChar = firstChar + 6;
          Return *on;
       When %scan('DTM':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('A/R':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('P/R':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('S/R':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('PPR':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('BSI':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('PBR':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When firstChar + 3 <= size
        and %scan('EFT':text:firstChar) <> 0
        and firstChar >= 3
        and %scan('THEFT':%subst(text:%scan('EFT':text:firstChar)-2:5)) = 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('SS#':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('SSN':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When firstChar + 3 <= size
        and %scan('LOB':text:firstChar) <> 0
        and %scan('GLOBAL':%subst(text:%scan('LOB':text:firstChar)-1:6)) = 0;
          lastChar = firstChar + 3;
          Return *on;
       When firstChar + 3 <= size
        and %scan('PPO':text:firstChar) <> 0
        and %scan('SUPPORT':%subst(text:%scan('PPO':text:firstChar)-2:7)) = 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('H/R':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('CPT':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('ERM':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('DCI':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('IRS':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('DWC':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('NOI':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('HBA':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('ICD':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('NDC':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('XX ':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('MC ':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('CA ':text:firstChar) <> 0
        or  %scan('CA-':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('FL ':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('IL ':text:firstChar) = 1;
          lastChar = firstChar + 2;
          Return *on;
       When firstChar + 2 <= size
        and %scan('IL ':text:firstChar) <> 0
        and %scan('TAIL':%subst(text:%scan('IL ':text:firstChar)-2:4)) = 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('MA ':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When firstChar + 2 <= size
        and %scan('MS ':text:firstChar) <> 0
        and %scan('TEMS':%subst(text:%scan('MS ':text:firstChar)-2:4)) = 0
        and %scan('ORMS':%subst(text:%scan('MS ':text:firstChar)-2:4)) = 0
        and %scan('AIMS':%subst(text:%scan('MS ':text:firstChar)-2:4)) = 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('MS-':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('TX ':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('WC ':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('UR ':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('CM':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When firstChar + 2 <= size
        and %scan('ID ':text:firstChar) <> 0
        and %scan('PAID':%subst(text:%scan('ID ':text:firstChar)-2:4)) = 0
        and %scan('VOID':%subst(text:%scan('ID ':text:firstChar)-2:4)) = 0
        and %scan('VALID':%subst(text:%scan('ID ':text:firstChar)-2:5)) = 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('FS':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       When %scan('WCPOLS':text:firstChar) <> 0;
          lastChar = firstChar + 6;
          Return *on;
       When %scan('WCSTATS':text:firstChar) <> 0;
          lastChar = firstChar + 7;
          Return *on;
       When %scan('WCSTAT':text:firstChar) <> 0;
          lastChar = firstChar + 6;
          Return *on;
       When %scan('DMEPOS':text:firstChar) <> 0;
          lastChar = firstChar + 6;
          Return *on;
       When %scan('RVU':text:firstChar) <> 0;
          lastChar = firstChar + 3;
          Return *on;
       When %scan('OPPS':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When %scan('GPCI':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When %scan('NCCI':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When %scan('DASD':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When %scan('IPPS':text:firstChar) <> 0;
          lastChar = firstChar + 4;
          Return *on;
       When %scan('PC':text:firstChar) <> 0;
          lastChar = firstChar + 2;
          Return *on;
       Other;
          Return *off;
       Endsl;
      /End-free

     P KeepCaps        e


     P TitleCase       B
     D TitleCase       PI                  like(OPMNTT)
     D  inText                             like(OPMNTT) const

     D i               S             10I 0
     D j               S             10I 0
     D char            S              1A
     D prevChar        S              1A
     D size            s             10I 0
     D text            s                   like(OPMNTT)

     D lower           C                   Const('abcdefghijklmnopqrstuvwxyz')
     D UPPER           C                   Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

      /Free
       text = inText;
       size = %size(text);

       For i = 1 to size;
          If KeepCaps(text:i:j);
             i = j;
             iter;
          Endif;
          prevChar = Char;
          char = %SubSt(text : i : 1);
          If prevChar >= 'A' and PrevChar <= 'Z'
           and char >= 'A' and char <= 'Z'
           or prevChar = '''';
              %SubSt(text : i : 1) = %XLate(UPPER : lower : char);
          EndIf;
       EndFor;

       Return leftJstify(text);

      /End-Free

     P TitleCase       E



