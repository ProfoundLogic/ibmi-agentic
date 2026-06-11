     hCopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h option(*nodebugio: *srcstmt)
     h DFTACTGRP(*no) BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: This program will write record in Menu Command
     h*               Processing file and Update the respective
     h*               record in Menu Option file.
     h*
     h*  Called From: None
     h*  Calls to: None
     h*
     h*  Created on 06/11/2013 by C:282 for Redmine 2785
     h**********************************************************************
     h* Revisions:
     h*
     h* 12/18/14 C:295 Redmine: 4827
     h*                Added the RUNQRY handling.
     h*                Fix the code as compsuite doesn't require RCDSLT option
     h*                when running from compsuite screen.
     h*
     h* 07/10/13 C:282 Redmine: 3245
     h*                Added the RUNQRY handling.
     h*                Fix the code as some menu options were not updated
     h*                on running the converison program.
     h*
     h**********************************************************************
      *  Menu/Option File
     fmmopp     uf   e             disk    Rename(pop01:pop00)
      *  Menu Command Processing File
     fstmcl     if a e           k disk    Rename(pmc01:pmc00)
      *
     d up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
      ******* Beg Chg ******** 07/10/13 ********************************
     d***** charTrail       c                   '00001'
     d decTrail        s              5  0
      ******* End Chg ******** 07/10/13 ********************************
     d charOPexCl      s            230A
     d charProcId      s             25A
      ******* Beg Add ******** 07/10/13 ********************************
     d charProId       s             25A
      ******* End Add ******** 07/10/13 ********************************
      ******* Beg Add ******** 12/18/14 ********************************
     d intQry          s              2  0
     d intQrylen       s              2  0
     d intlength       s              2  0
     d intQrypos       s              2  0
     d charPro1        s            230A
     d charPro2        s            230A
     d charexcl        s            230A
      ******* End Add ******** 12/18/14 ********************************
     d intCmdLen       s              2  0
     d intEndPos       s              2  0 Inz(20)
     d EOF             s               n   inz( *OFF )
     d psds          esds                  extname(szq1p)
      * Update Menu option file
     d UpdMMOPP        pr
      * Write in Menu Command Line
     d WrtSTMCP        pr
      *
      /copy sPrDate
      /copy sPrString
      /copy sPrRunCmd
     *===========================================================================
     *    Main Processing
     *===========================================================================
      /Free
       RunCmd('ARCOBJ OBJ(MMOPP) OBJTYPE(*FILE)':32);
       Setll *Start mmopp;
       DoW Not EOF;
       Read mmopp;

          If %EOF(mmopp);
             Leave;
          Endif;

          // Checking for all Active Records
          If OPstat = 'A' and  OPyno ='Y';

             charOPexCl = *Blanks;
             charProcId = *Blanks;
             //******* Beg Chg ******** 07/10/13 ********************************
             charProId  = *Blanks;
             //******* End Chg ******** 07/10/13 ********************************
             intCmdLen  = 0;

          // Checking for Required commands
             charOPexCl = %trim(rmvApost(%trim(Upper(OPexCl))));
             Select;
             When %Scan('WRKSBMJOB':charOPexCl:1) > 0;
                Iter;

             When %Scan('DSPSBMJOB':charOPexCl:1) > 0;
                Iter;

             When %Scan('SBMJOB':charOPexCl:1) > 0;
                intCmdLen  = %Len('SBMJOB');
             //******* Beg Chg ******** 07/10/13 ********************************
             // charProcId = %SubSt(charOPexCl:intCmdLen+1:intEndPos)
             // + charTrail;
                charProId = %SubSt(charOPexCl:intCmdLen+1:intEndPos);
             //******* End Chg ******** 07/10/13 ********************************

             When %Scan('CPYF':charOPexCl:1) > 0;
                intCmdLen  = %Len('CPYF');
             //******* Beg Chg ******** 07/10/13 ********************************
             // charProcId = %SubSt(charOPexCl:intCmdLen+1:intEndPos)
             //  + charTrail;
                charProId = %SubSt(charOPexCl:intCmdLen+1:intEndPos);
             //******* End Chg ******** 07/10/13 ********************************

             //******* Beg Add ******** 07/10/13 ********************************
             When %Scan('RUNQRY':charOPexCl:1) > 0;
                intCmdLen  = %Len('RUNQRY');
             //******* Beg Chg ******** 12/18/14 ********************************
                intQry = %Scan('RCDSLT(*YES)':charOPexCl:1);
                If intQry > 0;
                   intQrylen =  %Len('RCDSLT(*YES)');
                   intQrypos = intQry + intQrylen;
                   intlength = intQry - 1;
                   charPro1 = %SubSt(charOPexCl:1:intlength);
                   charPro2 = %SubSt(charOPexCl:intQrypos+1:intEndPos);
                   Charexcl = %trim(charPro1) + %trim(charPro2) ;
                Endif;
                   charProId = %SubSt(charOPexCl:intCmdLen+1:intEndPos);
             //******* End Chg ******** 12/18/14 ********************************
             //******* End Add ******** 07/10/13 ********************************

             Other;
             EndSl;

             //******* Beg CHG ******** 07/10/13 ********************************
             // If charProcId <> *Blank ;
             // Chain (charProcId) pmc00;
             // If %Found(stmcl);
             //    Iter;
             // Else ;
             If charProId <> *Blank ;
                decTrail = 0;
                Dou not %found(stmcl);
                   decTrail = decTrail + 1;
                   charProcId = %Trim(charProId) + %EditC(decTrail:'X');
                   Chain (charProcId) pmc00;
                EndDo;
             //******* End CHG ******** 07/10/13 ********************************
                WrtSTMCP();
                UpdMMOPP();
             //******* Beg Del ******** 07/10/13 ********************************
             // Endif;                       // %Found(stmcl);
             //******* End Del ******** 07/10/13 ********************************
             Endif;                          // charProId <> *Blank;

          Endif;                             // OPstat = 'A' and  OPyno ='Y';
       EndDo;                                // Not %Eof(mmopp);

       *InLr = *On;                          // End of Processing

      /End-Free
     *===========================================================================
     *  UpdMMOPP - Update the record in Menu option file
     *===========================================================================
     p UpdMMOPP        b
     d UpdMMOPP        pi

      /Free
       OPyno = 'N';
       OPexCl = 'STMENUCMD (''' + charProcId + ''')' ;
       OPuser = Q1user;
       OPpgm  = Q1pgm;
       OPtime = TimeTo6(SystemTime());
       OPdt   = DateTo7(SystemDate());
       Update pop00;
      /End-Free

     p UpdMMOPP        e
     *===========================================================================
     *   WrtSTMCP - Write the record in menu command processing file
     *===========================================================================
     p WrtSTMCP        b
     d WrtSTMCP        pi

      /Free
       MCid    =  charProcId;
       MCTitle =  Opmntt;
       MCmsg   =  *Blanks;
       MCbtn1  =  *Blanks;
       MCbtn2  =  *Blanks;
       MCbtn3  =  *Blanks;
       //***** Beg chg ******** 12/18/14 ********************************
       //*****MCcmd1  =  OPexCl;
       If intQry > 0;
          MCcmd1  =  Charexcl;
       Else;
          MCcmd1  =  OPexCl;
       Endif;
       //******* End chg ******** 12/18/14 ********************************
       MCcmd2  =  *Blanks;
       MCcmd3  =  *Blanks;
       MCuser =   Q1user;
       MCpgm  =   Q1pgm;
       MCtime =   TimeTo6(SystemTime());
       MCdt   =   DateTo7(SystemDate());
       Write pmc00;
      /End-Free

     p WrtSTMCP        e
