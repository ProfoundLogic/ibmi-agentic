     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')

    H/TITLE SA045R - API to Determine Default SSN and Authority
     H*********************************************************************
     H*  Created:  08/14/12    C:282   Project: WC2030
    H*********************************************************************
     H* REVISED:
     H*
     H* xx/xx/xx C:xxx PROJECT xxxxxx
     H*                xxxxxx
     H*
    H*********************************************************************

      * SSN default criteria table
     fstssl1    if   e           k disk    rename(pss01:pss00)
     fstssp     o    e             disk    rename(pss01:pss0p)
      * User Parameters
     fsmupl     if   e           k disk    rename(pup01:pup00)

     d psds          esds                  extname(szq1p)

     d intCo#          s              3  0
     d strCtId         s             10
     d strUtype        s              1
     d strULvl         s              3
     d strCType        s              1
     d strCLvl         s              3
     d strType         s              1
     d outAuthority    s              1
     d strCType2       s              1
     d strCType3       s              1
     d strCType4       s              1
     d strCType5       s              1
     d strCType6       s              1
     d intCounter      s              1  0
      *-------------------------------------------------------------------------
      * Prototype Declaration
      *-------------------------------------------------------------------------
     dsrccsc           pr                  Extpgm('SRCCSC')
     d strUtype                       1    const
     d strULvl                        3    const
     d strCType                       1    const
     d strCLvl                        3    const
     d strType                        1    const
     d outAuthority                   1    const
     d strCType2                      1    const options(*nopass)
     d strCType3                      1    const options(*nopass)
     d strCType4                      1    const options(*nopass)
     d strCType5                      1    const options(*nopass)
     d strCType6                      1    const options(*nopass)

     d/copy sPrDate
      **************************************************************
      * KLIST
      **************************************************************
      * SSN default criteria table
     c     keySTSSL1     klist
     c                   kfld                    intCo#
     c                   kfld                    strCtId

      **************************************************************
      * PLIST
      **************************************************************

     c     *entry        plist
     c                   parm                    inCo#             3 0          Company
     c                   parm                    inControlId      10            Control ID
     c                   parm                    outMask           1            Masking
     c                   parm                    outAuthority      1            Authorized

      **************************************************************
      * Main
      **************************************************************
      /free

       // Retrieve user security level and user security type.
       Chain Q1user pup00;
       If %Found(SMupl);
          strUtype  = UPtype;
          strUlvl   = %EditC(UPlvl:'X');
       EndIf;

       intCo# = inCo#;
       strCtId = inControlId;
       outMask = *blanks;
       outAuthority = '0';


       Chain keySTSSL1 pss00;

       // Chain the file STSSP to verify the record exist, if not exist than write the
       // record in STSSP and use data from the *GLOBAL record.

       If Not %Found(STssl1);
          Exsr  WriteRecord;

          strCtId = '*GLOBAL';
          Chain keySTSSL1 pss00;
          If %Found(STssl1) And SSmask <> *blanks;
             outMask = ssMASK;
             Exsr RtvUsrAuth;
          EndIf;

       // Chain the file STSSP to verify the record exist, if exist but SSstat <> 'A'
       // than use data from the *GLOBAL record.

       ElseIf %Found(STssl1) And SSstat <> 'A';

          strCtId = '*GLOBAL';
          Chain keySTSSL1 pss00;
          If %Found(STssl1) And SSmask <> *blanks;
             outMask = ssMASK;
             Exsr RtvUsrAuth;
          EndIf;

       Else;
          outMask = ssMASK;
          Exsr RtvUsrAuth;

       EndIf;

       *InLr = *On;
      /end-free
      *====================================================================
      * Write records to STSSP
      *====================================================================
      /free
       Begsr WriteRecord;

        Clear pss0p;

        SSco#  = inCo#;
        SSctid = inControlId;
        SSdesc = *Blanks;
        SSlvl1 = 001;
        SSty01 = '*';
        SSstat = 'I';

        SSipgm = q1pgm;
        SSiusr = q1user;
        SSidt  = DateTo7(SystemDate);
        SSitim = TimeTo6(SystemTime);
        SSpgm  = q1pgm;
        SSuser = q1user;
        SSdt   = DateTo7(SystemDate);
        SStime = TimeTo6(SystemTime);

        Write  pss0p;

       Endsr;
      /end-free
      *====================================================================
      * Retrieve the User Authority to see/change the SSN - RtvUsrAuth
      *====================================================================
      /free

       Begsr RtvUsrAuth;

       strType   = '1';
       strCLvl   = %EditC(SSlvl1:'X');
       strCType  = SSty01;
       strCType2 = SSty02;
       strCType3 = SSty03;
       strCType4 = SSty04;
       strCType5 = SSty05;
       strCType6 = SSty06;

       intCounter = 1;
       Dow 6 >= intCounter;
          If intCounter = 1;
             strCLvl   = %EditC(SSlvl1:'X');
          ElseIf intCounter = 2;
             strCLvl   = %EditC(SSlvl2:'X');
          ElseIf intCounter = 3;
             strCLvl   = %EditC(SSlvl3:'X');
          ElseIf intCounter = 4;
             strCLvl   = %EditC(SSlvl4:'X');
          ElseIf intCounter = 5;
             strCLvl   = %EditC(SSlvl5:'X');
          ElseIf intCounter = 6;
             strCLvl   = %EditC(SSlvl6:'X');
          EndIf;

          Srccsc(strUtype:strUlvl:strCType:strCLvl:
                  strType:outAuthority:strCType2:strCType3:
                  strCType4:strCType5:strCType6);

          intCounter = intCounter + 1;

          If outAuthority = '0';
             Leave;
          Endif;

       EndDo;

       Endsr;

      /end-free
