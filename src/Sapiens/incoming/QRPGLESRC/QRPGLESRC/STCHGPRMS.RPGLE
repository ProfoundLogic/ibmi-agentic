     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*====================================================================
    H/TITLE StChgParms - Change user parms
    H*====================================================================
    H*  Created: 12/05/12    C:33    Project: Transform
    H*====================================================================
     D* Entry
     DStChgPrms        pr                  ExtPgm('STCHGPRMS')
     D inPrefix                       1    options(*nopass)
     DStChgPrms        pi
     D inPrefix                       1    options(*nopass)
     D*
     DdsStkEnt         ds
     D stkPgm                        10
     D stkLib                        10
     D*
     DaryStkEnt        ds                  likeds(dsStkEnt) dim(128)
     D*
     DCheckObjExists   pr                  ExtPgm('SRCHKOBJ')
     D object                        10    const
     D type                           7    const
     D error                          1n
     D*
     DGetProgram       pr            10
     D*
     DPgmExists        pr             1n
     D object                        10    const
     D*
     DRtvCallStk       pr                  ExtPgm('RTVCALLSTK')
     D                                5p 0
     D                               20a   Dim(#ofElem)
     D*
     DUserParms        pr                  ExtPgm(userParmPgm)
     D*
     D##entNbr         s              5p 0 inz(10)
     DuserParmPgm      s             10
     D#ofElem          c                   128
     D*
     D psds          esds                  extname(szq1p)

      /Free

       Select;
       When %parms >= 1 and inPrefix <> *blanks;
          userParmPgm = inPrefix + '0006R';
       Other;
          Callp RtvCallStk(##entNbr:aryStkEnt);
          userParmPgm = GetProgram;
       Endsl;

       Callp UserParms();

       *inlr = *on;
       Return;

      /End-free

      *====================================================================
     PGetProgram       b
     DGetProgram       pi            10
     D*
     DsampleSelect     s             10    inz
     Dprogram          s             10    inz(' 0006R')
     Di1               s              3  0

      /Free
       For i1 = 1 to #ofElem;
          If aryStkEnt(i1).stkPgm <> q1Pgm;
             Leave;
          Endif;
       Endfor;

       // Any program execptions to call different versions of the user
       //  parm propram should be added within the selects
       Select;
       When aryStkEnt(i1).stkPgm = sampleSelect;
          %subst(program:1:1) = 'S';
       Other;
          %subst(program:1:1) = %subst(aryStkEnt(i1).stkPgm:1:1);
       Endsl;

       If not PgmExists(program);
          %subst(program:1:1) = 'S';
       Endif;

       Return program;

      /End-free
     PGetProgram       e

      *====================================================================
     PPgmExists        b
     DPgmExists        pi             1n
     D inPgm                         10    const

     Derror            s              1n

      /Free
       Callp CheckObjExists(inPgm:'*PGM':error);
       Return not error;
      /End-free

     PPgmExists        e
