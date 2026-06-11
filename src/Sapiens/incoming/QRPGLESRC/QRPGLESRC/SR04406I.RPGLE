     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
      /TITLE SR04406I - CA - Farm Schedule Endorsement
      *===============================================================*
      * WC040406I                                                     *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 07/26/21                                          *
      * Programmer: C:079                                             *
      * Project   : WC2861                                            *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
      *                                                               *
      * 10/31/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*

      ** Copybook F specs
     F/COPY SCOPYEN01F

      * Employer Payroll
     Fwmepl7    if   e           k disk

      ** Copybook D specs
     D/COPY SCOPYEN01D

      ** Form ID
     D QQFORM          S             15A   inz('WC040406I   ')
     D QQCo#           S              3    inz
     D QQFnd           S              3    inz
     D QQEmp#          S              9    inz
     D QQDiv           S              5    inz
     D QQFyr           S              3    inz
     D QQMode          S              1    inz
     D QQRrn           S              9  0 inz
     D QQMod2          S              1    inz

      ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')

     D SR04406i        PR
     D QQCo#                          3
     D QQFnd                          3
     D QQEmp#                         9
     D QQDiv                          5
     D QQFyr                          3
     D QQMode                         1
     D QQRrn                          9  0
     D QQMod2                         1

     D SR04406i        PI
     D  x1Co#                         3
     D  x1Fnd                         3
     D  x1Emp#                        9
     D  x1Div                         5
     D  x1Fyr                         3
     D  xxMode                        1
     D  xxRrn                         9  0
     D  xxMod2                        1

      ** Additional input program parameters

      /Free

       qqCo# = x1Co#;
       qqFnd = x1Fnd;
       qqEmp# = x1Emp#;
       qqDiv = x1Div;
       qqFyr = x1Fyr;
       qqMode = xxMode;

       //* Copybook C specs
        /COPY SCOPYEN01C

       //=====================================================================
       //  OTHINF - Subroutine to load form specific information & write record
       //=====================================================================
       Begsr OthInf;
       Endsr;

       //=====================================================================
       //  PgmStr - Beginning of program processing
       //=====================================================================
       Begsr PgmStr;
        RunningSRtext = *On;
       Endsr;

       //=====================================================================
       //  PgmEnd - End of program processing
       //=====================================================================
       Begsr PgmEnd;
       Endsr;

       //=====================================================================
       //  Valid - Subroutine to attach this form to the policy
       //=====================================================================
       Begsr Valid;

       Setll (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) WMEPL7;
       Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) WMEPL7;

       Dow Not %EOF(WMEPL7);

          If (EPclas = 0079 and EPext = '2')
            or (EPclas = 0044) or (EPclas = 0036)
            or (EPclas = 0171)
            or (EPclas = 0038 and EPext = '2')
            or (EPclas = 0016)
            or (EPclas = 0045) or (EPclas = 0041)
            or (EPclas = 0034 and EPext = '001')
            or (EPclas = 0034 and EPext = '2')
            or (EPclas = 0038 and EPext = '001')
            or (EPclas = 0079 and EPext = '001')
            or (EPclas = 0079 and EPext = '1')
            or (EPclas = 0038 and EPext = '1')
            or (EPclas = 0034 and EPext = '1')
            or (EPclas = 0172) or (EPclas = 0040)
            or (EPclas = 0005) or (EPclas = 0035) or (EPclas = 0050);
             CrtFrm = *On;
             Leave;
          Endif;

          reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) WMEPL7;
       Enddo;

       Endsr;
      /End-Free
