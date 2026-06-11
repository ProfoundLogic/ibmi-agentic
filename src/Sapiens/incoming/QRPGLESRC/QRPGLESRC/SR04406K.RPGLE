     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
      /TITLE SR04406K - CA - Farm Schedule Endorsement
      *===============================================================*
      * WC040406K                                                     *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 06/27/25                                          *
      * Programmer: C:291                                             *
      * Project   : WC3012                                            *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
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
     D QQFORM          S             15A   inz('WC040406K   ')
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

     D SR04406K        PR
     D QQCo#                          3
     D QQFnd                          3
     D QQEmp#                         9
     D QQDiv                          5
     D QQFyr                          3
     D QQMode                         1
     D QQRrn                          9  0
     D QQMod2                         1

     D SR04406K        PI
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

          If (EPclas = 5)
            or (EPclas = 16)
            or (EPclas = 34 and %Int(EPext) = 1)
            or (EPclas = 34 and %Int(EPext) = 2)
            or (EPclas = 35)
            or (EPclas = 36)
            or (EPclas = 38 and %Int(EPext) = 1)
            or (EPclas = 38 and %Int(EPext) = 2)
            or (EPclas = 40)
            or (EPclas = 41)
            or (EPclas = 45)
            or (EPclas = 50)
            or (EPclas = 79 and %Int(EPext) = 1)
            or (EPclas = 79 and %Int(EPext) = 2)
            or (EPclas = 171)
            or (EPclas = 172);
             CrtFrm = *On;
             Leave;
          Endif;

          reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) WMEPL7;
       Enddo;

       Endsr;
      /End-Free
