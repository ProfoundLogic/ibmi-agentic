     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
     //
    H/TITLE SR353C - Form 35-3C - OK-Mandatory Optional Deductible Accept/Reject
     //===============================================================*
     //                                                               *
     // Program Description:                                          *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXFMP) prior to a form merge.                     *
     //                                                               *
     // Created: 03/07/18   C:33   150WCK                             *
     //===============================================================*
     // Revisions:
     //
     //===============================================================*
     // Notes:                                                        *
     //                                                               *
     //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     //                                                               *
     //  QQMODE = 1 - Called to determine if form should be attached. *
     //  QQMODE = 2 - Called to print endorsement.                    *
     //  QQMODE = 3 - Called for additional input.                    *
     //  QQMODE = 4 - Called for mid-term processing.                 *
     //===============================================================*
     fwmldl1    if   e           k disk
    
     // Copybook F specs
     F/COPY SCOPYEN01F
    
     //=========================================================================
     //  Definition Specs
     //=========================================================================
     // Copybook D specs
     D/COPY SCOPYEN01D

     //=========================================================================
     //  /COPY required constants
     //=========================================================================
     // Form ID
     D qqForm          c                   const('FORM353C  ')
     // Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
     //=========================================================================
     //  PLIST definitions
     //=========================================================================

     //* Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
     //=========================================================================
     //  Main
     //=========================================================================
    
     // Copybook C specs
     C/COPY SCOPYEN01C
    
      /Free
      //=========================================================================
      //  OthInf - Subroutine to load form specific information & write record
      //=========================================================================
       Begsr OthInf;
    
       Endsr;
    
      //=========================================================================
      //  PGMSTR - Beginning of program processing
      //=========================================================================
       Begsr PgmStr;
       runningSRText = *on;
       Endsr;
    
      //=========================================================================
      //  PGMEND - End of program processing
      //=========================================================================
       Begsr PgmEnd;
    
       Endsr;
    
    
      //=========================================================================
      //  VALID - Subroutine to attach this form to the policy
      //=========================================================================
       Begsr Valid;
    
         crtFrm = *on;
         // Form included if no Large Deductible exists
         Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) wmldl1;
         Select;
         When %found(wmldl1) and LDefD1 <> LDexp1;
            crtFrm = *off;
         When %found(wmldl1) and LDefD2 <> LDexp2;
            crtFrm = *off;
         Endsl;
    
       Endsr;
      /End-free
