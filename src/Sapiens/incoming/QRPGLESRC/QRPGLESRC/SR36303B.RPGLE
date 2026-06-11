     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR36303B - OR - Oregon Employee Leasing Labor Contractor
      *===============================================================*
      * WC36303BA- Oregon Employee Leasing Labor Contractor           *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * Created on 10/26/2017 by c:62 for Project WC2569
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
     FWledl     if   e           k disk    Prefix(fm2:2)
     F                                     Rename(ped01:ped00)
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15a   inz('WC360303B ')

      ** Additional entry program
     D ADDPGM          C                   CONST('SR36303BI ')

     D/COPY SCOPYEN01D

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      *entry parameters
     D SR36303B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR36303B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
     C     keyWledl      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
     C                   kfld                    xxFrm1
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
     C     FMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      *  OthInf - Subroutine to load form specific information & write record
      *=========================================================================
     C     OthInf        Begsr
      *
     C                   Movel(P)  XXFORM        XXFRM1           15

      /Free
       WriteInCopybook = *Off;

       Setll keyWledl ped00;
       Reade keyWledl ped00;

          Dow Not %Eof(wledl);
             FMc401 =  FM2c401;                   //Legal Name 1
             FMc402 =  FM2c402;                   //Legal Name 2
             FMc403 =  FM2c403;                   //Legal Name 3
             FMc404 =  FM2c404;                   //Entity Status

             FMc405  = FM2c405;                   //Address 1
             FMc406  = FM2c406;                   //Address 2
             FMc407  = %Trim(FM2c301) + ', ' +    //Address 3
                       %Trim(FM2c101) + '  ' +
                       %Subst(%EditC(FM2fein:'X'):1:5) + '-' +
                       %Subst(%EditC(FM2fein:'X'):6:4);

             // Write the output record in WXFMP.
             Exsr WriteWorkFile;

             Reade keyWledl ped00;
          EndDo;
      /End-Free
     C*
      *
     C                   Endsr
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PgmStr        Begsr
      *
      *
     C                   Endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PgmEnd        Begsr
      *
      *
     C                   Endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
      *
     C                   ENDSR
