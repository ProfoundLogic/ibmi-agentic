     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR04343A - CA Designated Operations at Designated Locations Exclusion
      *===============================================================*
      * WC040343A- Designated Operations at Designated Locations      *
      *            Exclusion.                                         *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 10/19/17                                          *
      * Programmer: C:291                                             *
      * Project   : WC2531                                            *
      *                                                               *
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
     D qqForm          s             15a   inz('WC040343A ')

      ** Additional entry program
     D ADDPGM          C                   CONST('SR04343AI ')

     D/COPY SCOPYEN01D

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      *entry parameters
     D SR04343A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04343A        PI
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
          FMc401 =  FM2c401;                   //Operation Name
          FMc402 =  FM2c402;                   //Standard Classification Number
          FMc403 =  FM2c406;                   //Abbreviated Phraseology
          FMFL01  = FM2FL01;                   //Check box 1
          FMFL02  = FM2FL02;                   //Check box 2
          FMc404 =  FM2c404;                   //Insured Name
          FMc405 =  FM2c405;                   //Insurer
          FMc301 =  FM2c301;                   //Policy Number
          FMDt01 =  FM2Dt01;                   //Policy Eff date
          FMDt02 =  FM2Dt02;                   //Policy Exp date

          FMead1  = FM2c403;                   //Address 1

          FMead2  = ' ' ;                      //Address 2
          FMead2 = %Trim(FM2c201) + ', ' +
          %Trim(FM2c101) + '  ' +
          %Trim(FM2c102) + '-' +
          %Trim(FM2c103);

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
