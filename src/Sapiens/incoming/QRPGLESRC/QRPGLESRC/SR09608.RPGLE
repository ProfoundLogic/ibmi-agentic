     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR09608 - Notification to Others of Cancellation
      *===============================================================*
      * WC090608 - Notification to Others of Cancellation             *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * Created on 01/16/2024 by C:344 for Project 148WEL
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      *                For single print endorsements provide specific *
      *                group and employer so that correct sig is found*
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
     D qqForm          s             15a   inz('WC090608  ')

      ** Additional entry program
     D ADDPGM          C                   CONST('SR09608I  ')

     D/COPY SCOPYEN01D

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      *entry parameters
     D SR09608         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR09608         PI
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

          If %found ;
             If not %Open(WXFMPMULTI);
                Exsr OpenWorkFile;
             Endif;
          Endif;

          Dow Not %Eof(wledl);
             FMc401 =  FM2c401;                   //Company Name 1
             FMc402 =  FM2c402;                   //Company Name 2
             FMc403 =  FM2c403;                   //Company Name 3

             FMc404  = FM2c404;                   //Address 1
             FMc405  = FM2c405;                   //Address 2
             FMc406  = %Trim(FM2c301) + ', ' +    //Address 3
                       %Trim(FM2c101) + '  ' +
                       %Subst(%EditC(FM2fein:'X'):1:5) + '-' +
                       %Subst(%EditC(FM2fein:'X'):6:4);

             If %Subst(FM2c102:1:3) = '000';      //Phone #
                FMc407 = %Subst(FM2c102:4:3) + '-' + %Subst(FM2c102:7:4);
             Else;
                FMc407 = '(' + %Subst(FM2c102:1:3) + ') ' +
                   %Subst(FM2c102:4:3) + '-' + %Subst(FM2c102:7:4);
             EndIf;

             // Write the output record in WXFMPMULTI
             Exsr WriteMultiRcd;

             Reade keyWledl ped00;
          EndDo;

          If %Open(WXFMPMULTI);
             Exsr CloseWorkFile;
          Endif;

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
