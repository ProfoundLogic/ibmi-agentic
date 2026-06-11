     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00412 - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000412
      //****************************************************************
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 12/22/99                                          *
      // Programmer: C:90                                              *
      // Project   : 48WTK                                             *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/26/22 C:333 Project: WC2834 (Recompile)                    *
      //                Endorsement Recognition of Cancellation.       *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*

      /COPY SCOPYEN01F
     FWXPEL     UF   E           K DISK
     FWXPEL1    IF   E           K DISK
     F                                     RENAME(PPE01:PPE02)
      /COPY WWCPOLDAT4

     D qqFORM          s             15A   Inz('WC000412  ')
     D ADDPGM          C                   CONST('SRXXXXXXX ')

     D SR00412         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR00412         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /COPY SCOPYEN01D

     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0
     C     PEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
     C     *LIKE         DEFINE    PEUNID        XXUNID
     C     *LIKE         DEFINE    PEEND#        X1END#
      /COPY SCOPYEN01C

      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     OTHINF        BEGSR
     C     ELEXM3        IFNE      *ZERO
     C                   Z-ADD     ELEXM3        FMA541
     C                   Z-ADD     ELEMD3        XXDT7             7 0
     C                   ELSE
     C     ELEXM2        IFNE      *ZERO
     C                   Z-ADD     ELEXM2        FMA541
     C                   Z-ADD     ELEMD2        XXDT7
     C                   ELSE
     C                   Z-ADD     ELEXM1        FMA541
     C                   Z-ADD     ELEMD1        XXDT7
     C                   ENDIF
     C                   ENDIF
      ** Write the output record
     C                   WRITE     PFM01
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
     C     #1FLAG        IFEQ      '0'
      * Update the endorsement reporting file with the proper data.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
      * Find the unique ID number.
     C                   MOVE      *HIVAL        XXDT
     C     PEKEY         SETGT     PPE02
     C     PEKEY1        READPE    PPE02                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PEUNID        XXUNID
     C                   ENDIF
     C                   MOVEL     XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   E00412
     C                   MOVE      XXDT7         X4EDT
     C     X4EDT         IFEQ      *ZERO
     C                   MOVE      ELPFDT        X4EDT
     C                   ENDIF
     C                   Z-ADD     FMA541        X4MOD
     C                   MOVEL(P)  E00412        PEDATA
     C                   UPDATE    PPE01
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
     C     ELEXM1        IFNE      *ZERO
     C                   MOVE      *ON           CRTFRM
     C                   ENDIF
     C                   ENDSR
