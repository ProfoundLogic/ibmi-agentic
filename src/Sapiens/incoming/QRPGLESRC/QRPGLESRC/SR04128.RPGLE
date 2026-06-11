      *  (C) Copyright Progressive Data Solutions, Inc., 1999
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR04128 - IMPORTANT INFO-CA INS GUARANTEE ASSOCIATION
      *===============================================================*
      * SR04128 - Important Info-CA Ins Guarantee Association         *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 09/02/99                                          *
      * Programmer: C:90                                              *
      * Project   : 48WRE                                             *
      *===============================================================*
      * REVISIONS:                                                    *
      *
      * 07/14/11 C:061 Support 77336
      *              - Convert source to RPGIV.
      *              - Do not attach for flat cancelled divisions.
      *                                                               *
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
      *
      *=========================================================================
      *  Arrays
      *=========================================================================
     D ST              S              2    DIM(4)                               States found
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqFORM          C                   CONST('WC040128  ')
      ******* beg del ****** 07/14/11 *********************************
      ******* Additional entry program
     D******* ADDPGM          C                   CONST('          ')
      ******* Copybook I specs
     D*******/COPY SCOPYEN01I
      ******* end del ****** 07/14/11 *********************************
      *
      ** Deductible per occurance amount
     D                 DS
     D  DA                     1     44  2
     D                                     DIM(4)                               Deductible amount
     D  FMAM01                 1     11  2
     D  FMAM02                12     22  2
     D  FMAM03                23     33  2
     D  FMAM04                34     44  2
      *
      ** States to print
     D                 DS
     D  SP                     1     40
     D                                     DIM(4)                               States to print
     D  FMC101                 1     10
     D  FMC102                11     20
     D  FMC103                21     30
     D  FMC104                31     40
      ******* beg add ****** 07/14/11 *********************************

      ** Additional entry program
     D ADDPGM          C                   CONST('SRXXXXXXX ')

     D/COPY SCOPYEN01D


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
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
      ******* end add ****** 07/14/11 *********************************
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs (KLISTS and mainline)
     C/COPY SCOPYEN01C
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information
      *=========================================================================
     C     OTHINF        BEGSR
      *
     C                   WRITE     PFM01
      *
     C                   ENDSR
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR                                                  PGMSTR
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     ENST
      *
     C                   MOVE      *ON           CRTFRM
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDSR                                                  VALID
