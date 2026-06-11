      * (C) Copyright FISERV Insurance Solutions, Inc., 2007
      *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR00403
      *===============================================================*
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      *
      * 05/24/23 C:342 Project 150WFX                                 *
      *              - Auto Attach WC000403                           *
      *                                                               *
      * 07/14/11 C:061 Support 77336
      *              - Do not attach for flat cancelled divisions.
      *              - Convert to RPGIV.
      *                                                               *
      * 02/21/07 C:61  PROJECT: 139WGW                                *
      *                Rewrite for version 7.                         *
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
      *
      ** Copybook F specs
     F/COPY SCOPYEN01F
      *
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *********** beg add ***** 07/14/11 **********************************
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
      *********** end add ***** 07/14/11 **********************************
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqFORM          C                   CONST('WC000403  ')
      *
      ** Additional entry program
     D ADDPGM          C                   CONST('SRXXXXX   ')
      *
      *
      *=========================================================================
      *  *LIKE DEFN definitions
      *=========================================================================
      *
      *
      *=========================================================================
      *  Data structures and constants
      *=========================================================================
      ** Copybook I specs
      *********** beg del ***** 07/14/11 **********************************
     I******* /COPY SCOPYEN01I
      *********** end del ***** 07/14/11 **********************************
      *
      *
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
      *
      *********** beg add ***** 07/14/11 **********************************
    
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
      *********** end add ***** 07/14/11 **********************************
      *
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      *
      *
      *=========================================================================
      *  OTHINF - Subroutine to load form specific information & write record
      *=========================================================================
     C     OTHINF        BEGSR
      *
      ** Write the output record
     C                   WRITE     PFM01
      *
     C                   ENDSR
      *
      *
      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PGMSTR        BEGSR
      *
      *********** beg add ***** 05/24/23 **********************************
     C                   MOVE      *ON           RunningSRtext
      *********** end add ***** 05/24/23 **********************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PGMEND        BEGSR
      *
      *
     C                   ENDSR                                                  PGMEND
      *
      *
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
     C                   SELECT
      *
      *********** beg chg ***** 05/24/23 **********************************
     C**** CLTID         WHENEQ    '139'
     C**** RSKY          CHAIN     PRS01                              60
      ****
      ** Found a division with a matching jurisdiction state
     C**** *IN60         IFEQ      *OFF
      ****
     C**** RSJRST        IFEQ      'MO'
     C****               MOVE      *ON           CRTFRM
     C****               ENDIF                                                  ENST
      ****
     C****               ENDIF                                                  *IN60 = *OFF
      ****
     C****               OTHER
     C****               MOVE      *ON           CRTFRM
     C     ELEXM1        WHENEQ    *ZEROS
     C     ELEMD1        ANDEQ     *ZEROS
     C                   MOVE      *ON           CRTFRM
     C
     C     ELEXMD1NF     WHENEQ    'Y'
     C                   MOVE      *ON           CRTFRM
     C
      *********** end chg ***** 05/24/23 **********************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  VALID
      *
      *
