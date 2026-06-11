     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 1999
     *===============================================================*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR00102 - ENDORSEMENT ATTACH/PRINT PROGRAM
     *===============================================================*
     * WC000102 - Federal Coal Mine Health & Safety Act Coverage     *
     *            Endorsement                                        *
     *                                                               *
     * PROGRAM DESCRIPTION:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * CREATED   : 10/06/98                                          *
     * PROGRAMMER: C:114                                             *
     * PROJECT   : CM027                                             *
     *                                                               *
     * REWRITE   : 12/02/99                                          *
     * PROGRAMMER: C:113                                             *
     * PROJECT   : CM027                                             *
     *===============================================================*
     * REVISIONS:                                                    *
     *                                                               *
     * 01/22/04 C:65  PROJECT: 114WAK/SUPP#40498                     *
     *                Change processing to get current date/time id  *
      *                from chaining out to wxpel1 in date/time order.*
     *                Populate maintenance fields on wxpep.          *
     *                                                               *
     * 09/09/03 C:65  PROJECT: 114WAK/SUPP#37919                     *
     *                CHANGE PROCESSING TO GET CURRENT TIME/DATE ID. *
      *                                                               *
     *  04/30/01 C:65  PROJECT: 114WAK                               *
     *                 Add processing to write endorsement data to   *
     *                 the WXPEP reporting file for PA/DE/NJ states. *
     *                                                               *
     *  04/07/00 C:90  PROJECT: SEQ#:WA096/22505                     *
     *                 Added missing entry parm.                     *
     *                                                               *
     *  12/02/99 C:113 PROJECT: CM027                                *
     *                 Rewrite to use RPGIV and copybook.            *
     *                                                               *
     *  04/29/99 C:113 PROJECT:                                      *
     *                 If F3 or F12 is pressed, pass back an 'F'     *
     *                 through QQCALL.                               *
     *                                                               *
     *  10/09/98 C:09 PROJECT CM027                                  *
     *                ONLY ACCUMULATE PREMIUM FOR A GIVEN FUND YEAR. *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * NOTES:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
     *                                                               *
     *  qqmode = 1 - Called to determine if form should be attached. *
     *  qqmode = 2 - Called to print endorsement.                    *
     *  qqmode = 3 - Called for additional input.                    *
     *  qqmode = 4 - Called for midterm processing.                  *
     *===============================================================*
      ** Additional Information File
     Fwxfmp1    if   e           k disk    usropn
     F                                     rename(pfm01:pfm02)
     F*** begin add ***  04/30/01  *****************************************
     Fwxpel     uf a e           k disk
     F*** BEGIN DEL ***  09/09/03  ************************************
     F****wxpel1    if   e           k disk
     F****                                     rename(ppe01:ppe02)
     F*** END DEL *****  09/09/03  ************************************
     Fststl3    if   e           k disk
     F*** end add *****  04/30/01  *****************************************
     F*** BEGIN ADD ***  09/09/03  ************************************
     f*** begin chg *  01/22/04  *******************************************
     F*****WMSHL1    IF   E           K DISK
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     f*** end chg ***  01/22/04  *******************************************
     F*** END ADD *****  09/09/03  ************************************
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D*** begin add ***  04/30/01  *****************************************
     D/COPY WWCPOLDAT4
     D*** end add *****  04/30/01  *****************************************
     *=========================================================================
     *  Copybook required constants
     *=========================================================================

     ** Form ID
     D qqform          c                   const('WC000102  ')
     C*** begin add ***  04/30/01  *****************************************
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     C*** end add *****  04/30/01  *****************************************

     ** Additional entry program
     D addpgm          c                   const('SR00102I  ')
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
      **** begin add **** 04/07/00 *****************************************
     C                   parm                    qqmode            1
      **** end   add **** 04/07/00 *****************************************
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     ** Additional Input file
     C     fmKey3        klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxform
     C*** begin add ***  04/30/01  *****************************************
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     c*** begin add ***  01/22/04  *****************************************
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     c*** end add ***  01/22/04  *******************************************
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
     C     stky          KList
     C                   KFld                    xxkey            10
     C*** end add *****  04/30/01  *****************************************
     C*** BEGIN ADD ***  09/09/03  ************************************
     C*
     C* SUBMISSION HISTORY FILE
     C*
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
     C*** END ADD *****  09/09/03  ************************************
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs (PLISTs, KLISTs and mainline)
     C/COPY SCOPYEN01C
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information
     *=========================================================================
     C     othinf        begsr
    
    
     ** Write the output record.
     C                   open      wxfmp1
     C     fmKey3        chain     pfm02                              60
    
     ** Move the custom fields to print.
     C                   movel     f3c101        fmc101
     C                   movel     f3c102        fmc102
     C                   movel     f3c103        fmc103
     C                   movel     f3c104        fmc104
     C                   movel     f3c105        fmc105
     C                   movel     f3c106        fmc106
     C                   movel     f3c107        fmc107
     C                   movel     f3c108        fmc108
     C                   movel     f3c109        fmc109
     C                   movel     f3c110        fmc110
    C*
     C                   write     pfm01
     C                   close     wxfmp1
     C*** begin add ***  04/30/01  *****************************************
      *
      * Update the endorsement reporting file with the proper data.
      *
     C*** BEGIN CHG ***  09/09/03  ************************************
     C*****pekey         setgt     ppe02
     C*****pekey         readpe    ppe02                                  60
      *
      * Find the unique ID number.
      *
     C*****              If        *in60 = *off
     C*****              eval      xxunid = peunid
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     C*** begin chg ***  01/22/04  ************************************
     C*****SHKEY         CHAIN     PSH01                              60
     C*****              If        *in60 = *off
     C*****              Z-ADD     SHUNID        xxunid
     C*****              ENDIF
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C*** end chg *****  01/22/04  ************************************
     C*** END CHG *****  09/09/03  ************************************
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   clear                   E00102
      *
     C                   movel     f3c101        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   movel     xxst          xxsch
     c                   endif
     C                   movel     f3c102        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c103        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c104        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c105        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c106        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c107        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c108        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c109        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     C                   movel     f3c110        xxkey
     C     stky          chain     pst01                              60
     C                   If        *in60 = *off
     c                   move      stnst         xxst              2
     c                   cat       xxst:0        xxsch
     c                   endif
     c                   move      *blanks       pedata
     c                   movel     E00407        pedata
     c*** begin add ***  01/22/04  *****************************************
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
     c*** end add *****  01/22/04  *****************************************
      *
     C                   update    ppe01
     C                   endif                                                  *in60 ifeq off
      *
     C*** BEGIN DEL ***  09/09/03  ************************************
     C*********          endif                                                  *in60 ifeq off
     C*** END DEL *****  09/09/03  ************************************
     C*** end add *****  04/30/01  *****************************************
    
     C                   endsr
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     ** This is a manual attach endorsement with additional input.
     C                   move      *on           crtfrm
    
     C                   endsr
