     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE FIXSMKTP - LOAD THE NEW KEY TYPE FIELDS
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *                                                                        *
     * CREATED   : 01/31/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
      ** DARTS Key Type File
     FSMKTL1    UF   E           K DISK

      ** IMAGE Index File
     FIMITP     IF   E             DISK

     **************************************************************************
     * CheckObj - External program prototype.                                 *
     **************************************************************************
      * Description: Check for the presence of an object.  Error return is     *
      *              blank for success.                                        *
      *                                                                        *
     * Parameters : 10a - object to check                                     *
      *               7a - type of object                                      *
      *               1a - error return                                        *
      *                                                                        *
      * Returns    : none                                                      *
     **************************************************************************
      ** Procedure Prototype
     D CheckObj        PR                  ExtPgm('SRCHKOBJ')
     D  object                       10a
     D  type                          7a
     D  error                         1a

     *========================================================================*
     * Variable Definitions
     *========================================================================*
     D pType           S              7a   Inz('*PGM   ')
     D pError          S              1a
     D pSrch           S             10a
     d pSplit          S             10a
     D kDPgm           S             10a


     *========================================================================*
     * KList Definitions
     *========================================================================*
     C     KTkey         KList
     C                   KFld                    KDPGM

     *========================================================================*
     * Main
     *========================================================================*
     C                   Read      IMITP                                  99
     C                   DoW       *in99 = *off
     C                   MoveL     ITDPGM        KDPGM
     C     KTkey         Chain     PKT01                              60

     C                   If        *in60 = *off
     C                   If        KTINPG = *BLANKS
     C                   Eval      KTINPG = ITPNAM
     C                   EndIf
     C                   If        KTDESC = *BLANKS
     C                   Eval      KTDESC = ITDESC
     C                   EndIf
     C                   If        KTVPGM = *BLANKS
     C                   Eval      KTVPGM = ITVPGM
     C                   EndIf

     C                   If        KTSRC1 = *BLANKS
     C                   Eval      pSrch = 'S' + KTKTYP + '01R'
     C                   CALLP     CheckObj(pSrch:pType:pError)
     C                   If        pError = '0'
     C                   Eval      KTSRC1 = pSrch
     C                   EndIf
     C                   EndIf

     C                   If        KTSRC2 = *BLANKS
     C                   Eval      pSrch = 'S' + KTKTYP + '02R'
     C                   CALLP     CheckObj(pSrch:pType:pError)
     C                   If        pError = '0'
     C                   Eval      KTSRC2 = pSrch
     C                   EndIf
     C                   EndIf

     C                   If        KTSRC3 = *BLANKS
     C                   Eval      pSrch = 'S' + KTKTYP + '03R'
     C                   CALLP     CheckObj(pSrch:pType:pError)
     C                   If        pError = '0'
     C                   Eval      KTSRC3 = pSrch
     C                   EndIf
     C                   EndIf

     C                   If        KTSRC4 = *BLANKS
     C                   Eval      pSrch = 'S' + KTKTYP + '04R'
     C                   CALLP     CheckObj(pSrch:pType:pError)
     C                   If        pError = '0'
     C                   Eval      KTSRC4 = pSrch
     C                   EndIf
     C                   EndIf

     C                   If        KTSPLT = *BLANKS
     C                   Eval      pSplit = %subst(ITPNAM : 1 : 2) + 'IS' +
     C                                      %subst(ITPNAM : 5)
     C                   CALLP     CheckObj(pSplit:pType:pError)
     C                   If        pError = '0'
     C                   Eval      KTSPLT = pSplit
     C                   EndIf
     C                   EndIf

     C                   Update    PKT01
     C                   EndIf

     C                   Read      IMITP                                  99
     C                   EndDo

     C                   Eval      *inLR = *on
