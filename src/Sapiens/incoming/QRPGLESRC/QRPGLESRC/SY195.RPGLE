     H* (c) 2012 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     Hoption(*nodebugio)
     H*********************************************************************
     H/title SY195 - Conversion Program
     H*********************************************************************
     H*                                                                   *
     H*  Created: 11/02/10   C:183   Project: SY195                       *
     H*********************************************************************
     H*  Revisions:                                                       *
     H*                                                                   *
     H* 11/16/12 C:282 Project: SY195                                     *
     H*                  - Update the path for data center client.        *
     H*                                                                   *
     H* 08/17/12 C:191 Project: SY195                                     *
     H*                  - Fix updates                                    *
     H*                                                                   *
     H* 10/17/11 C:190 Project: SY195                                     *
     H*                  - QDLS Migration to IFS                          *
     H*                                                                   *
     H*********************************************************************
     FSDCMLH    uf   e           k disk
     F
     FSMMDL2    uf   e           k disk
     F
      ***** BEG ADD ***** 10/17/11 ***********************************
     FIMDOL1    uf   e           k disk
     F*
     D root            s             10
     D***** BEG ADD ***** 08/17/12 ***********************************
     D xxdcid          s                   like(cmdcid)
     D pos             s              5  0
     D***** END ADD ***** 08/17/12 ***********************************
      *** Beg Add ***** 11/16/12 **************************************
     D xxflrp          s             81
      *** End Add ***** 11/16/12 **************************************
     D*
      *** Beg Add ***** 11/16/12 **************************************
     c                   Call      'SA030R'
     c                   parm      *Blanks       qqsufx            3
     c                   parm      *Blanks       qqflrp           80
     c
     c                   eval      xxflrp = *Blanks
     c                   If        qqflrp <> *Blanks
     c                   eval      xxflrp = %trim(qqflrp) + '/'
     c                   EndIf
      *** End Add ***** 11/16/12 **************************************
     C***** BEG CHG ***** 08/17/12 ***********************************
      * Get Root
     C*************      call      'RTVDOCROOT'
     C*************      parm                    root
     C*
     C                   eval      root = 'QDLS'
     C***** END CHG ***** 08/17/12 ***********************************
      ***** END ADD ***** 10/17/11 ***********************************

      * SDCMLH
     C                   read      SDCMLH
     C                   dow       not %eof(SDCMLH)
     C                   if        CMIFSP = ' '
     C                   if        CMFOLD <> ' ' or CMDCID <> ' '
     C***** BEG CHG ***** 10/17/11 ***********************************
     C******             eval      CMUNCN = %trim(CMFOLD) + '/' +
     C******                       CMDCID
     C                   eval      xxdcid = cmdcid
     C***** BEG ADD ***** 08/17/12 ***********************************
     C                   if        CMDCID = '***IMAGE***'
     C                   eval      xxdcid = cmidoc
     C                   endif
     C                   eval      pos = %scan('.':cmdcid)
     C                   if        pos > 0 or cmdcid = '***IMAGE***'
     C***** END ADD ***** 08/17/12 ***********************************
     C                   eval      CMUNCN = '/' + %trim(root) + '/' +
      *** Beg Add ***** 11/16/12 **************************************
     C                                      %trim(XXFLRP) +
      *** End Add ***** 11/16/12 **************************************
     C                                      %trim(CMFOLD) + '/' +
     C                                      %trim(XXDCID)
     C***** END CHG ***** 10/17/11 ***********************************
     C***** BEG ADD ***** 08/17/12 ***********************************
     C                   endif
     C***** END ADD ***** 08/17/12 ***********************************
     C                   else
     C                   eval      CMUNCN = ' '
     C                   endif
     C                   eval      CMIFSP = 'Q'
     C                   update    PCM01
     C                   endif
     C                   read      SDCMLH
     C                   enddo                                                  not %eof(SDCMLH)

      * SMMDL2
     C                   read      SMMDL2
     C                   dow       not %eof(SMMDL2)
     C                   if        MDUNCF = ' '
     C                   if        MDOFLR <> ' ' or MDOID <> ' '
     D***** BEG CHG ***** 10/17/11 ***********************************
     C*****              eval      MDUNCP = %trim(MDOFLR) + '/' +
     C*****                        MDOID
     C                   eval      MDUNCP = '/' + %trim(root) + '/' +
      *** Beg Add ***** 11/16/12 **************************************
     C                                      %trim(XXFLRP) +
      *** End Add ***** 11/16/12 **************************************
     C                                      %trim(MDOFLR) + '/' +
     C                                      %trim(MDOID)
     D***** END CHG ***** 10/17/11 ***********************************
     C                   else
     C                   eval      MDUNCP = ' '
     C                   endif
     C                   eval      MDUNCF = 'Q'
     C                   update    PMD01
     C                   endif
     C                   read      SMMDL2
     C                   enddo                                                  not %eof(SDCMLH)

     C***** BEG ADD ***** 10/17/11 ***********************************
      * IMDOL1
     C                   read      IMDOL1
     C                   dow       not %eof(IMDOL1)
     C                   if        DOIFSP = ' '
     C                   if        DOFLR <> ' ' or DODCID <> ' '
     C                   eval      DOUNCN = '/' + %trim(root) + '/' +
      *** Beg Add ***** 11/16/12 **************************************
     C                                      %trim(XXFLRP) +
      *** End Add ***** 11/16/12 **************************************
     C                                      %trim(DOFLR) + '/' +
     C                                      %trim(DOSFLR) + '/' +
     C                                      %trim(DODCID)
     C                   else
     C                   eval      DOUNCN = ' '
     C                   endif
     C                   eval      DOIFSP = 'Q'
     C                   update    PDO01
     C                   endif
     C                   read      IMDOL1
     C                   enddo                                                  not %eof(IMDOL1)
     C***** END ADD ***** 10/17/11 ***********************************
     C                   eval      *inlr = *on
     C                   return
