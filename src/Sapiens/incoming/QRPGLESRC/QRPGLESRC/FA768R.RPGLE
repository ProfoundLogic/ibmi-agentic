     H*Copyright('(c) 2023 Sapiens. All Rights Reserved.')            *
     H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*                                                               *
     H dftactgrp(*no) actgrp(*caller)
     H* API - FA768R - Florida: Retrieve EOB X-ref                    *
     H*****************************************************************
     H* Description:  Retrieves the EOB X-ref                         *
     H*                                                               *
     H*  Called From: FA713R - API to call Adjudication Programs      *
     H*                                                               *
     H*  Calls To:    None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 05/23/2011 by C:251   for  Project FS389           *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*****************************************************************
     F*
     FFAEOP     IF   E             DISK
     FFDDEL     UF   E           K DISK
     FFAEEL     UF   E           K DISK
     FFTEBLFL   IF   E           K DISK
     F*
     Dsrdti            S              8  0
     Dsrffmt           S              8
     Dsrtfmt           S              8
     Dsrdto            S             10
     Dsrerr            S              1
     D*
     D@@call           S              1
     D@@rrn            S              9  0
     D@@rtcd           S              1
     D*
     Dkkco#            S                   like(eoco#)
     Dkksub            S                   like(eosub#)
     Dkkfnd            S                   like(eofnd)
     Dkkfyr            S                   like(eofyr)
     Dkkcase           S                   like(eocase)
     Dkkeob#           S                   like(eoeob#)
     Dkkkey            S                   like(ebkey)
     D*
     DPSDS           ESDS                  extname(SZQ1P)
     D*
     D                 DS
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@rtcd
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C*================================================================
     C* Key lists
     C*================================================================
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     C*
     C     eeky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     deky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*================================================================
     C* Main line
     C*================================================================
     C*
     C* CHAIN TO FAEOP TO GET THE DETAILS
     C*
     C     @@rrn         chain     FAEOP
     C                   if        %found
     C                   eval      kkco# = eoco#
     C                   eval      kksub = eosub#
     C                   eval      kkfnd = eofnd
     C                   eval      kkfyr = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C*
     C** Read through FAEEL for the EOB code update - Header Level
     C*
     C     eeky          setll     FAEEL
     C     eeky          reade     FAEEL
     C*
     C                   dow       not %EOF
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               if        eeeob = '396' or eeeob = '397' or
     C****                         eeeob = '398' or eeeob = '97'
     C                   if        eeeobcd = '396' or eeeobcd = '397' or
     C                             eeeobcd = '398' or eeeobcd = '97'
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   exsr      SrUpdHdr
     C                   endif
     C*
     C     eeky          reade     FAEEL
     C                   enddo
     C*
     C** Read through FDDEL for the EOB code source - Detail line level
     C*
     C     deky          setll     FDDEL
     C     deky          reade     FDDEL
     C*
     C                   dow       not %EOF
     C*
     C                   if        deesrc = 'N'  or deesrc = 'C'
     C                   exsr      SrUpdDtl
     C                   endif
     C*
     C     deky          reade     FDDEL
     C                   enddo
     C
     C                   endif
     C*
     C                   return
     C*--------------------------------------------------------------------------
     C*    SrUpdHdr - Update Header EOB codes/Descriptions
     C*--------------------------------------------------------------------------
     C     SrUpdHdr      Begsr
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               eval      kkkey = eeeob
     C                   eval      kkkey = eeeobcd
     C**** End Chg *** 07/18/23 **************************************
     C                   exsr      SrXRef
     C*
     C* If X-ref found , get the description and update
     C*
     C                   if        kkkey <> *blanks
     C*
     C     ebky          chain     PEB01
     C                   if        %found
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               eval      eeeob  = ebkey
     C                   eval      eeeobcd  = ebkey
     C**** End Chg *** 07/18/23 **************************************
     C                   eval      eenot1 = ebdesc
     C                   eval      eenot2 = ebdsc2
     C                   eval      eenot3 = ebdsc3
     C                   eval      eenot4 = ebdsc4
     C*
    C* Load maintenance fields
     C                   eval      eestat = 'A'
     C                   eval      eeuser = q1user
     C                   eval      eepgm = q1pgm
     C*
     C                   TIME                    DSDTTM
     C                   z-add     dstime        eetime
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         eedt
     C*
     C                   update    PEE01
     C                   endif                                                  % found
     C                   endif                                                  kkkey <> *blanks
     C*
     C                   Endsr
     C*--------------------------------------------------------------------------
     C*    SrUpdDtl - Update Detail EOB codes/Descriptions
     C*--------------------------------------------------------------------------
     C     SrUpdDtl      Begsr
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               eval      kkkey = deeob
     C                   eval      kkkey = deeobcd
     C**** End Chg *** 07/18/23 **************************************
     C                   exsr      SrXRef
     C*
     C* If X-ref found , get the description and update
     C*
     C                   if        kkkey <> *blanks
     C*
     C     ebky          chain     PEB01
     C                   if        %found
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               eval      deeob  = ebkey
     C                   eval      deeobcd  = ebkey
     C**** End Chg *** 07/18/23 **************************************
     C                   eval      denot1 = ebdesc
     C                   eval      denot2 = ebdsc2
     C                   eval      denot3 = ebdsc3
     C                   eval      denot4 = ebdsc4
     C*
    C* Load maintenance fields
     C                   eval      destat = 'A'
     C                   eval      deuser = q1user
     C                   eval      depgm = q1pgm
     C*
     C                   TIME                    DSDTTM
     C                   z-add     dstime        detime
     C*
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         dedt
     C*
     C                   update    PDE01
'    C*
     C                   endif                                                  % found
     C                   endif                                                  kkkey <> *blanks
     C*
     C                   Endsr
     C*--------------------------------------------------------------------------
     C*    SrXRef - Get X-Ref for EOB codes
     C*--------------------------------------------------------------------------
     C     SrXRef        Begsr
     C*
     C*
     C     ebky          chain     PEB01
     C                   if        %found
     C                   eval      KKKey = ebxref
     C                   else
     C                   eval      KKKey = *blanks
     C                   endif
     C                   Endsr
     C*
