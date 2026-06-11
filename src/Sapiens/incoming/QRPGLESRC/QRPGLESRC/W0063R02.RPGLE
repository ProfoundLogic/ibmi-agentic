     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/TITLE W0063R02 - Fee Type Information
     H*********************************************************************
     H* Created: 02/11/11   C:33   Project: WC1851                        *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
     Fw0063f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for maintaining record)
     Fwtftp     uf a e             disk    rename(pft01:dbrcdf)
     Fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwtftl     if   e           k disk    rename(pft01:pft00)
     F                                     infds(ftFDS)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Fee Type Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D xxAR            s                   like(FTBLAR)
     D xxKey           s                   like(GTKEY)
     D found           s              1n
     D saveScType      s                   like(scType)
     D addMode         s              1n
     D dspMode         s              1n
     D chgMode         s              1n
     D cpyMode         s              1n
     D cpyModeMass     s              1n
     D#ofSFD           c                   60
     D sf              s              1    dim(#ofSFD) ctdata perrcd(1)         Screen Functions
     D sfd             s             35    dim(#ofSFD) alt(SF)
     D#ofFL            c                   14
     D fl              s             78    dim(#ofFL)                           Function lines
     D fl1             s             78    dim(#ofFL)                           Function lines
     D fl2             s             78    dim(#ofFL)                           Function lines
     D fl3             s             78    dim(#ofFL)                           Function lines
     D#ofVF            c                   24
     D vf              s              1    dim(#ofVF)                           Valid functions
     D vf1             s              1    dim(#ofVF)                           Valid functions
     D vf2             s              1    dim(#ofVF)                           Valid functions
     D vf3             s              1    dim(#ofVF)                           Valid functions
     D*
     D ##SFD           ds          3500
     D ftFDS           ds
     D ftRRN                 397    400b 0
     D fids01        e ds                  EXTNAME(szq2p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DAllowMultiples   pr             1n
     D inKey                               like(GTKEY)  const
     D*
     DCopyFee          pr
     D key                                 like(FTTYPE) const
     D desc                                like(FTDESC) const
     D fromGrp                             like(FTFND)  const
     D toGrp                               like(FTFND)  const
     D*
     DGenTableDesc     pr                  like(GTDESC)
     D inKey                               like(GTKEY)  const
     D inTableId                           like(GTTBID) const
     D*
     DMassCopyFee      pr
     D fromGrp                             like(FTFND) const
     D toGrp                               like(FTFND) const
     D*
     DSearchGenTable   pr                  like(GTKEY)
     D inTableId                           like(GTTBID) const
     D*
     DValidGenTable    pr             1n
     D inKey                               like(GTKEY)  const
     D inTableId                           like(GTTBID) const
     D*
     D/copy sPrDate
     D/copy sPrString
     D/copy wPrAr
     D/copy wPrGroup
    C*
     C/copy sCopyMT41c
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     keyWTFTL      klist
     C                   kfld                    ##Co#
     C                   kfld                    FNFND
     C                   kfld                    scType
     C                   kfld                    scDesc
     C*
     C     key1WTFTL     klist
     C                   kfld                    ##Co#
     C                   kfld                    FNFND
     C*
     C*================================================================
     C* BldLns - Builds functions lines
     C*================================================================
     C     BldLns        begsr
     C*
     C                   sub       fl1#1         fl1#1             3 0
     C                   sub       fl1#2         fl1#2             3 0
     C                   sub       fl2#1         fl2#1             3 0
     C                   sub       fl2#2         fl2#2             3 0
     C                   sub       fl3#1         fl3#1             3 0
     C                   sub       fl3#2         fl3#2             3 0
     C                   movea(p)  sf            ##sf
     C                   movea(p)  sfd           ##sfd
     C*
     C     1             do        #ofScr        xxScr#            1 0
     C*
     C                   MOVE      xxScr#        ##Scr#
     C*
     C                   call      'SRMNTFNC'                           99
     C                   parm      @@Mode        ##Mode            1
     C                   parm                    ##sf            100
     C                   parm                    ##sfd
     C                   parm                    fl
     C                   parm                    vf
     C                   parm                    ##Scr#            1
     C                   parm      q1pgm         ##Pgm            10
     C*
     C                   select
     C                   when      xxScr# = 1
     C                   movea     fl            fl1
     C                   movea     vf            vf1
     C                   eval      q2Recf = 'SCR01'
     C                   when      xxScr# = 2
     C                   movea     fl            fl2
     C                   movea     vf            vf2
     C                   eval      q2Recf = 'SCR02'
     C                   when      xxScr# = 3
     C                   movea     fl            fl3
     C                   movea     vf            vf3
     C                   eval      q2Recf = 'SCR03'
     C                   endsl
      *
     C                   exsr      KyDo
      *
     C                   enddo
      *
     C                   endsr
      *
     C*===============================================================
     C* ChgCDA - Update values in the function array
     C*===============================================================
     C     ChgCDA        BEGSR
     C*
     C                   move      *off          found
     C                   z-add     1             I                 2 0
     C*
     C*  Go through array looking for a match
     C*
     C                   dow       not found  and sf(i) <> '*'
     C*
     C                   if        sf(i) = xxScrN
     C                   movel     sfd(i)        dumNum            2
     C                   if        xxNmbr = dumNum
     C                   movel(p)  sfd(i)        dumOFD           30
     C                   move      xxCDA         dumOFD
     C                   movel     dumOFD        sfd(i)
     C                   eval      found = *on
E03  C                   endif                                                  ZZNUM=DUMNUM
E02  C                   endif                                                  OF,I=xxScrN
     C*
     C                   eval      i = i + 1                                    INCREMENT COUNT
     C*
E01  C                   enddo                                                  found DOWEQ *OFF
     C*
     C                   endsr
     C*
     C*================================================================
     C*  ChkVFK - Check for valid function key being pressed
     C*================================================================
     C     ChkVFK        begsr
      *
     C                   SUB       SF#           SF#               3 0
     C                   TESTB     '1'           Q2IAID                   99
      *
B01  C     *IN99         IFEQ      *OFF                                         Function pressd
      *
     C                   TESTB     '7'           Q2IAID                   99
     C   99              ADD       1             SF#
     C                   TESTB     '6'           Q2IAID                   99
     C   99              ADD       2             SF#
     C                   TESTB     '5'           Q2IAID                   99
     C   99              ADD       4             SF#
     C                   TESTB     '4'           Q2IAID                   99
     C   99              ADD       8             SF#
     C                   TESTB     '0'           Q2IAID                   99
     C   99              ADD       12            SF#
      *
B02  C     VF(SF#)       IFEQ      *OFF
     C                   SETON                                        90  95
E02  C                   ENDIF
      *
E01  C                   ENDIF
      *
     C                   endsr
      *
    C*====================================================================
    C* CopyRoutine - Process copy(s)
    C*====================================================================
     C     CopyRoutine   begsr
     C*
     C     @@rrn         chain     dbRcdF
     C*
     C                   eval      scFromGrp = FTFND
     C*
     C                   exsr      CopyWindow
     C*
     C                   if        *inkl
     C                   leavesr
     C                   endif
     C*
     C                   select
     C* Mass copy
     C                   when      @@Mode = 'M'
     C                   callp     MassCopyFee(scFromGrp:scToGrp)
     C* Single copy
     C                   when      @@Mode = 'P'
     C                   callp     CopyFee(FTTYPE:FTDESC:scFromGrp:scToGrp)
     C*
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* CopyWindow - Process copy window
    C*====================================================================
     C     CopyWindow    begsr
     C*
     C                   select
     C                   when      @@Mode = 'M'
     C                   eval      scMass = 'Mass Copy'
     C                   when      @@Mode = 'P'
     C                   eval      scMass = 'Single Copy'
     C                   endsl
     C                   eval      scMass = CenterFld(scMass)
     C*
     C                   dou       not *in90
     C                              or *inkl
     C                   eval      *in90 = *off
     C                   exfmt     WDWCPY
     C                   if        not *inkl
     C                   exsr      ErrCkWdwCpy
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
    C*====================================================================
    C* DltPre - Pre-processing before delete occurs
    C*====================================================================
     C     DltPre        begsr
     C                   endsr
     C*
    C*====================================================================
    C* DsplyR - Processing after displaying information
    C*====================================================================
     C     DsplyR        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     ErrCk1        begsr
     C*
     C                   if        scType <> saveScType
     C                   exsr      ResetType
     C                   leavesr
     C                   endif
     C*
     C                   if        AddMode
     C                              and not ValidGenTable(scType:'FEES')
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
     C*
     C                   if        AddMode
     C                              and not AllowMultiples(scType)
     C                   seton                                        88  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scDesc = *blanks
     C                   seton                                        72  90
     C                   leavesr
     C                   endif
     C*
     C* Check for duplicate record
     C                   if        AddMode
     C     keyWTFTL      chain     pft00
     C                   if        %found
     C                   seton                                        73  90
     C                   endif
     C                   endif
     C*
     C* Billing A/R
     C                   if        ValidCode(scBlAr) = *off
     C                   seton                                        74  90
     C                   leavesr
     C                   endif
     C                   if        GetARType(scBlAr) <> 'F'
     C                              or GetARCat(scBlAr) <> 'F'
     C                   seton                                        84  90
     C                   leavesr
     C                   endif
     C*
     C* Payment A/R
     C                   if        ValidCode(scPyAr) = *off
     C                   seton                                        75  90
     C                   leavesr
     C                   endif
     C                   if        GetARType(scPyAr) <> 'Q'
     C                              or GetARCat(scPyAr) <> 'F'
     C                   seton                                        85  90
     C                   leavesr
     C                   endif
     C*
     C* Transfer A/R
     C                   if        ValidCode(scTrAr) = *off
     C                   seton                                        76  90
     C                   leavesr
     C                   endif
     C                   if        GetARType(scTrAr) <> 'Q'
     C                              or GetARCat(scTrAr) <> 'F'
     C                   seton                                        86  90
     C                   leavesr
     C                   endif
     C*
     C* A/R Codes must be unique
     C                   if        scBlAr = scPyAr
     C                              or scBlAr = scTrAr
     C                              or scPyAr = scTrAr
     C                   seton                                        77  90
     C                   leavesr
     C                   endif
     C     key1WTFTL     setll     pft00
     C                   dou       %eof
     C     key1WTFTL     reade     pft00
     C                   if        not %eof and ftRRN <> @@rrn and
     C                             ((scBlAr = FTBLAR
     C                                or scBlAr = FTPYAR
     C                                or scBlAr = FTTRAR)
     C                              or
     C                              (scPyAr = FTBLAR
     C                                or scPyAr = FTPYAR
     C                                or scPyAr = FTTRAR)
     C                              or
     C                              (scTrAr = FTBLAR
     C                                or scTrAr = FTPYAR
     C                                or scTrAr = FTTRAR)
     C                              )
     C                   seton                                        78  90
     C                   leavesr
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
     C     ErrCk2        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
     C     ErrCk3        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ErrCkWdwCpy - Error checking for window (WDWCPY)
    C*====================================================================
     C     ErrCkWdwCpy   begsr
     C*
     C                   if        scFromGrp = scToGrp
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scToGrp <> *zero
     C                              and ValidGrp(scToGrp) = *off
     C                   seton                                        72  90
     C                   leavesr
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* KjDo - Processing for F10
    C*====================================================================
     C     KjDo          begsr
     C*
     C                   select
     C* A/R Code Search
     C                   when      hfld = 'SCTYPE'
     C                              and addMode
     C                   eval      xxKey = SearchGenTable('FEES')
     C                   if        xxKey = *blanks
     C                   seton                                        7189
     C                   leavesr
     C                   endif
     C                   eval      SCTYPE = xxKey
     C                   exsr      ResetType
     C* A/R Code Search
     C                   when      hfld = 'SCBLAR'
     C                              or hfld = 'SCPYAR'
     C                              or hfld = 'SCTRAR'
     C                   eval      xxAR = ARCodeSrch(*inkc:*inkl)
     C                   if        xxAR <> *blank
     C                   select
     C                   when      hfld = 'SCBLAR'
     C                   eval      scBlAr = xxAR
     C                   seton                                        7589
     C                   when      hfld = 'SCPYAR'
     C                   eval      scPyAr = xxAR
     C                   seton                                        7689
     C                   when      hfld = 'SCTRAR'
     C                   eval      scTrAr = xxAR
     C                   seton                                        7789
     C                   endsl
     C                   else
     C                   select
     C                   when      hfld = 'SCBLAR'
     C                   seton                                        7489
     C                   when      hfld = 'SCPYAR'
     C                   seton                                        7589
     C                   when      hfld = 'SCTRAR'
     C                   seton                                        7689
     C                   endsl
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr
      *
      *===============================================================
      * KyDo - Processing for F24
      *===============================================================
     C     KyDo          BEGSR
     C*
     C                   select
     C* SCR01
     C                   when      q2Recf = 'SCR01'
     C     fl1#2         ADD       1             fl1#1
B01  C     fl1#1         IFGT      12
     C     fl1(fl1#1)    OREQ      *BLANKS
     C                   Z-ADD     1             fl1#1
E01  C                   END
     C     fl1#1         ADD       1             fl1#2
     C                   MOVEL     fl1(fl1#1)    SC1FK1
     C                   MOVEL     fl1(fl1#2)    SC1FK2
     C* SCR02
     C                   when      q2Recf = 'SCR02'
     C     fl2#2         ADD       1             fl2#1
B01  C     fl2#1         IFGT      12
     C     fl2(fl2#1)    OREQ      *BLANKS
     C                   Z-ADD     1             fl2#1
E01  C                   END
     C     fl2#1         ADD       1             fl2#2
     C                   MOVEL     fl2(fl2#1)    SC2FK1
     C                   MOVEL     fl2(fl2#2)    SC2FK2
     C* SCR03
     C                   when      q2Recf = 'SCR03'
     C     fl3#2         ADD       1             fl3#1
B01  C     fl3#1         IFGT      12
     C     fl3(fl3#1)    OREQ      *BLANKS
     C                   Z-ADD     1             fl3#1
E01  C                   END
     C     fl3#1         ADD       1             fl3#2
     C                   MOVEL     fl3(fl3#1)    SC3FK1
     C                   MOVEL     fl3(fl3#2)    SC3FK2
     C*
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MaintFlds - Load maintenance fields
    C*====================================================================
     C     MaintFlds     begsr
     C*
     C                   eval      FTUSER = q1User
     C                   eval      FTPGM  = q1Pgm
     C                   eval      FTDT   = DateTo7(SystemDate)
     C                   eval      FTTIME = TimeTo6(SystemTime)
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MaintFldsInit - Load initial maintenance fields
    C*====================================================================
     C     MaintFldsInit begsr
     C*
     C                   exsr      MaintFlds
     C*
     C                   eval      FTIUSR = FTUSER
     C                   eval      FTIPGM = FTPGM
     C                   eval      FTIDT  = FTDT
     C                   eval      FTITM  = FTTIME
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
     C*
     C                   eval      scTypeName = GenTableDesc(FTTYPE:'FEES')
     C*
     C                   eval      scType = FTTYPE
     C                   eval      scDesc = FTDESC
     C                   eval      scBlAr = FTBLAR
     C                   eval      scPyAr = FTPYAR
     C                   eval      scTrAr = FTTRAR
     C*
     C                   eval      saveScType = scType
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MvScDb - Move screen fields to data base fields
    C*====================================================================
     C     MvScDb        begsr
     C*
     C                   eval      FTCO#  = FNCO#
     C                   eval      FTFND  = FNFND
     C*
     C                   eval      FTTYPE = scType
     C                   eval      FTDESC = scDesc
     C                   eval      FTBLAR = scBlAr
     C                   eval      FTPYAR = scPyAr
     C                   eval      FTTRAR = scTrAr
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OTHERR - Processing for other modes
    C*====================================================================
     C     otherr        begsr
     C                   endsr
     C*
    C*====================================================================
    C* OthFks - Processing for other functions keys
    C*====================================================================
     C     OthFks        begsr
     C*
     C                   select
     C                   when      q2Recf = 'SCR01'
     C                   movea     vf1           vf
     C                   when      q2Recf = 'SCR02'
     C                   movea     vf2           vf
     C                   when      q2Recf = 'SCR03'
     C                   movea     vf3           vf
     C                   endsl
     C*
     C                   exsr      ChkVFK
     C*
     C                   if        not *in90
     C*
     C     *inkj         caseq     *on           KjDo                     90
     C     *inky         caseq     *on           KyDo                     90
     C                   endcs
     C*
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
     C                   eval       *inlr = *on
     C                   endsr
     C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      chgMode = @@Mode = 'C'
     C                   eval      cpyMode = @@Mode = 'P'
     C                   eval      cpyModeMass = @@Mode = 'M'
     C*
     C                   eval      *in40 = addMode
     C                   eval      *in09 = dspMode
     C*
     C     @@rrn2        chain     WMFNP
     C*
     C                   eval      scFnd  = FNFND
     C                   eval      scFNam = FNNAME
     C*
     C                   select
     C                   when      @@Mode = 'M'
     C                              or @@Mode = 'P'
     C                   exsr      CopyRoutine
     C                   eval      *inlr = *on
     C                   return
     C                   endsl
     C*
     C                   exsr      SetFks
     C                   exsr      BldLns
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PreOth - Pre-processing for other modes
    C*====================================================================
     C     PreOth        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ResetType - Reset the type information
    C*====================================================================
     C     ResetType     begsr
     C*
     C                   eval      saveScType = scType
     C                   eval      scTypeName = GenTableDesc(scType:'FEES')
     C                   if        scDesc = *blanks
     C                   eval      scDesc = scTypeName
     C                   endif
     C                   seton                                        728990
     C*
     C                   endsr
     C*
      *===============================================================
      * SetFks - Set key values
      *===============================================================
     C     SetFks        BEGSR
      *
     C                   move      *blanks       xxScrN            1
     C                   move      *blanks       xxNmbr            2
     C                   move      *blanks       xxCDA             3
      *
      *
     C                   endsr
     C*
    C*====================================================================
    C* UpdatR - Update data base record and after processing
    C*====================================================================
     C     UpdatR        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      MvScDb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
     C*
    C*====================================================================
    C* WriteR - Write data base record and after processing
    C*====================================================================
     C     WriteR        begsr
     C                   clear                   dbrcdf
     C                   exsr      MvScDb
     C                   exsr      MaintFldsInit
     C                   write     dbrcdf
     C                   feod      wtftp
     C                   endsr





     P*====================================================================
     P* Validate generic table value
     P*====================================================================
     PAllowMultiples   b
     DAllowMultiples   pi             1n
     D inKey                               like(GTKEY)  const

     C                   select
     C                   when      inKey = 'GEN'
     C                   return    *off
     C                   other
     C                   return    *on
     C                   endsl

     PAllowMultiples   e





     P*====================================================================
     P* Process copy
     P*====================================================================
     PCopyFee          b
     DCopyFee          pi
     D key                                 like(FTTYPE) const
     D desc                                like(FTDESC) const
     D fromGrp                             like(FTFND)  const
     D toGrp                               like(FTFND)  const

     C     fromKeyWTFTL  klist
     C                   kfld                    ##Co#
     C                   kfld                    fromGrp
     C                   kfld                    key
     C                   kfld                    desc
     C*
     C     toKeyWTFTL    klist
     C                   kfld                    ##Co#
     C                   kfld                    toGrp
     C                   kfld                    FTTYPE
     C                   kfld                    FTDESC

     C     fromKeyWTFTL  chain     pft00
     C* Do not process if record not found
     C                   if        not %found
     C                   return
     C                   endif
     C*
     C                   if        toGrp = *zero
     C                   exsr      GroupCopy
     C                   return
     C                   endif
     C*
     C     toKeyWTFTL    chain     pft00
     C* Do not process if record is found for TO group
     C                   if        %found
     C                   return
     C                   endif
     C*
     C* Write the record for the new group
     C                   eval      FTFND  = toGrp
     C                   eval      FTUSER = q1User
     C                   eval      FTPGM  = q1Pgm
     C                   eval      FTDT   = DateTo7(SystemDate)
     C                   eval      FTTIME = TimeTo6(SystemTime)
     C                   eval      FTIUSR = FTUSER
     C                   eval      FTIPGM = FTPGM
     C                   eval      FTIDT  = FTDT
     C                   eval      FTITM  = FTTIME
     C                   write     dbrcdf
     C                   feod      wtftp
     C*
     C                   return
     C*
     C*====================================================================
     C* Proecess groups (this will create a recursive call)
     C*====================================================================
     C     GroupCopy     begsr
     C*
     C     ##Co#         setll     pfn00
     C*
     C                   dou       %eof(WMFNL)
     C     ##Co#         reade     pfn00
     C                   if        not %eof
     C* Do not process same group
     C                   if        FNFND = fromGrp
     C                   iter
     C                   endif
     C*
     C                   callp     CopyFee(key:desc:fromGrp:FNFND)
     C*
     C                   endif
     C*
     C                   enddo
     C*
     C                   endsr

     PCopyFee          e





     P*====================================================================
     P* Get generic table description
     P*====================================================================
     PGenTableDesc     b
     DGenTableDesc     pi                  like(GTDESC)
     D inKey                               like(GTKEY)  const
     D inTableId                           like(GTTBID) const

     C     keyWTGTL      klist
     C                   kfld                    ##Co#
     C                   kfld                    inTableId
     C                   kfld                    inKey

     C                   clear                   GTDESC
     C     keyWTGTL      chain     pgt00
     C                   return    GTDESC

     PGenTableDesc     e





     P*====================================================================
     P* Process mass copy
     P*====================================================================
     PMassCopyFee      b
     DMassCopyFee      pi
     D fromGrp                             like(FTFND)  const
     D toGrp                               like(FTFND)  const

     C     keyWTFTL      klist
     C                   kfld                    ##Co#
     C                   kfld                    fromGrp

     C     keyWTFTL      setll     pft00
     C                   dou       %eof(wtftl)
     C     keyWTFTL      reade     pft00
     C                   if        not %eof
     C                   callp     CopyFee(FTTYPE:FTDESC:fromGrp:toGrp)
     C                   endif
     C                   enddo
     C                   return

     PMassCopyFee      e





     P*====================================================================
     P* Search generic table
     P*====================================================================
     PSearchGenTable   b
     DSearchGenTable   pi                  like(GTKEY)
     D inTableId                           like(GTTBID) const
     D*
     D##PrDs           s             40
     D##Call           s              1
     D##TbId           s                   like(GTTBID)
     D##Key            s                   like(GTKEY)
     D##RtCd           s              1

     C                   call      'W2493R'
     C                   parm      scPrDs        ##PrDs
     C                   parm      '2'           ##Call
     C                   parm      inTableId     ##TbId
     C                   parm      *blanks       ##Key
     C                   parm                    ##RtCd
     C*
     C                   return    ##Key

     PSearchGenTable   e





     P*====================================================================
     P* Validate generic table value
     P*====================================================================
     PValidGenTable    b
     DValidGenTable    pi             1n
     D inKey                               like(GTKEY)  const
     D inTableId                           like(GTTBID) const

     C     keyWTGTL      klist
     C                   kfld                    ##Co#
     C                   kfld                    inTableId
     C                   kfld                    inKey

     C     keyWTGTL      chain     pgt00
     C                   return    %found

     PValidGenTable    e
** Description--------------CDAN
103F3=Exit                  YYY
105F5=Refresh               YYN
110F10=Search               YNY
112F12=Cancel               YYY
203F3=Exit                  YYN
303F3=Exit                  YYN
*
