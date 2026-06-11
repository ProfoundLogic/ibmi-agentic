     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
    H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
    H*====================================================================
     H*  Revisions:
     H*
     H* 10/05/10 C:251 Project: WC1944
     H*                Add Edit TPOC date > CMS Date of Incident
     H*
    H*====================================================================
     Fw0716f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdmsp     uf a e             disk    rename(pms01:dbrcdf)
     Fwdmsl     if   e           k disk    rename(pms01:pms00)
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     F                                     infds(cmfds)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     F*** Beg Add *** 10/05/10 ****************************************
     FSSYLF     IF   E           K DISK             IGNORE(SPC01:SFB01:SRC01:
     F                                              SDR01:SBC01:SCS01:SGL01:
     F                                              SP201)
     F*** End Add *** 10/05/10 ****************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Multiple Settlements'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D addMode         s              1n
     D chgMode         s              1n
     D dspMode         s              1n
     D ##Call          s                   like(@@Mode)
     D ##Case          s                   like(CMCASE)
     D ##Co#           s                   like(CMCO#)
     D ##CoNm          s             40
     D ##FNam          s             40
     D ##Fnd           s                   like(CMFND)
     D ##Fyr           s                   like(CMFYR)
     D ##Key           s                   like(GTKEY)
     D ##PrDs          s                   like(scprds)
     D ##RtCd          s              1
     D ##TbId          s                   like(GTTBID)
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D kkKey           s                   like(GTKEY)
     D kkTbId          s                   like(GTTBID)
     D fnRRN           s              9  0
     D emRRN           s              9  0
     D ClaimAPI        c                   const('WA006R')
     D countWDMSL      s              3  0 inz
     D*
     D cmfds           ds
     D  cmRRN                397    400b 0
     D*
     D                 ds
     D line1                   1     79
     D  line1a                 1      6
     D  line1b                 8     10
     D  line1c                12     71
     D*
     D                 ds
     D line2                   1     79
     D  scYrCs                 8     18
     D  scSS#                 26     36
     D  scClNm                39     58
     D  scAcDt                69     78
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*** Beg Add *** 10/05/10 ****************************************
     D xxsyk           S              2
     D xxfl12          S              1
     D*** End Add *** 10/05/10 ****************************************
    D*
     D/copy wPrGroup
     D/copy wPrEmplr
    D*
     D/copy sCopyMT42C
    D*
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
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
     C*
     C     ClaimPl       plist
     C                   parm      CMCO#         ##Co#
     C                   parm      CMFND         ##Fnd
     C                   parm      CMFYR         ##Fyr
     C                   parm      CMCASE        ##Case
     C                   parm                    line1
     C                   parm                    line2
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     keyWDMSL      klist
     C                   kfld                    CMCO#
     C                   kfld                    CMFND
     C                   kfld                    CMFYR
     C                   kfld                    CMCASE
     C*
     C     gtky          klist
     C                   kfld                    CMCO#
     C                   kfld                    kkTbId
     C                   kfld                    kkKey
     C*** Beg Add *** 10/05/10 ****************************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK
     C*** End Add *** 10/05/10 ****************************************
     C*
    C*====================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*====================================================================
     C     dltpre        begsr
     C                   endsr
    C*
    C*====================================================================
    C* DSPLYR - Processing after displaying information
    C*====================================================================
     C     dsplyr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     errck1        begsr
    C*
     C  n90              if        sccams <> *blanks
     C                   eval      kkTbId = 'CAMS'
     C                   eval      kkKey  = sccams
     C     gtky          chain     pgt00
     C                   if        not %found
     C                              or GTSTAT <> 'A'
     C                   seton                                        71  90
     C                   endif
     C                   endif
    C*
     C  n90              if        sctpoa = *zeros
     C                   seton                                        72  90
     C                   endif
    C*
     C  n90              if        DtCheck(sctpod) = *zeros
     C                   seton                                        73  90
     C                   endif
    C*
     C  n90              if        CvtToDate(sctpod) < CvtToDate(CMACDT)
     C                   seton                                        75  90
     C                   endif
     C*** Beg Add *** 10/05/10 ****************************************
     C*
     C*
     C* GET SYSTEM CONTROL FILE
     C*
     C                   eval      xxfl12 = *blanks
     C                   eval      xxsyk ='SY'
     C     SYKY          chain     SSY01
     C                   if        %found
     C                   eval      xxfl12 = %subst(syfl12:2:1)
     C                   endif
     C*
     C*** TPOC date must be greater than CMS Date of Incident
     C*** if Medicare Secondary Payer is installed
     C*
     C                   if        xxfl12 = 'Y'
     C  n90              if        CvtToDate(sctpod) <= CvtToDate(CMCMDI)
     C                   seton                                        77  90
     C                   endif
     C                   endif
     C*** End Add *** 10/05/10 ****************************************
    C*
     C  n90              if        scdlyd <> *zeros
     C                              and DtCheck(scdlyd) = *zeros
     C                   seton                                        74  90
     C                   endif
    C*
     C  n90              if        scdlyd <> *zeros and
     C                              CvtToDate(scdlyd) < CvtToDate(CMACDT)
     C                   seton                                        76  90
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
     C     errck2        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
     C     errck3        begsr
     C                   endsr
    C*
    C*====================================================================
    C* Processing for F10
    C*====================================================================
     C     kjDo          begsr
     C*
     C                   select
     C*
     C* Method of Settlement
     C                   when      hfld = 'SCCAMS'
     C*
     C                   call      'W2493R'
     C                   parm      scprds        ##PrDs
     C                   parm      *blanks       ##Call
     C                   parm      'CAMS'        ##TbId
     C                   parm      *blanks       ##Key
     C                   parm      *blanks       ##RtCd            1
     C*
     C                   select
     C                   when      ##RtCd = 'A'
     C                   eval      *inkc = *on
     C                   when      ##RtCd = 'B'
     C                              or ##Key = *blanks
     C                   seton                                        71  89
     C                   when      ##RtCd = '*'
     C                   eval      sccams = ##Key
     C                   eval      kkTbId = 'CAMS'
     C                   eval      kkKey  = sccams
     C                   clear                   GTDESC
     C     gtky          chain     pgt00
     C                   eval      sccamsd = GTDESC
     C                   seton                                        72  89
     C                   endsl
     C*
     C*
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* Load the mainteance fields
    C*====================================================================
     C     MaintFlds     begsr
     C*
     C                   eval      MSUSER = q1user
     C                   eval      MSPGM  = q1pgm
     C                   eval      MSDT   = DateTo7(SystemDate)
     C                   eval      MSTIME = TimeTo6(SystemTime)
     C*
     C                   if        addMode
     C                   eval      MSSTAT = 'A'
     C                   eval      MSIUSR = MSUSER
     C                   eval      MSIPGM = MSPGM
     C                   eval      MSIDT  = MSDT
     C                   eval      MSITIM = MSTIME
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
     C     mvdbsc        begsr
    C*
     C                   eval      sccams = MSCAMS
     C                   eval      sctpoa = MSTPOA
     C                   eval      sctpod = DtMDYY(MSTPOD)
     C                   eval      scdlyd = DtMDYY(MSDLYD)
    C*
     C                   eval      kkTbId = 'CAMS'
     C                   eval      kkKey  = sccams
     C                   clear                   GTDESC
     C     gtky          chain     pgt00
     C                   eval      sccamsd = GTDESC
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*====================================================================
     C     mvscdb        begsr
    C*
     C                   eval      MSCO#  = CMCO#
     C                   eval      MSFND  = CMFND
     C                   eval      MSFYR  = CMFYR
     C                   eval      MSCASE = CMCASE
    C*
     C                   eval      MSCAMS = sccams
     C                   eval      MSTPOA = sctpoa
     C                   eval      MSTPOD = DtCYMD(sctpod)
     C                   eval      MSDLYD = DtCYMD(scdlyd)
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHERR - Processing for other modes
    C*====================================================================
     C     otherr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     othfks        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     pgmend        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
     C     pgmstr        begsr
     C*
     C                   eval      *in40 = @@Mode = 'A'
     C                   eval      *in09 = @@Mode = 'D'
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      chgMode = @@Mode = 'C'
     C*
     C* Chain out to claim file
     C     @@rrn2        chain     wmcmp
     C*
     C* Maximum of 4 records allowed
     C                   if        addMode
     C                   clear                   countWDMSL
     C     keyWDMSL      setll     wdmsl
     C                   dou       %eof
     C     keyWDMSL      reade     wdmsl
     C                   if        not %eof
     C                   eval      countWDMSL = countWDMSL + 1
     C                   endif
     C                   enddo
     C                   if        countWDMSL >= 4
     C                   eval      @@Mode = '4'
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C                   endif
     C*
     C*  If claim was converted from another system, display old claim #
     C                   if        CMCVT# <> *blank
     C                   eval      scCvt = CMCVT#
     C                   eval      *in64 = *on
     C                   endif
     C*
     C*  Agency claim # (or, state claim #)
     C                   eval      scSCl# = CMACL#
     C*
     C*  Call claims API
     C                   call      ClaimAPI      ClaimPl
     C*
     C*  Get employer name
     C                   eval      scEDN = EmpDivName(CMEMP#:
     C                                                CMDIV:
     C                                                emRRN:
     C                                                CMFND:
     C                                                CMCO#)
     C*
     C*  Get group name
     C                   eval      scFnd = CMFND
     C                   eval      scFNam = GetGrpName(CMFND:
     C                                                 fnRRN:
     C                                                 CMCO#)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PREOTH - Pre-processing for other modes
    C*====================================================================
     C     preoth        begsr
     C                   endsr
    C*
    C*====================================================================
    C* UPDATR - Update data base record and after processing
    C*====================================================================
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   exsr      maintFlds
     C                   update    dbrcdf
E01  C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WRITER - Write data base record and after processing
    C*====================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   exsr      maintFlds
     C                   write     dbrcdf
     C                   feod      wdmsp
     C*
     C                   eval      outFlg = *on
    C*
     C                   endsr
