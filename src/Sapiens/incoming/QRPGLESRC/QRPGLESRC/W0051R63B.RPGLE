     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*NEW)
     H*********************************************************************
     H/TITLE W0051R63B - Maintain Fee Charges - Collateral
     H*********************************************************************
     H*  Warning - This program does not use copy code in standard
     H*            expected ways.
     H*
     H*  Created: 03/11/10    C:33    Project: WC1851                     *
     H*********************************************************************
     H* REVISIONS:
     H*                                                                   *
     H* 09/21/11  C:65  Project: WC1851                                   *
     H*                 Change activation group above to *NEW.            *
     H*
     H* 05/09/11  C:65  Project: WC1851
     H*                 Change clear function to reset fee detail to group
     H*                 values if override flag is set.
     H*
     H* 02/11/11  C:33  Project: WC1851
     H*                 Rewrite. No Comments.
     H*
     H* 05/26/10  C:61  Project: WC1851
     H*                 Fix calls to searches. Screen issues.
     H*
     H*********************************************************************
     FW0051F63B CF   E             WORKSTN INFDS(FIDS01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWDEFP     UF A E             DISK    RENAME(PEF01:DBRCDF)
     FWDEFL1    IF   E           K DISK
     Fwdell     if   e           k disk    rename(pel01:pel00)
     Fwtfil     if   e           k disk    rename(pfi01:pfi00)
     Fwtftl     if   e           k disk    rename(pft01:pft00)
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     Fwhfdl     if   e           k disk    rename(pfd01:pfd00)
     Fwdfcl1    if   e           k disk
     Fwtgtl     if   e           k disk
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
     Fwmahp     if   e             disk    rename(pah01:pah0p)
      *
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D @@co#           s              3  0
     D @@fnd           s              3  0
     D @@fnm           s             60
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     d xxSt            s                   like(RSJRST)
     d xxANam          s             40
     d xxKey           s                   like(GTKEY )
     d xzTbId          s                   like(GTTBID)
     d saveScExCo      s                   like(scExCo)
     d agtFein         s              9  0
     D agtMod          s              3  0
     d ahRRN           s                   like(@@rrn)
     d apRRN           s                   like(@@rrn)
     d emRRN           s                   like(@@rrn)
     d fnRRN           s                   like(@@rrn)
     d addMode         s              1n
     d cpyMode         s              1n
     d dspMode         s              1n
     d updMode         s              1n
     d optNm           s             40
     d found           s              1n
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D #OFSCR          C                   CONST(1)
      *
     D FIDS01        e ds                  extName(SZQ2P)
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
     D*
     DAddDots          pr                  like(scExcPr)
     D inPrompt                            like(scExcPr) const
     D*
     DGetExcPrompt     pr                  like(scExcPr)
     D inDesc                              like(FTDESC)  const
     D*
     DGetStopPrompt    pr                  like(scStopPr)
     D inDesc                              like(FTDESC)  const
     D*
     DGretestDtlDt     pr              d
     D*
     DEmpOvrExists     pr             1n
     D*
     DFeeDtlExists     pr             1n
      *
     D/copy wPrAgent
     D/copy wPrEmplr
     D/copy wPrGroup
     D/copy wPrProduce
    C*
     C/copy sCopyMT42c
      *
     C*================================================================
     C*  PLIST Definitions
     C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C*================================================================
     C*  KLIST Definitions
     C*================================================================
     C*
     C     elKy1         klist
     C                   kfld                    EFCO#
     C                   kfld                    EFFND
     C                   kfld                    EFEMP#
     C                   kfld                    EFDIV
     C                   kfld                    EFFYR
     C*
     C* KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    xzTbId
     C                   KFLD                    xxKey
     C*
     C*  GROUP DETAIL FILE
     C*
     C     F1KY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
      *
     C     RSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
     C     keyWTFTL      KLIST
     C                   KFLD                    EFCO#
     C                   KFLD                    EFFND
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
      *
     C     fiKy          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
      *
     C     fiKy1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
     C                   KFLD                    XXST
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
     C*
     C*===============================================================
     C* Clear employer override data
     C*===============================================================
     C     ClrEmpOvr     BEGSR
     C*
     C                   if        not FeeDtlExists
     c                   clear                   sccobs
     c                   clear                   sccopn
     c                   clear                   sccoff
     c                   clear                   sccoma
     c                   endif
     C*
     C                   ENDSR                                                  CHGSTS
     c*** begin add ***  05/09/11  ***********************************************
     C*===============================================================
     C* Reset employer override data back to group values.
     C*===============================================================
     C     ResEmpOvr     BEGSR
     C*
     C                   if        not FeeDtlExists
     C                   eval      SCCOBS = FICOBB
     C                   eval      SCCOMA = FIMNCA
     C                   eval      SCCOPN = FICOPN
     C                   eval      SCCOFF = FICOFF
     c                   endif
     C*
     C                   ENDSR
     c*** end add *****  05/09/11  ***********************************************
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C     CHGSTS        BEGSR
     C                   ENDSR                                                  CHGSTS
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C* If exclusion flag is set, clear override data
     C                   if        scExCo = 'Y'
     C                              and saveScExCo <> scExCo
     C                   eval      saveScExCo = scExCo
     C                   exsr      ClrEmpOvr
     C                   seton                                        09  90
     C                   leavesr
     C                   endif
      *
      * Exclude employer from colatteral
     C  n90              if        (scexco <> *blanks)
     C                              and (scexco <> 'Y')
     c                   eval      *in90 = *on
     c                   eval      *in79 = *on
     C                   endif
      *
     C* Validation of stop date....
     C                   if        scSCBD <> *zeros
     C                              and DtCheck(scSCBD) = *zeros
     C                   seton                                        78  90
     C                   leavesr
     C                   endif
      *
     C* Stop date can not be less than fee detail date
     C                   if        scSCBD <> *zeros
     C                              and CvtToDate(scSCBD) < GretestDtlDt
     C                   seton                                        89  90
     C                   leavesr
     C                   endif
      *
      * Collateral Fee billing... Billing based on should be valid..
     C  n90              if        scCoBs <> *blanks
     C                   eval      xzTbId = 'COBB'
     C                   eval      xxKey  = scCoBs
     C     GTKY          chain     PGT01
     C                   if        not %found
     C                              or GTSTAT <> 'A'
     C                   seton                                        71  90
     C                   endif
     C                   endif
      *
     C  n90              if        scCoBs <> *blanks
     C                              and (scCoBs = 'E' or scCoBs = 'P')
     C                   clear                   F1CLMS
     C     F1KY1         chain     PF100
     C                   if        F1CLMS = 'Y'
     C                   seton                                        72  90
     C                   endif
     C                   endif
      *
      * Validation on Collateral Fee ... Percentage/Flat Fee
     C  n90              if        scCoBs <> *blanks
     C                              and (SCCOBS = 'C' or SCCOBS = 'E' or
     C                                   SCCOBS = 'I' or SCCOBS = 'P')
     C                   select
     C                   when      SCCOPN = *zeros
     C                   seton                                        73  90
     C                   when      SCCOFF <> *zeros
     C                   seton                                        74  90
     C                   endsl
     C                   endif
      *
     C                   if        scCoBs <> *blanks
     C                              and scCoBs = 'F'
     C                   select
     C                   when      SCCOPN <> *zeros
     C                   seton                                        75  90
     C                   when      SCCOFF = *zeros
     C                   seton                                        76  90
     C                   endsl
     C                   endif
      *
      * IF Collateral flat fee is entered, the Minimum Collateral  amount should be Zeros
     C  n90              if        scCoff <> *zeros
     C                              and scComa <> *zeros
     C                   seton                                        77  90
     C                   endif
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   if        DtCheck(scSCBD) <> *zeros
     C                   eval      scSCBD = DtCheck(scSCBD)
     C                   endif
     C*
     C                   select
     C*
     C                   when      hfld = 'SCCOBS'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##PrDs           40
     C                   PARM      '2'           ##Call            1
     C                   PARM      'COBB'        ##Id              4
     C                   PARM      *BLANKS       ##Key            10
     C                   PARM                    ##RtCd            1
     C*
     C                   select
     C                   when      ##Key <> *blanks
     C                   seton                                        38
     C                   eval      scCoBs = ##Key
     C                   other
     C                   seton                                        37
     C                   endsl
     C*
     C                   endsl
     C*
     C                   ENDSR                                                  KJDO
      *
      *
      *===============================================================
      * KUDO - F20 = Clear Override Fields
      *===============================================================
     C     KUDO          BEGSR
      *
     c                   exfmt     wdw01
     c   kl              eval      *inkl = *off

     c                   if        *inkg = *on
     c                   clear                   scexco
     c*** begin add ***  05/09/11  ***********************************************
     c                   if        FICAOE = 'Y'
     c                   exsr      ResEmpOvr
     c                   else
     c*** end add *****  05/09/11  ***********************************************
     C                   exsr      ClrEmpOvr
     c*** begin add ***  05/09/11  ***********************************************
     c                   endif
     c*** end add *****  05/09/11  ***********************************************
     c                   exsr      updatr
     C                   eval      *in09 = *off
     c                   eval      *in38 = *on
     C                   eval      *in81 = *on
     c                   endif

     C                   ENDSR                                                  KUDO
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
     C*===============================================================
     C* Load employer information from group level
     C*===============================================================
     C     LoadFromGrp   begsr
     C*
     C                   eval      EFCOBS = FICOBB
     C                   eval      EFCOMA = FIMNCA
     C                   eval      EFCOPN = FICOPN
     C                   eval      EFCOFF = FICOFF
     C*
     C                   endsr                                                  ERRCK3

    C*================================================================
    C* Load Maintenance Fields
    C*================================================================
     C     MaintFlds     begsr

     c                   eval      EFDT   = DateTo7(SystemDate)
     c                   eval      EFTIME = TimeTo6(SystemTime)                 Maintaince time
     c                   eval      EFUSER = q1user                              Maintaince user
     c                   eval      EFPGM  = q1pgm                               Maintaince Pgm

     C                   endsr
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   select
     C*
     C                   when      addMode
     C                   exsr      LoadFromGrp
     C                   when      EFEXCO <> 'Y' and not EmpOvrExists
     C                   exsr      LoadFromGrp
     C                   endsl
     C*
     C                   eval      scExCo = EFEXCO
     C                   eval      scCoBs = EFCOBS
     C                   eval      scCoMa = EFCOMA
     C                   eval      scCoPn = EFCOPN
     C                   eval      scCoff = EFCOFF
     C                   eval      scScBd = DtMDYY(EFSCBD)
     C*
     C                   eval      saveScExCo = scExCo
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C     MVSCDB        BEGSR
     C*
     C                   eval      EFEXCO = scExCo
     C*
     C                   if        scExCo <> 'Y'
     C                   eval      EFCOBS = scCoBs
     C                   eval      EFCOMA = scCoMa
     C                   eval      EFCOPN = scCoPn
     C                   eval      EFCOFF = scCoff
     C                   eval      EFSCBD = DtCYMD(scScBd)
     C                   endif
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        BEGSR
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
     C                   if        *in90
     C                   leavesr
     C                   endif
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKU         CASEQ     *ON           KUDO                     90
     C     *INKY         CASEQ     *ON           KYDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C     pgmend        begsr
     C*
     C                   eval      *inlr = *on
     C*
     C                   endsr
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
     C     @@rrn         chain(n)  dbRcdF
     C*
     C                   eval      scCo#  = EFCO#
     C                   eval      scFnd  = EFFND
     C                   eval      scName = GetGrpName(EFFND:fnRRN:EFCO#)
     C                   eval      scEmp# = EFEMP#
     C                   eval      scDiv  = EFDIV
     C                   eval      scFyr  = EFFYR
     C                   eval      sc#Div = DivCount(EFCO#:EFFND:EFEMP#)
     C                   eval      scEDN  = EmpDivName(EFEMP#:EFDIV:emRRN:
     C                                                 EFFND:EFCO#)
     C                   eval      scCxDt = DtMDYY(GetCxDate(EFCO#:EFFND:
     C                                                       EFEMP#:EFDIV))
     C                   eval      scAgt# = GetEmpAgt(EFCO#:EFFND:EFEMP#:
     C                                       EFDIV:*zero:*zero:xxANam:ahRRN)
     C                   eval      scANam = xxANam
     C     ahRRN         chain     WMAHP
     C                   eval      scProd = AHPROD
     C                   eval      agtFein = GetAgtFein(AHCO#:AHFND:AHAGT#:
     C                                        agtMod)
     C                   eval      scPNam = GetProdNm(agtFein:agtMod:AHPROD:
     C                                       apRRN:AHCO#)
     C*
     C                   eval      *in70 = SCCXDT <> *zeros
     C*
     C     keyWTFTL      chain     wtftl
     C                   eval      scExcPr = GetExcPrompt(FTDESC)
     C                   eval      optnm = %trim(FTDESC) + ' Emp Dtl'
     C                   exsr      GetPgT
     C                   eval      scStopPr = GetStopPrompt(FTDESC)
     C*
     C                   clear                   ELPFDT
     C     elKy1         chain     pel00
     C*
     C* Get the rating state
     C                   clear                   xxSt
     C     rsKy1         chain     prs00
     C                   eval      xxSt = RSJRST
      *
     C     fiKy          setgt     pfi00
     C                   dou       %eof
     C                   clear                   FICAOE
     C                   clear                   FICOBB
     C                   clear                   FICOPN
     C                   clear                   FICOFF
     C                   clear                   FIMNCA
     C     fiKy1         readpe    pfi00
     C                   if        not %eof
     C                              and (FIEXDT = *zero or FIEXDT > ELPFDT)
     C                   leave
     C                   endif
     C                   enddo
      *
     C                   eval      scGrCoBs = FICOBB
     C                   eval      scGrCoPn = FICOPN
     C                   eval      scGrCoff = FICOFF
     C                   eval      scGrComa = FIMNCA
      *
      * If Collateral Allow Override is 'N', then claim fields will not
      * be editable fields...
     C                   clear                   scOvrInfo
     C                   if        FICAOE = 'N'
     C                   eval      scOvrInfo = 'Group info does not-
     C                              allow for override. Group info will be-
     C                              used for fee calc.'
     C                   eval      @@Mode = 'D'
     C                   endif
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      cpyMode = @@Mode = 'P'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      updMode = @@Mode = 'C'
     C*
     C                   eval      *in09 = @@Mode = 'D'
     C                   eval      *in39 = @@Mode = 'D'
     C                   eval      *in40 = @@Mode = 'A'

      *  *in38 - Controlls ablility to update fields in change mode
     c                   eval      *in38   = *off
     C                   select
     c                   when      updMode
     C                              and FICAOE = 'N'
     C                   eval      *in09 = *on
     C                   eval      *in38 = *off
     c                   when      updMode
     c                              and EFEXCO <> *blank
     C                   eval      *in09 = *on
     c                   when      updMode
     c                              and EFEXCO = *blank
     c                   eval      *in38   = *on
     c                   when      addMode
     c                   eval      cpyFlg = *on
     C                   if        FICAOE = 'Y'
     c                   eval      *in38 = *on
     C                   endif
     c                   endsl
     C*
     C                   exsr      SetFks
     C                   exsr      BldLns
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C     PREOTH        BEGSR
     C                   ENDSR
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
     C                   if        FeeDtlExists
     C                   eval      xxScrN = '1'
     C                   eval      xxNmbr = '20'
     C                   eval      xxCDA  = 'NNN'
     C                   exsr      ChgCDA
     C                   endif
      *
     C                   endsr
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C     UPDATR        BEGSR

     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
E01  C                   endif

     c                   eval      *in38 = *off

     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C     WRITER        BEGSR

     c                   exsr      Updatr
     c                   eval      *in38 = *on
     c                   eval      outFlg = *on

     C                   ENDSR                                                  WRITER





    P*====================================================================
    P* Add dots to the prompt
    P*====================================================================
     PAddDots          b
     DAddDots          pi                  like(scExcPr)
     D inPrompt                            like(scExcPr) const

     DoutPrompt        s                   like(scExcPr)
     DmaxSize4Dots     s              3  0
     DinSize           s              3  0
     DinLen            s              3  0
     Di1               s              3  0
     DoddOrEven        s              1  0

      /free
       eval inSize = %size(scExcPr);
       eval inLen = %len(%trim(inPrompt));
       if %rem(inSize:2) = 1;
        eval oddOrEven = 1;
       endif;
       eval maxSize4Dots = inSize - 9;
       for i1 =1 to inSize;
        select;
        when i1 = inSize;
         eval %subst(outPrompt:i1) = ':';
        when %rem(i1:2) = oddOrEven;
         eval %subst(outPrompt:i1) = '.';
        endsl;
       endfor;
       eval %subst(outPrompt:1:inLen) = %trim(inPrompt);
       select;
       when inLen >= maxSize4Dots;
        for i1 =inLen+1 to inSize-1;
         eval %subst(outPrompt:i1:1) = ' ';
        endfor;
       when inLen < inSize-1;
        eval %subst(outPrompt:inLen+1:1) = ' ';
       endsl;
       return outPrompt;
      /end-free

     PAddDots          e





    P*====================================================================
    P* Get the exclusion prompt
    P*====================================================================
     PGetExcPrompt     b
     DGetExcPrompt     pi                  like(scExcPr)
     D inDesc                              like(FTDESC) const

     DoutPrompt        s                   like(scExcPr)
     DinSize           s              3  0

      /free
       eval inSize = %size(scExcPr);
       eval outPrompt = AddDots('Exclude employer from '
                                + %trim(%xlate(up:lo:inDesc)));
       eval %subst(outPrompt:inSize-5:5) = '(Y/ )';
       return outPrompt;
      /end-free

     PGetExcPrompt     e





    P*====================================================================
    P* Get the stop date prompt
    P*====================================================================
     PGetStopPrompt    b
     DGetStopPrompt    pi                  like(scStopPr)
     D inDesc                              like(FTDESC) const

     DoutPrompt        s                   like(scStopPr)
     DinSize           s              3  0

      /free
       eval inSize = %size(scStopPr);
       eval outPrompt = AddDots('Stop '
                                + %trim(%xlate(up:lo:inDesc))
                                + ' date');
       return outPrompt;
      /end-free

     PGetStopPrompt    e





    P*====================================================================
    P* Determine if employer override information exists
    P*====================================================================
     PEmpOvrExists     b
     DEmpOvrExists     pi             1n

     C                   return    EFCOBS <> *blank
     C                              or EFCOPN <> *zeros
     C                              or EFCOFF <> *zeros
     C                              or EFCOMA <> *zeros
     C                              or EFSCBD <> *zeros

     PEmpOvrExists     e





    P*====================================================================
    P* Determine if employer has any fee detail already written
    P*====================================================================
     PFeeDtlExists     b
     DFeeDtlExists     pi             1n

     C     keyWHFDL      klist
     C                   kfld                    scCo#
     C                   kfld                    scFnd
     C                   kfld                    scEmp#
     C                   kfld                    scDiv
     C                   kfld                    scFyr

     C     keyWHFDL      chain     whfdl
     C                   return    %found

     PFeeDtlExists     e





    P*====================================================================
    P* Determine if employer has any fee detail already written
    P*====================================================================
     PGretestDtlDt     b
     DGretestDtlDt     pi              d

     C     keyWDFCL1     klist
     C                   kfld                    scCo#
     C                   kfld                    scFnd
     C                   kfld                    scEmp#
     C                   kfld                    scDiv
     C                   kfld                    scFyr

     C                   clear                   FCPODT
     C     keyWDFCL1     setgt     wdfcl1
     C     keyWDFCL1     readpe    wdfcl1
     C*
     C                   return    CvtToDate(FCPODT)

     PGretestDtlDt     e

** Description--------------CDAN
103F3=Exit                  YYY
105F5=Refresh               YYN
110F10=Search               YNY
112F12=Cancel               YYY
120F20=Clear override       YNN
203F3=Exit                  YYN
303F3=Exit                  YYN
*
