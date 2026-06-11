     H Copyright('(c) 2022 Sapiens. All Rights Reserved.')
      *****************************************************************
      *TITLE W6550R - RETURN PREMIUM CHECK PRINTING PROGRAM
      *                                                               *
      *    CREATED BY:  C:183  12/26/07   PROJECT# 81WZB              *
      *****************************************************************
      *  CALLED FROM: W6550C - RETURN PREM CHECK OVERRIDE             *
      *                                                               *
      *  CALLS TO   : NONE                                            *
      *                                                               *
      *****************************************************************
      * REVISIONS:                                                    *
     H*                                                               *
      *  10/13/22 C:291 Project: 129WXD                               *
      *                 Print Federal Name on Refund checks if it     *
      *                 exist and the checkbox is checked at company  *
      *                 level.                                        *
      *                                                               *
     H* 04/30/19 C:062 Project: 152WAB                                *
     H*                Apply Optacomp Mods.                           *
     H*                                                               *
     H* 06/19/18 C:62 PROJECT 129WVB                                  *
     H*               Adjust MICR position for CoveRisk               *
     H*                                                               *
     H* 08/03/17 C:62 PROJECT SY395                                   *
     H*               Add Scriptura processing.                       *
     H*
     H* 09/11/13 C:53 PROJECT 147WCC
     H*               Change MICR positions for OPTACOMP groups.
     H*                                                               *
     H* 07/08/13 C:282 Project: 132WQN Priss #38                      *
     H*                Added the special character (!).               *
      *                                                               *
     H* 05/20/13 C:190 Project: 132WQN                                *
     H*                Layout changes as per client.                  *
     H*                                                               *
      * 03/15/13 C:279 Project:SY266 Redmine: 2539
      *                Changed to handle the bad data in R4PZIP.
      *                                                               *
      * 03/04/13 C:282 Project: 132WQN                                *
      *                Print Comm/Return Prem Chks/Lansing Ofc        *
      *                                                               *
      * 11/16/10 C:180 SUPPORT 74628
      *                Fix font for Payee's fith line to be same
      *                as font for prior payee lines.
      *                                                               *
      * 04/09/08 C:53  Project: 93WHC                                 *
      *                Back to base with the following changes:       *
      *                08/16/06 51434 - Lowered the micr line by 1/4".
      *                06/23/05 58360 - Adjust bank name, date & check #
      *
      * 04/08/08 C:53  SUPPORT: WA081 62969                           *
      *                MOVE RETURN PREMIUM CHECK STUB INFORMAION UP.  *
      *                                                               *
      * 04/07/08 C:136 SUPPORT: WA081 62972                           *
      *                UPDATE A/R AND AGENCY BILLING. FUNCTIONALITY   *
      *                MOVED FROM W6560R                              *
      *                                                               *
      * 04/01/08 C:136 PRISS: 26 (81WZB)                              *
      *                COMBINE OUTPUT INTO ONE SPOOL FILE             *
      *                                                               *
      * 03/20/08 C:136 Project: 81WZB                                 *
      *                Added Dividend detail to the check stub.       *
      *****************************************************************
     FWAR4P     UF   E             DISK
      *
     FWMFNL     IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
      *
     FWTABL     IF   E           K DISK
      *
      *  Bank Information file - CO#/APPL/GRP/APPL/ACCT#/EFDT(D)
     FWMBIL2    IF   E           K DISK
      *
     FWFSDL7    IF   E           K DISK    USROPN
     FWMEAP     O    E             DISK    RENAME(PEA01 : PEA0P)
     FWDABP     O    E             DISK    RENAME(PAB01 : PAB0P)
     FWDABL     IF   E           K DISK    RENAME(PAB01 : PAB00)
     FWSASL     UF   E           K DISK    RENAME(PAS01 : PAS00)
     FWMAAL     IF   E           K DISK    Rename(paa01:paa00)
     Fwdell     if   e           k disk    Rename(pel01:pel00) InfDS(dbfDS)
     Fwmrsl     if   e           k disk    Rename(prs01:prs00)
     Fwmaal1    if   e           k disk
     Fwmagl     if   e           k disk    Rename(pag01:pag00)
     Fwxrpc     if a e             disk
     FSXPCP     uf a e             disk    Rename(ppc01:spc0p)
     F                                     Prefix(s_)
     FXMLPREL1  uf   e           k Disk
      *** Beg Add ***** 10/13/22 ***************************************
     Fsmcol     if   e           k disk    Rename(pco01:pco00)
      *** End Add ***** 10/13/22 ***************************************
      *
     FCHECKS    O    F  145        PRINTER OFLIND(*INOF)
      *
     FFILECOPY  O    F  145        PRINTER OFLIND(*INOA)
     F                                     INFDS(PIDS01)
      *
     D CLTID           S              3
     D SYS_DATE        S               D   DATFMT(*USA)
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D SIGMS1          S              4    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 1
     D SIGMS2          S             26    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 2
     D EMN             S             50    DIM(5)
     D xxyr            S              4  0
     d charRtCd        s              3
     d intFein         s              9  0
     d intMod          s              3  0
     d charAmt         s             12
     d intDate         s              8  0
     d*** Beg Add ***** 10/13/22 ******************************************
     d empName         S             40
     d*** End Add ***** 10/13/22 ******************************************
     d dbfDS           DS
     d  dbRRN                397    400B 0

     d processType     s            100    inz
     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d xmlFlowName     s                   like(s_PCXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFileName     s                   like(s_PCXML)
     d xmlFile         s             60    inz
     d xmlKey          s             60    inz
     d xmlLink         s                   like(XPLink)
     d xmlParm         s             61
     d prefix          s              2    inz('PB')
     d massFlag        s              1    Inz('Y')
     d isMassReq       s               n   Inz(*On)
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     d isFirstTime     s               n   Inz(*On)
     d chrCompany#     s              3    Inz('000')
     d chrGroup#       s              3    Inz('000')
     d chrRRN          s              9    Inz('000000000')
     d outPreString    s             10    inz
     d checkDate       s              8  0
     d policyRRN       s              9  0
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  R4PZIP                 1      9
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
      *
     D                 DS
     D  XXTRAN                 1     11
     D  XXAMPR                 1      1
     D  BITRNS                 2      5
     D  BIBNK#                 6      9
     D  BICKDG                10     10
     D  XXAMP2                11     11
     D                 DS
     D  XXCNBR                 1     10
     D  XXSLSH                 1      1
     D  R4CHK#                 2      8  0
     D  XXSLS2                 9      9
     D  XXBLK1                10     10
      *
     D                 DS
     D  VDY                    1      3
     D  VDY1                   1      1
     D  VDY2                   2      2
     D  VDY3                   3      3
     d RPrmChkDtl      ds           697
     d charPayTyCo                    4    Overlay(RPrmChkDtl)
     d charState                      3    Overlay(RPrmChkDtl:5)
     d charPolSym                     3    Overlay(RPrmChkDtl:8)
     d charChkTyp                     2    Overlay(RPrmChkDtl:11)
     d charChkNumber                 10    Overlay(RPrmChkDtl:13)
     d charCol1                      10    Overlay(RPrmChkDtl:23)
     d charChkAmt                    14    Overlay(RPrmChkDtl:33)
     d charChkDt                      8    Overlay(RPrmChkDtl:47)
     d charCol2                      12    Overlay(RPrmChkDtl:55)
     d charPayeeName                 45    Overlay(RPrmChkDtl:67)
     d charCol3                      35    Overlay(RPrmChkDtl:112)
     d charPayeeAddr1                45    Overlay(RPrmChkDtl:147)
     d charCol4                      15    Overlay(RPrmChkDtl:192)
     d charPayeeAddr2                45    Overlay(RPrmChkDtl:207)
     d charCol5                      15    Overlay(RPrmChkDtl:252)
     d charPayeeCty                  35    Overlay(RPrmChkDtl:267)
     d charCol6                      35    Overlay(RPrmChkDtl:302)
     d charPayeeSt                    2    Overlay(RPrmChkDtl:337)
     d charCol7                       1    Overlay(RPrmChkDtl:339)
     d charPayeeZip                   5    Overlay(RPrmChkDtl:340)
     d charCol8                      12    Overlay(RPrmChkDtl:345)
     d charPolicy                    16    Overlay(RPrmChkDtl:357)
     d charRenewal                    2    Overlay(RPrmChkDtl:373)
     d charEffDt                      8    Overlay(RPrmChkDtl:375)
     d charPolName                   60    Overlay(RPrmChkDtl:383)
     d charPolAddr1                  60    Overlay(RPrmChkDtl:443)
     d charPolAddr2                  60    Overlay(RPrmChkDtl:503)
     d charPolCty                    35    Overlay(RPrmChkDtl:563)
     d charCol9                      35    Overlay(RPrmChkDtl:598)
     d charPolSt                      3    Overlay(RPrmChkDtl:633)
     d charPolZip                     5    Overlay(RPrmChkDtl:636)
     d charCol10                     12    Overlay(RPrmChkDtl:641)
     d charNote                      45    Overlay(RPrmChkDtl:653)
      *
     d RPrmChkDtl1     DS           535
     d charCol11                     75    Overlay(RPrmChkDtl1)
     d charAgtNumber                 20    Overlay(RPrmChkDtl1:76)
     d charAgcyName                  80    Overlay(RPrmChkDtl1:96)
     d charAgcyAddr1                 60    Overlay(RPrmChkDtl1:176)
     d charAgcyAddr2                 60    Overlay(RPrmChkDtl1:236)
     d charAgcyCty                   35    Overlay(RPrmChkDtl1:296)
     d charCol12                     35    Overlay(RPrmChkDtl1:331)
     d charAgcySt                     3    Overlay(RPrmChkDtl1:366)
     d charAgcyZip                    5    Overlay(RPrmChkDtl1:369)
     d charCol13                     12    Overlay(RPrmChkDtl1:374)
     d charAgcyPh                    10    Overlay(RPrmChkDtl1:386)
     d charCol14                     19    Overlay(RPrmChkDtl1:396)
     d charAcctNumber                10    Overlay(RPrmChkDtl1:415)
     d charAcctName                  80    Overlay(RPrmChkDtl1:425)
     d charCol15                     24    Overlay(RPrmChkDtl1:505)
     d charBankCd                     6    Overlay(RPrmChkDtl1:529)
     d charSpl                        1    overlay(RPrmChkDtl1:535)
      *
     d                 ds
     d  intDate8               1      8  0
     d  intMM                  1      2  0
     d  intDD                  3      4  0
     d  intYYYY                5      8  0
      *
     D XVMSG1          C                   CONST('Void after')
     D XVMSG2          C                   CONST('days')
      *
     D XCACA           C                   CONST('AGENT COMMISSION -
     D                                     ACCOUNT')
     D VOID90          C                   CONST('VOID AFTER 90 DAYS')
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D PIDS01        E DS                  EXTNAME(SZQ4P)
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d updatAR         pr
     d updatAB         pr
     d updatAS         pr
     D*
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)

     d* Data Queue Process
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const

     D/copy sPrModule
     D/copy sPrXML
     D/copy sPrDate
      *
     C     BIKY1         KLIST
     C                   KFLD                    R4CO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    R4FND
     C                   KFLD                    R4CKTP
     C                   KFLD                    R4CKDT
      *
     C     BIKY2         KLIST
     C                   KFLD                    R4CO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    R4FND
     C                   KFLD                    R4CKTP
      *
      *  EMPLOYER MASTER
      *
     C     EMKY          KLIST
     C                   KFLD                    R4CO#
     C                   KFLD                    R4FND
     C                   KFLD                    R4EMP#
     C                   KFLD                    R4DIV
      *
      *  FUND MASTER
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     TTKY          KLIST
     C                   KFLD                    R4CO#
     C                   KFLD                    TAB10            10
      *
      *  Dividend Distribution Work File
      *
     C     sdky          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMCUST
      *  Employer Detail File
     C     keyWDELL      Klist
     C                   Kfld                    R4co#
     C                   Kfld                    R4fnd
     C                   Kfld                    R4emp#
     C                   Kfld                    R4div
     C                   Kfld                    R4fyr
      *  Rate Set Code Master File
     C     keyWMRSL      Klist
     C                   Kfld                    R4CO#
     C                   Kfld                    charRtCd
      *  Agent Assignment File
     C     keyWMAAL1     Klist
     C                   Kfld                    R4co#
     C                   Kfld                    R4fnd
     C                   Kfld                    R4agt#
      *  Agent Master File
     C     keyWMAGL      Klist
     C                   Kfld                    R4co#
     C                   Kfld                    intFein
     C                   Kfld                    intMod
      *
     C     *LIKE         DEFINE    BIEFDT        XXDT
     C     *LIKE         DEFINE    BIEFDT        XXCYMD
     C     *LIKE         DEFINE    BI10A3        DESC
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     *ENTRY        PLIST
     C                   PARM                    @RRN#             9 0
      *
      * Retrieve Scxriptura Flags
     C                   ExSr      GetScriptFlgs
     C                   MOVEL     '03011B'      CHR             256
     C                   MOVE      *BLANKS       HEX             128
     C                   MOVE      *BLANK        RTCODE            1
      *
     C                   CALL      'CVTHEX'
     C                   PARM                    CHR
     C                   PARM                    HEX
     C                   PARM                    RTCODE
     C                   MOVEL     HEX           ESC               3
      *
     C* Check data client data area
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
      *
     c     *USA          MOVE      *DATE         SYS_DATE
      *
      * READ WAR4P BASED ON RRN PASSED FROM W6550C
      *
     C     @RRN#         CHAIN     PR401                              50
      *
     C     R4BTYP        IFEQ      'A'
     C     R4BTYP        OREQ      'D'
     C     R4BTYP        OREQ      'P'
      *
     C                   MOVE      'RP'          XXAPPL
      *
     C                   ELSE
      *
     C                   MOVE      'IC'          XXAPPL
      *
     C                   ENDIF                                                  R4BTYP IFEQ 'A'
      *
      *  GET EMPLOYER NAME
      *
     C     EMKY          CHAIN     PEM01                              60
      /Free
      //*** Beg Add ***** 10/13/22 ********************************
       Chain (R4Co#) pco00;
        If COfedrc = 'Y' and EMfdnm <> *Blank;
           empName = EMfdnm;
        Else;
           empName = EMName;
        Endif;
      //*** End Add ***** 10/13/22 ********************************
      /End-Free
      *
      *  GET A/R CODE DESCRIPTION
      *
     C                   EVAL      DESC = *BLANKS
     C                   MOVEL(P)  R4ARCD        TAB10
     C     TTKY          CHAIN     PTT01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     TTDESC        DESC
     C                   ENDIF                                                  *in61 = *off
      *
     C     BIKY1         SETLL     PBI01                              76
     C     BIKY2         READE     PBI01                                  76
      *
     C     *IN76         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKY2         READE     PBI01                                  76
     C                   ENDDO                                                  *IN76 = *OFF
      *
     C     *IN76         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    *IN76 = '1'
      *
     C                   MOVE      *BLANKS       @@VOID           19
     C*
     C                   SELECT
     C     ##CLT#        WHENEQ    '136'
     C                   MOVE      *ON           *IN15                          COND XXX
     C                   MOVE      *BLANKS       @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '139'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '081'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *ON           *IN79
     C*
     C                   OTHER
      *
      * This code initializes the 'Void after x days' display field
      * If the number of days (BI50N3) is *zero the message
      * field (XVOAFT) is blanked out
      *
     C                   MOVE      *BLANKS       XVOAFT
      *
     C     BI50N3        IFGT      *ZERO
     C                   MOVE      BI50N3        XVDAYS            3 0
     C                   MOVE      XVDAYS        VDY
     C                   MOVE      *BLANKS       XXVDAY            3
      *
     C     XVDAYS        IFLT      10
     C                   MOVE      *BLANK        VDY1
     C                   MOVE      *BLANK        VDY2
     C                   MOVEL     VDY3          XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFLT      100
     C                   MOVE      *BLANK        VDY1
     C                   MOVEL     VDY2          XXVDAY
     C     XXVDAY        CAT       VDY3:0        XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFGT      99
     C                   MOVE      VDY           XXVDAY
     C                   ENDIF                                                  XVDAYS > 99
      *
     C                   ENDIF                                                  XVDAYS < 100
      *
     C                   ENDIF                                                  XVDAYS < 10
      *
     C                   MOVEL     XVMSG1        XVOAFT           19
     C     XVOAFT        CAT       XXVDAY:1      XVOAFT
     C     XVOAFT        CAT       XVMSG2:1      XVOAFT
     C                   MOVEL     XVOAFT        @@VOID
     C                   MOVE      *OFF          *IN78
     C                   ENDIF                                                  BI50N3 > *ZERO
      *
     C                   MOVE      *OFF          *IN78
      *
     C                   ENDSL                                                  ##CLT# = '136'
      *
     C                   MOVEL     R4CKDT        XXDT
      *
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C                   Eval      checkDate = dtMDYY(R4ckDt)
      *
     C                   MOVE      R4CO#         XXCO#             3 0
     C                   MOVE      R4FND         XXFND             3 0
     C                   MOVE      '/'           XXSLSH            1
     C                   MOVE      '/'           XXSLS2            1
     C                   MOVE      '&'           XXAMPR            1
     C                   MOVE      '&'           XXAMP2            1
     C                   MOVE      *BLANKS       XXBLK2            2
     C                   MOVE      *BLANKS       XXBLK1            1
      /EJECT
      *
      *  GET NAME AND MAILING ADDRESS
      *
     C                   MOVE      *BLANKS       XXNAME           32
     C                   MOVE      *BLANKS       XXADDR           32
     C                   MOVE      *BLANKS       XXADR2           32
     C                   MOVE      *BLANKS       XXMCSZ           32
     C                   MOVEL     R4PTNM        XXNAME
     C                   MOVEL(P)  R4PTNM        XXNAM1           50
     C                   MOVEL(P)  R4PTN2        XXWK10           10
     C                   MOVE      XXWK10        XXNAM1
     C                   MOVE(P)   R4PTN2        XXNAM2           50
     C                   MOVEL     R4PAD1        XXADDR
     C                   MOVEL     R4PAD2        XXADR2
      *
      *   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     R4PST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   IF        R4PZIP = ' '
     C                   EVAL      R4PZIP = '000000000'
     C                   ENDIF
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END                                                    XXZPM2 <> 0
      *
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     R4PCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXMCSZ           32
     C                   MOVE      *BLANKS       EMN
     C                   Z-ADD     1             A                 2 0
     C                   MOVEL     XXNAM1        EMN(A)
     C                   ADD       1             A
     C     XXNAM2        IFNE      *BLANKS
     C                   MOVEL     XXNAM2        EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXNAM2 <> *BLANKS
     C                   MOVEL     XXADDR        EMN(A)
     C                   ADD       1             A
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        EMN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XXADR2 <> *blanks
     C                   MOVEL     XXMCSZ        EMN(A)
     C                   ADD       1             A
      *
      *  LOAD DOLLAR AMOUNT SPELLED OUT
      *
     C                   MOVE      R4AMT         QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
      *
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       XXAMT1           60
     C                   MOVE      *BLANKS       XXAMT2           60
     C                   MOVEL     QQAMT1        XXAMT1
     C                   MOVEL     QQAMT2        XXAMT2
      /EJECT
      *
      *  Access Fund Master to obtain Fund Short Name
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C                   Select
     C                   When      ##clt# = '093'
     C                   Eval      *in80 = *On
     C                   When      (##clt# = '114' Or ##clt# = '152') and
     C                             FNgrpAsc = 'OPTACOMP'
     C                   Eval      *in20 = *On
     C                   When      ##clt# = '129'
     C                   Eval      *in21 = *On
     C                   Other
     C                   Eval      *in22 = *On
     C                   EndSl
      *
      *  Assign MACRO to PCL5 control characters with Bank Information Macro ID
      *
     C                   MOVE      BIMACR        TEMP4             4
     C     '&f'          CAT       TEMP4         TEMP6             6
     C     TEMP6         CAT       'y3X'         MACRO             9
      *
     C     BIAXOU        IFEQ      *BLANK
     C                   MOVE      *BLANKS       XXAXOU
     C                   MOVE      XXCNBR        XXAXOU           20
     C                   ELSE
     C                   MOVE      BIAXOU        XXAXOU
     C                   END                                                     BIAXOU = *blank
      *
     C     BIBNK#        CAT(P)    '/'           XXBT#5            5
     C     XXBT#5        CAT(P)    BITRNS        XXBT#             9
      *
      *
     C     BILOGO        IFEQ      'N'
     C                   MOVE      '1'           *IN73
     C                   ELSE
     C                   EVAL      *IN73 = *OFF
     C                   ENDIF                                                   BILOGO = 'N'
      *
     C     BITRST        IFEQ      'N'
     C                   MOVEL(P)  XCACA         XXWCA            35
     C                   END                                                     BITRST = 'N'
      *
      *  Determine if Second Signature Verbage should Print
      *
     C                   MOVE      *OFF          *IN85
     C                   MOVE      *OFF          *IN83
     C     BI$LMT        IFGT      0
     C     R4AMT         ANDGT     BI$LMT
     C                   MOVE      '1'           *IN85
     C                   MOVE      *ON           *IN83
     C                   END                                                    BI$LMT > 0
      *
      * Check for First Signature entry
      *
     C                   EVAL      CH1 = *BLANKS
     C                   EVAL      *IN86 = *OFF
     C     BI10A1        IFNE      *BLANK
     C                   MOVEL     BI10A1        CH1               7
     C                   MOVE      '1'           *IN86
     C                   END                                                    BI10A1 <> *BLANK
      *
      * Check for Second Signature entry
      * If $$$ Limit entered and Check amount greater than limit,
      * then do NOT print signature
      *
     C                   MOVE      *OFF          *IN87
     C                   EVAL      CH2 = *BLANKS
     C     BI10A2        IFNE      *BLANK
     C                   MOVEL     BI10A2        CH2               7
     C                   MOVE      '1'           *IN87
     C                   MOVE      *ON           *IN83
     C                   END                                                    BI10A2 <> *BLANK
      /Free
       If ##CLT# = '132';
          charChkTyp = 'RP';
          charChkNumber = '000' + %EditC(R4chk#:'X');
          charAmt       = %EditC(R4amt:'3');
          charChkAmt    = '  ' + charAmt;
      /End-Free
     C                   Eval      intDate8 = dtMDYY(R4ckDt)
      /Free
         charChkDt  = %EditC(intYYYY:'X') + %EditC(intMM:'X')
                       + %EditC(intDD:'X');
          charBankCd    = 'REFCW1';
          charNote      = R4com1;
          charPayeeName =  xxNAME;
          charPayeeAddr1 = R4pad1;
          charPayeeAddr2 = R4pad2;
          charPayeeCty   = R4pcty;
          charPayeeSt    = R4pst;
          charPayeeZip   = R4pzip;
          charPayTyCo    = '5CWR';
       // Populating the value form WMEMP file.
          //*** Beg Chg ***** 10/13/22 *********************************
          //***charPolName    = EMname;
          charPolName    = empName;
          //*** End Chg ***** 10/13/22 *********************************
          charPolAddr1   = EMmad1;
          charPolAddr2   = EMmad2;
          charPolCty     = EMmcty;
          charPolSt      = EMmst;
          charPolZip     = %EditC(EMmzip:'X');
          charPolSym     = *Blanks;

       // WDELL
          charRtCd = *Blanks;
          Chain keyWDELL pel00;
          If %Found(wdell);
             charRtCd = ELrtcd;
             Chain keyWMRSL prs00;
             If %Found(wmrsl);
                charState  = RSjrst;
                charPolicy = %Editc(ELemp#:'X') + %EditC(ELdiv:'X');
      /End-Free
     C                   Eval      intDate8 = dtMDYY(ELpfDt)
      /Free
                charEffDt  = %EditC(intYYYY:'X') + %EditC(intMM:'X')
                              + %EditC(intDD:'X');
             EndIf;
          EndIf;

          RPCDTA1 = RPrmChkDtl;

       // Subroutine to Load Agency Information.
          ExSr LodAgentInfo;
          charSpl = '!';

       // Write the record in wrok file WXRPC.
          RPCDTA2 = RPrmChkDtl1;

          Write RPC01;

       Else;
      /End-Free
      *  PRINT CHECK
      *
     C                   If        isUseSpoolOut
     C  N77              EXCEPT    BANNER
     C  N77              EXCEPT    BANNEF
      *
     C                   EXCEPT    CHECK
     C                   EXCEPT    CHECKF
     C                   EndIf
     C*
     C*** Write to Scriptura 1099 Workfile
     C                   If        isUseScriptura = *On
     C                   Exsr      wrtSXPCP
     C                   Endif
      *
      /Free
       EndIf;
      /End-Free
     C*  If client = 81 and processing a dividend distribution, print the dividend
     C*  information on the check stub.
     C*
     C                   IF        ##CLT#= '081' and r4sdfl = 'Y'
     C*
     c                   open      wfsdl7
     C     SDKY          CHAIN     PSD01                              99
     C                   IF        *in99 = *OFF
     C                   eval      xxyr = SDFYR + 1900
     C                   If        isUseSpoolOut
     C                   EXCEPT    DET2
     C                   EndIf
     c                   close     wfsdl7
     C                   ENDIF
     C*
     C                   ELSE                                                   CLTID = '081'
      *
      /Free
       If ##CLT# <> '132';
      /End-Free
     C                   If        isUseSpoolOut
     C                   EXCEPT    DET
     C                   Endif
      /Free
       Endif;
      /End-Free
      *
     C                   ENDIF                                                  CLTID = '081'
      *
      * UPDATE THE CHECK REQUEST STATUS = 'Y' AND SORT = BLANK FOR THE CHECK REQUEST RECORD
      *
     C                   EVAL      R4STAT = 'Y'
     C                   EVAL      R4SORT = *BLANKS
     C     *CYMD         MOVE      SYS_DATE      R4DATE
     C                   TIME                    R4TIME
     C                   EVAL      R4PGM  = Q1PGM
     C                   EVAL      R4USER = Q1USER
     C                   UPDATE    PR401
      *
     C                   IF        ##CLT#= '081'
     C                             and R4SDFL = 'Y'
     C                   CALL      'W7994R1'
     C                   PARM                    @RRN#
     C                   ENDIF                                                  CLTID = '081'
      *
      *
      *   Update A/R or Agency Billing if Batch Type isn't an Independent Cash Distribution
      *
     c                   select
     c                   when      R4BTYP = 'I'
     c                   when      R4ACFL = 'N'
     c                   callp     updatAR
     c                   when      R4ACFL = 'Y'
     c                   callp     updatAB
     c                   callp     updatAS
     c                   endsl
      /free
       // Call Scriptura Print Process
       xmlParm = xmlFileName + '$';
       If xmlFileName <> ' ';
          S4401C(xmlParm);
       EndIf;

      /end-free
      *
     C     NCHK#         TAG
     C                   MOVE      '1'           *IN77
      *
     C     TERM          TAG
     C                   RETURN
      /Free
       //-------------------------------------------------------------
       // GetXMLFileNm - Get the XML File Name
       //--------------------------------------------------------------
        Begsr GetXMLFileNm;

           xmlFile = ' ';
           message = %trim(processType);
           string = ' ';

           If isMassReq = *On;
              StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':
               '  ':string:'000':'000':massFlag:xmlLink);

              isMassReq = *Off;
              xmlFileName  = xmlFile;
              xmlLink      = xmlFile;
              Chain (xmlFile) xp100;
              If %Found(xmlPrel1);
                 XPCat = 'PS';
                 Update xp100;
              EndIf;
           EndIf;

           // Register Individual Entries
           Chain (R4co#:R4fnd:R4emp#:R4div:R4fYr) pel00;
           policyRRN   = dbRRN;
           chrRRN      = %EditC(policyRRN:'X');
           chrCompany# = %EditC(ELco#:'X');
           chrGroup#   = %Editc(ELfnd:'X');
           StLetter('XMLREQUEST':xmlFile:message:'E':chrRRN:prefix:
           string:chrCompany#:chrGroup#:massFlag:xmlLink);
           If xmlFileName = ' ';
              xmlFileName = xmlFile;
           Endif;

           //* The template XML file name must be pulled from record for work files for a
           //* single request.  Otherwise it has been returned from the XML Request call.
           If xmlLink = ' ';
              Chain (xmlFile) xp100;
              xmlFile = XPXmlT;
           Else;
              Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
           Endif;

           XPData = 'Y';

           XPPgm  = Q1pgm;
           XPUser = Q1user;
           XPTime = TimeTo6(SystemTime());
           XPDt   = DateTo7(SystemDate());

           Update xp100;

        Endsr;
       //-------------------------------------------------------------
       // GetScriptFlgs - Get Scriptura Flags
       //--------------------------------------------------------------
       Begsr GetScriptFlgs;

          processType    = 'Return Premium Checks';
          xmlFileName    = *blanks;
          isUseSpoolOut  = UseSpoolOut(processType);
          isUseScriptura = UseScriptura(processType);
          isUse3rdParty  = Use3rdXml(processType);
          isUseXML       = isUseScriptura or isUse3rdParty;

       Endsr;
      /End-Free
      *************************************************************************
      * LodAgentInfo - Subroutine to populate the Agency information
      *************************************************************************
      /Free
       BegSr LodAgentInfo;

       Chain keyWMAAL1 paa01;
       If %Found (wmaal1);
          intMod  = AAmod;
          intFein = AAfein;
          Chain keyWMAGL pag00;
          If %Found(wmagl);
             charAgtNumber = %EditC(R4agt#:'X');
             charAgcyName  = AGname;
             charAgcyAddr1 = AGmad1;
             charAgcyAddr2 = AGpad1;
             charAgcyCty   = AGmcty;
             charAgcySt    = AGmst;
             charAgcyZip   = %EditC(AGmzip:'X');
             charAgcyPh    = %EditC(AGare1:'X') + %EditC(AGphn1:'X');
          Endif;

       Endif;
       EndSr;
       //-------------------------------------------------------------
       // WrtSXPCP - Write to Scriptura Premium check workfile
       //--------------------------------------------------------------
       Begsr WrtSXPCP;

          ExSr GetXmlFileNm;

          Clear spc0p;

          s_PCxml  = xmlFile;
          s_PCaxou = xxAxou;
          s_PCtran = xxTran;
          s_PConus = BIonus;
          s_PCrtUp = BIrtUp;
          s_PCrtLw = BIrtLw;
          s_PCchk# = R4chk#;
          s_PC40a1 = BI40A1;
          s_PC40a2 = BI40A2;
          s_PC40a3 = BI40A3;
          s_PC40a4 = BI40A4;
          s_PCdate = checkDate;
          s_PCai01 = BIai01;
          s_PCai02 = BIai02;
          s_PCai03 = BIai03;
          s_PCamt  = R4amt;
          s_PCamt1 = xxAmt1;
          s_PCamt2 = xxAmt2;
          s_PCvoid = @@void;
          s_PCptNm1= emn(1);
          s_PCptNm2= emn(2);
          s_PCptAd1= emn(3);
          s_PCptAd2= emn(4);
          s_PCptAd3= emn(5);
          s_PCsgMs1= SIGMS1(1);
          s_PC$lmt = BI$lmt;
          s_PCsgMs2= SIGMS2(1);
          s_PCcmd1 = BIcmd1;
          s_PCcmd2 = BIcmd2;
          s_PCfYr  = R4fYr;
          s_PCemp# = R4emp#;
          s_PCdiv  = R4div;
          //*** Beg Chg ***** 10/13/22 *********************************
          //***s_PCeName= EMname;
          s_PCeName= empName;
          //*** End Chg ***** 10/13/22 *********************************
          s_PCdesc = desc;
          s_PCfnd  = R4fnd;
          s_PCfName= FNname;
          s_PCcom1 = R4com1;
          s_PCcom2 = R4com2;
          s_PCcom3 = R4com3;
          s_PCfnNm = SDfnNm;
          s_PCsFnd = SDfnd;
          s_PCsEmp#= SDemp#;
          s_PCsDiv = SDdiv;
          s_PCemNm = SDemNm;
          s_PCagt# = SDagt#;
          s_PCagNm = SDagNm;
          s_PCnmPr = SDnmPr;
          s_PCclFn = SDclFn;
          s_PCtcIn = SDtcin;
          s_PCcfSd = SDcfSd;
          s_PCperc = SDperc;
          s_PCckAm = SDckAm;
          s_PCiDt  = DateTo7(systemDate());
          Write spc0p;
       Endsr;
      /End-Free
      /EJECT
      * ================================================================
      *      O U T P U T
      * ================================================================
      *
     OCHECKS    E   N77      BANNER      0  3 01
      *                                           RESET PRINTER TO DFT
     O              N77      ESC
     O              N77                             'E'
      *                                           Portriat Orientation
     O              N77      ESC
     O              N77                             '&l0O'
     O              N77      ESC
     O              N77                             '&l0E'
     O              N77      ESC
     O              N77                             '&a0L'
     O*
     O                       ESC
     O                                              '&l1H'
     O*** Font 158 Helv. Bold 14.4
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O*
     O                       ESC
     O                                              '*p1000x1000Y'
     O                                              'CHECKS FOR FUND: '
     O                       R4FND
      * ================================================================
     OCHECKS    E            CHECK       0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
     O*                                           PORTRAIT FONT HERE!
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                           LOAD OVERLAY (CHECK Mac#1)
     O                       ESC
     O                                              '&l1H'
     O                       ESC
     O                       MACRO
      * ******************************************************************
     O* ** MICR BANK INFO *****
     O*
     O*                                        MICR Font Change
     O          E            CHECK       0  0
     O                       ESC
     O                                              '(8M'
     O                       ESC
     O                                              '(s0p8h8v0s0b80T'
      * added condition N20 to the next 3 printed lines.
      **** MICR LINE BASE *****
     O          E    22      CHECK       0  0
     O                       ESC
     O                                              '*p20x980Y'
     O                       XXAXOU
     O          E    22      CHECK       0  0
     O                       ESC
     O                                              '*p770x980Y'
     O                       XXTRAN
     O          E    22      CHECK       0  0
     O                       ESC
     O                                              '*p1185x980Y'
     O                       BIONUS
      **** MICR LINE CLT093 *****
     O          E    80      CHECK       0  0
     O                       ESC
     O                                              '*p20x1055Y'
     O                       XXAXOU
     O*
     O          E    80      CHECK       0  0
     O                       ESC
     O                                              '*p770x1055Y'
     O                       XXTRAN
     O*
     O          E    80      CHECK       0  0
     O                       ESC
     O                                              '*p1185x1055Y'
     O                       BIONUS
     O*
      **** MICR LINE OPTACOMP ***
     O          E    20      CHECK       0  0
     O                       ESC
     O                                              '*p30x1035Y'
     O                       XXAXOU
     O          E    20      CHECK       0  0
     O                       ESC
     O                                              '*p780x1035Y'
     O                       XXTRAN
     O          E    20      CHECK       0  0
     O                       ESC
     O                                              '*p1230x1035Y'
     O                       BIONUS
      **** MICR LINE CoveRisk *****
     O          E    21      CHECK       0  0
     O                       ESC
     O                                              '*p10x980Y'
     O                       XXAXOU
     O          E    21      CHECK       0  0
     O                       ESC
     O                                              '*p765x980Y'
     O                       XXTRAN
     O          E    21      CHECK       0  0
     O                       ESC
     O                                              '*p1185x980Y'
     O                       BIONUS
     O*
      * ******************************************************************
     O          E            CHECK       0  0
      *                                                Font Change
     O**  Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E            CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O                                              '*p2250x120Y'
     O                       ESC
     O                                              '&d1D'
     O                       BIRTUP
     O                       ESC
     O                                              '&d@'
     O          E            CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O                                              '*p2250x155Y'
     O                       BIRTLW
     O          E            CHECK       0  0
      *                                                Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p1850x174Y'
     O                       R4CHK#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      *   Change Font to COURIER 12 BOLD to print group name
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p12h10v0s3b3T'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 1
     O                       ESC
     O                                              '*p050x150Y'
     O                       BI40A1
      **
     O          E    73      CHECK       0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      **                                               Font Change
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 2
     O                       ESC
     O                                              '*p050x190Y'
     O                       BI40A2
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 3
     O                       ESC
     O                                              '*p050x230Y'
     O                       BI40A3
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 4
     O                       ESC
     O                                              '*p050x270Y'
     O                       BI40A4
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      **  Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      *                                                Check Date
     O                       ESC
     O                                              '*p1250x174Y'
     O                       XXDATE        Y
     O          E            CHECK       0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *                                                Font Change
     O          E            CHECK       0  0
      *                                                Font Change
     O**                                               8 PT.
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E    79      CHECK       0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p350x600Y'
     O                       BIAI01
     O          E    80      CHECK       0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1710x225Y'
     O                       BIAI01
     O          E   N79N80   CHECK       0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1700x225Y'
     O                       BIAI01
     O          E    80      CHECK       0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1710x260Y'
     O                       BIAI02
     O          E   N79N80   CHECK       0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1700x260Y'
     O                       BIAI02
      *
     O          E    80      CHECK       0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1710x295Y'
     O                       BIAI03
     O          E   N79N80   CHECK       0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1700x295Y'
     O                       BIAI03
      *
     O          E            CHECK       0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       R4AMT                  '    ,   , $0.  '
      *
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       XXAMT1
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       XXAMT2
     O          E            CHECK       0  0
     O*   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p10vsb5T'
     O*
     O          E            CHECK       0  0
     O                       ESC
     O*
     O                                              '*p1925x570Y'
     O*
     O                       @@VOID
     O          E    79      CHECK       0  0
     O                       ESC
     O                                              '*p1725x272Y'
     O                                              'Refund Acct.'
     O          E            CHECK       0  0
      *  Courier 10, using 12 point, 10 pitch  "Bitmapped" font
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p12h12v0s0b3T'
     O                       ESC
     O                                              '*p250x650Y'
     O                       EMN(1)
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x700Y'
     O                       EMN(2)
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x750Y'
     O                       EMN(3)
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x800Y'
     O                       EMN(4)
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x850Y'
     O                       EMN(5)
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x900Y'
     O                                              ' '
      *
      *  UNDERLINE FOR FIRST SIGNATURE
      *
     O          E    78      CHECK       0  0
     O                       ESC
     O                                              '*p1400x840Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                                              '(s0p2.0h71.94v0s0b255T'
     O                                              '!"#$%&'''
      *
      *  Print underline for Second Signature in 2 situations:
      *     1) Second Signature is entered.
      *     2) Zero < $$$ Limit < Check Amount.
      *
     O          E    83      CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                                              '(s0p2.0h71.94v0s0b255T'
     O                                              '!"#$%&'''
      *
      *********************************************************************
      *  Signatures and $$$ limit
      *
     O          E    85      CHECK       0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *       E 00     85      CHECK
     O                       ESC
     O                                              '*p1400x605Y'
     O*       E 00     85      CHECK
     O                       SIGMS1
     O                       BI$LMT                 ' ,   ,   , $0'
     O                       SIGMS2
      *
     O          E    87      CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD2
     O                       CH2
      *
     O          E    86      CHECK       0  0
     O                       ESC
     O                                              '*p1400x840Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD1
     O                       CH1
      *
      ***************************************************************
      *    REST OF PAGE IS COURIER 16.66 PITCH (17)
      ***************************************************************
     O          E            CHECK       0  0
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                            FONT MED/COR. 8.5PT/16.66
     O          E            CHECK       0  0
      *                                            Position at 0"
     O                       ESC
     O                                              '*p00x00Y'
     O*
     O          E            DET            1 10
     O                                           16 'CHECK NUMBER   :'
     O                       R4CHK#        Z   +  1
     O          E            DET            1
     O                                           16 'CHECK DATE . . :'
     O                       XXDATE        Y   +  1
     O          E            DET            1
     O                                           16 'POLICY YEAR    :'
     O                       R4FYR             +  1
     O          E            DET            1
     O                                           16 'EMP/DIV/NAME   :'
     O                       R4EMP#        Z   +  1
     O                       R4DIV         Z   +  1
      *** Beg Chg ***** 10/13/22 ***************************************
     O*****                  EMNAME            +  1
     O                       EMPNAME           +  1
      *** End Chg ***** 10/13/22 ***************************************
     O          E            DET            1
     O                                           16 'A/R CODE DESC  :'
     O                       DESC              +  1
     O          E            DET            1
     O                                           16 'CHECK AMOUNT   :'
     O                       R4AMT         J   +  1
     O          E            DET            1
     O                                           16 'GROUP #  . . . :'
     O                       R4FND         Z   +  1
     O                       FNNAME            +  1
     O          E            DET            1
     O                                           16 'COMMENTS . . . :'
     O                       R4COM1            +  1
     O          E            DET            1
     O                       R4COM2            + 17
     O          E            DET            1
     O                       R4COM3            + 17
     O*
     O          E            DET2           2 15
     O                                         + 25 'DIVIDEND DISTRIBUTION'
     O                                         +  1 'STATEMENT'
     O          E            DET2           1
     O                       SDFNNM            +  2
     O          E            DET2           1
     O                                         +  2 'Group number:'
     O                       SDFND         4   +  1
     O          E            DET2           2
     O                                         +  2 'Policy year :'
     O                       XXYR              +  1
     O          E            DET2           1
     O                       SDEMP#            +  2
     O                                         +  0 '-'
     O                       SDDIV             +  0
     O          E            DET2           2
     O                       SDEMNM            +002
     O          E            DET2           2
     O                                         +002 'Agent:'
     O                       SDAGT#            +002
     O                       SDAGNM            +002
     O          E            DET2           2
     O                                           44 'Calculated premium  . :'
     O                       SDNMPR        J     69
     O          E            DET2           2
     O                                           44 'Claim group allocation:'
     O                       SDCLFN        J     69
     O          E            DET2           2
     O                                           44 'Total claims incurred :'
     O                       SDTCIN        J     69
     O          E            DET2           2
     O                                           44 'Claims surplus  . . . :'
     O                       SDCFSD        J     69
     O          E            DET2           2
     O                                           44 'Percent of return . . :'
     O                       SDPERC        J     69
     O                                         +000 '%'
     O          E            DET2           1
     O                                           44 'Total check amount  . :'
     O                       SDCKAM        J     69
     **====================================================================*
     OFILECOPY  E   N77      BANNEF      0  3 01
      *                                           RESET PRINTER TO DFT
     O              N77      ESC
     O              N77                             'E'
      *                                           Portrait Orientation
     O              N77      ESC
     O              N77                             '&l0O'
     O              N77      ESC
     O              N77                             '&l0E'
     O              N77      ESC
     O              N77                             '&a0L'
     O*
     O                       ESC
     O                                              '&l4H'
     O*** Font 158 Helv. Bold 14.4
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O*
     O                       ESC
     O                                              '*p1000x1000Y'
     O                                              'FILE COPY FOR FUND: '
     O                       R4FND
     OFILECOPY  E            CHECKF      0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
     O*                                           PORTRAIT FONT HERE!
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                           LOAD OVERLAY (CHECK Mac#1)
     O                       ESC
     O                                              '&l4H'
     O                       ESC
     O                       MACRO
     O          E            CHECKF      0  0
      *                                                Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1850x174Y'
     O                       R4CHK#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECKF      0  0
      *   Change Font to COURIER 12 BOLD to print group name
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p12h10v0s3b3T'
     O** ITALIC, NOT BOLD                   '(s0p12h10v1s0T'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 1
     O                       ESC
     O                                              '*p050x150Y'
     O                       BI40A1
      **
     O          E    73      CHECKF      0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      **                                               Font Change
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 2
     O                       ESC
     O                                              '*p050x190Y'
     O                       BI40A2
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 3
     O                       ESC
     O                                              '*p050x230Y'
     O                       BI40A3
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 4
     O                       ESC
     O                                              '*p050x270Y'
     O                       BI40A4
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECKF      0  0
      **  Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
     O          E            CHECKF      0  0
      *                                                Check Date
     O                       ESC
     O                                              '*p1250x174Y'
     O                       XXDATE        Y
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECKF      0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *                                                Font Change
     O          E            CHECKF      0  0
      *                                                Font Change
     O**                                               8 PT.
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E    80      CHECKF      0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1710x225Y'
     O                       BIAI01
     O          E   N80      CHECKF      0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1700x225Y'
     O                       BIAI01
     O          E    80      CHECKF      0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1710x260Y'
     O                       BIAI02
     O          E   N80      CHECKF      0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1700x260Y'
     O                       BIAI02
      *
     O          E    80      CHECKF      0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1710x295Y'
     O                       BIAI03
     O          E   N80      CHECKF      0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1700x295Y'
     O                       BIAI03
     O*********************************************************************
     O          E            CHECKF      0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
     O          E            CHECKF      0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       R4AMT                  '    ,   , $0.  '
     O*
     O          E            CHECKF      0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       XXAMT1
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       XXAMT2
     O          E            CHECKF      0  0
     O*  COURIER 10, USING 12 POINT, 10 PITCH  "BITMAPPED" FONT
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p12h12v0s0b3T'
     O*                                           CHECK NAME/ADDRESS
     O                       ESC
     O                                              '*p250x650Y'
     O                       EMN(1)
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x700Y'
     O                       EMN(2)
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x750Y'
     O*** End chg ***** 05/24/05 ***************************************
     O                       EMN(3)
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x800Y'
     O                       EMN(4)
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x850Y'
     O                       EMN(5)
     O*
     O          E    85      CHECKF      0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O                       ESC
     O                                              '*p1400x605Y'
     O*
     O          E            CHECKF      0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1600x700Y'
     O                                              'FILE COPY'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1500x830Y'
     O                                              'NON-NEGOTIABLE'
      ***************************************************************
      *    REST OF PAGE IS COURIER 16.66 PITCH (17)
      ***************************************************************
     O          E            CHECKF      0  0
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                            FONT MED/COR. 8.5PT/16.66
     O          E            CHECKF      0  0
      *                                            Position at 0"
     O                       ESC
     O                                              '*p00x00Y'
     O*
     O          E            DET            1 27
     O                                           16 'CHECK NUMBER   :'
     O                       R4CHK#        Z   +  1
     O          E            DET            1
     O                                           16 'CHECK DATE . . :'
     O                       XXDATE        Y   +  1
     O          E            DET            1
     O                                           16 'POLICY YEAR    :'
     O                       R4FYR             +  1
     O          E            DET            1
     O                                           16 'EMP/DIV/NAME   :'
     O                       R4EMP#        Z   +  1
     O                       R4DIV         Z   +  1
     O*** Beg Chg ***** 10/13/22 ****************************************
     O***                    EMNAME            +  1
     O                       EMPNAME           +  1
     O*** End Chg ***** 10/13/22 ****************************************
     O          E            DET            1
     O                                           16 'A/R CODE DESC  :'
     O                       DESC              +  1
     O          E            DET            1
     O                                           16 'CHECK AMOUNT   :'
     O                       R4AMT         J   +  1
     O          E            DET            1
     O                                           16 'GROUP #  . . . :'
     O                       R4FND         Z   +  1
     O                       FNNAME            +  1
     O          E            DET            1
     O                                           16 'COMMENTS . . . :'
     O                       R4COM1            +  1
     O          E            DET            1
     O                       R4COM2            + 17
     O          E            DET            1
     O                       R4COM3            + 17
     O          E            DET2           2 15
     O                                         + 25 'DIVIDEND DISTRIBUTION'
     O                                         +  1 'STATEMENT'
     O          E            DET2           1
     O                       SDFNNM            +  2
     O          E            DET2           1
     O                                         +  2 'Group number:'
     O                       SDFND         4   +  1
     O          E            DET2           2
     O                                         +  2 'Policy year :'
     O                       XXYR              +  1
     O          E            DET2           1
     O                       SDEMP#            +  2
     O                                         +  0 '-'
     O                       SDDIV             +  0
     O          E            DET2           2
     O                       SDEMNM            +002
     O          E            DET2           2
     O                                         +002 'Agent:'
     O                       SDAGT#            +002
     O                       SDAGNM            +002
     O          E            DET2           2
     O                                           44 'Calculated premium  . :'
     O                       SDNMPR        J     69
     O          E            DET2           2
     O                                           44 'Claim group allocation:'
     O                       SDCLFN        J     69
     O          E            DET2           2
     O                                           44 'Total claims incurred :'
     O                       SDTCIN        J     69
     O          E            DET2           2
     O                                           44 'Claims surplus  . . . :'
     O                       SDCFSD        J     69
     O          E            DET2           2
     O                                           44 'Percent of return . . :'
     O                       SDPERC        J     69
     O                                         +000 '%'
     O          E            DET2           1
     O                                           44 'Total check amount  . :'
     O                       SDCKAM        J     69
     p*===========================================================================
     p* updatAR - updates WMEAP (A/R)
     p*===========================================================================
     p updatAR         b
     d updatAR         pi

     d kkkey           s                   like(ttkey)

     c     ttky          klist
     c                   kfld                    EACO#
     c                   kfld                    kkkey

     c                   clear                   pea0P
     c                   eval      EACO#  = R4CO#
     c                   eval      EAFND  = R4FND
     c                   eval      EAEMP# = R4EMP#
     c                   eval      EADIV  = R4DIV
     c                   eval      EAFYR  = R4FYR
     c                   eval      EAAMT  = R4AMT
     c                   eval      EACHK# = R4CHK#
     c                   eval      EAARCD = R4ARCD
     c                   eval      kkkey  = EAARCD
     c     ttky          chain     ptt01
     c                   eval      EAARTY = TTTYPE
     c                   eval      EACTYP = TTCTYP
     c                   eval      EADESC = TTDESC
     c                   eval      EAPODT = R4CKDT
     c                   time                    dsdttm
     C                   Eval      EAtrDt = dtCYMD(dsDate)
     c                   eval      EADT   = EATRDT
     c                   eval      EATIME = DSTIME
     c                   eval      EAUSER = Q1USER
     c                   eval      EAPGM  = Q1PGM
     c                   write     pea0P

     p updatAR         e
     p*===========================================================================
     p* updatAB - updates WDABP  (Agency A/R)
     p*===========================================================================
     p updatAB         b
     d updatAB         pi

     d kkkey           s                   like(ttkey)

     c     aaky          klist
     c                   kfld                    ABCO#
     c                   kfld                    ABFEIN
     c                   kfld                    ABMOD

     c     ttky          klist
     c                   kfld                    ABCO#
     c                   kfld                    kkkey

     c                   clear                   pab0p
     c                   eval      ABCO#  = R4CO#
     c                   eval      ABFEIN = R4FEIN
     c                   eval      ABMOD  = R4MOD
     c                   clear                   AAAGT#
     c     aaky          chain     paa00
     c                   eval      ABAGT# = AAAGT#
     c                   eval      ABSTDT = R4STDT
     c                   eval      ABFND  = R4FND
     c                   eval      ABEMP# = R4EMP#
     c                   eval      ABDIV  = R4DIV
     c                   eval      ABFYR  = R4FYR
     c                   eval      ABAMT  = R4AMT
     c                   eval      ABCHK# = R4CHK#
     c                   eval      ABARCD =  R4ARCD
     c                   movel(p)  R4ARCD        KKKEY
     c     ttky          chain     ptt01
     c                   eval      ABARTY = TTTYPE
     c                   eval      ABDESC = TTDESC
     c                   eval      ABPODT = R4CKDT
     c                   time                    dsdttm
     C                   Eval      ABtrDt = dtCYMD(dsDate)
     c                   eval      ABDT   = ABTRDT
     c                   eval      ABTIME = DSTIME
     c                   eval      ABUSER = Q1USER
     c                   eval      ABPGM  = Q1PGM
     c                   eval      ABIDT  = ABTRDT
     c                   eval      ABITIM = DSTIME
     c                   eval      ABIUSR = Q1USER
     c                   eval      ABIPGM = Q1PGM
     c                   write     PAB0P

     p updatAB         e
     p*===========================================================================
     p* updatAS - updates WSASP
     p*===========================================================================
     p updatAS         b
     d updatAS         pi

     C     abky          klist
     C                   kfld                    ABCO#
     C                   kfld                    ABSTDT
     C                   kfld                    ABAGT#

     C     asky          klist
     C                   kfld                    ABCO#
     C                   kfld                    ABSTDT
     C                   kfld                    ABAGT#

     C     *like         define    ASBILL        xxbill
     C     *like         define    ASPAID        xxpaid

     c                   clear                   xxbill
     c                   clear                   xxpaid
     c     abky          setll     pab00
     c     abky          reade     pab00
     c                   dow       not(%eof)
     c                   select
     c                   when      ABARTY = 'A' or
     c                             ABARTY = 'B' or
     c                             ABARTY = 'D' or
     c                             ABARTY = 'F' or
     C                             ABARTY = 'H' or
     C                             ABARTY = 'N'
     C                   add       ABNET         xxbill
     c                   when      ABARTY = 'P' or
     c                             ABARTY = 'I' or
     c                             ABARTY = 'Q' or
     c                             ABARTY = 'T'
     c                   add       ABNET         xxpaid
     c                   when      ABARTY = 'R' or
     c                             ABARTY = 'V'
     c                   sub       ABNET         xxpaid
     c                   endsl
     c     abky          reade     pab00
     c                   enddo

     c     asky          chain     pas00
     c                   if        %found
     c                   eval      ASBILL = xxbill
     c                   eval      ASPAID = xxpaid
     c                   eval      ASBAL = ASBILL - ASPAID
     c                   time                    dsdttm
     C                   Eval      ASdt = dtCYMD(dsDate)
     c                   eval      ASTIME = DSTIME
     c                   eval      ASUSER = Q1USER
     c                   eval      ASPGM  = Q1PGM
     C                   update    pas00
     c                   endif

     p updatAS         e
** SIGNATURE MESSAGE PART ONE
OVER
** SIGNATURE MESSAGE PART TWO
 REQUIRES SECOND SIGNATURE
