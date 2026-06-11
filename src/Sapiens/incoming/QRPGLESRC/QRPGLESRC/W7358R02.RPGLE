     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7358R02 - LOSS RECORDS
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W7358R                                          *
     H*                                                               *
     H*  CALL TO:  SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 06/06/98  PROGRAMMER: C:87  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7358F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDSLP     UF A E             DISK
     F                                     RENAME(PSL01:DBRCDF)
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d charSS#         s              9
      ***  End   Add  ***  06/09/20  ****************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Loss Records')
     D COMAPI          C                   CONST('WA001R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     SLCO#         KKCO#
     C                   Z-ADD     SLFND         KKFND
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     SLFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     SLRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   Z-ADD     SLEMP#        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C     EMKY          SETLL     PEM01
     C                   READ      PEM01                                  60
     C                   Z-ADD     SLEMP#        SCEMP#
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   Z-ADD     SLFYR         SCFYR
     C*
     C*  SCR01
     C                   Z-ADD     SLCLAS        SCCLAS
     C                   Z-ADD     SLPRVR        SCPRVR
     C                   Z-ADD     SLLOS#        SCLOS#
     C                   Z-ADD     SL#CLM        SC#CLM
     C*
     C                   Z-ADD     SLACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      SLCLM#        SCCLM#
     C                   Z-ADD     SLCLST        SCCLST
     C                   Z-ADD     SLINJ#        SCINJ#
     C                   Z-ADD     SLCAT#        SCCAT#
     C                   Z-ADD     SLINDM        SCINDM
     C                   Z-ADD     SLMEDI        SCMEDI
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              Z-ADD     SLSS#         SCSS#
      /Free
       clear charSS#;
       charSS# = %editc(SLSS#:'X');
       scSS# = MaskData('SSN':'SLSS#':charSS#);
      /End-Free
      *****  End Chg  ***  06/09/20  ************************
     C                   MOVE      SLUPDT        SCUPDT
     C                   Z-ADD     SLLC1         SCLC1
     C                   Z-ADD     SLLC2         SCLC2
     C                   Z-ADD     SLLC3         SCLC3
     C                   Z-ADD     SLLC4         SCLC4
     C                   Z-ADD     SLLC5         SCLC5
     C                   Z-ADD     SLJRST        SCJRST
     C                   Z-ADD     SLMCTP        SCMCTP
     C*
     C*  SCR02
     C                   Z-ADD     SLPART        SCPART
     C                   Z-ADD     SLNATR        SCNATR
     C                   Z-ADD     SLCAUS        SCCAUS
     C                   MOVE      SLVOCR        SCVOCR
     C                   MOVE      SLLUMP        SCLUMP
     C                   Z-ADD     SLFRAU        SCFRAU
     C                   Z-ADD     SLPDIN        SCPDIN
     C                   Z-ADD     SLPDMD        SCPDMD
     C                   Z-ADD     SLCATF        SCCATF
     C                   Z-ADD     SLEATF        SCEATF
     C                   Z-ADD     SLDEDR        SCDEDR
     C                   Z-ADD     SLALAP        SCALAP
     C                   Z-ADD     SLALAI        SCALAI
     C                   MOVE      SLOCCU        SCOCCU
     C*
     C     SLASWG        IFEQ      'Y'
     C                   MOVE      *ON           *IN91
     C                   Z-ADD     2             #OFSCR
     C                   ELSE
     C                   MOVE      *OFF          *IN91
     C                   Z-ADD     1             #OFSCR
     C                   ENDIF
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     KKCO#         SLCO#
     C                   Z-ADD     KKFND         SLFND
     C*
     C                   Z-ADD     SCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         SLRPMY
     C*
     C                   Z-ADD     SCEMP#        SLEMP#
     C                   Z-ADD     SCFYR         SLFYR
     C                   Z-ADD     SCCLAS        SLCLAS
     C                   Z-ADD     SCPRVR        SLPRVR
     C                   Z-ADD     SCLOS#        SLLOS#
     C                   Z-ADD     SC#CLM        SL#CLM
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SLACDT
     C*
     C                   MOVE      SCCLM#        SLCLM#
     C                   Z-ADD     SCCLST        SLCLST
     C                   Z-ADD     SCINJ#        SLINJ#
     C                   Z-ADD     SCCAT#        SLCAT#
     C                   Z-ADD     SCINDM        SLINDM
     C                   Z-ADD     SCMEDI        SLMEDI
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              Z-ADD     SCSS#         SLSS#
      /Free
       clear charSS#;
       charSS# = UnMaskData('SSN':'SLSS#':SCSS#);
       SLss# = %Dec(charSS#:9:0);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C                   MOVE      SCUPDT        SLUPDT
     C                   Z-ADD     SCLC1         SLLC1
     C                   Z-ADD     SCLC2         SLLC2
     C                   Z-ADD     SCLC3         SLLC3
     C                   Z-ADD     SCLC4         SLLC4
     C                   Z-ADD     SCLC5         SLLC5
     C                   Z-ADD     SCJRST        SLJRST
     C                   Z-ADD     SCMCTP        SLMCTP
     C*
     C*  SCR02
     C                   Z-ADD     SCPART        SLPART
     C                   Z-ADD     SCNATR        SLNATR
     C                   Z-ADD     SCCAUS        SLCAUS
     C                   MOVE      SCVOCR        SLVOCR
     C                   MOVE      SCLUMP        SLLUMP
     C                   Z-ADD     SCFRAU        SLFRAU
     C                   Z-ADD     SCPDIN        SLPDIN
     C                   Z-ADD     SCPDMD        SLPDMD
     C                   Z-ADD     SCCATF        SLCATF
     C                   Z-ADD     SCEATF        SLEATF
     C                   Z-ADD     SCDEDR        SLDEDR
     C                   Z-ADD     SCALAP        SLALAP
     C                   Z-ADD     SCALAI        SLALAI
     C                   MOVE      SCOCCU        SLOCCU
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     2             #OFSCR            1 0
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WDSLP
     C                   ENDSR
