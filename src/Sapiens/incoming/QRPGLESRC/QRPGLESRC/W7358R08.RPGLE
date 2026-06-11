     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7358R08 - ICR RECORDS
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W7358R                                          *
     H*                                                               *
     H*  CALL TO:  SRCVTMY                                            *
     H*                                                               *
     H*  CREATED: 01/26/07  PROGRAMMER: C:185 PROJECT: 114WNI         *
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
     FW7358F08  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FWDSIP     UF A E             DISK
     F                                     RENAME(PSI01:DBRCDF)
     F*  LOGICAL  file definition (for maintaining record)
     F*DSIL   IF  E           K        DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D*
     D***PY SCOPYMT01I
     D*
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('ICR Records')
     D COMAPI          C                   CONST('WA001R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@RPMY            5 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CLM#           12
     C                   PARM                    @@STCD            2 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY           KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@RPMY
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CLM#
     C                   KFLD                    @@STCD
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
     C                   Z-ADD     SICO#         KKCO#
     C                   Z-ADD     SIFND         KKFND
     C                   Z-ADD     SIFND         SCFND
     C******************** MOVELFNNAME    SCFNAM
     C*
     C                   Z-ADD     SIRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   Z-ADD     SIEMP#        KKEMP#
     C                   Z-ADD     *LOVAL        KKDIV
     C                   Z-ADD     *LOVAL        KKDIV
     C     EMKY          SETLL     PEM01
     C                   READ      PEM01                                  60
     C                   Z-ADD     SIEMP#        SCEMP#
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   Z-ADD     SIFYR         SCFYR
     C*
     C*  SCR01
     C                   MOVEL     SIRPT#        SCRPT#
     C                   MOVE      SIRTYP        SCRTYP
     C                   MOVE      SICLM#        SCCLM#
     C                   MOVE      SILSCV        SCLSCV
     C                   Z-ADD     SIYREX        SCYREX
     C******************************************
     C*******              Z-ADDSIDOD     SRDTI
     C*******              CALL 'SRCVTDAT'DTMDY
     C*******              MOVELSRDTO     SCDOD
     C                   MOVEL     SIDOD         SCDOD
     C******************************************
     C******************************************
     C*******              Z-ADDSIDTRP    SRDTI
     C*******              CALL 'SRCVTDAT'DTMDY
     C*******              MOVELSRDTO     SCDTRP
     C                   MOVEL     SIDTRP        SCDTRP
     C******************************************
     C******************************************
     C                   Z-ADD     SIDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SCDOB
     C******MM/DD/YY       Z-ADDSIDOB     SCDOB
     C******************************************
     C******************************************
     C*********            Z-ADDSIDTCL    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDY
     C*********            MOVELSRDTO     SCDTCL
     C                   MOVEL     SIDTCL        SCDTCL
     C******************************************
     C                   MOVE      SIMTST        SCMTST
     C                   Z-ADD     SIWKTB        SCWKTB
     C                   Z-ADD     SISSAM        SCSSAM
     C                   MOVE      SIATTY        SCATTY
     C                   MOVE      SIINCT        SCINCT
     C                   MOVE      SISDS1        SCSDS1
     C                   MOVE      SISBM1        SCSBM1
     C                   MOVE      SISWK1        SCSWK1
     C                   MOVE      SISLS1        SCSLS1
     C                   MOVE      SISDS2        SCSDS2
     C                   MOVE      SISBM2        SCSBM2
     C                   MOVE      SISWK2        SCSWK2
     C                   MOVE      SISLS2        SCSLS2
     C                   MOVE      SINSDS        SCNSDS
     C                   MOVE      SINSLS        SCNSLS
     C*
     C**********           Z-ADDSIATDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     SCATDT
     C                   MOVEL     SIATDT        SCATDT
     C*
     C**********           Z-ADDSISUMD    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     SCSUMD
     C                   MOVEL     SISUMD        SCSUMD
     C*
     C                   Z-ADD     SIGRSS        SCGRSS
     C*
     C*  SCR02
     C                   MOVE      SIBCR1        SCBCR1
     C                   MOVE      SIBCD1        SCBCD1
     C******************************************
     C*********            Z-ADDSIBBD1    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDY
     C*********            MOVELSRDTO     SCBBD1
     C                   MOVEL     SIBBD1        SCBBD1
     C******************************************
     C                   MOVE      SIBCR2        SCBCR2
     C                   MOVE      SIBCD2        SCBCD2
     C******************************************
     C*********            Z-ADDSIBBD2    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDY
     C*********            MOVELSRDTO     SCBBD2
     C                   MOVEL     SIBBD2        SCBBD2
     C******************************************
     C                   MOVE      SIBCR3        SCBCR3
     C                   MOVE      SIBCD3        SCBCD3
     C******************************************
     C*********            Z-ADDSIBBD3    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDY
     C*********            MOVELSRDTO     SCBBD3
     C                   MOVEL     SIBBD3        SCBBD3
     C******************************************
     C                   MOVE      SIBCR4        SCBCR4
     C                   MOVE      SIBCD4        SCBCD4
     C******************************************
     C*********            Z-ADDSIBBD4    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDY
     C*********            MOVELSRDTO     SCBBD4
     C                   MOVEL     SIBBD4        SCBBD4
     C******************************************
     C                   MOVE      SIBCR5        SCBCR5
     C                   MOVE      SIBCD5        SCBCD5
     C******************************************
     C*********            Z-ADDSIBBD5    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDY
     C*********            MOVELSRDTO     SCBBD5
     C                   MOVEL     SIBBD5        SCBBD5
     C******************************************
     C                   Z-ADD     SILIAB        SCLIAB
     C                   Z-ADD     SIPDMD        SCPDMD
     C                   Z-ADD     SIPPR         SCPPR
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      SISS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'SISS#':SISS#);
      /End-Free
      *****  End Chg  ***  06/09/20  ************************
     C                   Z-ADD     SIPVFP        SCPVFP
     C                   Z-ADD     SIFNRL        SCFNRL
     C                   Z-ADD     SIPDIN        SCPDIN
     C                   Z-ADD     SIINCM        SCINCM
     C                   Z-ADD     SIINCI        SCINCI
     C                   Z-ADD     SIRMRG        SCRMRG
     C                   Z-ADD     SIPIB         SCPIB
     C*
     C*SCR03
     C                   MOVE      SINMIN        SCNMIN
     C                   MOVE      SINMCR        SCNMCR
     C                   MOVE      SIOCCP        SCOCCP
     C                   MOVE      SIEMST        SCEMST
     C                   MOVE      SISURG        SCSURG
     C******************************************
     C                   Z-ADD     SIDOH         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SCDOH
     C*****MM/DD/YY        Z-ADDSIDOH     SCDOH
     C******************************************
     C                   Z-ADD     SIVIND        SCVIND
     C                   Z-ADD     SIVTRN        SCVTRN
     C                   Z-ADD     SIVEVL        SCVEVL
     C                   Z-ADD     SIHOSP        SCHOSP
     C                   Z-ADD     SILEGD        SCLEGD
     C                   Z-ADD     SIDISP        SCDISP
     C                   Z-ADD     SIPTPD        SCPTPD
     C                   Z-ADD     SIRHBP        SCRHBP
     C                   Z-ADD     SIPRMP        SCPRMP
     C                   Z-ADD     SIDTHP        SCDTHP
     C                   Z-ADD     SISTLA        SCSTLA
     C                   Z-ADD     SIPHPD        SCPHPD
     C                   Z-ADD     SIMETD        SCMETD
     C                   Z-ADD     SIIMDP        SCIMDP
     C                   Z-ADD     SIANNU        SCANNU
     C*
     C                   Z-ADD     SIDMDP        SCDMDP
     C                   Z-ADD     SIVRHB        SCVRHB
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*  SCR01
     C                   MOVEL     SCRPT#        SIRPT#
     C                   MOVE      SCRTYP        SIRTYP
     C                   MOVE      SCCLM#        SICLM#
     C                   MOVE      SCLSCV        SILSCV
     C                   MOVE      SCYREX        SIYREX
     C                   MOVE      SCDOD         SIDOD
     C                   MOVE      SCDTRP        SIDTRP
     C                   MOVEL     SCDOB         SIDOB
     C                   MOVE      SCDTCL        SIDTCL
     C                   MOVE      SCMTST        SIMTST
     C                   MOVE      SCWKTB        SIWKTB
     C                   MOVE      SCSSAM        SISSAM
     C                   MOVE      SCATTY        SIATTY
     C                   MOVE      SCINCT        SIINCT
     C                   MOVE      SCSDS1        SISDS1
     C                   MOVE      SCSBM1        SISBM1
     C                   MOVE      SCSWK1        SISWK1
     C                   MOVE      SCSLS1        SISLS1
     C                   MOVE      SCSDS2        SISDS2
     C                   MOVE      SCSBM2        SISBM2
     C                   MOVE      SCSWK2        SISWK2
     C                   MOVE      SCSLS2        SISLS2
     C                   MOVE      SCNSDS        SINSDS
     C                   MOVE      SCNSLS        SINSLS
     C*
     C**********           Z-ADDSCATDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     SIATDT
     C                   MOVEL     SCATDT        SIATDT
     C*
     C**********           Z-ADDSCSUMD    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     SISUMD
     C                   MOVEL     SCSUMD        SISUMD
     C*
     C                   MOVE      SCGRSS        SIGRSS
     C*
     C*  SCR02
     C                   MOVE      SCBCR1        SIBCR1
     C                   MOVE      SCBCD1        SIBCD1
     C                   MOVE      SCBBD1        SIBBD1
     C                   MOVE      SCBCR2        SIBCR2
     C                   MOVE      SCBCD2        SIBCD2
     C                   MOVE      SCBBD2        SIBBD2
     C                   MOVE      SCBCR3        SIBCR3
     C                   MOVE      SCBCD3        SIBCD3
     C                   MOVE      SCBBD3        SIBBD3
     C                   MOVE      SCBCR4        SIBCR4
     C                   MOVE      SCBCD4        SIBCD4
     C                   MOVE      SCBBD4        SIBBD4
     C                   MOVE      SCBCR5        SIBCR5
     C                   MOVE      SCBCD5        SIBCD5
     C                   MOVE      SCBBD5        SIBBD5
     C                   MOVE      SCLIAB        SILIAB
     C                   MOVE      SCPDMD        SIPDMD
     C                   MOVE      SCPPR         SIPPR
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      SCSS#         SISS#
      /Free
       SIss# = UnMaskData('SSN':'SISS#':SCSS#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C                   MOVE      SCPVFP        SIPVFP
     C                   MOVE      SCFNRL        SIFNRL
     C                   MOVE      SCPDIN        SIPDIN
     C                   MOVE      SCINCM        SIINCM
     C                   MOVE      SCINCI        SIINCI
     C                   MOVE      SCRMRG        SIRMRG
     C                   MOVE      SCPIB         SIPIB
     C*
     C*SCR03
     C                   MOVE      SCNMIN        SINMIN
     C                   MOVE      SCNMCR        SINMCR
     C                   MOVE      SCOCCP        SIOCCP
     C                   MOVE      SCEMST        SIEMST
     C                   MOVE      SCSURG        SISURG
     C                   MOVEL     SCDOH         SIDOH
     C                   MOVE      SCVIND        SIVIND
     C                   MOVE      SCVTRN        SIVTRN
     C                   MOVE      SCVEVL        SIVEVL
     C                   MOVE      SCHOSP        SIHOSP
     C                   MOVE      SCLEGD        SILEGD
     C                   MOVE      SCDISP        SIDISP
     C                   MOVE      SCPTPD        SIPTPD
     C                   MOVE      SCRHBP        SIRHBP
     C                   MOVE      SCPRMP        SIPRMP
     C                   MOVE      SCDTHP        SIDTHP
     C                   MOVE      SCSTLA        SISTLA
     C                   MOVE      SCPHPD        SIPHPD
     C                   MOVE      SCMETD        SIMETD
     C                   MOVE      SCIMDP        SIIMDP
     C                   MOVE      SCANNU        SIANNU
     C*
     C                   MOVE      SCDMDP        SIDMDP
     C                   MOVE      SCVRHB        SIVRHB
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
     C                   Z-ADD     3             #OFSCR            1 0
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
     C                   FEOD      WDSIP
     C                   ENDSR
