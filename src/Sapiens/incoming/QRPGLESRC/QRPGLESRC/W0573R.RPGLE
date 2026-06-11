     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H* 05/30/18 C:053 Project : WC2515
     H*                Additional contact name change
     H*
     H* 06/29/09 C:191 WC1806                                         *
     H*                EFT - UPDATE CALL PARAMS TO W0028R            *
     H*                                                               *
     H*  05/20/09 C:33  Project: WC1770    Priss #15                  *
     H*                 Remove F6 using standard methodology.         *
     H*                                                               *
     H*  09/20/07 C:136 PROJECT: WC997                                *
     H*                 ADD PARMS FOR CALL TO W/W EFT                 *
     H*                                                               *
     H* 08/09/07 C:191 PROJECT WC1453                                 *
     H*              - Add option 70 = EFT Account Info, Option not   *
     H*                allowed if the EFT control Flag is not a 'Y'   *
     H*                                                               *
     H*  03/20/98 C:15 SEQ#: WC060/10782                              *
     H*                Added code to save the RRN passed into the     *
     H*                pgm because it is used when adding a new       *
     H*                record thru F6.                                *
     H*                                                               *
     H*  03/19/98 C:33 SEQ#: WC095/10663                              *
     H*                CLEAR RRN PARM AFTER USE.                      *
     H*                                                               *
     H*  03/05/98 C:85 PROJECT: WC577                                 *
     H*                CHANGED TO NOT ALLOW ENTRY OF ADDITIONAL       *
     H*                CLAIMANT NAME/ADDRESS IF UPFL2 IS 'N'.         *
     H*                                                               *
     H*  1/02/98 C:15 Project: WC659                                  *
     H*               Changed pgm so the status selection will not be *
     H*               displayed on the subset screen if call from a   *
     H*               pmt pgm.                                        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0573F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWDACP     IF   E             DISK
     F                                     RENAME(PAC01:PFRCDF)
     FWDACL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAC01:DBRCDF)
     FWMCMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*                                                                *********
     F*** BEGIN ADD *** 03/05/98 ***********************************************
     FWMUPL     IF   E           K DISK
     F*** END   ADD *** 03/05/98 ***********************************************
     F*** BEGIN ADD **** 08/09/07 ***********************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F*** END ADD ****** 08/09/07 ***********************************
     F*                                                                *********
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     F*** BEGIN CHG **** 08/09/07 ***********************************  *********
     D*******             OF      1  11  1   OFD    35   Options, Function
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     F*** END CHG ***** 08/09/07 ***********************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D*
     D                 DS
     D  LN1                    1     79
     D  LN1A                   1      6
     D  LN1B                   8     10
     D  LN1C                  12     71
     D*
     D                 DS
     D  LN2                    1     79
     D  SCYRCS                 8     18
     D  SCSS#                 26     36
     D  SCCLNM                39     58
     D  SCACDT                69     78
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Additional Addresses')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0573R02')
     D COMAPI          C                   CONST('WA001R')
     D EMPAPI          C                   CONST('WA005R')
     D CLMAPI          C                   CONST('WA006R')
     D EXRAPI          C                   CONST('WA010R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D XCEXRE          C                   CONST('EXCS REIN ')
     D XCBC01          C                   CONST('FILE STATUS IS OUT, -
     D                                     USER - XXXXXXXXXX')
     D XCBC02          C                   CONST('FILE STATUS IS IN')
     D MODEA           C                   CONST('-Change/Add-')
     D MODEB           C                   CONST('-Display-')
     D MODEC           C                   CONST('-Search-')
     D*** BEGIN ADD **** 08/09/07 ***********************************
     D                 DS
     D  XXCLM                  1     13
     D  XXFND                  1      3
     D  XXFYR                  4      6
     D  XXCASE                 7     13

     I*** END ADD *** 08/09/07 ***************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@PMTF            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** BEGIN CHG *** 03/20/98 ***********************************************
     C******************** PARM @@RRN     ##RRN2  90
     C                   PARM      SVRRN         ##RRN2            9 0
     C*** END CHG ***** 03/20/98 ***********************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Claim line API
     C*
     C     CLMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##YR
     C                   PARM                    ##CASE
     C                   PARM                    LN1
     C                   PARM                    LN2
     C*
     C*  Employer Emp#/Div/Name API
     C*
     C     EMPPL         PLIST
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ENAM
     C                   PARM                    ##EDN            80
     C*
     C*  Message API
     C*
     C     EXRPL         PLIST
     C                   PARM      @@RRN         ##RRN             9 0          FOR WMCMP
     C                   PARM                    ##FNDR           13 2
     C                   PARM                    ##EXRE            1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##YR
     C                   KFLD                    ##CASE
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##YR
     C                   KFLD                    ##CASE
     C                   KFLD                    KKPNAM
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*                                                                *********
     C*** BEGIN ADD *** 03/05/98 ***********************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** END   ADD *** 03/05/98 ***********************************************
     C*** BEGIN ADD **** 08/09/07 *************************************
     C*  KEY LIST FOR THE SYSTEM CONTROL FILES
     C*
     C     SYKY          KLIST
     C                   KFLD                    XSKEY
     C*** END ADD ****** 08/09/07 *************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    ACPNAM        U1PDSC
     C     *LIKE         DEFINE    ACPNAM        U2PDSC
     C     *LIKE         DEFINE    ACPNAM        KKPNAM
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMFND         ##FND
     C     *LIKE         DEFINE    CMFYR         ##YR
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    EMEMP#        ##EMP#
     C     *LIKE         DEFINE    EMDIV         ##DIV
     C     *LIKE         DEFINE    EMNAME        ##ENAM
     C*** BEGIN ADD *** 03/20/98 ***********************************************
     C     *LIKE         DEFINE    @@RRN         SVRRN
     C*** END ADD ***** 03/20/98 ***********************************************
     C*** BEGIN ADD **** 08/09/07 *************************************
     C     *LIKE         DEFINE    WCKEY         XSKEY
     C*** END  ADD ***** 08/09/07 *************************************
     C*
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
B01  C     SSPNAM        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKPNAM
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKPNAM
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCPNAM        KKPNAM
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     ACPNAM        SCPNAM
     C                   MOVEL     ACPNAM        SCNAME
     C*** Beg Add *** 05/30/18 *******************************************
     C     ACFNAM        IFNE      *BLANK
     C     ACFNAM        CAT(P)    ACPNAM:1      SCNAME
     C                   ENDIF
     C*** Beg Add *** 05/30/18 *******************************************
     C                   MOVEL     ACADR1        SCADR1
     C                   MOVEL     ACCTY         SCCTY
     C                   MOVEL     ACCKAL        SCCKAL
     C                   MOVEL     ACSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
      *** BEGIN ADD **** 08/09/07 ***********************************
     C*===============================================================
     C* OPT70 - SUBROUTINE FOR OPTION 70 (EFT ACCOUNT INFO)
     C*===============================================================
     C     OPT70         BEGSR
     C*
     C                   MOVE      ACFND         XXFND
     C                   MOVE      ACFYR         XXFYR
     C                   MOVE      ACCASE        XXCASE
     C*
     C                   MOVE      ACSEQ#        ##SEQ#
     C                   MOVE      XXCLM         ##CLM
     C*
     C*** BEG ADD ***** 06/29/09 ***************************************
     C                   MOVEL     'WA073R'      OBJNAM           10
     C                   MOVEL     '*PGM'        OBJTYP            7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    OBJNAM           10
     C                   PARM                    OBJTYP            7
     C                   PARM                    OBJERR            1
     C     OBJERR        IFEQ      *OFF
     C                   MOVE      'D'           #1TYPE            1
     C                   ELSE
     C                   MOVE      'C'           #1TYPE            1
     C                   ENDIF
     C*** END ADD ***** 06/29/09 ***************************************
     C*
     C                   CALL      'W0028R'                             99
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C***************      PARM           #1RRN   90
     C                   PARM      SCDBRR        #1RRN             9 0
     C*** END   CHG ****** 06/29/09 ************************************
     C                   PARM                    ##CLM            13
     C                   PARM      *BLANKS       ##FEIN           12
     C                   PARM                    ##SEQ#            5 0
      *** BEG ADD ***** 09/20/07 *********************************
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C*************        PARM 'C'       #1TYPE  1
     C                   PARM                    #1TYPE            1
     C*** END   CHG ****** 06/29/09 ************************************
      *** END ADD ***** 09/20/07 *********************************
     C*
     C                   ENDSR                                                  OPT70
      ***** END ADD ***** 08/09/07 ************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     F*** BEGIN ADD **** 08/09/07 ***********************************
     C     SCSEL         CASEQ     '70'          OPT70
     F*** END   ADD **** 08/09/07 ***********************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     8             SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*** BEGIN ADD *** 01/02/98 ***********************************************
     C     @@PMTF        COMP      'Y'                                    31    PMT PGM SRCH
     C*** END ADD ***** 01/02/98 ***********************************************
     C*
     C* DETERMINE SCREEN MODE
     C*
     C                   SELECT
     C*
     C     @@CALL        WHENEQ    ' '
     C*
     C                   MOVE(P)   MODEA         SCMODE
     C*
     C     @@CALL        WHENEQ    '1'
     C*
     C                   MOVE(P)   MODEB         SCMODE
     C*
     C     @@CALL        WHENEQ    '2'
     C*
     C                   MOVE(P)   MODEC         SCMODE
     C*
     C                   ENDSL
     C*                                                                *********
     C*** BEGIN ADD *** 03/05/98 ***********************************************
     C*
     C*  CHAIN TO THE USER PARAMETER FILE
     C*  IF UPFL2 IS 'N' THEN BLANK OUT 'F6=ADD'
     C*  TO DISALLOW ADDITIONAL CLAIMANT NAME/ADDRESS.
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *OFF
     C     UPFL2         IFEQ      'N'
     C***  Begin Chg  ***  05/20/09  **************************************
     C**********           MOVE *BLANKS   OFD,7
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C***  End   Chg  ***  05/20/09  **************************************
     C                   MOVE      *ON           *IN32
     C                   ENDIF                                                  UPFL2=N
     C                   ENDIF                                                  *IN99=OFF
     C*                                                                *********
     C*** END   ADD *** 03/05/98 ***********************************************
     C*
     C* CHAIN OUT TO CLAIM FILE
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C*
     C* GET EXCESS REINSURANCE INFO
     C*
     C     CMEXCS        IFNE      'Y'
     C     ##RRN         IFNE      *ZERO
     C*
     C                   CALL      EXRAPI        EXRPL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##EXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C*
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C*
     C                   ENDIF
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     CMCVT#        IFNE      *BLANK
     C*
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C*
     C                   ENDIF
     C*
     C*  AGENGY CLAIM# (OR, SATE CLAIM#)
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
     C*
     C* CALL CLAIMS API
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##YR
     C                   MOVE      CMCASE        ##CASE
     C*
     C                   CALL      CLMAPI        CLMPL
     C*
     C* CHAIN OUT TO EMPLOYER FILE TO GET EMPLOYER NAME
     C*
     C                   MOVE      CMEMP#        ##EMP#
     C                   MOVE      CMDIV         ##DIV
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   MOVEL     EMNAME        ##ENAM
     C*
     C* CALL EMPLOYER API TO COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      EMPAPI        EMPPL
     C*
     C                   MOVEL     ##EDN         SCEDN
     C*
     C* CHAIN OUT TO GROUP MASTER TO GET GROUP NAME
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C*** BEGIN ADD *** 03/20/98 ***********************************************
     C                   MOVE      @@RRN         SVRRN
     C*** END ADD ***** 03/20/98 ***********************************************
     C***  BEGIN ADD  ***  03/19/98  *********************************
     C                   CLEAR                   @@RRN
     C***  END   ADD  ***  03/19/98  *********************************
     C*
     C*** BEGIN ADD **** 08/09/07 ***********************************  *********
     C* Disable option 70, if EFT Install Flag is not 'Y'
     C*
     C                   MOVEL(P)  'WC'          XSKEY
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL35        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '70'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD ****** 08/09/07 **********************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C     KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C     KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C     KEY2          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C     KEY2          SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSPNAM        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  ACPNAM        U1PDSC
     C     LO:HI         XLATE(P)  SSPNAM        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     ACSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*  If payments entry caller, list only active with check allowed.
     C*
     C     @@PMTF        IFEQ      'Y'
     C     ACSTAT        IFNE      'A'
     C     ACCKAL        ORNE      'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Additional name       Address               City              Chk   Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
17070=EFT Account Info      YYN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
