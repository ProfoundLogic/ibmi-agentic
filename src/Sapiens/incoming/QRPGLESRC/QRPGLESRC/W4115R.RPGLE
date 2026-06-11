     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*
     H* ***************************************************************
     H/TITLE W4115R - GENERATE POLICY DEPT LETTERS - SCRN PGM
     H* ***************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for the required information    *
     H* in order to submit a job to print a Policy Dept. Letter.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     MENU                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     W4115C                                                    *
     H*                                                               *
     H*  PROGRAMMER: C:30                                             *
     H*                                                               *
     H*  CREATED:    11/28/94               PROJECT: WC375            *
     H*                                                               *
     H* ***************************************************************
     H*                                                               *
     H* SCREEN PROCESSING:
     H*    SCR01: FUND:                   From User Parameters
     H*           FUND YEAR:              Req
     H*           FROM EMPL/DIV:          Req
     H*           TO EMPL/DIV:            Opt, If left blank - defaults to From
     H*           The department names (and selection fields) are part of a
     H*           subfile processed from file WMDEL.
     H*    SCR02: The entry portion of the screen is based upon a subfile.
     H*           The fields are displayed from the following:
     H*               CHOICE:             ADD: input only
     H*                                   UPD: marked if  record in  WSDLL
     H*               DOCUMENT ID:        gotten from WMDOL
     H*               DOCUMENT DESC:      gotten from WMDOL
     H*               ADDITIONAL INPUT:   gotten from DOAIFL in WMDOL
     H*               # OF COPIES:        ADD: input only, written to WSDLL
     H*                                   UPD: gotten from DL#CPY from WSDLL
     H* *******************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  04/08/15 C:65  Project: SY251                                *
     H*                 Call new SPILETTER API - this program will    *
     H*                 now be a shell for new process - only process *
     H*                 in this manner if Scriptura Module is enabled.*
     H*                                                               *
     H*  10/17/11 C:277 Project: SY195                                *
     H*                 QDLS migration to IFS                         *
     H*                 Add two parameters with command               *
     H*                                                               *
     H*  07/17/06 C:136 SUPPORT: WA099 53676                          *
     H*                 CORRECT PARM MISMATCH ON YEAR SEARCH          *
     H*                                                               *
     H*  05/27/06  C:02 ADD "INTERACTIVE PROCESSING" INFO             *
     H*                                                               *
     H* 08/08/00 C:61  SUPPORT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H*  05/27/98 C:09 Project: DR048/11740                           *
     H*                FROM/TO EMPLOYER VALUES NOT PROPERLY LOADED    *
     H*                WHEN PROGRAM IS CALLED FROM OPTION 20 ON THE   *
     H*                WORK WITH EMPLOYER POLICY YEARS SCREEN.            *
     H* *******************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                                   *
     H*  70    CALLED FROM ANOTHER PROGRAM THAT PASSES CORRECT INFO   *
     H* N70    CALLED FROM MENU                                       *
     H*                                                                   *
     H* SCR01:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    WCR0517-INVALID FYR FOR FUND                           *
     H*  21    WCR0694-INVALID FROM EMPLOYER/DIV FOR FUND YEAR        *
     H*  22    WCR0694-INVALID TO EMPLOYER/DIV FOR FUND YEAR          *
     H*  24    SCM0510-INVALID CHAR USED TO INDICATE A SELETED RECORD *
     H*  25    WCR0501-ACTIVE EMPLOYER FLAG MUST BE Y OR N            *
     H*  26    WCR0833-SELECTION REQUIRED                             *
     H*  27    WCR0835-EMPLOYER RANGE MUST BE ASCENDING               *
     H*  28    WCR0728-CURSOR MUST BE ON EMPLR LINE TO DO EMPLR SEARCH*
     H*  29    POSITION CURSOR AT FROM EMPLOYER                       *
     H*  30    POSITION CURSOR AT TO EMPLOYER                         *
     H*  31    POSITION CURSOR AT ACTIVE EMPLOYER FLAG                *
     H*  32    ROLL UP/DOWN IS INVALID AT THIS TIME                   *
     H*  33    ROLL UP/DOWN IS INVALID AT THIS TIME                   *
     H*  35    POSITION CURSOR TO FUND YEAR                           *
     H*  38    WCR0538-INVALID FROM EMPLOYER NUMBER ENTERED           *
     H*  39    WCR0947-EITHER EMPLOYER DETAIL OR AGENT HISTORY INFO   *
     H*        MUST BE ESTABLISHED FOR "FROM" FYR/EMPLOYER            *
     H*  41    WCR0947-EITHER EMPLOYER DETAIL OR AGENT HISTORY INFO   *
     H*        MUST BE ESTABLISHED FOR "TO" FYR/EMPLOYER              *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*  94    USED TO DETERMINE WHETHER USER HAS ATTEMPTED A ROLL    *
     H*        DOWN AND NO PRIOR DEPARTMENT RECORDS EXIST.            *
     H*  99    DEPT CODE VALUE BROUGHT FROM REQUEST LETTER SEARCH     *
     H*        LOAD A '1' INTO CORRESPONDING SCSEL (TURN ON BYPASS FL)*
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    SCM0510-INVALID CHAR USED TO INDICATE A SELETED RECORD *
     H*  22    WCR0833-SELECTION REQUIRED                             *
     H*  23    WCR0834-NO SCREEN AVAILABLE FOR ADDTL INPUT            *
     H*  24      SCM0516-DOCUMENT NOT FOUND IN SPECIFIED FOLDER       *
     H*  25      SCM0517-NOT AUTHORIZED TO DOCUMENT OR FOLDER         *
     H*  26      SCM0518-USER NOT ENROLLED IN OFFICE 400              *
     H*  27      SCM0519-UNABLE TO PROCESS REQUEST FOR THIS DOCUMENT  *
     H*  28    INVALID OUTQ (MCR0509)                                 *
     H*  29
     H*  34    WCR0943-REPOSTION DOCUMENT NAME NOT FOUND              *
     H*  35    POSITION CURSOR TO REPOSITION DOCUMENT NAME            *
     H*  36    WCR0945-SUSPENSION DATE MUST EQUAL THE CURRENT DATE    *
     H*        FOR REQUESTS SUBMITTED FOR IMMEDIATE PROCESSING        *
     H* ***************************************************************
     FW4115F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN1)
     F                                     SFILE(SCR02S:RRN2)
     F*
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
      *** BEG DEL ***** 07/17/06 **********************************
     F*********WDF1P   IF  E                    DISK
     F*********            PF101                             KRENAMEPF102
      *** END DEL ***** 07/17/06 **********************************
     FWMEML     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELL1    IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWSDLL     UF A E           K DISK
     FWSDLL3    O    E           K DISK
     F                                     RENAME(PDL01:PDL02)

     D*
     D DID             S             12    DIM(999)
     D*
     D OPTNM           C                   CONST('Generate Policy Depa-
     D                                     rtment Letters')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*   ADDITIONAL PROGRAM PARMS
     D*
     D                 DS
     D  PARMS                  1    164
     D  QQCO#                  1      3
     D  QQFND                  4      6
     D  QQREQ#                 7     11
     D  QQDPCD                12     16
     D  QQDCID                17     28
     D  QQDESC                29     56
     D  QQFLDR                57    136
     D  QQFEMP               137    145
     D  QQFDIV               146    150
     D  QQTEMP               151    159
     D  QQTDIV               160    164
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** begin add ***  04/08/15  *********************************************
     d xaRrn           s              9
     d xxRrn           s              9  0

      /copy SprLetter
     d/copy sPrGenPop
     D*** end add *****  04/08/15  *********************************************

     IPDL01
     I              DLREQ#                      HIREQ#
     I              DLCO#                       HICO#
     I              DLFND                       HIFND
     I              DLFYR                       HIFYR
     I              DLFEMP                      HIFEMP
     I              DLFDIV                      HIFDIV
     I              DLTEMP                      HITEMP
     I              DLTDIV                      HITDIV
     I              DLAEFL                      HIAEFL
     I              DLDPCD                      HIDPCD
     I              DLFLDR                      HIFLDR
     I              DLDCID                      HIDCID
     I              DL#CPY                      HI#CPY
     I              DLQTFL                      HIQTFL
     I              DLSTAT                      HISTAT
     I              DLIUSR                      HIIUSR
     I              DLUSER                      HIUSER
     I              DLPGM                       HIPGM
     I              DLIPGM                      HIIPGM
     I              DLTIME                      HITIME
     I              DLITIM                      HIITIM
     I              DLDT                        HIDT
     I              DLIDT                       HIIDT

     C*
     C*   COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  DEPT MASTER FILE
     C*
     C     DEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    SCDPCD
     C*
     C     DEKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C*
     C*  DEPT/DOC ID MASTER FILE
     C*
     C     DOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    SCDPCD
     C*
     C*  POLICY DEPT LETTER WORKFILE
     C*
     C     DLKY          KLIST
     C                   KFLD                    HICO#
     C                   KFLD                    HIFND
     C*
     C     DLKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    SCDPCD
     C                   KFLD                    SCFLDR
     C                   KFLD                    SCDCID
     C*
     C     DLKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXFEMP
     C                   KFLD                    XXFDIV
     C                   KFLD                    XXTEMP
     C                   KFLD                    XXTDIV
     C                   KFLD                    XXDPCD
     C                   KFLD                    XXREQ#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND AUXILIARY FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    DEDPCD        SVDPCD
     C     *LIKE         DEFINE    DLDPCD        XXDPCD
     C     *LIKE         DEFINE    DLFEMP        XXFEMP
     C     *LIKE         DEFINE    DLFDIV        XXFDIV
     C     *LIKE         DEFINE    DLTEMP        XXTEMP
     C     *LIKE         DEFINE    DLTDIV        XXTDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C*
      *------------------------*
      *   INITIALIZE VARIABLE WORK FIELDS
      *------------------------*
     C     *LIKE         DEFINE    DEDPCD        HLDPCD
     C     *LIKE         DEFINE    DEDPNM        HLDPNM
     C     *LIKE         DEFINE    DOFLDR        HLFLDR
     C     *LIKE         DEFINE    DODCID        HLDCID
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    DLFYR         XXFYR
     C     *LIKE         DEFINE    DLFEMP        XXEMP#
     C     *LIKE         DEFINE    DLFDIV        XXDIV
     C     *LIKE         DEFINE    DLFEMP        QQEMP#
     C     *LIKE         DEFINE    DLFDIV        QQDIV
     C     *LIKE         DEFINE    DLREQ#        XXREQ#
     C     *LIKE         DEFINE    DLREQ#        NWREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1            Q=QUOTE SYSTEM
     C                   PARM                    ZQEMP#            9
     C                   PARM                    ZQDIV             5
     C                   PARM                    QQDEPT            2
     C                   PARM                    ZQCO#             3 0
     C                   PARM                    ZQFND             3 0
     C                   PARM                    ZQFYR             3 0
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     QQCALL        IFNE      '2'                                          CALL FROM PGM
     C                   MOVE      *ON           *IN35
     C                   ENDIF
     C*
     C*** begin add ***  04/08/15  *********************************************
     C*
     C                   CALL      'SA047R'
     C                   PARM      *BLANK        QQScrip           1
     C*
     C                   If        QQScrip = 'Y'
     C*
     C     Q1PRMS        IFEQ      1
     C*
     C                   Movel(p)  qqPrds        xaRRN
     C                   Testn                   xaRRN                60
     C*
     C                   If        *in60 = *on
     C*
     C                   Move      xaRRN         xxRRN
     C
      /free
                         DocWindow(xxRRN:'PS');
      /end-free
     C                   Endif
     C*
     C                   Else
      /free
       //***PopOneBtn('The mass functionality is not available for +
       //***           department letters yet.':'Department Letters':'Ok');
                         DocWindow(0:'PS');
      /end-free
     C                   Endif
     C
     C                   GOTO      Term
     C
     C                   Endif
     C*** end add *****  04/08/15  *********************************************
     C*
     C                   TIME                    DSDTTM
     C**** ADD 5/27/06 *********************************************************
     C*** CHECK IF DTM IS INSTALLED *** DTMINS FIELD HOLDS THIS VALUE **********
     C                   CALL      'DTMCHK'
     C                   PARM      *BLANKS       DTMINS            1
     C*
     C*** CHECK TO SEE THE DEFAULT EDITOR FOR THIS USER
     C*
     C                   CALL      'DA510R'
     C                   PARM      Q1USER        X$USER           10
     C                   PARM      *BLANKS       X$EDTR           10
     C                   PARM      *BLANKS       X$RTNC            1
     C*
     C*
     C**** END ADD 5/27/06 *****************************************************
     C*
     C     QQPRDS        IFEQ      *BLANKS
     C                   MOVEL     OPTNM         QQPRDS
     C                   ENDIF                                                  QQPRDS=*BLANKS
     C*
     C                   MOVE      QQPRDS        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   MOVE      QQCALL        HLCALL            1
     C                   MOVE      '1'           *IN35
     C                   MOVE      ZQEMP#        SCFEMP
     C                   MOVE      ZQDIV         SCFDIV
     C                   MOVE      QQDEPT        XXDEPT
     C*
     C*
      *------------------------*
      **  RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   MOVEL     *BLANK        UPOUTQ
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C                   Z-ADD     UPFND         SCFND
     C  N70              Z-ADD     UPFYR         SCFYR
     C                   END
     C*
     C                   MOVE      *OFF          *IN70
     C*
     C     Q1PRMS        IFGT      5
     C*
     C                   MOVE      *ON           *IN70
     C                   Z-ADD     ZQCO#         XXCO#
     C                   Z-ADD     ZQFND         XXFND
     C                   Z-ADD     ZQFND         SCFND
     C                   Z-ADD     ZQFYR         XXFYR
     C                   Z-ADD     ZQFYR         SCYR
     C                   Z-ADD     ZQFYR         SCFYR
     C*** BEGIN CHG *** 05/27/98 ***********************************************
     C*********************MOVE ZQEMP#    SCFEMP
     C*********************MOVE ZQDIV     SCTEMP
     C                   MOVE      ZQEMP#        SCFEMP
     C                   MOVE      ZQDIV         SCFDIV
     C                   MOVE      ZQEMP#        SCTEMP
     C                   MOVE      ZQDIV         SCTDIV
     C*** END CHG ***** 05/27/98 ***********************************************
     C                   MOVE      ZQEMP#        XXEMP#
     C                   MOVE      ZQDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C                   ENDIF                                                  Q1PRMS>5
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT1
     C                   EXSR      LOAD1                                        LOAD RCDS FROM FILE
     C*
     C   70              MOVE      'Y'           SCAEFL
     C  N70              MOVE      'N'           SCAEFL
     C  N70              MOVE      *ON           *IN29
     C  N70              MOVE      *OFF          *IN35
     C*
      *------------------------*
      **  DISPLAY POINT
      *------------------------*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
      /If Not Defined (WEB)
     C                   WRITE     SCR01
      /Endif
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      '1'           *IN01
      /If Not Defined (WEB)
     C                   WRITE     SCR02
      /Endif
     C                   EXFMT     SCR02C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C*===============================================================
     C*  F10=SEARCH
     C*===============================================================
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   Z-ADD     DBRRN         ##RRN
     C                   CLEAR                   ##RRN2
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C*
     C                   CALL      'W0011R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
      *** BEG ADD ***** 07/17/06 ******************************************
     C                   PARM      *ZERO         ##FYR             3 0
      *** END ADD ***** 07/17/06 ******************************************
     C*
      *** BEG CHG ***** 07/17/06 ******************************************
     C********** ##RRN     IFNE *ZERO
     C**********           MOVE *ON       *IN29
     C********** ##RRN     CHAINWDF1P                60
     C**********           Z-ADDF1FYR     SCFYR
     C**********           ELSE
     C**********           MOVE *ON       *IN35
     C**********           ENDIF                           ##RRN<>*ZERO
     C     ##FYR         IFNE      *ZERO
     C                   MOVE      *ON           *IN29
     C                   Z-ADD     ##FYR         SCFYR
     C                   ELSE
     C                   MOVE      *ON           *IN35
     C                   ENDIF
      *** END CHG ***** 07/17/06 ******************************************
     C**
     C     HFLD          WHENEQ    'SCFEMP'
     C     HFLD          OREQ      'SCFDIV'
     C     HFLD          OREQ      'SCTEMP'
     C     HFLD          OREQ      'SCTDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN2            9 0
     C                   PARM                    ##RRN             9 0
     C                   PARM      ' '           ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C*
     C     ##RRN2        IFNE      *ZERO
     C     ##RRN2        CHAIN     WMEMP                              60
     C                   ENDIF                                                  ##RRN2
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFEMP'
     C     HFLD          OREQ      'SCFDIV'
     C*
     C     ##RRN2        IFNE      *ZERO
     C                   MOVE      *ON           *IN30
     C                   Z-ADD     EMEMP#        SCFEMP
     C                   Z-ADD     EMDIV         SCFDIV
     C                   ELSE
     C                   MOVE      *ON           *IN29
     C                   ENDIF                                                  ##RRN2<>*ZERO
     C*
     C     HFLD          WHENEQ    'SCTEMP'
     C     HFLD          OREQ      'SCTDIV'
     C*
     C     ##RRN2        IFNE      *ZERO
     C                   MOVE      *ON           *IN31
     C                   Z-ADD     EMEMP#        SCTEMP
     C                   Z-ADD     EMDIV         SCTDIV
     C                   ELSE
     C                   MOVE      *ON           *IN30
     C                   ENDIF                                                  ##RRN2<>*ZERO
     C*
     C                   ENDSL
     C**
     C                   ENDSL
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ELSE
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKC=*ON
     C*
     C                   ENDIF                                                  *INKJ=*ON
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      *BLANKS       QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C**                   MOVE *ZEROS    SCFYR
     C**                   MOVE *ZEROS    SCFEMP
     C**                   MOVE *ZEROS    SCFDIV
     C**                   MOVE *ZEROS    SCTEMP
     C**                   MOVE *ZEROS    SCTDIV
     C**                   MOVE *BLANKS   SCAEFL
     C**                   MOVE *BLANKS   SCDPCD
     C                   MOVE      *BLANKS       SVDPCD
     C                   MOVE      '1'           *IN35                          DSPATR(PC)
     C                   GOTO      TOP
     C                   END
     C*
     C     Q2RECF        IFNE      'SCR01'
     C     Q2RECF        ANDNE     'SCR01C'
     C     Q2RECF        ANDNE     'SCR01S'
     C     Q2RECF        ANDNE     'SCR02'
     C     Q2RECF        ANDNE     'SCR02C'
     C     Q2RECF        ANDNE     'SCR02S'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   EXSR      INIT2
     C                   EXSR      LOAD2
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C********           WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C****************************************
     C*  F13 = CHG USER PARAMETERS           *
     C****************************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*******            WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   END
     C                   MOVE      *BLANKS       SCDPCD
     C                   GOTO      TOP
     C                   END                                                    END *INKM
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C*
     C**  CHECK FOR END OF FILE (EITHER SCREEN)
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C************
     C*   SCR01  *
     C************
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      '0'           *IN94
     C                   EXSR      LOAD1
     C                   END                                                    END EQ SCR01
     C*
     C                   GOTO      STRT
     C**
     C                   END                                                    END 1 *IN18 ROLLUP
     C****************************************
     C*   *IN19 -  ROLLDOWN REQUEST          *
     C****************************************
     C     *IN19         IFEQ      '1'                                          ROLLUP
     C************
     C*   SCR01  *
     C************
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C*
     C     *IN94         CABEQ     '1'           STRT                     33
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      '1'           *IN94
     C                   GOTO      TOP
     C*
     C                   END                                                    END EQ SCR01
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END 1 *IN19 ROLLDOWN
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*****************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR01S'      SCR01T
     C     Q2RECF        CABEQ     'SCR01C'      SCR01T
     C*
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02S'      SCR02T
     C     Q2RECF        CABEQ     'SCR02C'      SCR02T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C*  VALIDATE INFORMATION ENTERED: (IF CALLED FROM MENU)
     C*
     C*  FUND*YEAR MUST BE ENTERED
     C*
     C  N70SCFYR         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KY          CHAIN     PF101                              20
     C  N70*IN20         CABEQ     '1'           STRT
     C                   MOVE      *OFF          *IN20
     C*
     C*  VALIDATE THE EMPLOYER/DIV EXISTS
     C*
     C     Q1PRMS        IFLE      5
     C*
     C                   Z-ADD     SCFEMP        XXEMP#
     C                   Z-ADD     SCFDIV        XXDIV
     C     ELKY          CHAIN     PEL01                              39
     C     *IN39         CABEQ     '1'           STRT
     C*
     C*  IF ENTERED, VALIDATE THE TO EMPLOYER/DIV EXISTS
     C*
     C     SCTEMP        IFNE      *ZERO
     C     SCTDIV        ORNE      *ZERO
     C                   Z-ADD     SCTEMP        XXEMP#
     C                   Z-ADD     SCTDIV        XXDIV
     C     ELKY          CHAIN     PEL01                              41
     C     *IN41         CABEQ     '1'           STRT
     C                   END
     C*
     C                   ENDIF                                                  Q1PRMS<=5
     C*
     C*  IF TO EMPLR#/DIV LEFT BLANK, THEN LOAD FROM EMPLR#/DIV INTO IT
     C*
     C     SCTEMP        IFEQ      *ZERO
     C     SCTDIV        ANDEQ     *ZERO
     C                   Z-ADD     SCFEMP        SCTEMP
     C                   Z-ADD     SCFDIV        SCTDIV
     C                   END
     C*
     C*  EMPLOYER RANGE MUST BE ASCENDING
     C*
     C                   MOVEL     SCFEMP        X1EMP            14 0
     C                   MOVE      SCFDIV        X1EMP
     C                   MOVEL     SCTEMP        X2EMP            14 0
     C                   MOVE      SCTDIV        X2EMP
     C  N70X1EMP         IFGT      X2EMP
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  ACTIVE EMPLOYER FLAG MUST BE Y OR N
     C*
     C  N70SCAEFL        IFNE      'Y'
     C     SCAEFL        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   READC     SCR01S                                 50
     C*
     C*  CHECK FOR DEPARTMENT SELECTION
     C*
     C     NXTRD1        TAG
     C     *IN50         IFEQ      '0'
     C*
     C**  IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C**  SELECT A RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C*
     C     SCSEL1        IFNE      '1'
     C     SCSEL1        ANDNE     ' '
     C                   MOVE      '1'           *IN24
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C*
     C**  IF THE SUBFILE HAS CHANGED BUT THE USER USED A BLANK         INDICATE
     C**  READ NEXT CHANGED RECORD (NXTRD1 TAG)
     C*
     C     SCSEL1        IFEQ      ' '
     C*
     C*  IF THE USER BLANKED OUT A PREVIOUS SELECTION BE SURE TO
     C*  CLEAR THE VARIABLE SVDPCD THAT MAY HAVE BEEN SET IF F7 =
     C*  CLAIM LETTER REQUEST SEARCH WAS USED
     C*
     C     SCDPCD        IFEQ      SVDPCD
     C                   MOVE      *BLANKS       SVDPCD
     C                   END                                                    SCDPCD = SVDPCD
     C                   READC     SCR01S                                 50
     C                   GOTO      NXTRD1
     C                   END                                                    END NE ' '
     C*
     C**  IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C**  SAVE THAT DATA TO BE PASSED TO NEXT SCREEN
     C**  PGMR NOTE:  ONLY 1ST DEPARTMENT WILL BE SELECTED & USED
     C*
     C     SCSEL1        IFEQ      '1'
     C*
     C**  LOAD SCR02 INFORMATION
     C*
     C                   EXSR      INIT2
     C                   EXSR      LOAD2
     C                   GOTO      STRT
     C*
     C                   END                                                    END EQ '1'
     C*
     C**  IF THE SUBFILE HAS NOT BEEN CHANGED
     C*
     C                   ELSE                                                   ELSE 0 *IN50
     C*
     C*  REDISPLAY SCR01 (SELECTION REQUIRED)
     C*
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN50
     C*
      *------------------------*
     C     SCR02T        TAG
      *------------------------*
     C*
     C*  CHECK FOR REPOSITION
     C*
     C                   Z-ADD     1             #@$2
     C     SCUDID        IFNE      *BLANKS
     C                   Z-ADD     1             W                 3 0
     C     SCUDID        LOOKUP    DID(W)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     W             #@$2
     C                   MOVE      *BLANKS       SCUDID
     C                   MOVE      '0'           *IN35
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     1             #@$2
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C                   MOVE      *BLANKS       SCUDID
     C                   END
     C                   MOVE      '0'           ERR               1
     C*
     C*  VALIDATE INFORMATION ENTERED:
     C*  CHECK FOR LETTER SELECTION
     C*
     C                   READC     SCR02S                                 50
     C*
     C     *IN50         IFEQ      '1'
     C*  SUBFILE HAS NOT CHANGED, REDISPLAY SCR02, SELECTION REQUIRED
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C*  SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*  VERIFY SELECTION
     C*
     C**  IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A CORRECT VALUE
     C**  SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C*
     C     SCSEL2        IFNE      '1'                                          INVALID VALUE
     C     SCSEL2        ANDNE     '5'
     C     SCSEL2        ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C*
     C**  IF THE SUBFILE HAS CHANGED BUT THE USER USED A BLANK
     C**  READ NEXT CHANGED RECORD (NXT2 TAG)
     C*
     C     SCSEL2        IFEQ      ' '
     C                   GOTO      NXT2
     C                   END                                                    END EQ ' '
     C*
     C**  IF THE SUBFILE HAS CHANGED AND USER HAS REQUESTED TO VIEW A DOCUMENT
     C**  VIEW DOCUMENT, THEN READ NEXT CHANGED RECORD (NXT2 TAG)
     C*
     C     SCSEL2        IFEQ      '5'
     C                   MOVEL     SCDCID        QQDCID
     C                   MOVEL     SCFLDR        QQFLDR
     C                   MOVE      *ZEROS        XXSQ#             5
     C                   MOVEL     *BLANKS       XXKY             40
     C                   MOVEL     *BLANKS       XXFL03            1
     C                   MOVEL     *BLANK        QQRTCD            1
     C** ADD 5/27/06 ****************************************************
     C*
     C     DTMINS        IFEQ      'Y'
     C     X$EDTR        ANDEQ     '*MSWORD'
     C                   CALL      'DTDSPDOC'
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C                   PARM                    X$EDTR
     C                   PARM      *BLANKS       QQRTCD
     C*
     C                   GOTO      BYPDT
     C*
     C                   ENDIF
     C** END ADD 5/27/06 ****************************************************
     C                   CALL      'SDSPDOC'
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C                   PARM                    XXKY
     C                   PARM                    XXSQ#
     C                   PARM                    XXFL03
     C                   PARM                    QQRTCD
     C                   PARM      *BLANKS       QQMSFL            1
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM      *BLANK        QQUNCN          255
     C                   PARM      *BLANK        QQFSF             1
     C*** End   Add *** 10/17/11 **************************************
     C** ADD 5/27/06 ****************************************************
     C     BYPDT         TAG
     C                   MOVEL     SCOUTQ        XX8A              8
     C     XX8A          IFEQ      '*INTERAC'
     C                   MOVE      *ON           *IN84
     C                   ELSE
     C                   MOVE      *OFF          *IN84
     C                   ENDIF
     C** END ADD 5/27/06 ****************************************************
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C     QQRTCD        IFEQ      '3'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C     QQRTCD        IFEQ      '4'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      NXT2
     C                   END
     C                   MOVE      SCSEL2        HLSEL2            1
     C                   MOVE      *BLANKS       SCSEL2
     C                   UPDATE    SCR02S
     C                   GOTO      STRT
     C                   END                                                    END EQ '5'
     C*
     C*   IF ADD:
     C**  CHECK NEXT PROCESSING ON RECORD (I.E. VALIDATION, ADDITIONAL INPUT)
     C*
     C     SCSEL2        IFEQ      '1'
     C*
     C*  DOCUMENT OUTPUT QUEUE MUST BE VALID ON SYSTEM.
     C*** ADD 5/27/06 ******************************************************
     C                   MOVEL     SCOUTQ        XX8A              8
     C     XX8A          IFEQ      '*INTERAC'
     C                   MOVE      *ON           *IN84
     C                   ELSE
     C                   MOVE      *OFF          *IN84
     C                   ENDIF
     C     *IN84         IFEQ      *OFF
     C*** END ADD 5/27/06 ******************************************************
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XFOUND            1
     C     XFOUND        IFEQ      '1'
     C                   MOVE      *ON           *IN28                          ERROR
     C                   MOVE      *ON           *IN29                          RECORD CHANGED
     C                   UPDATE    SCR02S
     C                   MOVE      *OFF          *IN29
     C                   GOTO      STRT
     C                   END
     C*** ADD 5/27/06 ******************************************************
     C                   ENDIF
     C*** END ADD 5/27/06 ******************************************************
     C*
     C*         IF THIS IS A NEW DOCUMENT REQUEST W/O ADDITIONAL INPUT,
     C*         WRITE RECORD IN WSDLP
     C*         THEN RETURN TO NXT2 TAG TO LOOK FOR OTHER CHOICES
     C*
     C     SCAINP        IFEQ      *BLANK
     C                   EXSR      WRTUPD
     C                   MOVE      SCSEL2        HLSEL2
     C                   MOVE      *BLANKS       SCSEL2
     C                   UPDATE    SCR02S
     C                   EXSR      SUBMIT
     C                   GOTO      NXT2
     C*
     C                   ELSE                                                   ELSE EQ SCAINP
     C*
     C**  A NEW REQUEST WITH ADDITIONAL INPUT WILL CALL THE APPROPIRATE
     C*   PROGRAM, WRITE A DOCUMENT REQUEST RECORD TO WSDLP AND RETURN TO
     C*   NXT2 TAG TO LOOK FOR OTHER CHOICES
     C**  IF NO SCREEN HAS BEEN INDICATED FOR THIS ADDTL INPUT,
     C**  SEND ERROR MESSAGE
     C*
     C     SCPGMN        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           ERR
     C                   UPDATE    SCR02S
     C                   GOTO      STRT
     C                   END                                                    END EQ *BLKS
     C*
     C                   MOVE      SCSEL2        HLSEL2
     C                   MOVE      *BLANKS       SCSEL2
     C*
     C*   CALL ADDITIONAL PROGRAM
     C*
     C                   EXSR      WRTUPD
     C                   EXSR      ADDPGM
     C*
     C     QQRTCD        IFEQ      'A'                                          F3 PRESSED
     C                   GOTO      TERM
     C                   END
     C*
     C     QQRTCD        IFEQ      'B'                                          F12 PRESSED
     C                   MOVE      *BLANK        SCOUTQ
     C                   MOVE      *ZERO         SC#CPY
     C                   ELSE
     C                   END
     C*
     C                   UPDATE    SCR02S
     C*
     C     QQRTCD        IFEQ      'B'                                          F12 PRESSED
     C                   MOVE      *BLANK        QQRTCD
     C                   ELSE
     C                   EXSR      SUBMIT
     C                   END
     C*
     C                   GOTO      NXT2
     C                   END                                                    END EQ SCAINP
     C*
     C                   END                                                    END EQ '1'
     C*
     C**  READ NEXT CHANGED RECORD
     C*
     C     NXT2          TAG
     C*
     C                   READC     SCR02S                                 50
     C                   ENDDO                                                  END DOWEQ *IN50
     C*
     C   70              GOTO      TERM
     C*
     C**                   Z-ADD*ZERO     SCFYR
     C**                   Z-ADD*ZERO     SCFEMP
     C**                   Z-ADD*ZERO     SCFDIV
     C**                   Z-ADD*ZERO     SCTEMP
     C**                   Z-ADD*ZERO     SCTDIV
     C**                   MOVE 'N'       SCAEFL
     C*
     C                   MOVE      *OFF          *IN26                          RESET INV SEL SCR01
     C                   MOVE      *OFF          *IN50                          RESET
     C                   EXSR      INIT1
     C                   EXSR      LOAD1                                        LOAD RCDS FROM FILE
     C*
     C                   GOTO      STRT
     C*
      *=======================
      * * *   END OF PROGRAM
      *=======================
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT1 SUBROUTINE
      *====================================================================*
     C     INIT1         BEGSR
     C                   Z-ADD     0             RRN1              5 0
     C                   MOVE      *BLANKS       SCSEL1
     C                   MOVE      *BLANKS       HLDPCD
     C                   MOVE      *BLANKS       HLDPNM
     C                   MOVE      *BLANKS       BYPASS
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C**  SCR01
     C     DEKY          SETLL     PDE01                              60
     C*
     C**  CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   ENDSR
      *====================================================================*
      * * *   INIT2 SUBROUTINE
      *====================================================================*
     C     INIT2         BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C                   MOVE      *BLANKS       HLSEL2
     C                   MOVE      *BLANKS       SCSEL2
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *BLANKS       SCFLDR
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANKS       SCUDID
     C                   MOVE      '1'           *IN35
     C                   MOVE      *BLANKS       DID
     C*
     C* REPOSITION FILE
     C*
     C     DOKY          SETLL     PDO01
     C*
     C* CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C*
     C                   ENDSR
      *====================================================================*
      * * *   INITIALIZE ADDITIONAL INPUT FILE FIELDS
      *====================================================================*
     C     INITAF        BEGSR
     C                   CLEAR                   PDL02
     C                   ENDSR
      *====================================================================*
      * * *   LOAD1 - LOAD SCR01 - SUBROUTINE
      *====================================================================*
     C     LOAD1         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 10 LINES
     C*
      ***** BEG DEL ******* 08/08/00 **********************************
     C**********           DO   10        X       50
      ***** END DEL ******* 08/08/00 **********************************
      ***** BEG ADD ******* 08/08/00 **********************************
      *
     C                   Z-ADD     1             X                 5 0
      *
     C     X             DOUEQ     11
     C     *IN60         OREQ      *ON
      ***** END ADD ******* 08/08/00 **********************************
      *
     C     DEKY1         READE     PDE01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     DESTAT        ANDEQ     'A'
     C                   ADD       1             RRN1
     C                   MOVEL     DEDPCD        SCDPCD
     C                   MOVEL     DEDPNM        SCDPNM
      *
     C     *IN99         IFEQ      '1'
     C     SVDPCD        OREQ      DEDPCD
     C                   MOVE      '1'           SCSEL1
     C                   MOVEL     DEDPCD        HLDPCD
     C                   MOVEL     DEDPNM        HLDPNM
     C                   MOVE      '1'           BYPASS            1
     C                   MOVE      '0'           *IN99
      *
     C                   ELSE
      *
     C                   MOVE      ' '           SCSEL1
     C                   END
      *
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
      ***** BEG ADD ******* 08/08/00 **********************************
     C                   ADD       1             X
      ***** END ADD ******* 08/08/00 **********************************
      *
     C                   ELSE
      *
     C                   MOVE      '1'           *IN05
     C                   END
      *
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN1          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      *BLANKS       SCDPNM
     C                   Z-ADD     1             RRN1
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C**  TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN1          #@$1
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD2 - LOAD SCR02 -  SUBROUTINE
      *====================================================================*
     C     LOAD2         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   Z-ADD     *ZEROS        Z                 3 0
     C                   MOVE      '0'           *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     DOKY          READE     PDO01                                  60
     C*
     C*
     C     *IN60         IFEQ      '0'
     C     DOSTAT        ANDEQ     'A'
     C                   ADD       1             RRN2
     C                   MOVEL     DODCID        SCDCID
     C                   ADD       1             Z                 3 0
     C                   MOVEL     DODCID        DID(Z)
     C                   MOVEL     DOFLDR        SCFLDR
     C                   MOVEL     DODESC        SCDESC
     C                   MOVEL     DOPGMN        SCPGMN
     C     DOAIFL        IFEQ      'Y'
     C                   MOVEL     'YES'         SCAINP
     C                   ELSE                                                   ELSE Y DOAIFL
     C                   MOVEL     '   '         SCAINP
     C                   END                                                    END Y DOAIFL
     C*
     C                   MOVE      *ZEROS        SC#CPY
     C*** BEGIN CHG ***** 09/01/95 ************************************
     C**********           MOVE *BLANKS   SCPRV2
     C                   MOVEL     UPOUTQ        SCOUTQ
     C*** END CHG ******* 09/01/95 ************************************
     C**** ADD 5/27/06 *****************************************************
     C*** IF THE DOINTR FLAG IS SET TO "I" THEN THIS IS TO BE INTERACTIVE REQUES
     C***  REQUEST.  PUT THE "*INTERACT*" INTO THE OUTQ FIELD, SET INDICATOR TO
     C***  MAKE PROTECTED, (OUTQ AND #COPIES).  NO UNDERLINE, HIGHLIGHT?
     C***  WILL ALSO CHANGE THE EDITING FROM SCR02S TO NOT EDIT VALUES IF THE IN
     C***  INDICATOR IS ON (MEANING INTERACTIVE).     (ONLY IF DTM IS INSTALLED)
     C***
     C***
     C                   MOVE      *OFF          *IN84
     C     DOINTR        IFEQ      'I'
     C     DTMINS        ANDEQ     'Y'
     C                   MOVEL     '*INTERAC'    SCOUTQ
     C                   MOVE      'T*'          SCOUTQ
     C                   MOVE      *ON           *IN84
     C                   ENDIF
     C****
     C***
     C**
     C**** END ADD 5/27/06 *************************************************
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR02S
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCSEL2
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVE      *BLANKS       SCFLDR
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPGMN
     C                   MOVE      *BLANKS       SCAINP
     C                   MOVE      *ZEROS        SC#CPY
     C*** BEGIN ADD ***** 09/01/95 ************************************
     C                   MOVE      *BLANKS       SCOUTQ
     C*** END ADD ******* 09/01/95 ************************************
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR02S
     C                   END
     C*
     C**  TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     1             #@$2
     C*
     C                   ENDSR
      *====================================================================*
      * * *   WRITE/UPDATE WORKFILE WSDLP - POLICY DEPT LETTER WORKFILE
      *====================================================================*
     C     WRTUPD        BEGSR
     C*
     C*  CALL CNTROL TO OBTAIN A 'REQUEST NUMBER'
     C*
     C                   EXSR      CNTROL
     C*
     C*  THIS IS A NEW REQUEST, INITIALIZE WORKFILE FIELDS
     C*
     C                   EXSR      INITAF
     C*
     C                   Z-ADD     XXCO#         DLCO#
     C                   Z-ADD     XXFND         DLFND
     C                   Z-ADD     SCFYR         DLFYR
     C                   Z-ADD     SCFEMP        DLFEMP
     C                   Z-ADD     SCFDIV        DLFDIV
     C                   Z-ADD     SCTEMP        DLTEMP
     C                   Z-ADD     SCTDIV        DLTDIV
     C                   MOVE      SCAEFL        DLAEFL
     C                   Z-ADD     XXREQ#        DLREQ#
     C*
     C                   MOVE      SCDPCD        DLDPCD
     C                   MOVE      SCFLDR        DLFLDR
     C                   MOVE      SCDCID        DLDCID
     C     SC#CPY        IFNE      *ZEROS
     C                   Z-ADD     SC#CPY        DL#CPY
     C                   ELSE
     C                   Z-ADD     1             DL#CPY
     C                   END
     C                   MOVE      HLCALL        DLQTFL
     C                   MOVE      'A'           DLSTAT
     C                   MOVEL     Q1USER        DLIUSR
     C                   MOVEL     Q1PGM         DLIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLIDT
     C                   Z-ADD     DSTIME        DLITIM
     C                   MOVE      SCOUTQ        DLOUTQ
     C**** ADD 5/27/06 *************************************************
     C                   MOVEL     DLOUTQ        XX8A              8
     C     XX8A          IFEQ      '*INTERAC'
     C                   MOVEL     'QPRINT  '    DLOUTQ
     C                   MOVE      '  '          DLOUTQ
     C                   ENDIF
     C**** END ADD 5/27/06 *************************************************
     C*
     C                   WRITE     PDL02
     C                   MOVE      'Y'           XXWRUP            1
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* ADDPGM SUBROUTINE              ** Subroutine **   A D D P G M *
     C*****************************************************************
     C     ADDPGM        BEGSR
     C*
     C*   ASSIGN FIELDS TO PARMS DS, THEN CALL ADDITIONAL PROGRAM
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXREQ#        QQREQ#
     C                   MOVE      SCDPCD        QQDPCD
     C                   MOVE      SCDCID        QQDCID
     C                   MOVE      SCDESC        QQDESC
     C                   MOVE      SCFLDR        QQFLDR
     C                   MOVE      SCFEMP        QQFEMP
     C                   MOVE      SCFDIV        QQFDIV
     C                   MOVE      SCTEMP        QQTEMP
     C                   MOVE      SCTDIV        QQTDIV
     C*
     C                   CALL      SCPGMN
     C                   PARM                    QQPRDS           40
     C                   PARM                    PARMS
     C                   PARM                    XXDEPT
     C                   PARM                    QQRTCD
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SUBMIT SUBROUTINE              ** Subroutine **   S U B M I T *
     C*****************************************************************
     C     SUBMIT        BEGSR
     C*
     C*   ASSIGN PARM FIELDS, THEN CALL SUBMITTER CLP
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXREQ#        QQREQ#
     C                   MOVE      SCDPCD        QQDPCD
     C                   MOVE      SCDCID        QQDCID
     C                   MOVE      SCFLDR        QQFLDR
     C*
     C                   CALL      'W4115C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQREQ#
     C                   PARM                    QQDPCD
     C                   PARM                    QQFLDR
     C                   PARM                    QQDCID
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* CNTROL - SUBROUTINE TO MAINTAIN A UNIQUE REQUEST NUMBER
     C*****************************************************************
     C     CNTROL        BEGSR
     C*
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C*
     C     DLKY          CHAIN     PDL01                              65
     C*
     C     *IN65         IFEQ      *OFF
     C*
     C     HIREQ#        IFLT      99999
     C     HIREQ#        ADD       1             HIREQ#
     C                   ELSE
     C                   Z-ADD     1             HIREQ#
     C                   END
     C*
     C*  UPDATE CONTROL RECORD
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C                   Z-ADD     DSTIME        HITIME
     C                   MOVEL     Q1USER        HIUSER
     C                   MOVEL     Q1USER        HIPGM
     C*
     C                   UPDATE    PDL01
     C*
     C                   ELSE
     C*
     C*  WRITE THE CONTROL RECORD FOR DEPARTMENT REQUESTS
     C*
     C                   Z-ADD     1             HIREQ#
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C                   Z-ADD     *HIVAL        HIFYR
     C                   Z-ADD     *HIVAL        HIFEMP
     C                   Z-ADD     *HIVAL        HIFDIV
     C                   Z-ADD     *HIVAL        HITEMP
     C                   Z-ADD     *HIVAL        HITDIV
     C                   MOVE      *BLANKS       HIAEFL
     C                   MOVE      *BLANKS       HIDPCD
     C                   MOVE      *BLANKS       HIFLDR
     C                   MOVE      *BLANKS       HIDCID
     C                   Z-ADD     *HIVAL        HI#CPY
     C                   MOVE      *BLANKS       HIQTFL
     C                   MOVE      'A'           HISTAT
     C                   MOVE      Q1USER        HIIUSR
     C                   MOVE      Q1PGM         HIIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C                   Z-ADD     DSTIME        HIITIM
     C                   WRITE     PDL01
     C                   END
     C*
     C                   Z-ADD     HIREQ#        XXREQ#
     C*
     C                   ENDSR
     C*
